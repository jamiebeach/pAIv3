(in-package #:pai-v3.test)

(define-event-type "test.note" :schema (("thread" :string) ("text" :string)))
(define-event-type "test.other" :schema (("x" :integer)))

;; Inline: count notes per thread. A text of "boom" makes the reducer fail.
(define-projection "test.threads"
  :kind :inline :consumes ("test.note")
  :tables ("CREATE TABLE IF NOT EXISTS t_threads (thread TEXT PRIMARY KEY, notes INTEGER NOT NULL)")
  :entity (lambda (e) (obj-get (event-payload e) "thread"))
  :reduce (lambda (db e)
            (let ((thread (obj-get (event-payload e) "thread")))
              (sql-exec db "INSERT INTO t_threads(thread,notes) VALUES(?1,1) ON CONFLICT(thread) DO UPDATE SET notes=notes+1" thread)
              (when (string= "boom" (obj-get (event-payload e) "text"))
                (error "reducer exploded")))))

;; Async: total length of note text.
(define-projection "test.length"
  :kind :async :consumes ("test.note")
  :tables ("CREATE TABLE IF NOT EXISTS t_length (k INTEGER PRIMARY KEY CHECK(k=1), total INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (sql-exec db "INSERT INTO t_length(k,total) VALUES(1,?1) ON CONFLICT(k) DO UPDATE SET total=total+excluded.total"
                      (length (obj-get (event-payload e) "text")))))

(defun notes (k thread)
  (sql-value (kernel-db k) "SELECT notes FROM t_threads WHERE thread=?1 LIMIT 1" thread))

(deftest inline-applies-in-append-transaction
  (with-temp-kernel (k)
    (is (equal "ready" (projection-status k "test.threads")))
    (append! k "test.note" (obj "thread" "a" "text" "one"))
    (append! k "test.note" (obj "thread" "a" "text" "two"))
    (append! k "test.other" (obj "x" 1))
    (is (= 2 (notes k "a")))
    (is (= (ledger-head k) (projection-cursor k "test.threads")) "cursor follows head")))

(deftest reducer-failure-quarantines-and-degrades-but-never-blocks
  (with-temp-kernel (k)
    (append! k "test.note" (obj "thread" "a" "text" "fine"))
    (let ((bad (append! k "test.note" (obj "thread" "a" "text" "boom"))))
      (is (event-by-id k (event-id bad)) "the failing event is still committed")
      (is (= 1 (notes k "a")) "the failing event's partial writes are rolled back")
      (is (= 1 (length (quarantined-events k :projection "test.threads"))))
      (is (equal "a" (first (first (degraded-entities k "test.threads"))))
          "the affected entity is marked degraded"))
    (append! k "test.note" (obj "thread" "a" "text" "after"))
    (is (= 2 (notes k "a")) "later events still apply")))

(deftest async-steps-are-bounded-and-resumable
  (with-temp-kernel (k)
    (dotimes (i 10) (append! k "test.note" (obj "thread" "t" "text" "abc")))
    (is (= 4 (step-async-projection k "test.length" :max-events 4)))
    (is (= 12 (sql-value (kernel-db k) "SELECT total FROM t_length LIMIT 1")))
    (is (= 4 (step-async-projection k "test.length" :max-events 4)))
    (is (= 2 (step-async-projection k "test.length" :max-events 4)))
    (is (= 0 (step-async-projection k "test.length" :max-events 4)))
    (is (= 30 (sql-value (kernel-db k) "SELECT total FROM t_length LIMIT 1")))
    (is (= (ledger-head k) (projection-cursor k "test.length")))))

(deftest reopen-keeps-state-without-reading-history
  (let ((path (format nil "/tmp/pai-v3-reopen-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path)
      (append! k "test.note" (obj "thread" "r" "text" "x")))
    (with-temp-kernel (k :path path)
      (is (equal "ready" (projection-status k "test.threads")))
      (append! k "test.note" (obj "thread" "r" "text" "y"))
      (is (= 2 (notes k "r"))))))

(deftest new-projection-on-existing-history-needs-rebuild
  (let ((path (format nil "/tmp/pai-v3-new-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path)
      (append! k "test.note" (obj "thread" "n" "text" "x")))
    (define-projection "test.late"
      :kind :inline :consumes ("test.note") :reduce (lambda (db e) (declare (ignore db e))))
    (unwind-protect
         (with-temp-kernel (k :path path)
           (is (equal "needs-rebuild" (projection-status k "test.late")) "never silently replayed")
           (is (equal "test.late" (first (first (projections-not-ready k)))) "listed as not ready")
           (is (search "test.late (needs-rebuild: new projection over existing history)" (not-ready-warning k))
               "the warning names it and why")
           (append! k "test.note" (obj "thread" "n" "text" "y"))
           (is (equal "ready" (projection-status k "test.threads")) "others unaffected"))
      (remhash "test.late" pai-v3.kernel::*projections*))))

(deftest a-ready-agent-has-no-warning
  (with-temp-kernel (k)
    (append! k "test.note" (obj "thread" "w" "text" "x"))
    (is (null (projections-not-ready k)))
    (is (null (not-ready-warning k)))))

(deftest version-bump-needs-rebuild
  (let ((path (format nil "/tmp/pai-v3-ver-~d.sqlite3" (random 1000000)))
        (p (find-projection "test.threads")))
    (with-temp-kernel (k :path path)
      (append! k "test.note" (obj "thread" "v" "text" "x")))
    (setf (pai-v3.kernel::projection-version p) 2)
    (unwind-protect
         (with-temp-kernel (k :path path)
           (is (equal "needs-rebuild" (projection-status k "test.threads")))
           (append! k "test.note" (obj "thread" "v" "text" "y"))
           (is (null (notes k "v")) "its tables start again empty, and nothing is applied until a rebuild"))
      (setf (pai-v3.kernel::projection-version p) 1))))

(deftest a-version-change-may-change-the-tables
  (let ((path (format nil "/tmp/pai-v3-layout-~d.sqlite3" (random 1000000)))
        (p (find-projection "test.threads")))
    (with-temp-kernel (k :path path)
      (append! k "test.note" (obj "thread" "w" "text" "x")))
    (let ((old-tables (pai-v3.kernel::projection-tables p)))
      (setf (pai-v3.kernel::projection-version p) 2
            (pai-v3.kernel::projection-tables p)
            (list "CREATE TABLE IF NOT EXISTS t_threads (thread TEXT PRIMARY KEY, notes INTEGER NOT NULL, colour TEXT)"
                  "CREATE INDEX IF NOT EXISTS t_threads_colour ON t_threads(colour)"))
      (unwind-protect
           (with-temp-kernel (k :path path)
             (is (equal "needs-rebuild" (projection-status k "test.threads"))
                 "it opens: the index on the new column meets the new table")
             (is (null (sql-value (kernel-db k) "SELECT colour FROM t_threads WHERE thread=?1 LIMIT 1" "w"))))
        (setf (pai-v3.kernel::projection-version p) 1
              (pai-v3.kernel::projection-tables p) old-tables)))))

(run-suite "kernel-projections")
