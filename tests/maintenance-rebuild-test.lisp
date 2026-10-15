(handler-bind ((warning #'muffle-warning))
  (ql:quickload "pai-v3-maintenance" :silent t))

(in-package #:pai-v3.test)
(use-package '#:pai-v3.maintenance)

(define-event-type "test.tick" :schema (("n" :integer)))

(define-projection "test.sum"
  :kind :inline :consumes ("test.tick")
  :tables ("CREATE TABLE IF NOT EXISTS t_sum (k INTEGER PRIMARY KEY CHECK(k=1), total INTEGER NOT NULL, count INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (sql-exec db "INSERT INTO t_sum(k,total,count) VALUES(1,?1,1) ON CONFLICT(k) DO UPDATE SET total=total+excluded.total,count=count+1"
                      (obj-get (event-payload e) "n"))))

(define-projection "test.model"
  :kind :async :consumes ("test.tick") :derivation :model
  :reduce (lambda (db e) (declare (ignore db e))))

(defun sum-row (k)
  (sql-row (kernel-db k) "SELECT total,count FROM t_sum WHERE k=1 LIMIT 1"))

(defun bump-version (name delta)
  (incf (pai-v3.kernel::projection-version (find-projection name)) delta))

(deftest rebuild-restores-a-needs-rebuild-projection
  (let ((path (format nil "/tmp/pai-v3-rb-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path)
      (dotimes (i 25) (append! k "test.tick" (obj "n" i))))
    (bump-version "test.sum" 1)
    (unwind-protect
         (with-temp-kernel (k :path path)
           (is (equal "needs-rebuild" (projection-status k "test.sum")))
           (is (eq :done (rebuild-projection k "test.sum" :page-size 7)))
           (is (equal "ready" (projection-status k "test.sum")))
           (is (equal '(300 25) (sum-row k)))
           (append! k "test.tick" (obj "n" 1000))
           (is (equal '(1300 26) (sum-row k)) "inline application resumes"))
      (bump-version "test.sum" -1))))

(deftest rebuild-is-resumable-after-interruption
  (with-temp-kernel (k)
    (dotimes (i 25) (append! k "test.tick" (obj "n" 1)))
    (is (eq :paused (rebuild-projection k "test.sum" :page-size 5 :max-pages 2)))
    (is (equal "rebuilding" (projection-status k "test.sum")))
    (is (= 10 (projection-cursor k "test.sum")) "cursor persisted per page")
    (append! k "test.tick" (obj "n" 1))
    (is (eq :done (rebuild-projection k "test.sum" :page-size 5)))
    (is (equal '(26 26) (sum-row k)) "no event lost or applied twice")))

(deftest model-derived-projections-are-never-rebuilt
  (with-temp-kernel (k)
    (signals model-derived-rebuild-refused (rebuild-projection k "test.model"))))

(deftest verify-matches-incremental-state
  (with-temp-kernel (k)
    (dotimes (i 40) (append! k "test.tick" (obj "n" (* i i))))
    (multiple-value-bind (ok live rebuilt)
        (verify-projection k "test.sum" (format nil "/tmp/pai-v3-verify-~d.sqlite3" (random 1000000)))
      (is ok (format nil "~a vs ~a" live rebuilt)))
    ;; Corrupt the live row: verification must notice.
    (sql-exec (kernel-db k) "UPDATE t_sum SET total=total+1 WHERE k=1")
    (is (not (verify-projection k "test.sum" (format nil "/tmp/pai-v3-verify-~d.sqlite3" (random 1000000)))))))

(run-suite "maintenance-rebuild")
