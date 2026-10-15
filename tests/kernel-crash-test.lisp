(in-package #:pai-v3.test)

;;; REL-1: a process killed in the middle of an append or an async step loses
;;; nothing committed and applies nothing twice. The crash happens in a child
;;; SBCL that dies abruptly (no unwinding, no cleanup) inside the transaction.

(define-event-type "crash.tick" :schema (("n" :integer)))

(defparameter *crash-defs* "
(define-event-type \"crash.tick\" :schema ((\"n\" :integer)))
(define-projection \"crash.inline\" :kind :inline :consumes (\"crash.tick\")
  :tables (\"CREATE TABLE IF NOT EXISTS c_inline (k INTEGER PRIMARY KEY CHECK(k=1), total INTEGER NOT NULL)\")
  :reduce (lambda (db e)
            (sql-exec db \"INSERT INTO c_inline(k,total) VALUES(1,?1) ON CONFLICT(k) DO UPDATE SET total=total+excluded.total\"
                      (obj-get (event-payload e) \"n\"))
            (when (= 13 (obj-get (event-payload e) \"n\"))
              (when (probe-file \"/tmp/crash-inline\") (sb-ext:exit :abort t)))))
(define-projection \"crash.async\" :kind :async :consumes (\"crash.tick\")
  :tables (\"CREATE TABLE IF NOT EXISTS c_async (k INTEGER PRIMARY KEY CHECK(k=1), total INTEGER NOT NULL)\")
  :reduce (lambda (db e)
            (sql-exec db \"INSERT INTO c_async(k,total) VALUES(1,?1) ON CONFLICT(k) DO UPDATE SET total=total+excluded.total\"
                      (obj-get (event-payload e) \"n\"))
            (when (= 7 (obj-get (event-payload e) \"n\"))
              (when (probe-file \"/tmp/crash-async\") (sb-ext:exit :abort t)))))
")

;; The parent declares the same projections so it can reopen the store.
(eval (let ((*package* (find-package '#:pai-v3.test)))
        `(progn ,@(with-input-from-string (s *crash-defs*)
                    (loop for f = (read s nil s) until (eq f s) collect f)))))

(defun run-child (path body)
  "Run BODY (a string of forms) in a fresh SBCL against store PATH."
  (let ((script (format nil "/tmp/pai-v3-child-~d.lisp" (random 1000000))))
    (with-open-file (out script :direction :output :if-exists :supersede)
      (format out "(load \"/opt/quicklisp/setup.lisp\")
(push #p\"/v3/\" asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (ql:quickload \"pai-v3\" :silent t))
(defpackage #:child (:use #:cl #:pai-v3.kernel))
(in-package #:child)
~a
(defvar *k* (open-kernel ~s :agent-id \"crash-agent\" :key (ironclad:hex-string-to-byte-array ~s)))
~a
(close-kernel *k*)
(sb-ext:exit :code 0)" *crash-defs* path (ironclad:byte-array-to-hex-string *test-key*) body))
    (prog1 (sb-ext:process-exit-code
            (sb-ext:run-program "/usr/bin/sbcl"
                                (list "--noinform" "--non-interactive" "--load" script)
                                :output nil :error nil))
      (delete-file script))))

(defun c-total (k table)
  (or (sql-value (kernel-db k) (format nil "SELECT total FROM ~a WHERE k=1 LIMIT 1" table)) 0))

(deftest kill-mid-append-loses-only-the-uncommitted-batch
  (let ((path (format nil "/tmp/pai-v3-crash-~d.sqlite3" (random 1000000))))
    (ignore-errors (delete-file "/tmp/crash-async"))
    (with-open-file (s "/tmp/crash-inline" :direction :output :if-exists :supersede))
    (unwind-protect
         (is (/= 0 (run-child path "
(dotimes (i 10) (append! *k* \"crash.tick\" (obj \"n\" (1+ i))))
(append-batch! *k* (list (list \"crash.tick\" (obj \"n\" 11))
                         (list \"crash.tick\" (obj \"n\" 12))
                         (list \"crash.tick\" (obj \"n\" 13))))"))
             "child died inside the append transaction")
      (delete-file "/tmp/crash-inline"))
    (with-temp-kernel (k :path path :agent-id "crash-agent")
      (is (= 10 (ledger-head k)) "the interrupted batch left no rows")
      (is (= 55 (c-total k "c_inline")) "projection matches committed events exactly")
      (is (= 10 (projection-cursor k "crash.inline")))
      (append! k "crash.tick" (obj "n" 100))
      (is (= 155 (c-total k "c_inline")) "the store continues normally"))))

(deftest kill-mid-async-step-applies-each-event-exactly-once
  (let ((path (format nil "/tmp/pai-v3-crash-~d.sqlite3" (random 1000000))))
    (ignore-errors (delete-file "/tmp/crash-inline"))
    (run-child path "(dotimes (i 10) (append! *k* \"crash.tick\" (obj \"n\" (1+ i))))
(step-async-projection *k* \"crash.async\" :max-events 4)")
    (with-open-file (s "/tmp/crash-async" :direction :output :if-exists :supersede))
    (unwind-protect
         (is (/= 0 (run-child path "(step-async-projection *k* \"crash.async\" :max-events 4)"))
             "child died inside the second step")
      (delete-file "/tmp/crash-async"))
    (with-temp-kernel (k :path path :agent-id "crash-agent")
      (is (= 10 (c-total k "c_async")) "only the first committed step is visible")
      (is (= 4 (projection-cursor k "crash.async")))
      (loop while (plusp (step-async-projection k "crash.async" :max-events 4)))
      (is (= 55 (c-total k "c_async")) "every event applied exactly once"))))

(run-suite "kernel-crash")
