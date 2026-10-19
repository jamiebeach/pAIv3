(in-package #:pai-v3.test)

(deftest connection-printing-hides-credentials
  (let ((connection (pai-v3.kernel::%make-db nil "synthetic-private-path")))
    (setf (pai-v3.kernel::db-key connection) "SYNTHETIC-KEY-MUST-NOT-PRINT")
    (let ((text (prin1-to-string (list connection))))
      (is (search "DB" text))
      (is (not (search "SYNTHETIC-KEY" text)))
      (is (not (search "synthetic-private-path" text))))))

(declaim (notinline diagnostic-credential-frame))
(defun diagnostic-credential-frame (credential)
  (declare (optimize (debug 3) (speed 0)))
  (prog1 (with-output-to-string (out)
           (pai-v3.kernel::print-safe-backtrace out :count 40))
    (assert (stringp credential))))

(deftest backtraces-keep-functions-without-arguments
  (let ((text (diagnostic-credential-frame "SYNTHETIC-TOKEN-MUST-NOT-PRINT")))
    (is (search "DIAGNOSTIC-CREDENTIAL-FRAME" text))
    (is (not (search "SYNTHETIC-TOKEN" text)))
    (is (not (search "remaining frames unavailable" text)))))
(run-suite "cli-diagnostics")
