(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; The watchdog: an agent that stops answering is restarted, with a record of
;;; what every thread was doing, and says so afterwards. The restart itself is
;;; replaced here by a recorder.

;;; A kernel in a directory of its own, so the files the watchdog leaves beside the ledger are countable.
(defmacro with-wd-kernel ((k) &body body)
  `(let ((dir (format nil "/tmp/pai-wd-~d/" (random 100000000))))
     (ensure-directories-exist dir)
     (let ((pai-v3.api::*watchdog-dump-wait* 0.3))
       (with-temp-kernel (,k :path (format nil "~aledger.db" dir)) ,@body))))

(deftest it-waits-before-it-restarts
  (let ((since nil))
    (multiple-value-bind (next action) (pai-v3.api::watchdog-step since t 1000 120)
      (is (null next)) (is (eq :ok action) "healthy: nothing to do"))
    (multiple-value-bind (next action) (pai-v3.api::watchdog-step nil nil 1000 120)
      (is (= 1000 next)) (is (eq :unhealthy action) "the first failure starts the clock")
      (setf since next))
    (multiple-value-bind (next action) (pai-v3.api::watchdog-step since nil 1100 120)
      (is (= 1000 next)) (is (eq :unhealthy action) "still within the allowance")
      (setf since next))
    (multiple-value-bind (next action) (pai-v3.api::watchdog-step since nil 1120 120)
      (declare (ignore next))
      (is (eq :restart action) "failed for the whole allowance"))
    (multiple-value-bind (next action) (pai-v3.api::watchdog-step since t 1119 120)
      (is (null next)) (is (eq :ok action) "one good answer clears it"))))

(deftest a-wedged-agent-leaves-its-evidence-and-exits
  (with-wd-kernel (k)
    (let* ((exits 0)
           (pai-v3.api::*watchdog-exit* (lambda () (incf exits)))
           (dir (pai-v3.api::%state-dir k)))
      (pai-v3.api::%restart-wedged k 150 "2026-10-19T14:38:52Z")
      (is (= 1 exits) "it ended the process")
      (let ((dumps (directory (merge-pathnames "watchdog-*.txt" dir))))
        (is (= 1 (length dumps)) "and saved every thread's backtrace")
        (is (search "watchdog thread dump" (uiop:read-file-string (first dumps)))))
      (is (probe-file (pai-v3.api::%pending-file dir)) "with a note for the next start")
      (is (= 1 (pai-v3.api::%recent-exits dir)))
      ;; A restart that does not cure it must not become a loop.
      (pai-v3.api::%restart-wedged k 150 "x") (pai-v3.api::%restart-wedged k 150 "x")
      (is (= 3 exits))
      (pai-v3.api::%restart-wedged k 150 "x")
      (is (= 3 exits) "no more than three an hour"))))

(deftest only-the-newest-dumps-are-kept
  (with-wd-kernel (k)
    (let ((dir (pai-v3.api::%state-dir k)) (pai-v3.api::*watchdog-keep-dumps* 2))
      (dolist (stamp '("20260101T000001Z" "20260101T000002Z" "20260101T000003Z"))
        (with-open-file (o (merge-pathnames (format nil "watchdog-~a.txt" stamp) dir) :direction :output :if-exists :supersede)
          (write-line "old" o)))
      (pai-v3.api::%write-thread-dump dir)
      (is (= 2 (length (directory (merge-pathnames "watchdog-*.txt" dir)))) "the oldest were removed"))))

(deftest the-next-start-says-what-happened
  (with-wd-kernel (k)
    (let ((dir (pai-v3.api::%state-dir k)))
      (pai-v3.api::%leave-note dir 130 "2026-10-19T14:38:52Z" "watchdog-20261019T143852Z.txt")
      (pai-v3.api::%announce-recovery k)
      (is (null (probe-file (pai-v3.api::%pending-file dir))) "the note is used once")
      (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1"
                     "notice" "%restarted by my watchdog%watchdog-20261019T143852Z.txt%")
          "the operator is told where the evidence is")
      (is (= 1 (or (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1"
                              (kernel-agent-id k) "watchdog.recovered") 0))
          "and it is in the ledger")
      (pai-v3.api::%announce-recovery k)
      (is (= 1 (or (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1"
                              (kernel-agent-id k) "watchdog.recovered") 0))
          "and not again"))))

(deftest a-stuck-check-does-not-stick-the-watchdog
  (is (eq :stuck (pai-v3.api::%within 1 (lambda () (sleep 30) :ok))) "a check that never finishes is reported")
  (is (eq :ok (pai-v3.api::%within 5 (lambda () :ok))))
  (is (eq :error (pai-v3.api::%within 5 (lambda () (error "boom")))) "and one that fails"))

(deftest the-database-check-notices-a-lock-nobody-releases
  (with-wd-kernel (k)
    (is (pai-v3.api::%db-writable-p k 5) "free: it takes a write lock")
    (let* ((release nil)
           (holder (sb-thread:make-thread
                    (lambda () (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock k))
                                 (loop until release do (sleep 0.1)))))))
      (sleep 0.5)
      (is (not (pai-v3.api::%db-writable-p k 1)) "held by another thread: reported as not writable, within the deadline")
      (setf release t)
      (sb-thread:join-thread holder :default nil)
      (sleep 1.2)
      (is (pai-v3.api::%db-writable-p k 5) "and fine again once released"))))

(deftest the-loop-restarts-only-after-the-allowance-and-the-grace
  ;; The watchdog runs in a thread of its own, which sees global values, not a test's let bindings.
  (with-wd-kernel (k)
    (let* ((exits 0) (answers t)
           (saved (list pai-v3.api::*watchdog-exit* pai-v3.api::*watchdog-probes* pai-v3.api::*watchdog-timing*))
           (agent (pai-v3.api::make-agent :kernel k)))
      (unwind-protect
           (progn
             (setf pai-v3.api::*watchdog-exit* (lambda () (incf exits))
                   pai-v3.api::*watchdog-probes* (lambda (kernel port) (declare (ignore kernel port)) answers)
                   pai-v3.api::*watchdog-timing* (list 1 3 0)       ; check each second; stall 3 s; no grace
                   (pai-v3.api::agent-running agent) t)
             (let ((thread (pai-v3.api::start-watchdog agent 1)))
               (sleep 3)
               (is (= 0 exits) "healthy: nothing happens")
               (setf answers nil)
               (sleep 1.5)
               (is (= 0 exits) "failing, but not yet for the allowance")
               (sleep 5)
               (is (plusp exits) "failing for longer than the allowance: restarted")
               (setf (pai-v3.api::agent-running agent) nil)
               (sleep 2)
               (ignore-errors (bt:destroy-thread thread))))
        (setf pai-v3.api::*watchdog-exit* (first saved) pai-v3.api::*watchdog-probes* (second saved)
              pai-v3.api::*watchdog-timing* (third saved))))))

(run-suite "api-watchdog")
