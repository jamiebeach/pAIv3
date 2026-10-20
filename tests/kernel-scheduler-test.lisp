(in-package #:pai-v3.test)


(defvar *behaviour* nil "Function the test handler calls.")
(define-handler "test.step" :run (lambda (k w) (funcall *behaviour* k w)))
(define-handler "test.paid" :paid t :estimate-microusd 100000
  :run (lambda (k w) (record-spend k 100000 :work-id (getf w :id)) (obj "ok" t)))

(deftest completes-and-keeps-result
  (with-clock
    (with-temp-kernel (k)
      (let ((*behaviour* (lambda (k w) (declare (ignore k w)) (obj "answer" 42)))
            (id (request-work k "test.step" "e1" (obj "q" "?"))))
        (is (equal (list :ran id :completed) (run-scheduler-step k)))
        (is (equal "done" (getf (work-status k id) :status)))
        (is (eq :idle (run-scheduler-step k)))))))

(deftest operator-work-runs-first
  (with-clock
    (with-temp-kernel (k)
      (let* ((*behaviour* (lambda (k w) (declare (ignore k w)) nil))
             (bg (request-work k "test.step" "bg" nil :priority :private))
             (op (request-work k "test.step" "op" nil :priority :operator)))
        (is (= op (second (run-scheduler-step k))))
        (is (= bg (second (run-scheduler-step k))))))))

(deftest source-failures-dead-letter-immediately
  (with-clock
    (with-temp-kernel (k)
      (let ((*behaviour* (lambda (k w) (declare (ignore k w))
                           (error 'handler-failure :class :source :reason "input is malformed")))
            (id (request-work k "test.step" "s" nil)))
        (run-scheduler-step k)
        (is (equal "dead" (getf (work-status k id) :status)))
        (is (= id (first (first (dead-letters k)))))))))

(deftest transient-failures-back-off-then-succeed
  (with-clock
    (with-temp-kernel (k)
      (let* ((calls 0)
             (*behaviour* (lambda (k w) (declare (ignore k w))
                            (when (< (incf calls) 3) (error "provider timeout")) nil))
             (id (request-work k "test.step" "t" nil)))
        (run-scheduler-step k)
        (is (equal "queued" (getf (work-status k id) :status)))
        (is (eq :idle (run-scheduler-step k)) "not retried before its backoff")
        (advance 30) (run-scheduler-step k)
        (is (= 2 (getf (work-status k id) :attempts)))
        (advance 59) (is (eq :idle (run-scheduler-step k)) "backoff doubles")
        (advance 1) (run-scheduler-step k)
        (is (equal "done" (getf (work-status k id) :status)))))))

(deftest model-failures-get-one-recovery-after-cooldown
  (with-clock
    (with-temp-kernel (k)
      (let* ((*behaviour* (lambda (k w) (declare (ignore k w))
                            (error 'handler-failure :class :model :reason "invalid output")))
             (id (request-work k "test.step" "m" nil)))
        (dotimes (i 3) (run-scheduler-step k) (advance 3600))
        (is (equal "queued" (getf (work-status k id) :status)) "recovery attempt pending")
        (run-scheduler-step k)
        (is (equal "dead" (getf (work-status k id) :status)))
        (is (= 4 (getf (work-status k id) :attempts)))))))

(deftest refusal-records-once-with-reason-and-next-time
  (with-clock
    (with-temp-kernel (k)
      (let ((*budget* (list :daily-microusd 1000000 :operator-reserve 0.2)))
        ;; 01:00: background allowance is 800000/24 = 33333 < 100000 estimate.
        (advance 3600)
        (let ((id (request-work k "test.paid" "p" nil :priority :concern)))
          (destructuring-bind (outcome wid reason) (run-scheduler-step k)
            (is (eq :deferred outcome)) (is (= id wid))
            (is (search "pace" reason)))
          (is (= 0 (spent-today k)) "nothing charged")
          (let ((before (ledger-head k)))
            (dotimes (i 20) (advance 60) (run-scheduler-step k))
            (is (= before (ledger-head k)) "no re-attempts or records before the eligible time"))
          (setf *t* (parse-iso8601 (getf (work-status k id) :next-at)))
          (is (eq :ran (first (run-scheduler-step k))) "admitted exactly when the pace allows")
          (is (= 100000 (spent-today k "background"))))))))

(deftest operator-lane-uses-the-reserve
  (with-clock
    (with-temp-kernel (k)
      (let ((*budget* (list :daily-microusd 1000000 :operator-reserve 0.2)))
        (advance 60)
        (let ((id (request-work k "test.paid" "o" nil :priority :operator)))
          (is (eq :ran (first (run-scheduler-step k))) "operator is not paced")
          (is (equal "done" (getf (work-status k id) :status))))))))

(deftest unknown-handler-dead-letters-without-blocking
  (with-clock
    (with-temp-kernel (k)
      (let ((*behaviour* (lambda (k w) (declare (ignore k w)) nil))
            (bad (request-work k "no.such.handler" "x" nil))
            (good (request-work k "test.step" "y" nil)))
        (run-scheduler-step k) (run-scheduler-step k)
        (is (equal "dead" (getf (work-status k bad) :status)))
        (is (equal "done" (getf (work-status k good) :status)))))))

(deftest queue-is-a-rebuildable-projection
  (with-clock
    (with-temp-kernel (k)
      (let ((*behaviour* (lambda (k w) (declare (ignore k w)) nil)))
        (request-work k "test.step" "r" nil)
        (run-scheduler-step k)
        (is (equal "ready" (projection-status k "kernel.work")))
        (is (null (quarantined-events k :projection "kernel.work")))))))

(deftest budget-setting-reconsiders-original-work-without-bypassing-admission
  (with-clock
    (with-temp-kernel (k)
      (let ((origin (obj "principal_type" "operator")))
        (change-setting k "budget.daily_microusd" 0 :origin origin)
        (let ((id (request-work k "test.paid" "budget-wait" nil :priority :operator)))
          (is (eq :deferred (first (run-scheduler-step k))))
          (is (eq :idle (run-scheduler-step k)))
          ;; Deliberately do not advance the clock: event ordering must suffice.
          (change-setting k "budget.daily_microusd" 1 :origin origin)
          (is (eq :deferred (first (run-scheduler-step k))) "still over budget")
          (is (= 0 (spent-today k)))
          (is (eq :idle (run-scheduler-step k)) "refusal is reconsidered only once")
          (change-setting k "budget.daily_microusd" 200000 :origin origin)
          (is (equal (list :ran id :completed) (run-scheduler-step k)))
          (is (= 100000 (spent-today k)))
          (is (equal "done" (getf (work-status k id) :status))))))))

(deftest budget-changes-leave-provider-backoff-alone
  (with-clock
    (with-temp-kernel (k)
      (let* ((*behaviour* (lambda (k w) (declare (ignore k w)) (error "synthetic timeout")))
             (id (request-work k "test.step" "retry" nil)))
        (run-scheduler-step k)
        (change-setting k "budget.daily_microusd" 3000000
                        :origin (obj "principal_type" "operator"))
        (is (eq :idle (run-scheduler-step k)))
        (is (= 1 (getf (work-status k id) :attempts)))))))

(run-suite "kernel-scheduler")
