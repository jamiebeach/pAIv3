(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(deftest a-deferred-operator-turn-has-a-readable-admission-reason
  (with-clock
    (with-temp-kernel (k)
      (change-setting k "budget.daily_microusd" 1 :origin (obj "principal_type" "operator"))
      (let ((id (request-thought k "responding" "Synthetic operator turn")))
        (is (eq :deferred (first (run-scheduler-step k))))
        (let* ((peek (pai-v3.api::dev-peek k (format nil "work:~d" id) 5))
               (items (obj-get peek "items")))
          (is (= 1 (obj-get (first items) "daily_limit_microusd")))
          (is (= 0 (obj-get (first items) "spent_today_microusd")))
          (is (equal "daily budget exhausted" (obj-get (second items) "deferred_reason")))
          (is (equal "queued" (obj-get (second items) "status")))
          (is (obj-get (second items) "next_at")))))))

(deftest tool-use-peek-finishes-when-thought-ids-have-gaps
  ;; It once searched forever for the first thought of the window when the
  ;; ids in between were missing, pinning a core of the agent.
  (with-temp-kernel (k)
    (let ((ids (loop repeat 5 collect (request-thought k "concern" "Synthetic thought"))))
      (sql-exec (kernel-db k) "DELETE FROM thoughts WHERE id>?1 AND id<?2" (first ids) (car (last ids)))
      (sql-exec (kernel-db k) "UPDATE thoughts SET created_at=?1 WHERE id=?2"
                (iso8601 (- (now) (* 86400 10))) (first ids))
      (is (handler-case (sb-ext:with-timeout 20 (pai-v3.api::dev-peek k "tool-use" 1) t)
            (sb-ext:timeout () nil))
          "the peek answers"))))

(run-suite "api-work-peek")
