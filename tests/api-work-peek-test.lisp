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

(run-suite "api-work-peek")
