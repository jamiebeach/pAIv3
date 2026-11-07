(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Model-call captures are kept by age, not count (bead v3-g86): the request
;;; behind a failure must still be there when someone looks, days later.

(deftest captures-are-kept-for-hours-not-a-count
  (with-temp-kernel (k)
    (let ((db (kernel-db k)))
      (sql-exec db "INSERT INTO model_captures(call_event_id,at,request,response) VALUES(?1,?2,?3,?4)"
                900001 (iso8601 (- (now) (* 3600 100))) "{\"old\":true}" "{}")
      (sql-exec db "INSERT INTO model_captures(call_event_id,at,request,response) VALUES(?1,?2,?3,?4)"
                900002 (iso8601 (- (now) (* 3600 10))) "{\"recent\":true}" "{}")
      (loop for i from 1 to 250
            do (pai-v3.mind::capture-model-call k (append! k "web.searched" (obj "query" "SYNTHETIC" "status" 200)) "{}" "{}"))
      (is (null (sql-value db "SELECT 1 FROM model_captures WHERE call_event_id=?1 LIMIT 1" 900001))
          "past observability.capture_hours (48 by default), dropped")
      (is (sql-value db "SELECT 1 FROM model_captures WHERE call_event_id=?1 LIMIT 1" 900002)
          "ten hours old is kept, even after more than 200 newer calls")
      (is (= 251 (sql-value db "SELECT count(*) FROM model_captures WHERE at>=?1 LIMIT 1" "2000-01-01"))))))

(run-suite "mind-captures")
