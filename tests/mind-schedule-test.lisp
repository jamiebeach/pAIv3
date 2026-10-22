(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Scheduled items (bead v3-clp.8), ported from v2's scheduler.

(defparameter *op* (obj "principal_type" "operator"))
(defun utc (y mo d h mi) (encode-universal-time 0 mi h d mo y 0))
(defmacro at-utc ((u) &body body) `(let ((*clock* (lambda () ,u))) ,@body))
(defun new-york () (local-time:find-timezone-by-location-name "America/New_York"))

(deftest cron-matches-wall-time-across-dst
  (let ((z (new-york)))
    ;; 09:00 New York is 13:00 UTC in summer and 14:00 UTC in winter.
    (is (= (utc 2026 10 30 13 0) (cron-next "0 9 * * *" (utc 2026 10 30 12 0) z)))
    (is (= (utc 2026 11 2 14 0) (cron-next "0 9 * * *" (utc 2026 11 1 14 0) z)))
    ;; Weekdays only: Friday 2026-10-30 -> Monday 2026-11-02.
    (is (= (utc 2026 11 2 14 0) (cron-next "0 9 * * 1-5" (utc 2026 10 30 14 0) z)))
    ;; Clocks go back at 02:00 on 2026-11-01: 01:30 happens twice, fires once.
    (let ((first (cron-next "30 1 * * *" (utc 2026 11 1 0 0) z)))
      (is (= (utc 2026 11 1 5 30) first))
      (is (= (utc 2026 11 2 6 30) (cron-next "30 1 * * *" first z (local-slot first z)))))
    (is (= (utc 2028 2 29 5 0) (cron-next "0 0 29 2 *" (utc 2026 3 1 0 0) z)) "rare dates are found")
    (signals error (cron-next "0 9 * *" 0 z))
    (signals error (cron-next "61 9 * * *" 0 z))))

(deftest reminders-go-off-once-and-in-the-operators-zone
  (with-temp-kernel (k)
    (change-setting k "time.zone" "America/New_York" :origin *op*)
    (signals setting-refused (change-setting k "time.zone" "Mars/Olympus" :origin *op*))
    (at-utc ((utc 2026 10 12 14 0))          ; 10:00 in New York
      (add-schedule k "Take the bins out" :at "2026-10-12T19:00")
      (add-schedule k "Stretch" :cron "0 9 * * *" :mode "context")
      (signals error (add-schedule k "Too late" :at "2026-10-12T09:00"))
      (is (search "2026-10-12 19:00: Take the bins out"
                  (format nil "~{~a~}" (mapcar #'first (funcall (gethash :scheduled pai-v3.mind::*slot-sources*)
                                                                k (list :audience "operator") 500)))))
      (is (= 0 (run-due-schedules k)) "nothing is due yet"))
    (at-utc ((utc 2026 10 12 23 1))
      (is (= 1 (run-due-schedules k)))
      (is (= 0 (run-due-schedules k)) "claimed, so never twice")
      (is (search "Take the bins out" (dialogue-row-text (first (last (recent-dialogue k "operator" :token-budget 500)))))
          "notify mode posts the exact text"))
    (at-utc ((utc 2026 10 13 13 1))
      (is (= 1 (run-due-schedules k)) "the daily one fires")
      (let ((lines (mapcar #'first (funcall (gethash :scheduled pai-v3.mind::*slot-sources*)
                                            k (list :audience "operator") 500))))
        (is (search "Went off at 2026-10-13 09:00: Stretch" (first lines)) "context mode stays in mind")
        (is (search "2026-10-14 09:00 (repeats): Stretch" (second lines)) "and repeats")))))

(deftest audiences-see-only-their-own
  (with-temp-kernel (k)
    (at-utc ((utc 2026 10 12 12 0))
      (add-schedule k "Private" :in 600)
      (add-schedule k "For Sam" :in 600 :audience "person:sam")
      (let ((lines (mapcar #'first (funcall (gethash :scheduled pai-v3.mind::*slot-sources*)
                                            k (list :audience "person:sam") 500))))
        (is (= 1 (length lines)))
        (is (search "For Sam" (first lines)))))))

(deftest the-schedule-tool-and-waking
  (with-temp-kernel (k)
    (let* ((id (request-thought k "responding" "remind me" :audience "operator"))
           (run (pai-v3.mind::tool-run (gethash "schedule" pai-v3.mind::*tools*))))
      (at-utc ((utc 2026 10 12 12 0))
        (is (search "Scheduled" (funcall run k id (obj "action" "add" "text" "Call the clinic" "in" 3600 "wake" t))))
        (is (search "Not scheduled" (funcall run k id (obj "action" "add" "text" "x"))))
        (let ((listing (funcall run k id (obj "action" "list"))))
          (is (search "Call the clinic" listing))
          (let ((sid (subseq listing 0 (position #\Space listing))))
            (funcall run k id (obj "action" "add" "text" "Other" "in" 60))
            (is (search "Cancelled" (funcall run k id (obj "action" "cancel" "id" sid)))))))
      (at-utc ((utc 2026 10 12 13 0))
        (is (= 1 (run-due-schedules k)) "the cancelled one does not fire"))
      (at-utc ((utc 2026 10 12 13 0))
        (let ((woke (sql-value (kernel-db k) "SELECT count(*) FROM thoughts WHERE origin='noticing' LIMIT 1")))
          (is (= 0 woke) "no wake was asked for that one"))))))

(deftest the-context-says-what-time-it-is-where-the-operator-is
  (with-clock
    (with-temp-kernel (k)
      (change-setting k "time.zone" "America/New_York" :origin *op*)
      (let ((text (first (first (funcall (gethash :clock pai-v3.mind::*slot-sources*) k nil 80)))))
        (is (search "in America/New_York" text))
        (is (search (format nil "~a" (pai-v3.mind:local-slot (now) (local-time:find-timezone-by-location-name "America/New_York"))) text)
            "the local wall time")
        (is (search "other times say UTC" text) "and that other stamps say UTC")))))

(deftest conversation-is-stamped-in-the-operators-time
  (with-temp-kernel (k)
    (change-setting k "time.zone" "America/Toronto" :origin *op*)
    (is (equal "[Fri 2026-10-16 20:53 EDT web] Operator: hi"
               (pai-v3.mind::localize-dialogue k "[2026-10-17 00:53 web] Operator: hi"))
        "a UTC stamp after midnight is still the evening before, where the operator is")
    (is (equal "[Mon 2026-01-05 07:00 EST web] Me: ok"
               (pai-v3.mind::localize-dialogue k "[2026-01-05 12:00 web] Me: ok")))
    (is (equal "no stamp here" (pai-v3.mind::localize-dialogue k "no stamp here")))
    (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "hello")
    (is (search "Sam: hello" (dialogue-row-text (first (recent-dialogue k "operator")))))
    (is (not (search (subseq (now-iso8601) 0 10) (subseq (dialogue-row-text (first (recent-dialogue k "operator"))) 0 5)))
        "the line no longer starts with the bare UTC date")))

(deftest a-reminder-that-cannot-fire-retries-and-says-so-without-stopping-the-rest
  (with-temp-kernel (k)
    (at-utc ((utc 2026 10 12 12 0))
      (append! k "schedule.added" (obj "id" "s-poison" "kind" "cron" "text" "SYNTHETIC poison" "mode" "notify"
                                       "audience" "operator" "next_at" (iso8601 (utc 2026 10 12 11 0)) "cron" "not a cron"
                                       "timezone" "Not/AZone" "wake" :false))
      (add-schedule k "SYNTHETIC fine" :in 60))
    (at-utc ((utc 2026 10 12 12 5))
      (run-due-schedules k)
      (is (sql-value (kernel-db k) "SELECT 1 FROM scheduled_items WHERE id=?1 AND last_fired_at IS NOT NULL LIMIT 1"
                     (sql-value (kernel-db k) "SELECT id FROM scheduled_items WHERE text=?1 LIMIT 1" "SYNTHETIC fine"))
          "the good one still fired")
      (is (string< (now-iso8601) (sql-value (kernel-db k) "SELECT next_at FROM scheduled_items WHERE id=?1 LIMIT 1" "s-poison"))
          "the bad one waits an hour instead of failing every tick")
      (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1"
                     "notice" "%failed to go off%")
          "the operator is told"))))

(deftest a-context-reminder-wakes-unless-told-not-to
  (with-temp-kernel (k)
    (let ((id (request-thought k "responding" "remind me" :audience "operator"))
          (run (pai-v3.mind::tool-run (gethash "schedule" pai-v3.mind::*tools*))))
      (at-utc ((utc 2026 10 12 12 0))
        (funcall run k id (obj "action" "add" "text" "SYNTHETIC ctx" "in" 60 "mode" "context"))
        (funcall run k id (obj "action" "add" "text" "SYNTHETIC ctx quiet" "in" 60 "mode" "context" "wake" :false))
        (funcall run k id (obj "action" "add" "text" "SYNTHETIC note" "in" 60)))
      (flet ((wake (text) (sql-value (kernel-db k) "SELECT wake FROM scheduled_items WHERE text=?1 LIMIT 1" text)))
        (is (= 1 (wake "SYNTHETIC ctx")))
        (is (= 0 (wake "SYNTHETIC ctx quiet")))
        (is (= 0 (wake "SYNTHETIC note")))))))

(deftest a-failing-periodic-part-is-recorded-and-the-rest-still-run
  (with-temp-kernel (k)
    (clrhash pai-v3.mind::*periodic-failures-recorded*)
    (let ((ran nil))
      (pai-v3.mind::%periodic-part k "synthetic" (lambda (k) (declare (ignore k)) (error "SYNTHETIC broke")))
      (pai-v3.mind::%periodic-part k "synthetic" (lambda (k) (declare (ignore k)) (error "SYNTHETIC broke")))
      (pai-v3.mind::%periodic-part k "other" (lambda (k) (declare (ignore k)) (setf ran t)))
      (is ran)
      (is (= 1 (length (latest-event-ids k "periodic.failed" 10))) "recorded once an hour, not every tick"))))

(run-suite "mind-schedule")
