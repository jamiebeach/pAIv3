(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Concerns and quiet thinking (bead v3-clp.7), with v2's lessons: no
;;; thought without a concern, caps, reinforcement, lifetime limits,
;;; rotation, grace and paced spend.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defmacro with-quiet ((k) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (advance 43200)                                  ; noon UTC: half the day's budget is open
       (change-setting ,k "quiet.enabled" t :origin *operator*)
       (change-setting ,k "quiet.review" :false :origin *operator*)   ; tested on its own below
       (change-setting ,k "quiet.free_time_minutes" 0 :origin *operator*) ; likewise
       ,@body)))

(defun concern-thoughts (k)
  (sql-rows (kernel-db k) "SELECT id,reason FROM thoughts WHERE origin=?1 LIMIT 50" "concern"))

(deftest salience-is-the-weighted-blend
  (is (< (abs (- 1d0 (salience-of (obj "novelty" 1 "goal_relevance" 1 "personal_relevance" 1 "urgency" 1
                                       "uncertainty" 1 "predicted_value" 1 "social_relevance" 1 "surprise" 1))))
         1d-9))
  (is (< (abs (- 0.2d0 (salience-of (obj "novelty" 1)))) 1d-9)))

(deftest concerns-are-capped-and-the-same-question-reinforces
  (with-quiet (k)
    (change-setting k "concerns.max_open" 2 :origin *operator*)
    (multiple-value-bind (a outcome) (raise-concern k "Why is recall slow?") (is a) (is (eq :raised outcome)))
    (multiple-value-bind (b outcome) (raise-concern k "why is RECALL slow") (is b) (is (eq :reinforced outcome)))
    (is (= 1 (length (open-concerns k))) "a duplicate never adds one")
    (raise-concern k "What does Sam's book say about Gamma?")
    (is (search "most allowed" (nth-value 1 (raise-concern k "A third question"))) "the cap holds")
    (is (search "kind must be" (nth-value 1 (raise-concern k "x" :kind "whim"))))))

(deftest new-concerns-are-limited-per-day
  (with-quiet (k)
    (change-setting k "concerns.max_new_per_day" 1 :origin *operator*)
    (let ((a (raise-concern k "First")))
      (close-concern k a "satisfied" "done"))
    (is (search "allowance" (nth-value 1 (raise-concern k "Second"))))))

(deftest nothing-is-thought-without-a-concern
  (with-quiet (k)
    (is (equal "nothing open" (nth-value 1 (quiet-tick k))))
    (is (null (concern-thoughts k)))))

(deftest quiet-time-attends-the-most-salient-concern
  (with-quiet (k)
    (raise-concern k "A small thing" :features (obj "urgency" 0.1 "goal_relevance" 0.1))
    (let ((big (raise-concern k "A pressing thing" :features (obj "urgency" 1 "goal_relevance" 1 "personal_relevance" 1))))
      (let ((id (quiet-tick k)))
        (is id)
        (is (search "A pressing thing" (getf (thought-row k id) :reason)))
        (is (equal "concern" (getf (thought-row k id) :origin)))
        (is (= 1 (seventh (concern-row k big))) "attended once")))
    (is (equal "busy" (nth-value 1 (quiet-tick k))) "one at a time")))

(deftest quiet-time-waits-for-grace-revisit-and-pace
  (with-quiet (k)
    (is (equal "off" (progn (change-setting k "quiet.enabled" :false :origin *operator*)
                            (nth-value 1 (quiet-tick k)))))
    (change-setting k "quiet.enabled" t :origin *operator*)
    (raise-concern k "Something to look into")
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "hi"
                    :origin (obj "principal_type" "operator"))
    (is (equal "grace" (nth-value 1 (quiet-tick k))) "the operator just spoke")
    (advance 200)
    (let ((id (quiet-tick k)))
      (is id)
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id))
    (is (equal "nothing open" (nth-value 1 (quiet-tick k))) "not again before the revisit window")
    (advance 2000)
    (record-spend k 2000000 :lane "quiet")
    (is (equal "paced" (nth-value 1 (quiet-tick k))) "today's paced share is spent")))

(deftest a-concern-that-spends-its-lifetime-is-parked-and-said
  (with-quiet (k)
    (change-setting k "concerns.lifetime_steps" 2 :origin *operator*)
    (let ((c (raise-concern k "A bottomless question")))
      (sql-exec (kernel-db k) "UPDATE concerns SET steps=2 WHERE id=?1" c)
      (quiet-tick k)
      (is (equal "parked" (tenth (concern-row k c))))
      (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice")
          "the operator is told"))))

(deftest at-the-cap-the-stalest-investigated-concern-rotates
  (with-quiet (k)
    (change-setting k "concerns.max_open" 2 :origin *operator*)
    (let ((old (raise-concern k "Old")) (fresh (raise-concern k "Fresh")))
      (sql-exec (kernel-db k) "UPDATE concerns SET last_attended_at=?2 WHERE id=?1" old (iso8601 (- (now) 7200)))
      (quiet-tick k)
      (is (equal "retired" (tenth (concern-row k old))))
      (is (equal "open" (tenth (concern-row k fresh))) "never investigated: kept"))))

(deftest a-failed-thought-becomes-a-repair-concern
  (with-quiet (k)
    (quiet-tick k)                                      ; starts the cursor
    (let ((id (request-thought k "private" "ruminate")))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='failed',outcome='source: no model' WHERE id=?1" id)
      (quiet-tick k)
      (let ((repair (find "repair" (open-concerns k) :key #'second :test #'equal)))
        (is repair)
        (is (search (format nil "thought ~d" id) (third repair)))))))

(deftest the-concern-tool
  (with-quiet (k)
    (flet ((tool (&rest args) (funcall (pai-v3.mind::tool-run (gethash "concern" *tools*)) k 1 (apply #'obj args))))
      (is (search "Raised" (tool "action" "raise" "question" "Is the summary drifting?" "rationale" "it read oddly")))
      (let ((id (first (first (open-concerns k)))))
        (is (search "Is the summary drifting?" (tool "action" "list")))
        (is (equal "Noted." (tool "action" "note" "id" id "text" "checked two folds")))
        (is (search "checked two folds" (tool "action" "list")))
        (is (search "Satisfyd" (tool "action" "satisfy" "id" id "text" "fine")))
        (is (search "already closed" (tool "action" "retire" "id" id)))))))

(deftest after-a-conversation-quiet-time-reviews-it-once
  (with-quiet (k)
    (change-setting k "quiet.review" t :origin *operator*)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "Can you look into X later?"
                    :origin (obj "principal_type" "operator"))
    (advance 200)
    (let ((id (quiet-tick k)))
      (is id)
      (is (equal "introspection" (getf (thought-row k id) :origin)))
      (is (search "Look back over the recent conversation" (getf (thought-row k id) :reason)))
      (is (search "carry" (getf (thought-row k id) :reason)) "and asks whether anything should be carried")
      (is (search "may not have thought to ask" (getf (thought-row k id) :reason)))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id))
    (is (equal "nothing open" (nth-value 1 (quiet-tick k))) "one review per stretch of conversation")))

(deftest with-nothing-open-the-mind-wanders-over-its-memories
  (with-quiet (k)
    (is (equal "nothing open" (nth-value 1 (quiet-tick k))) "no memories and no summary: no material, no thought")
    (form-memory k :kind "fact" :content "Sam wrote a book called North Wind.")
    (let ((id (quiet-tick k)))
      (is id)
      (is (equal "introspection" (getf (thought-row k id) :origin)))
      (is (search "North Wind" (getf (thought-row k id) :reason)) "seeded with a memory")
      (is (search "carry" (getf (thought-row k id) :reason)) "wandering may notice something to carry")
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id))
    (is (equal "nothing open" (nth-value 1 (quiet-tick k))) "not again until it is due")
    (advance (* 6 3600))
    (is (quiet-tick k) "due again later")
    (is (= 2 (sql-value (kernel-db k) "SELECT times FROM rumination_seeds WHERE times>=?1 LIMIT 1" 1))
        "the seed is counted, so it becomes less likely next time")))

(deftest an-open-concern-comes-before-wandering
  (with-quiet (k)
    (form-memory k :kind "fact" :content "Something remembered.")
    (raise-concern k "Something to settle")
    (is (equal "concern" (getf (thought-row k (quiet-tick k)) :origin)))))

(deftest what-quiet-time-concluded-reaches-the-next-turn
  (with-quiet (k)
    (let ((id (raise-concern k "Is the summary still saying the merge is dead code?")))
      (close-concern k id "satisfied" "No: the merge is wired; the earlier claim was wrong.")
      (let ((lines (pai-v3.mind::%fill k :conclusions (list :audience "operator") 1200)))
        (is (search "the earlier claim was wrong" (format nil "~{~a~}" lines)) "the conclusion is in context")))
    (advance (* 4 86400))
    (is (null (pai-v3.mind::%fill k :conclusions (list :audience "operator") 1200)) "and ages out after a few days")))

(deftest a-failure-is-looked-at-at-once-with-repair-steps
  (with-quiet (k)
    (quiet-tick k)                                      ; the failure cursor starts here
    (let ((failed (request-thought k "work" "do something")))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='failed',outcome=?2 WHERE id=?1" failed "model failure: empty response")
      (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "hello"
                      :origin (obj "principal_type" "operator"))
      (let ((id (quiet-tick k)))
        (is id "it starts despite the operator having just spoken")
        (is (search "A failure, looked at at once" (getf (thought-row k id) :reason)))
        (is (search "inspect with what failures" (getf (thought-row k id) :reason)) "with the repair steps")
        (is (search "priority 0" (getf (thought-row k id) :reason))))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE origin=?1" "concern")
      (is (equal "grace" (nth-value 1 (quiet-tick k))) "once looked at, ordinary concerns wait as before"))))

(deftest a-concern-waits-for-its-horizon-and-the-global-floor
  (with-quiet (k)
    (let* ((c (raise-concern k "Check the experiment tomorrow"))
           (id (quiet-tick k))
           (horizon (iso8601 (+ (now) 86400))))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id)
      (flet ((note (&rest args)
               (funcall (pai-v3.mind::tool-run (gethash "concern" *tools*)) k id
                        (apply #'obj "action" "note" "id" c args))))
        (is (equal "Noted." (note "text" "Waiting for results" "next_look_at" horizon)))
        (advance 1800)
        (is (null (quiet-tick k)) "the old 30-minute cadence does not wake it")
        (note "text" "Still waiting")
        (is (equal horizon (nth 10 (concern-row k c))) "an ordinary note preserves the wait")
        (is (search "Give next_look_at" (note "next_look_at" "tomorrow")))
        (is (search "Give next_look_at" (note "next_look_at" "2026-02-30T12:00:00Z")))
        (is (search "Give next_look_at" (note "next_look_at" "2026-10-19T12:00:00+01:00")))
        (is (equal horizon (nth 10 (concern-row k c))) "invalid input changes nothing")
        (advance (- 86400 1801))
        (is (null (quiet-tick k)) "still waits one second before the horizon")
        (advance 1)
        (let ((next (quiet-tick k)))
          (is next "eligible exactly at the horizon")
          (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" next))
        (note "next_look_at" (iso8601 (+ (now) 60)))
        (advance 60)
        (is (null (quiet-tick k)) "a shorter horizon cannot bypass the global floor")
        (note "next_look_at" "")
        (is (null (nth 10 (concern-row k c))) "explicit empty string clears")
        (is (null (quiet-tick k)) "clearing also preserves the global floor")
        (advance 1740)
        (is (quiet-tick k) "ordinary revisit resumes")))))

(deftest waiting-concerns-survive-rotation-and-do-not-block-ready-concerns
  (with-quiet (k)
    (change-setting k "concerns.max_open" 2 :origin *operator*)
    (let ((waiting (raise-concern k "Await the experiment"))
          (ready (raise-concern k "Read the report")))
      (sql-exec (kernel-db k) "UPDATE concerns SET last_attended_at=?2 WHERE id=?1"
                waiting (iso8601 (- (now) 7200)))
      (append! k "concern.noted" (obj "concern_id" waiting "text" "Wait"
                                     "next_look_at" (iso8601 (+ (now) 86400))))
      (is (quiet-tick k))
      (is (equal "open" (tenth (concern-row k waiting))) "a scheduled wait is not abandonment")
      (is (= 0 (seventh (concern-row k waiting))))
      (is (= 1 (seventh (concern-row k ready))) "another concern can proceed"))))

(deftest a-thought-nothing-will-run-again-no-longer-blocks-quiet-time
  (with-temp-kernel (k)
    (let* ((stranded (request-thought k "responding" "SYNTHETIC stranded" :audience "operator"))
           (waiting (request-thought k "responding" "SYNTHETIC waiting on a child" :audience "operator"))
           (child (request-thought k "work" "SYNTHETIC child" :parent-id waiting))
           (old (iso8601 (- (now) 7200))))
      (append! k "work.completed" (obj "work_id" (sql-value (kernel-db k) "SELECT id FROM v3_work WHERE handler=?1 AND entity=?2 LIMIT 1" "thought" (princ-to-string stranded))))
      (append! k "work.completed" (obj "work_id" (sql-value (kernel-db k) "SELECT id FROM v3_work WHERE handler=?1 AND entity=?2 LIMIT 1" "thought" (princ-to-string waiting))))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='running',updated_at=?1" old)
      (pai-v3.mind::%close-orphans k)
      (is (equal "failed" (getf (thought-row k stranded) :status)) "nothing will run it again")
      (is (search "abandoned" (getf (thought-row k stranded) :outcome)))
      (is (equal "running" (getf (thought-row k waiting) :status)) "its child still has work queued")
      (is (equal "running" (getf (thought-row k child) :status)))
      (let ((fresh (request-thought k "responding" "SYNTHETIC just started" :audience "operator")))
        (append! k "work.completed" (obj "work_id" (sql-value (kernel-db k) "SELECT id FROM v3_work WHERE handler=?1 AND entity=?2 LIMIT 1" "thought" (princ-to-string fresh))))
        (pai-v3.mind::%close-orphans k)
        (is (equal "queued" (getf (thought-row k fresh) :status)) "a recent thought is left alone")))))

(deftest free-time-comes-before-concerns-and-is-about-the-world
  (with-quiet (k)
    (change-setting k "quiet.free_time_minutes" 120 :origin *operator*)
    (raise-concern k "SYNTHETIC something to settle")
    (let ((id (quiet-tick k)))
      (is (search "Free time." (getf (thought-row k id) :reason)))
      (is (search "brave_search" (getf (thought-row k id) :reason)))
      (is (search "Leave your own code" (getf (thought-row k id) :reason)))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended',outcome=?2 WHERE id=?1" id "SYNTHETIC-THREAD tidal clocks")
      (is (equal "concern" (getf (thought-row k (quiet-tick k)) :origin)) "not again until it is due"))
    (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE origin=?1" "concern")
    (advance (* 3 3600))
    (let ((id (quiet-tick k)))
      (is (search "SYNTHETIC-THREAD tidal clocks" (getf (thought-row k id) :reason)) "the last thread is offered"))))

(deftest repairs-at-once-are-capped-per-day
  (with-quiet (k)
    (change-setting k "quiet.repairs_per_day" 1 :origin *operator*)
    (quiet-tick k)
    (dotimes (i 2)
      (let ((failed (request-thought k "work" (format nil "SYNTHETIC ~d" i))))
        (sql-exec (kernel-db k) "UPDATE thoughts SET status='failed',outcome=?2 WHERE id=?1" failed "synthetic")))
    (let ((first (quiet-tick k)))
      (is (search "A failure, looked at at once" (getf (thought-row k first) :reason)))
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" first))
    (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "hello"
                    :origin (obj "principal_type" "operator"))
    (is (equal "grace" (nth-value 1 (quiet-tick k))) "the second no longer jumps the queue")))

(run-suite "mind-concerns")
