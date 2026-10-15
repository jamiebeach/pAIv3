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

(run-suite "mind-concerns")
