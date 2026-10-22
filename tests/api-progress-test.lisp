(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Progression: experience read from outcomes in the ledger, never from
;;; volume; the operator's sheet from quests and what they report; level-ups
;;; announced once, and not for history already there.

(defun skill-of (report who key)
  (find key (obj-get (obj-get report who) "skills") :key (lambda (s) (obj-get s "key")) :test #'equal))

(deftest levels-follow-the-curve
  (is (= 1 (pai-v3.api::level-for 0)))
  (is (= 1 (pai-v3.api::level-for 49)))
  (is (= 2 (pai-v3.api::level-for 50)))
  (is (= 3 (pai-v3.api::level-for 150)))
  (is (= 10 (pai-v3.api::level-for 2250)))
  (is (= 1 (pai-v3.api::level-for -40)) "never below one"))

(deftest outcomes-earn-and-volume-does-not
  (with-temp-kernel (k)
    (append! k "concern.closed" (obj "concern_id" "c-1" "status" "satisfied" "reason" "SYNTHETIC answered"))
    (append! k "concern.closed" (obj "concern_id" "c-2" "status" "retired" "reason" "SYNTHETIC not worth it"))
    (append! k "graph.fact.retracted" (obj "fact_id" "f-1" "reason" "SYNTHETIC wrong age (operator correction)"))
    (append! k "graph.fact.retracted" (obj "fact_id" "f-2" "reason" "SYNTHETIC caught myself"))
    (append! k "fleet.draft.settled" (obj "draft_id" "d-1" "fate" "dropped"))
    (append! k "turn.received" (obj "channel" "web" "audience" "operator" "speaker" "Sam" "text" "SYNTHETIC hi" "origin" (obj)))
    (let ((r (pai-v3.api::progress-report k)))
      (is (= (+ 15 4) (obj-get (skill-of r "agent" "verifier") "xp")) "the settled concern and the self-caught retraction; not the retired one")
      (is (= 3 (obj-get (skill-of r "agent" "cartographer") "xp")) "a correction taken in")
      (is (= -2 (obj-get (skill-of r "agent" "diplomat") "xp")) "a dropped draft costs a little")
      (is (= (+ 15 4 3 -2) (obj-get (obj-get r "agent") "xp")) "conversation itself earns nothing")
      (is (>= 4 (third (find "a fact I retracted myself, without being told" pai-v3.api::+xp-rules+ :key #'second :test #'equal)))
          "an honest retraction scores at least a first-hand fact")
      (is (plusp (length (obj-get r "rules")))))))

(deftest the-operator-levels-from-quests-and-what-they-report
  (with-temp-kernel (k)
    (append! k "carried.closed" (obj "id" "k-1" "state" "done"))
    (let ((run (pai-v3.mind::tool-run (gethash "log_progress" *tools*)))
          (id (request-thought k "responding" "SYNTHETIC" :audience "operator")))
      (is (search "Logged" (funcall run k id (obj "domain" "health" "title" "SYNTHETIC ran 5k" "metric" "5k_minutes" "value" 31 "better" "lower"))))
      (is (search "personal best" (funcall run k id (obj "domain" "health" "title" "SYNTHETIC ran 5k faster" "metric" "5k_minutes" "value" 29.5 "better" "lower"))))
      (is (not (search "personal best" (funcall run k id (obj "domain" "health" "title" "SYNTHETIC slower" "metric" "5k_minutes" "value" 33 "better" "lower")))))
      (is (search "Not logged" (funcall run k id (obj "domain" "luck" "title" "SYNTHETIC"))))
      (let ((r (pai-v3.api::progress-report k)))
        (is (= 15 (obj-get (skill-of r "operator" "quests") "xp")))
        (is (= (+ 10 25 10) (obj-get (skill-of r "operator" "health") "xp")))
        (is (= 33 (obj-get (first (obj-get (obj-get r "operator") "metrics")) "latest")))
        (is (= 10 (obj-get (skill-of r "agent" "reliability") "xp")) "the agent earns for the quest too")))))

(deftest level-ups-are-announced-once-and-history-does-not-ring
  (with-temp-kernel (k)
    (dotimes (i 4) (append! k "concern.closed" (obj "concern_id" (format nil "c-~d" i) "status" "satisfied" "reason" "SYNTHETIC")))
    (is (= 0 (pai-v3.api::announce-levels k)) "the first look records, silently")
    (dotimes (i 8) (append! k "concern.closed" (obj "concern_id" (format nil "d-~d" i) "status" "satisfied" "reason" "SYNTHETIC")))
    (is (plusp (pai-v3.api::announce-levels k)))
    (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1" "notice" "%Level up!%"))
    (is (= 0 (pai-v3.api::announce-levels k)) "and only once")))

(run-suite "api-progress")
