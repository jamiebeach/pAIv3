(in-package #:pai-v3.api)

;;; Progression (operator request 2026-10-22; the crew's own sketch on the
;;; board the same day). Stage one is a projection, not a balance: every
;;; point is read from an outcome already in the ledger, nothing accrues
;;; from volume (messages, thoughts, pictures), and an honest correction
;;; scores at least as much as a fact that was quietly right. The operator
;;; has a sheet too: carried items brought to done are quests, and what they
;;; report of their wealth, health and mind is logged by the agent as
;;; metrics with personal bests. A periodic look announces level-ups.

(defparameter +agent-skills+
  '(("verifier" . "Verifier") ("cartographer" . "Cartographer") ("smith" . "Smith")
    ("diplomat" . "Diplomat") ("reliability" . "Reliability"))
  "The agent's skill lines, by key and name.")

(defparameter +operator-skills+
  '(("quests" . "Quests") ("wealth" . "Wealth") ("health" . "Health") ("mind" . "Mind") ("craft" . "Craft"))
  "The operator's skill lines, by key and name.")

(defparameter +xp-rules+
  '(("verifier" "a concern settled with an answer" 15)
    ("verifier" "a fact I retracted myself, without being told" 4)
    ("cartographer" "an operator's correction taken into the graph" 3)
    ("cartographer" "entities merged as the operator said" 3)
    ("cartographer" "a fact corroborated by new evidence" 1)
    ("cartographer" "a fact learned first-hand from the operator" 2)
    ("smith" "a made thing delivered" 20)
    ("smith" "a made thing kept" 8)
    ("smith" "docket work completed" 25)
    ("diplomat" "a peer message sent as drafted" 4)
    ("diplomat" "a peer message sent after editing" 2)
    ("diplomat" "a peer message the review dropped" -2)
    ("diplomat" "a board thread resolved" 5)
    ("reliability" "something carried for the operator brought to done" 10)
    ("reliability" "a thought that failed" -3))
  "What earns and costs experience, for the page and the record.")

(defun %xp-rule (reason)
  (or (find reason +xp-rules+ :key #'second :test #'equal) (error "No XP rule ~s" reason)))

(define-event-type "progress.logged"
  :schema (("domain" :string) ("title" :string) ("xp" :integer)
           ("metric" :string :optional) ("value" :number :optional) ("unit" :string :optional)
           ("best" :boolean :optional) ("thought_id" :integer :optional)))
(define-event-type "progress.announced"
  :schema (("who" :string) ("skill" :string) ("level" :integer)))

(defun %earn (db e who skill amount reason)
  (sql-exec db "INSERT OR IGNORE INTO progress_xp(event_id,who,skill,amount,reason,at) VALUES(?1,?2,?3,?4,?5,?6)"
            (event-id e) who skill amount reason (event-at e)))

(defun %rule-earn (db e reason)
  (destructuring-bind (skill text amount) (%xp-rule reason)
    (%earn db e "agent" skill amount text)))

(defun %reduce-progress (db e)
  (let ((p (event-payload e)) (type (event-type e)))
    (flet ((f (k) (%given (obj-get p k))))
      (cond
        ((string= type "concern.closed")
         (when (equal "satisfied" (f "status")) (%rule-earn db e "a concern settled with an answer")))
        ((string= type "graph.fact.retracted")
         (if (search "(operator correction)" (or (f "reason") ""))
             (%rule-earn db e "an operator's correction taken into the graph")
             (%rule-earn db e "a fact I retracted myself, without being told")))
        ((string= type "graph.entity.merged")
         (when (search "(operator correction)" (or (f "reason") ""))
           (%rule-earn db e "entities merged as the operator said")))
        ((string= type "graph.fact.supported") (%rule-earn db e "a fact corroborated by new evidence"))
        ((string= type "graph.fact.admitted")
         (when (some (lambda (ev) (equal "original-utterance" (obj-get ev "kind"))) (or (f "evidence") '()))
           (%rule-earn db e "a fact learned first-hand from the operator")))
        ((string= type "artifact.disposition")
         (cond ((equal "delivered" (f "state")) (%rule-earn db e "a made thing delivered"))
               ((equal "kept" (f "state")) (%rule-earn db e "a made thing kept"))))
        ((string= type "docket.transitioned")
         (when (equal "completed" (f "state")) (%rule-earn db e "docket work completed")))
        ((string= type "fleet.draft.settled")
         (cond ((equal "sent" (f "fate")) (%rule-earn db e "a peer message sent as drafted"))
               ((equal "edited" (f "fate")) (%rule-earn db e "a peer message sent after editing"))
               ((equal "dropped" (f "fate")) (%rule-earn db e "a peer message the review dropped"))))
        ((string= type "fleet.board.thread.status")
         (when (equal "resolved" (f "status")) (%rule-earn db e "a board thread resolved")))
        ((string= type "carried.closed")
         (when (equal "done" (f "state"))
           (%rule-earn db e "something carried for the operator brought to done")
           (%earn db e "operator" "quests" 15 "a quest completed (something carried, done)")))
        ((string= type "thought.failed") (%rule-earn db e "a thought that failed"))
        ((string= type "progress.logged")
         (%earn db e "operator" (f "domain") (f "xp") (f "title"))
         (when (f "metric")
           (sql-exec db "INSERT OR IGNORE INTO progress_metrics(event_id,domain,metric,value,unit,best,at) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                     (event-id e) (f "domain") (f "metric") (f "value") (f "unit") (if (eq t (f "best")) 1 0) (event-at e))))
        ((string= type "progress.announced")
         (sql-exec db "INSERT INTO progress_levels(who,skill,level) VALUES(?1,?2,?3) ON CONFLICT(who,skill) DO UPDATE SET level=max(level,excluded.level)"
                   (f "who") (f "skill") (f "level")))))))

(define-projection "progress"
  :kind :inline
  :consumes ("concern.closed" "graph.fact.retracted" "graph.entity.merged" "graph.fact.supported" "graph.fact.admitted"
             "artifact.disposition" "docket.transitioned" "fleet.draft.settled" "fleet.board.thread.status"
             "carried.closed" "thought.failed" "progress.logged" "progress.announced")
  :tables ("CREATE TABLE IF NOT EXISTS progress_xp (event_id INTEGER NOT NULL, who TEXT NOT NULL, skill TEXT NOT NULL, amount INTEGER NOT NULL, reason TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(event_id,who,skill))"
           "CREATE INDEX IF NOT EXISTS progress_xp_who_idx ON progress_xp(who,skill,at)"
           "CREATE TABLE IF NOT EXISTS progress_metrics (event_id INTEGER PRIMARY KEY, domain TEXT NOT NULL, metric TEXT NOT NULL, value REAL NOT NULL, unit TEXT, best INTEGER NOT NULL, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS progress_metrics_metric_idx ON progress_metrics(metric,at)"
           "CREATE TABLE IF NOT EXISTS progress_levels (who TEXT NOT NULL, skill TEXT NOT NULL, level INTEGER NOT NULL, PRIMARY KEY(who,skill))")
  :reduce #'%reduce-progress)

;;; Levels: level L is reached at 25·L·(L−1) experience: 50 for 2, 150 for 3,
;;; 500 for 5, 2,250 for 10. Experience never goes below zero for a level.

(defun level-for (xp)
  (let ((xp (max 0 xp)))
    (loop for level from 1 while (>= xp (* 25 (1+ level) level)) finally (return level))))

(defun %xp-for-level (level) (* 25 level (1- level)))

(defun %xp-of (kernel who &key skill since)
  (or (sql-value (kernel-db kernel)
                 "SELECT sum(amount) FROM progress_xp WHERE who=?1 AND (?2='' OR skill=?2) AND (?3='' OR at>=?3) LIMIT 1"
                 who (or skill "") (or since ""))
      0))

(defun %skill-json (kernel who key name)
  (let* ((xp (%xp-of kernel who :skill key))
         (level (level-for xp))
         (week (%xp-of kernel who :skill key :since (iso8601 (- (now) (* 7 86400)))))
         (before (%xp-of kernel who :skill key :since (iso8601 (- (now) (* 14 86400))))))
    (obj "key" key "name" name "xp" xp "level" level
         "level_xp" (%xp-for-level level) "next_xp" (%xp-for-level (1+ level))
         "this_week" week "last_week" (- before week))))

(defun %sheet (kernel who skills)
  (let* ((xp (%xp-of kernel who)) (level (level-for xp)))
    (obj "xp" xp "level" level "level_xp" (%xp-for-level level) "next_xp" (%xp-for-level (1+ level))
         "skills" (mapcar (lambda (s) (%skill-json kernel who (car s) (cdr s))) skills)
         "recent" (mapcar (lambda (r) (obj "skill" (first r) "amount" (second r) "reason" (third r) "at" (fourth r)))
                          (sql-rows (kernel-db kernel)
                                    "SELECT skill,amount,reason,at FROM progress_xp WHERE who=?1 ORDER BY event_id DESC LIMIT 12" who)))))

(defun progress-report (kernel)
  "Both character sheets, the operator's metrics, and how experience is earned."
  (obj "agent" (let ((o (%sheet kernel "agent" +agent-skills+))) (setf (gethash "name" o) (fleet-own-name kernel)) o)
       "operator" (let ((o (%sheet kernel "operator" +operator-skills+)))
                    (setf (gethash "name" o) (setting kernel "operator.name")
                          (gethash "metrics" o)
                          (mapcar (lambda (r)
                                    (destructuring-bind (domain metric unit) r
                                      (let ((history (sql-rows (kernel-db kernel)
                                                               "SELECT value,at,best FROM progress_metrics WHERE metric=?1 ORDER BY at DESC LIMIT 30" metric)))
                                        (obj "domain" domain "metric" metric "unit" (or unit :null)
                                             "latest" (first (first history)) "at" (second (first history))
                                             "history" (mapcar (lambda (h) (obj "value" (first h) "at" (second h) "best" (= 1 (third h)))) history)))))
                                  (sql-rows (kernel-db kernel)
                                            "SELECT domain,metric,max(unit) FROM progress_metrics WHERE domain IN (?1,?2,?3,?4) GROUP BY domain,metric ORDER BY domain,metric LIMIT 50"
                                            "wealth" "health" "mind" "craft")))
                    o)
       "rules" (mapcar (lambda (r) (obj "skill" (first r) "what" (second r) "xp" (third r))) +xp-rules+)))

;;; The operator's progress, logged by the agent

(defun log-progress (kernel &key domain title metric value unit better thought-id)
  "Record something the operator did or reported. Returns a sentence."
  (unless (assoc domain (rest +operator-skills+) :test #'equal)
    (error "domain is wealth, health, mind or craft"))
  (unless (and (stringp title) (< 0 (length (string-trim " " title)) 300)) (error "say what was done, in a line"))
  (when (and metric (not (and (stringp metric) (< 0 (length metric) 80) (realp value))))
    (error "a metric needs a short name and a number value"))
  (let* ((prior (and metric (sql-value (kernel-db kernel)
                                       (if (equal better "lower")
                                           "SELECT min(value) FROM progress_metrics WHERE metric=?1 LIMIT 1"
                                           "SELECT max(value) FROM progress_metrics WHERE metric=?1 LIMIT 1")
                                       metric)))
         (best (and metric prior (if (equal better "lower") (< value prior) (> value prior))))
         (xp (+ 10 (if best 15 0))))
    (append! kernel "progress.logged"
             (obj "domain" domain "title" (string-trim " " title) "xp" xp
                  "metric" metric "value" (and metric (float value 1d0)) "unit" unit
                  "best" (if best t :false) "thought_id" thought-id))
    (format nil "Logged for the operator (~a, +~d XP~:[~;, a personal best~])." domain xp best)))

(define-tool "log_progress"
  :description "Log something the operator did or told me about their own progress, for their character sheet: domain wealth (net worth and finances), health (exercise, sleep, eating), mind (learning, reading, games like chess) or craft (projects, making). title: what they did, in a line. Optional metric (a short stable name such as net_worth, 5k_time_minutes, weight_kg, chess_rating), value (a number) and unit; better: higher (default) or lower, so a personal best is recognised. Log only what the operator reported or what is plainly in the record, never a guess."
  :summary "Log the operator's progress on their character sheet."
  :parameters (obj "type" "object"
                   "properties" (obj "domain" (obj "type" "string" "enum" (list "wealth" "health" "mind" "craft"))
                                     "title" (obj "type" "string") "metric" (obj "type" "string")
                                     "value" (obj "type" "number") "unit" (obj "type" "string")
                                     "better" (obj "type" "string" "enum" (list "higher" "lower")))
                   "required" (list "domain" "title"))
  :available '("operator")
  :core '("responding" "noticing")
  :run (lambda (kernel id args)
         (handler-case
             (log-progress kernel :domain (obj-get args "domain") :title (obj-get args "title")
                                  :metric (%given (obj-get args "metric")) :value (%given (obj-get args "value"))
                                  :unit (%given (obj-get args "unit")) :better (%given (obj-get args "better"))
                                  :thought-id id)
           (error (e) (format nil "Not logged: ~a" e)))))

;;; Level-ups, announced

(defun announce-levels (kernel)
  "Tell the operator about every level reached since the last look. The first
look only records where things stand, so a rebuilt history does not ring."
  (let ((first-look (null (sql-value (kernel-db kernel) "SELECT 1 FROM progress_levels WHERE who IN (?1,?2) LIMIT 1" "agent" "operator")))
        (said '()))
    (dolist (who '("agent" "operator"))
      (dolist (skill (cons '("overall" . "Overall") (if (equal who "agent") +agent-skills+ +operator-skills+)))
        (let* ((level (level-for (if (equal (car skill) "overall") (%xp-of kernel who) (%xp-of kernel who :skill (car skill)))))
               (known (or (sql-value (kernel-db kernel) "SELECT level FROM progress_levels WHERE who=?1 AND skill=?2 LIMIT 1" who (car skill)) 1)))
          (when (> level known)
            (append! kernel "progress.announced" (obj "who" who "skill" (car skill) "level" level))
            (unless first-look
              (push (format nil "~a~:[ ~a~;~*~] reached level ~d"
                            (if (equal who "agent") (fleet-own-name kernel) (setting kernel "operator.name"))
                            (equal (car skill) "overall") (cdr skill) level)
                    said))))))
    (when first-look
      (append! kernel "progress.announced" (obj "who" "agent" "skill" "overall" "level" (level-for (%xp-of kernel "agent")))))
    (when said
      (let ((text (format nil "Level up! ~{~a~^; ~}." (reverse said))))
        (post-notice kernel :channel "web" :audience "operator" :text text)
        (ignore-errors (notify kernel :topic "level-up" :importance 1 :summary text :deep-link "/stats"))))
    (length said)))

(pushnew 'announce-levels *periodic-hooks*)
