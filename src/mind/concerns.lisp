(in-package #:pai-v3.mind)

;;; Concerns and quiet thinking (bead v3-clp.7). A concern is something the
;;; agent means to look into: a curiosity, a follow-up, a repair of itself.
;;; In quiet time the most salient open concern gets a thought of its own.
;;;
;;; What v2 taught, kept here:
;;;  - never think from an empty prompt: with no open concern, nothing runs;
;;;  - open concerns are capped, duplicates reinforce rather than add, and new
;;;    ones are limited per day; retiring is its own state, not satisfaction;
;;;  - each concern has a lifetime step cap across all its thoughts, because a
;;;    concern picked again on every wake would otherwise spend without end;
;;;  - at the cap, the stalest investigated concern is rotated out, so the cap
;;;    limits the rate of interests rather than their existence;
;;;  - private thinking waits a grace period after the operator speaks, and
;;;    its spend is paced across the day;
;;;  - a concern that stops (lifetime spent, thought failed) tells the
;;;    operator; it never goes quiet.
;;; Quiet time has three kinds of material, in this order: what was said
;;; since the last review (a conversation review, as v2's curiosity review),
;;; open concerns, and, with none open, the agent's own memories, mood and
;;; summary (rumination: the mind wandering, as a person's does without
;;; outside stimulus). v2 turned idle thinking off because it had nothing to
;;; seed it; here every quiet thought starts from material, inner or outer.
;;; Salience follows the cognitive architecture design §7: a weighted blend of
;;; features, kept beside the score so it can be calibrated later, habituated
;;; by attention and raised by the curiosity drive.

(define-setting "quiet.enabled"
  :type :boolean :default :false :group "Quiet thinking"
  :description "Think about open concerns in quiet time.")
(define-setting "quiet.grace_seconds"
  :type :integer :min 0 :max 86400 :default 120 :group "Quiet thinking"
  :description "How long after the operator last spoke before quiet thinking may start.")
(define-setting "quiet.revisit_seconds"
  :type :integer :min 60 :max 604800 :default 1800 :group "Quiet thinking"
  :description "How long after attending to a concern before it may be attended again.")
(define-setting "quiet.daily_usd"
  :type :number :min 0 :max 100 :default 2.0d0 :group "Quiet thinking"
  :description "Most quiet thinking may spend in a UTC day.")
(define-setting "quiet.burst_percent"
  :type :integer :min 0 :max 100 :default 10 :group "Quiet thinking"
  :description "Share of the daily quiet budget open at midnight UTC; the rest opens evenly through the day.")
(define-setting "concerns.max_open"
  :type :integer :min 1 :max 64 :default 4 :group "Quiet thinking"
  :description "Most open concerns at once.")
(define-setting "concerns.max_new_per_day"
  :type :integer :min 0 :max 200 :default 8 :group "Quiet thinking"
  :description "Most new concerns in a UTC day.")
(define-setting "concerns.lifetime_steps"
  :type :integer :min 1 :max 1000 :default 48 :group "Quiet thinking"
  :description "Most model calls one concern may spend across all its visits before it is parked. Each visit is also limited by the concern row of thought.policy.")
(define-setting "concerns.rotation_idle_minutes"
  :type :integer :min 0 :max 100000 :default 60 :group "Quiet thinking"
  :description "At the cap, an investigated concern idle this long may be retired to make room; 0 never rotates.")

(define-setting "quiet.review"
  :type :boolean :default t :group "Quiet thinking"
  :description "After the operator goes quiet, review the new conversation for things worth looking into.")
(define-setting "quiet.rumination_minutes"
  :type :integer :min 0 :max 10080 :default 90 :group "Quiet thinking"
  :description "With no open concern, let the mind wander over memories and mood about this often (sooner when bored); 0 never.")

(define-event-type "quiet.reviewed"
  :schema (("through_event_id" :integer) ("thought_id" :integer)))
(define-event-type "quiet.ruminated"
  :schema (("memory_ids" :array :optional) ("thought_id" :integer)))

(define-event-type "concern.raised"
  :schema (("concern_id" :string) ("kind" :string) ("question" :string) ("rationale" :string)
           ("features" :object) ("salience" :number) ("source_event_ids" :array :optional) ("origin" :object)))
(define-event-type "concern.reinforced"
  :schema (("concern_id" :string) ("rationale" :string) ("salience" :number)))
(define-event-type "concern.noted"
  :schema (("concern_id" :string) ("text" :string)))
(define-event-type "concern.closed"
  :schema (("concern_id" :string) ("status" :string) ("reason" :string)))   ; satisfied | retired | parked

(defparameter +concern-kinds+ '("curiosity" "follow-up" "repair" "introspection"))
(defparameter +salience-weights+
  '(("novelty" . 0.20d0) ("goal_relevance" . 0.18d0) ("personal_relevance" . 0.15d0)
    ("urgency" . 0.12d0) ("uncertainty" . 0.10d0) ("predicted_value" . 0.10d0)
    ("social_relevance" . 0.08d0) ("surprise" . 0.07d0))
  "Design §7 starting weights; not truths. Features are kept beside the score.")

(defun salience-of (features)
  (min 1d0 (max 0d0 (loop for (name . weight) in +salience-weights+
                          sum (* weight (let ((v (obj-get features name))) (if (realp v) (float v 1d0) 0d0)))))))

(defun %normalised (question)
  "Lower-case letters and digits separated by single spaces, for dedupe (v2)."
  (string-trim " " (with-output-to-string (out)
                     (let ((space nil))
                       (loop for c across (string-downcase (or question ""))
                             do (if (alphanumericp c)
                                    (progn (when space (write-char #\Space out)) (setf space nil) (write-char c out))
                                    (setf space t)))))))

(defun %reduce-concerns (db e)
  (let ((p (event-payload e)) (type (event-type e)))
    (flet ((f (k) (obj-get p k)))
      (cond
        ((string= type "concern.raised")
         (sql-exec db "INSERT INTO concerns(id,kind,question,normalised,rationale,features,salience,status,steps,attended,last_attended_at,note,created_at,created_day,updated_at,source_event_id) VALUES(?1,?2,?3,?4,?5,?6,?7,'open',0,0,NULL,NULL,?8,?9,?8,?10)"
                   (f "concern_id") (f "kind") (f "question") (%normalised (f "question")) (f "rationale")
                   (json-encode (f "features")) (f "salience") (event-at e) (subseq (event-at e) 0 10) (event-id e))
         (when (equal "failed-thought" (obj-get (f "origin") "source"))
           (sql-exec db "INSERT OR IGNORE INTO concern_failures(concern_id) VALUES(?1)" (f "concern_id"))))
        ((string= type "concern.reinforced")
         (sql-exec db "UPDATE concerns SET salience=?2,rationale=?3,updated_at=?4 WHERE id=?1"
                   (f "concern_id") (f "salience") (f "rationale") (event-at e)))
        ((string= type "concern.noted")
         (sql-exec db "UPDATE concerns SET note=?2,updated_at=?3 WHERE id=?1" (f "concern_id") (f "text") (event-at e)))
        ((string= type "concern.closed")
         (sql-exec db "UPDATE concerns SET status=?2,note=coalesce(?3,note),updated_at=?4 WHERE id=?1"
                   (f "concern_id") (f "status") (f "reason") (event-at e)))
        ((and (string= type "thought.requested") (f "concern_id"))
         (sql-exec db "INSERT OR IGNORE INTO concern_thoughts(thought_id,concern_id) VALUES(?1,?2)" (event-id e) (f "concern_id"))
         (sql-exec db "UPDATE concerns SET attended=attended+1,last_attended_at=?2 WHERE id=?1" (f "concern_id") (event-at e)))
        ((and (string= type "thought.step") (equal "model" (f "kind")))
         (let ((concern (sql-value db "SELECT concern_id FROM concern_thoughts WHERE thought_id=?1 LIMIT 1"
                                   (sql-value db "SELECT root_id FROM thoughts WHERE id=?1 LIMIT 1" (f "thought_id")))))
           (when concern
             (sql-exec db "UPDATE concerns SET steps=steps+1 WHERE id=?1" concern))))
        ((string= type "quiet.reviewed")
         (sql-exec db "INSERT INTO quiet_marks(name,value,at) VALUES('reviewed',?1,?2) ON CONFLICT(name) DO UPDATE SET value=excluded.value,at=excluded.at"
                   (f "through_event_id") (event-at e)))
        ((string= type "quiet.ruminated")
         (sql-exec db "INSERT INTO quiet_marks(name,value,at) VALUES('ruminated',?1,?2) ON CONFLICT(name) DO UPDATE SET value=excluded.value,at=excluded.at"
                   (f "thought_id") (event-at e))
         (dolist (m (f "memory_ids"))
           (sql-exec db "INSERT INTO rumination_seeds(memory_id,times,last_at) VALUES(?1,1,?2) ON CONFLICT(memory_id) DO UPDATE SET times=times+1,last_at=excluded.last_at"
                     m (event-at e))))
        ((string= type "thought.ended")
         (let ((concern (sql-value db "SELECT concern_id FROM concern_thoughts WHERE thought_id=?1 LIMIT 1" (f "thought_id"))))
           (when (and concern (stringp (f "summary")) (plusp (length (f "summary"))))
             (sql-exec db "UPDATE concerns SET note=?2,updated_at=?3 WHERE id=?1" concern (f "summary") (event-at e)))))))))

(define-projection "concerns"
  :kind :inline
  :consumes ("concern.raised" "concern.reinforced" "concern.noted" "concern.closed"
             "thought.requested" "thought.step" "thought.ended" "quiet.reviewed" "quiet.ruminated")
  :tables ("CREATE TABLE IF NOT EXISTS concerns (id TEXT PRIMARY KEY, kind TEXT NOT NULL, question TEXT NOT NULL, normalised TEXT NOT NULL, rationale TEXT NOT NULL, features TEXT NOT NULL, salience REAL NOT NULL, status TEXT NOT NULL, steps INTEGER NOT NULL, attended INTEGER NOT NULL, last_attended_at TEXT, note TEXT, created_at TEXT NOT NULL, created_day TEXT NOT NULL, updated_at TEXT NOT NULL, source_event_id INTEGER NOT NULL)"
           "CREATE INDEX IF NOT EXISTS concerns_day ON concerns(created_day)"
           ;; Concerns the runtime raised from a failed thought: looked at at once.
           "CREATE TABLE IF NOT EXISTS concern_failures (concern_id TEXT PRIMARY KEY)"
           "CREATE INDEX IF NOT EXISTS concerns_status ON concerns(status,salience)"
           "CREATE TABLE IF NOT EXISTS concern_thoughts (thought_id INTEGER PRIMARY KEY, concern_id TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS quiet_marks (name TEXT PRIMARY KEY, value INTEGER NOT NULL, at TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS rumination_seeds (memory_id TEXT PRIMARY KEY, times INTEGER NOT NULL, last_at TEXT NOT NULL)")
  :entity (lambda (e) (or (obj-get (event-payload e) "concern_id") (princ-to-string (event-id e))))
  :reduce #'%reduce-concerns)

(defun open-concerns (kernel)
  "Open concerns, most salient first: (id kind question rationale salience steps attended last-attended note)."
  (sql-rows (kernel-db kernel)
            "SELECT id,kind,question,rationale,salience,steps,attended,last_attended_at,note FROM concerns WHERE status=?1 ORDER BY salience DESC LIMIT 64"
            "open"))

(defun concern-row (kernel id)
  (sql-row (kernel-db kernel)
           "SELECT id,kind,question,rationale,salience,steps,attended,last_attended_at,note,status FROM concerns WHERE id=?1 LIMIT 1"
           id))

(defun %new-today (kernel)
  (or (sql-value (kernel-db kernel) "SELECT count(*) FROM concerns WHERE created_day=?1 LIMIT 1"
                 (subseq (now-iso8601) 0 10))
      0))

(defun raise-concern (kernel question &key (kind "curiosity") (rationale "") (features (obj)) source-event-ids
                                          (origin (obj "principal_type" "agent")))
  "Raise a concern, or reinforce the open one that asks the same. Returns
(values id outcome) with outcome :raised, :reinforced or a refusal string."
  (unless (member kind +concern-kinds+ :test #'string=)
    (return-from raise-concern (values nil (format nil "kind must be one of ~{~a~^, ~}" +concern-kinds+))))
  (let* ((open (open-concerns kernel))
         (same (find (%normalised question) open :key (lambda (r) (%normalised (third r))) :test #'string=))
         (features (let ((f (obj)))
                     (loop for (name) in +salience-weights+
                           do (setf (gethash name f) (let ((v (obj-get features name))) (if (realp v) (max 0 (min 1 v)) 0.5d0))))
                     (setf (gethash "novelty" f) (if same 0.3d0 1d0))
                     f))
         (salience (salience-of features)))
    (cond (same
           (append! kernel "concern.reinforced"
                    (obj "concern_id" (first same) "rationale" (if (plusp (length rationale)) rationale (fourth same))
                         "salience" (min 1d0 (+ (fifth same) 0.1d0))))
           (values (first same) :reinforced))
          ((>= (length open) (setting kernel "concerns.max_open"))
           (values nil (format nil "already ~d open concerns, the most allowed; satisfy or retire one first"
                               (length open))))
          ((>= (%new-today kernel) (setting kernel "concerns.max_new_per_day"))
           (values nil "today's allowance of new concerns is used up"))
          (t (let ((id (format nil "c-~a" (base64url-encode (ironclad:random-data 6)))))
               (append! kernel "concern.raised"
                        (obj "concern_id" id "kind" kind "question" question "rationale" rationale
                             "features" features "salience" salience
                             "source_event_ids" (or source-event-ids '()) "origin" origin))
               (values id :raised))))))

(defun close-concern (kernel id status reason)
  (append! kernel "concern.closed" (obj "concern_id" id "status" status "reason" reason)))

;;; The tool

(define-tool "concern"
  :description "Keep track of what I mean to look into in quiet time. action:
raise (question, rationale; optionally kind curiosity|follow-up|repair|introspection, and
0-1 scores goal_relevance, personal_relevance, urgency, uncertainty, predicted_value), list,
note (id, text: progress so far), satisfy (id, text: what I found), retire (id, text: why I am
letting it go). Raising a question I already hold reinforces it. Open concerns are capped, so I
raise what matters, and close what is done."
  :summary "Keep track of what I mean to look into in quiet time."
  :available '("operator" "coding-agent")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "raise" "list" "note" "satisfy" "retire"))
                                     "id" (obj "type" "string") "question" (obj "type" "string")
                                     "rationale" (obj "type" "string") "text" (obj "type" "string")
                                     "kind" (obj "type" "string")
                                     "goal_relevance" (obj "type" "number") "personal_relevance" (obj "type" "number")
                                     "urgency" (obj "type" "number") "uncertainty" (obj "type" "number")
                                     "predicted_value" (obj "type" "number"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (let ((action (obj-get args "action")) (id (obj-get args "id")))
           (cond
             ((equal action "raise")
              (let ((question (obj-get args "question")))
                (if (not (and (stringp question) (< 0 (length question) 501)))
                    "Give the question, in at most 500 characters."
                    (multiple-value-bind (cid outcome)
                        (raise-concern kernel question :kind (or (obj-get args "kind") "curiosity")
                                                       :rationale (or (obj-get args "rationale") "")
                                                       :features args :source-event-ids (list thought-id))
                      (case outcome
                        (:raised (format nil "Raised ~a." cid))
                        (:reinforced (format nil "I already hold that; reinforced ~a." cid))
                        (t (format nil "Not raised: ~a." outcome)))))))
             ((equal action "list")
              (let ((open (open-concerns kernel)))
                (if (null open) "No open concerns."
                    (format nil "~{~a~^~%~}"
                            (loop for (cid kind question nil salience steps attended nil note) in open
                                  collect (format nil "~a [~a, salience ~,2f, ~d steps, attended ~d] ~a~@[~%  so far: ~a~]"
                                                  cid kind salience steps attended question note))))))
             ((not (and (stringp id) (concern-row kernel id))) "Give the id of a concern I hold.")
             ((not (equal "open" (tenth (concern-row kernel id)))) "That concern is already closed.")
             ((equal action "note")
              (append! kernel "concern.noted" (obj "concern_id" id "text" (or (obj-get args "text") "")))
              "Noted.")
             ((member action '("satisfy" "retire") :test #'equal)
              (close-concern kernel id (if (equal action "satisfy") "satisfied" "retired") (or (obj-get args "text") ""))
              (format nil "~:(~a~)d ~a." action id))
             (t "action must be raise, list, note, satisfy or retire")))))

;;; What it knows it is looking into

(define-slot-source :conclusions (kernel request max)
  ;; A concern closed in quiet time leaves the open list; what it concluded
  ;; still matters, and may correct something said earlier (bead v3-l3m).
  (loop for (question status note at) in
        (sql-rows (kernel-db kernel)
                  "SELECT question,status,note,updated_at FROM concerns WHERE status IN (?1,?2) AND updated_at>=?3 ORDER BY updated_at DESC LIMIT 8"
                  "satisfied" "retired" (iso8601 (- (now) (* 3 86400))))
        collect (format nil "- ~a (~a ~a)~@[: ~a~]" question status (subseq at 0 10)
                        (and note (if (> (length note) 300) (format nil "~a…" (subseq note 0 300)) note)))))

(define-slot-source :concerns (kernel request max)
  (loop for (id kind question nil nil steps nil nil note) in (open-concerns kernel)
        collect (format nil "- ~a (~a, ~a, ~d steps so far)~@[: so far, ~a~]" question kind id steps
                        (and note (if (> (length note) 200) (format nil "~a…" (subseq note 0 200)) note)))))

;;; Quiet time

(defun %quiet-allowance (kernel)
  "Micro-USD quiet thinking may have spent by now today: the burst share at
midnight UTC, the rest opening evenly through the day (v2's pacing)."
  (let* ((daily (* 1000000 (setting kernel "quiet.daily_usd")))
         (burst (/ (setting kernel "quiet.burst_percent") 100))
         (fraction (/ (mod (now) 86400) 86400)))
    (round (* daily (+ burst (* (- 1 burst) fraction))))))

(defun %seconds-since (iso) (and iso (- (now) (parse-iso8601 iso))))

(defun %raise-from-failures (kernel)
  "Raise a repair concern for each thought that failed or parked since the
last look, from the record alone: no model call."
  (dolist (row (sql-rows (kernel-db kernel)
                         "SELECT id,origin,status,outcome,source_event_id FROM thoughts WHERE status IN (?1,?2) AND id>?3 ORDER BY id LIMIT 20"
                         "failed" "parked" (%failures-cursor kernel)))
    (destructuring-bind (id origin status outcome source) row
      (declare (ignore source))
      (setf (%failures-cursor kernel) id)
      (unless (string= origin "concern")
        (raise-concern kernel (format nil "Why did ~a thought ~d end ~a?" origin id status)
                       :kind "repair"
                       :rationale (format nil "It ended ~a: ~a" status (or outcome "no reason recorded"))
                       :features (obj "personal_relevance" 0.8d0 "urgency" 0.6d0 "uncertainty" 0.7d0
                                      "predicted_value" 0.6d0 "surprise" 1d0)
                       :source-event-ids (list id)
                       :origin (obj "principal_type" "system" "source" "failed-thought"))))))

(defvar *failures-cursor* (make-hash-table :test 'eq :weakness :key)
  "Kernel -> the last failed or parked thought already turned into a concern.")

(defun %failures-cursor (kernel)
  (or (gethash kernel *failures-cursor*)
      ;; On start, look only forward: earlier failures were the operator's to see.
      (setf (gethash kernel *failures-cursor*)
            (or (sql-value (kernel-db kernel) "SELECT max(id) FROM thoughts WHERE status IN (?1,?2) LIMIT 1" "failed" "parked") 0))))

(defun (setf %failures-cursor) (value kernel) (setf (gethash kernel *failures-cursor*) value))

(defun %park-spent (kernel)
  "Park concerns whose lifetime steps are spent, and tell the operator."
  (let ((cap (setting kernel "concerns.lifetime_steps")))
    (dolist (row (open-concerns kernel))
      (destructuring-bind (id kind question rationale salience steps &rest ignore) row
        (declare (ignore kind rationale salience ignore))
        (when (>= steps cap)
          (close-concern kernel id "parked" (format nil "spent its ~d lifetime steps" cap))
          (post-notice kernel :channel "web" :audience "operator"
                              :text (format nil "I parked a concern after ~d steps without settling it: ~a" steps question)))))))

(defun %rotate (kernel)
  "At the cap, retire the stalest investigated concern idle long enough."
  (let ((open (open-concerns kernel)) (idle (* 60 (setting kernel "concerns.rotation_idle_minutes"))))
    (when (and (plusp idle) (>= (length open) (setting kernel "concerns.max_open")))
      (let ((stalest (first (sort (remove-if-not (lambda (r) (and (eighth r) (> (%seconds-since (eighth r)) idle)))
                                                 (copy-list open))
                                  #'string< :key #'eighth))))
        (when stalest
          (close-concern kernel (first stalest) "retired" "rotated out: investigated and idle, to make room"))))))

(defun %effective-salience (kernel row)
  "Salience habituated by attention (design §7.1) and raised by the curiosity drive."
  (let ((curiosity (or (ignore-errors (obj-get (nth-value 1 (affect-now kernel)) "curiosity")) 0)))
    (* (fifth row) (exp (- (/ (seventh row) 3d0))) (+ 0.75d0 (* 0.5d0 curiosity)))))

(defun %quiet-mark (kernel name)
  "(value at) of quiet mark NAME, or NIL."
  (sql-row (kernel-db kernel) "SELECT value,at FROM quiet_marks WHERE name=?1 LIMIT 1" name))

(defun %start-review (kernel)
  "A conversation review when the operator has said something since the last."
  (when (eq t (setting kernel "quiet.review"))
    (let* ((db (kernel-db kernel))
           (latest (sql-value db "SELECT max(event_id) FROM working_dialogue WHERE audience=?1 AND kind=?2 LIMIT 1"
                              "operator" "turn"))
           (seen (or (first (%quiet-mark kernel "reviewed")) 0)))
      (when (and latest (> latest seen))
        (let ((id (request-thought
                   kernel "introspection"
                   "Quiet time, after a conversation. Look back over the recent conversation and your open concerns. Is anything worth looking into later: a question you could not settle, something you said you would check, a curiosity, a problem in yourself? Raise at most two concerns with the concern tool, or reinforce one you already hold. If nothing warrants it, raise nothing and say so in a line.")))
          (append! kernel "quiet.reviewed"
                   (obj "through_event_id" (sql-value db "SELECT max(event_id) FROM working_dialogue WHERE audience=?1 LIMIT 1" "operator")
                        "thought_id" id))
          id)))))

(defparameter +repair-steps+
  "This is a repair: something of yours failed. Work from evidence, not memory. 1. Look: inspect with what failures shows
what the model provider actually sent on recent failed calls, and the failed thought's own steps show what led there.
2. Find the cause, and say how sure you are. 3. If the cause is in your own code and the fix is small and clear, make it
live with lisp_eval and check that it works; a live repair lasts until restart, so it must also be proposed. 4. If you
have the beads tool, open a bead with priority 0, the evidence and your fix (or your best diagnosis), and ping the
coding agent. 5. Tell the operator in a line what failed and what you did, with notify_operator if it matters now.")

(defun %concern-thought (kernel row)
  "Start a thought for the concern ROW."
  (destructuring-bind (id kind question rationale salience steps attended last note) row
    (declare (ignore salience last))
    (request-thought
     kernel "concern"
     (format nil "~:[Quiet time~;A failure, looked at at once~]. You are looking into one of your concerns (~a, ~a):~%~%~a~%~%~
Why it matters: ~a~@[~%~%Where you got to before: ~a~]~%~%This is visit ~d. Two budgets apply, in model calls: ~
this thought may make up to ~d, and this concern may spend ~d in total across all its visits, of which ~d are used. ~
Work on it now. Use the concern tool to note progress, satisfy it when it is settled, or retire it ~
if it is not worth more.~:[ If it is a problem in yourself, find or open a bead and ping the coding agent.~;~%~%~a~]"
             (zerop attended) id kind question rationale note (1+ attended)
             (getf (%policy kernel "concern") :max-calls) (setting kernel "concerns.lifetime_steps") steps
             (equal kind "repair") +repair-steps+)
     :concern-id id)))

(defun %start-failure-repair (kernel)
  "A repair concern raised from a failure and not yet looked at starts at
once, ahead of the operator's grace period: its evidence is freshest now."
  (let ((pick (find-if (lambda (r) (and (equal "repair" (second r)) (zerop (seventh r))
                                        (sql-value (kernel-db kernel) "SELECT 1 FROM concern_failures WHERE concern_id=?1 LIMIT 1" (first r))))
                       (open-concerns kernel))))
    (when pick (%concern-thought kernel pick))))

(defun %start-concern (kernel)
  "A thought for the most salient open concern ready to be attended."
  (let* ((revisit (setting kernel "quiet.revisit_seconds"))
         (ready (remove-if (lambda (r) (and (eighth r) (< (%seconds-since (eighth r)) revisit)))
                           (open-concerns kernel))))
    (when ready
      (%concern-thought kernel (first (sort (copy-list ready) #'> :key (lambda (r) (%effective-salience kernel r))))))))

(defun %rumination-seeds (kernel)
  "Up to two memories to wander over: strong ones, the less often seeded the
likelier, so one memory does not become a loop."
  (let* ((allowed (sensitivities-for kernel "operator"))
         (rows (apply #'sql-rows (kernel-db kernel)
                      (format nil "SELECT m.id,m.content,m.strength,coalesce(s.times,0) FROM memory_nodes m LEFT JOIN rumination_seeds s ON s.memory_id=m.id WHERE m.sensitivity IN (~a) ORDER BY m.strength DESC LIMIT 200"
                              (%in-list 1 (length allowed)))
                      allowed))
         (weighted (mapcar (lambda (r) (cons (* (random 1d0) (max 0.05d0 (third r)) (/ 1d0 (1+ (* 2 (fourth r))))) r))
                           rows)))
    (mapcar #'cdr (subseq (sort weighted #'> :key #'car) 0 (min 2 (length weighted))))))

(defun %start-rumination (kernel)
  "With nothing open, let the mind wander: over memories, mood and summary,
about every quiet.rumination_minutes, sooner when bored. Without any such
material, nothing."
  (let ((minutes (setting kernel "quiet.rumination_minutes")))
    (when (and (plusp minutes) (null (open-concerns kernel)))
      (let* ((boredom (or (ignore-errors (obj-get (affect-now kernel) "boredom")) 0.2d0))
             (due (/ (* 60 minutes) (+ 0.5d0 boredom)))
             (last (second (%quiet-mark kernel "ruminated"))))
        (when (or (null last) (>= (%seconds-since last) due))
          (let ((seeds (%rumination-seeds kernel))
                (summary (sql-value (kernel-db kernel) "SELECT text FROM working_summary WHERE audience=?1 LIMIT 1" "operator")))
            (when (or seeds summary)
              (let ((id (request-thought
                         kernel "introspection"
                         (format nil "Quiet time, with nothing in hand. Let your mind wander over what is already in you, ~
as a person's does when nothing is happening.~%~{~%A memory: ~a~}~@[~%~%Earlier, in summary: ~a~]~%~%~
Follow whatever is interesting in it, briefly. If something deserves real attention later, raise a concern. ~
If it does not, let it go: a line on what crossed your mind is enough. Do not go over the same ground twice."
                                 (mapcar #'second seeds)
                                 (and summary (if (> (length summary) 1200) (subseq summary 0 1200) summary))))))
                (append! kernel "quiet.ruminated" (obj "memory_ids" (mapcar #'first seeds) "thought_id" id))
                id))))))))

(defvar *quiet-starters* '()
  "Functions of the kernel that later layers offer as quiet-time material,
tried after the work docket and before rumination; each returns a thought id
or NIL. The fleet digest is one.")

(defun quiet-tick (kernel)
  "In quiet time, give one thought to the best material there is: a review of
new conversation, then the most salient open concern, then the work item that
comes first, then, with none of those, rumination. Returns the thought id, or NIL with the reason nothing started."
  (%raise-from-failures kernel)
  (%park-spent kernel)
  (%park-spent-work kernel)
  (%rotate kernel)
  (let ((db (kernel-db kernel)))
    (flet ((no (why) (return-from quiet-tick (values nil why))))
      (unless (eq t (setting kernel "quiet.enabled")) (no "off"))
      (when (sql-value db "SELECT 1 FROM thoughts WHERE status IN (?1,?2) LIMIT 1" "queued" "running") (no "busy"))
      (when (>= (spent-today kernel "quiet") (%quiet-allowance kernel)) (no "paced"))
      (let ((repair (%start-failure-repair kernel)))
        (when repair (return-from quiet-tick repair)))
      (let ((last-turn (sql-value db "SELECT at FROM working_dialogue WHERE audience=?1 AND kind=?2 ORDER BY event_id DESC LIMIT 1"
                                  "operator" "turn")))
        (when (and last-turn (< (%seconds-since last-turn) (setting kernel "quiet.grace_seconds"))) (no "grace")))
      (or (%start-review kernel)
          (%start-concern kernel)
          (%start-work kernel)
          (some (lambda (starter) (ignore-errors (funcall starter kernel))) *quiet-starters*)
          (%start-rumination kernel)
          (no "nothing open")))))
