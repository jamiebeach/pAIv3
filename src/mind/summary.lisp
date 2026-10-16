(in-package #:pai-v3.mind)

;;; The working summary (spec §5.2 "working summary"; v2 reference
;;; src/mind/conscious/working-context-summary.lisp). Conversation older than
;;; what the dialogue slot shows is folded, per audience, into one rolling
;;; summary: the previous summary plus the turns since, rewritten by the
;;; summary route's model. The result is a summary.updated event carrying the
;;; text and the last turn it covers, so the projection is pure and the paid
;;; work is never repeated by a rebuild. Turn time reads one row.
;;;
;;; A check that runs with the runtime's periodic work asks for a fold when
;;; enough unsummarised conversation has slipped out of the dialogue window.
;;; Folds are bounded, one at a time per audience, and admitted against the
;;; budget like any paid work.

(define-setting "summary.keep_recent_tokens"
  :type :integer :min 500 :max 50000 :default 4000 :group "Context"
  :description "Conversation this recent is left to the dialogue slot and not yet summarised.")
(define-setting "summary.fold_tokens"
  :type :integer :min 500 :max 50000 :default 3000 :group "Context"
  :description "How much older, unsummarised conversation builds up before it is folded into the summary.")
(define-setting "summary.max_input_tokens"
  :type :integer :min 1000 :max 100000 :default 12000 :group "Context"
  :description "The most conversation one fold reads; a longer backlog is folded in several steps.")
(define-setting "summary.target_words"
  :type :integer :min 50 :max 2000 :default 400 :group "Context"
  :description "About how long the summary should be.")

(define-event-type "summary.updated"
  :schema (("audience" :string) ("text" :string) ("covers_to" :integer)
           ("turns" :integer) ("model" :string :optional)))

(define-projection "working_summary"
  :kind :inline
  :consumes ("summary.updated" "turn.received")
  :tables ("CREATE TABLE IF NOT EXISTS working_summary (audience TEXT PRIMARY KEY, text TEXT NOT NULL, tokens INTEGER NOT NULL, covers_to INTEGER NOT NULL, at TEXT NOT NULL)"
           ;; Everyone who has spoken, so the fold check finds them with one keyed read.
           "CREATE TABLE IF NOT EXISTS summary_audiences (audience TEXT PRIMARY KEY, kind TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS summary_audiences_kind_idx ON summary_audiences(kind)")
  :entity (lambda (e) (obj-get (event-payload e) "audience"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (string= (event-type e) "turn.received")
                  (sql-exec db "INSERT OR IGNORE INTO summary_audiences(audience,kind) VALUES(?1,'spoken')"
                            (obj-get p "audience"))
              (sql-exec db "INSERT INTO working_summary(audience,text,tokens,covers_to,at) VALUES(?1,?2,?3,?4,?5) ON CONFLICT(audience) DO UPDATE SET text=excluded.text,tokens=excluded.tokens,covers_to=excluded.covers_to,at=excluded.at WHERE excluded.covers_to>working_summary.covers_to"
                        (obj-get p "audience") (obj-get p "text") (estimate-tokens (obj-get p "text"))
                        (obj-get p "covers_to") (event-at e))))))

(defun working-summary (kernel audience)
  "(text tokens covers-to) for AUDIENCE, or NIL."
  (sql-row (kernel-db kernel)
           "SELECT text,tokens,covers_to FROM working_summary WHERE audience=?1 LIMIT 1" audience))

(define-slot-source :summary (kernel request max)
  (let ((row (working-summary kernel (getf request :audience))))
    (and row (list (list (first row) (second row))))))

;;; What needs folding

(defun %unsummarised (kernel audience)
  "Turns for AUDIENCE after the summary and older than the recent window,
oldest first, as (event-id rendered tokens), bounded by the input limit."
  (let* ((covers (or (third (working-summary kernel audience)) 0))
         (recent (recent-dialogue kernel audience :token-budget (setting kernel "summary.keep_recent_tokens")))
         (window-start (if recent (dialogue-row-event-id (first recent)) most-positive-fixnum))
         (limit (setting kernel "summary.max_input_tokens"))
         (used 0))
    (loop for row in (sql-rows (kernel-db kernel)
                               "SELECT event_id,rendered,tokens FROM working_dialogue WHERE audience=?1 AND event_id>?2 AND event_id<?3 ORDER BY event_id LIMIT 400"
                               audience covers window-start)
          while (<= (+ used (third row)) limit)
          do (incf used (third row))
          collect row)))

(defun request-summaries (kernel)
  "Ask for a fold for each audience with enough unsummarised conversation.
Cheap: one small query per audience that has spoken."
  (dolist (audience (mapcar #'first (sql-rows (kernel-db kernel)
                                              "SELECT audience FROM summary_audiences WHERE kind=?1 LIMIT 256"
                                              "spoken")))
    (unless (work-pending-p kernel "summary" audience)
      (let ((pending (%unsummarised kernel audience)))
        (when (>= (reduce #'+ pending :key #'third) (setting kernel "summary.fold_tokens"))
          (request-work kernel "summary" audience (obj) :priority :maintenance))))))

;;; The fold

(defun summary-messages (prior turns target-words)
  (list (obj "role" "system"
             "content" (format nil "You keep a running summary of a conversation for the one who took part in it, written in their own first person, so they can pick up where things stand. Keep what matters later: decisions, commitments and who made them, open questions, plans, facts learned about people, and how the conversation felt. Drop small talk that led nowhere. Never invent anything. Write about ~d words of plain prose." target-words))
        (obj "role" "user"
             "content" (format nil "~@[Summary so far:~%~a~%~%~]Conversation since then:~%~{~a~^~%~}~%~%Write the new summary, covering both."
                               prior turns))))

(defun fold-summary (kernel work)
  "One bounded fold for the audience named by WORK's entity."
  (let* ((audience (getf work :entity))
         (turns (%unsummarised kernel audience)))
    (when turns
      (let* ((profile (profile-for kernel "summary"))
             (reply (complete kernel profile
                              (summary-messages (first (working-summary kernel audience))
                                                (mapcar #'second turns)
                                                (setting kernel "summary.target_words"))
                              :work-id (getf work :id)))
             (text (string-trim '(#\Space #\Newline #\Tab) (obj-get reply "content" ""))))
        (when (zerop (length text))
          (error 'handler-failure :class :model :reason "the model returned an empty summary"))
        (append! kernel "summary.updated"
                 (obj "audience" audience "text" text "covers_to" (first (car (last turns)))
                      "turns" (length turns) "model" (provider-profile-model profile)))
        (obj "covers_to" (first (car (last turns))))))))

(define-handler "summary" :run #'fold-summary :paid t :estimate-microusd 3000)

;;; Periodic work the runtime loop runs every few seconds, busy or idle.

(defvar *periodic-hooks* '()
  "Functions of the kernel that later layers add to the periodic work, such
as the fleet's notification retries.")

(defun run-periodic (kernel)
  "Fire due reminders, ask for any summary folds that are due, and give quiet
time to an open concern. Each part fails on its own."
  (ignore-errors (run-due-schedules kernel))
  (ignore-errors (request-summaries kernel))
  (ignore-errors (quiet-tick kernel))
  (ignore-errors (request-graph-formation kernel))
  (ignore-errors (request-episode-sealing kernel))
  (ignore-errors (request-missing-embeddings kernel))
  (dolist (hook *periodic-hooks*) (ignore-errors (funcall hook kernel))))
