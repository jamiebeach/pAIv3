(in-package #:pai-v3.api)

;;; Reflect, then speak (docs/reflect-then-speak.md). A reflection thinks with
;;; everything the agent knows and cannot publish. When it wants to say
;;; something to a peer it leaves a DRAFT; when the reflection is over, each
;;; draft becomes a thought for that peer's audience, with context filtered
;;; for her, which sends it, edits it, or drops it.
;;;
;;; The speaking thought is shown the draft's words and where it answers, never
;;; the reflection's reasons: it must judge the words, and cannot be argued into
;;; them by a thought that knew more than the peer may.

(define-event-type "fleet.draft.made"
  :schema (("draft_id" :string) ("thought_id" :integer) ("peer_id" :string) ("intent" :string) ("text" :string)
           ("thread_id" :string :optional) ("reply_to" :string :optional) ("why" :string :optional)))
(define-event-type "fleet.draft.settled"
  :schema (("draft_id" :string) ("fate" :string) ("note" :string :optional)))

(define-projection "fleet_drafts"
  :kind :inline
  :consumes ("fleet.draft.made" "fleet.draft.settled"
             "thought.requested" "thought.ended" "thought.failed" "thought.parked")
  :tables ("CREATE TABLE IF NOT EXISTS fleet_drafts (draft_id TEXT PRIMARY KEY, thought_id INTEGER NOT NULL, peer_id TEXT NOT NULL, thread_id TEXT, reply_to TEXT, intent TEXT NOT NULL, text TEXT NOT NULL, why TEXT, state TEXT NOT NULL DEFAULT 'open', fate TEXT, note TEXT, created_at TEXT NOT NULL, source_event_id INTEGER NOT NULL UNIQUE, speaking_thought_id INTEGER)"
           "CREATE INDEX IF NOT EXISTS fleet_drafts_state_idx ON fleet_drafts(state,created_at)"
           "CREATE INDEX IF NOT EXISTS fleet_drafts_thought_idx ON fleet_drafts(thought_id)"
           "CREATE INDEX IF NOT EXISTS fleet_drafts_speaking_idx ON fleet_drafts(speaking_thought_id)")
  :entity (lambda (e) (obj-get (event-payload e) "draft_id"))
  :reduce
  (lambda (db e)
    (let ((p (event-payload e)) (type (event-type e)))
      (flet ((f (k) (%given (obj-get p k))))
        (cond
          ((string= type "fleet.draft.made")
           (sql-exec db "INSERT INTO fleet_drafts(draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,why,created_at,source_event_id) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
                     (f "draft_id") (f "thought_id") (f "peer_id") (f "thread_id") (f "reply_to") (f "intent") (f "text") (f "why") (event-at e) (event-id e)))
          ((string= type "thought.requested")
           (when (f "source_event_id")
             (sql-exec db "UPDATE fleet_drafts SET state='speaking',speaking_thought_id=?2 WHERE source_event_id=?1 AND state='ready'"
                       (f "source_event_id") (event-id e))))
          ((member type '("thought.ended" "thought.failed" "thought.parked") :test #'equal)
           ;; Completed private thought: its drafts are ready. Incomplete speaking
           ;; steps remain visible for review, rather than silently disappearing.
           (sql-exec db "UPDATE fleet_drafts SET state='ready' WHERE thought_id=?1 AND state='open'" (f "thought_id"))
           (sql-exec db "UPDATE fleet_drafts SET state='unresolved',note='Speaking thought ended without settling the draft' WHERE speaking_thought_id=?1 AND state='speaking'"
                     (f "thought_id")))
          (t
           (sql-exec db "UPDATE fleet_drafts SET state='settled',fate=?2,note=?3 WHERE draft_id=?1" (f "draft_id") (f "fate") (f "note"))))))))

(define-setting "fleet.drafts_per_reflection"
  :type :integer :min 1 :max 16 :default 3 :group "Fleet"
  :description "Most peer drafts one private reflection may leave, including its subthoughts.")

(defun draft-rows (kernel &key (state "open") (limit 20))
  "Drafts in STATE as objects, oldest first."
  (mapcar (lambda (r)
            (destructuring-bind (id thought peer thread reply intent text why state fate note at) r
              (obj "draft_id" id "thought_id" thought "peer_id" peer "thread_id" (or thread :null) "reply_to" (or reply :null)
                   "intent" intent "text" text "why" (or why :null) "state" state "fate" (or fate :null) "note" (or note :null) "at" at)))
          (sql-rows (kernel-db kernel)
                    "SELECT draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,why,state,fate,note,created_at FROM fleet_drafts WHERE state=?1 ORDER BY created_at LIMIT ?2"
                    state limit)))

;;; Leaving a draft (a reflection only)

(define-tool "draft_fleet_message"
  :description "While reflecting, leave a draft of something to say to a peer. Nothing is sent now: when this thought is over, a separate step for that peer reads the draft with only what that peer may hear in front of it, and sends it, edits it or drops it. So write only what you would tell that peer. peer_id and text are required; thread_id and reply_to when answering a post on a board; intent is question, answer, fyi, request or proposal; why is a line for the operator's record (the peer's step never sees it). At most three drafts in one reflection."
  :summary "Leave a draft of something to say to a peer, to be checked and sent."
  :available '("operator")
  :core '("reflection" "introspection")
  :parameters (obj "type" "object"
                   "properties" (obj "peer_id" (obj "type" "string") "text" (obj "type" "string")
                                     "thread_id" (obj "type" "string") "reply_to" (obj "type" "string")
                                     "intent" (obj "type" "string") "why" (obj "type" "string"))
                   "required" (list "peer_id" "text"))
  :run (lambda (kernel thought-id args)
         (let* ((row (thought-row kernel thought-id))
                (root-id (getf row :root-id))
               (peer-id (obj-get args "peer_id")) (text (obj-get args "text"))
               (intent (or (%given (obj-get args "intent")) "fyi")))
           (cond ((not (reflecting-p row))
                  "Drafts are for reflecting. In this thought, send it directly with post_fleet_message or reply_fleet_board_message.")
                 ((not (and (stringp peer-id) (fleet-peer kernel peer-id)))
                  "Not done: that is not one of my peers; list_fleet_peers names them.")
                 ((not (and (stringp text) (< 0 (length (string-trim " " text)) 3001)))
                  "Not done: write 1 to 3000 characters.")
                 ((some (lambda (key)
                          (let ((value (%given (obj-get args key))))
                            (and value (not (and (stringp value) (<= (length value) 128))))))
                        '("thread_id" "reply_to"))
                  "Not done: thread_id and reply_to must be short identifiers.")
                 ((not (member intent +board-intents+ :test #'string=))
                  (format nil "Not done: intent is one of ~{~a~^, ~}." +board-intents+))
                 ((>= (or (sql-value (kernel-db kernel) "SELECT count(*) FROM fleet_drafts WHERE thought_id=?1 LIMIT 1" root-id) 0)
                      (setting kernel "fleet.drafts_per_reflection"))
                  (format nil "Not done: at most ~d drafts in one reflection." (setting kernel "fleet.drafts_per_reflection")))
                 (t (let ((id (format nil "d-~(~36r~)-~(~36r~)" (now) (random (expt 36 3)))))
                      (append! kernel "fleet.draft.made"
                               (obj "draft_id" id "thought_id" root-id "peer_id" peer-id "intent" intent "text" (string-trim " " text)
                                    "thread_id" (%given (obj-get args "thread_id")) "reply_to" (%given (obj-get args "reply_to"))
                                    "why" (let ((w (%given (obj-get args "why")))) (and (stringp w) (subseq w 0 (min 400 (length w)))))))
                      (format nil "Draft ~a left for ~a. It is not sent; it will be checked for that peer when this thought is over."
                              id (second (fleet-peer kernel peer-id)))))))))

;;; Settling a draft (the speaking step)

(define-tool "settle_draft"
  :description "Say what you did with a draft you were asked to check: fate is sent (as written), edited (sent in other words) or dropped (not sent), and note a line why."
  :summary "Record what became of a draft."
  :available :operator-and-peers
  :core '("work")
  :parameters (obj "type" "object"
                   "properties" (obj "draft_id" (obj "type" "string")
                                     "fate" (obj "type" "string" "enum" (list "sent" "edited" "dropped"))
                                     "note" (obj "type" "string"))
                   "required" (list "draft_id" "fate"))
  :run (lambda (kernel thought-id args)
         (let* ((draft (obj-get args "draft_id"))
                (row (and (stringp draft)
                          (sql-row (kernel-db kernel) "SELECT peer_id,state,speaking_thought_id FROM fleet_drafts WHERE draft_id=?1 LIMIT 1" draft)))
                (audience (getf (thought-row kernel thought-id) :audience)))
           (cond ((null row) "Not done: there is no such draft.")
                 ((not (and (equal audience (format nil "peer:~a" (first row)))
                            (eql (getf (thought-row kernel thought-id) :root-id) (third row))))
                  "Not done: that draft is not for the peer this thought is for.")
                 ((not (equal "speaking" (second row))) "Not done: that draft is not waiting to be settled.")
                 ((not (member (obj-get args "fate") '("sent" "edited" "dropped") :test #'equal))
                  "Not done: fate is sent, edited or dropped.")
                 (t (append! kernel "fleet.draft.settled"
                             (obj "draft_id" draft "fate" (obj-get args "fate")
                                  "note" (let ((n (%given (obj-get args "note")))) (and (stringp n) (subseq n 0 (min 300 (length n)))))))
                    (format nil "Recorded: ~a ~a." draft (obj-get args "fate")))))))

;;; Saying it: when a reflection is over, each of its drafts goes to a thought for that peer

(defun %speaking-prompt (kernel peer-id draft-id thread reply intent text)
  (let ((send (if (and thread reply (board-thread kernel thread))
                  (format nil "reply_fleet_board_message (thread_id ~a, reply_to ~a)" thread reply)
                  (format nil "post_fleet_message (peer_id ~a~@[, thread_id ~a~]~@[, reply_to ~a~])" peer-id thread reply))))
    (format nil "A draft reply for ~a was written while thinking privately, by a thought that knew things this peer may not be allowed to hear.~%~%~
Draft ~a (~a~@[, answering thread ~a~]~@[, message ~a~]):~%~%~a~%~%~
Read it as this peer's thought, with only what she may hear in front of you. Send it only if it says nothing she may not hear: you may edit the words, or drop it. ~
To send, use ~a. Then call settle_draft with draft_id ~a and what you did (sent, edited or dropped), and a line why."
            (second (fleet-peer kernel peer-id)) draft-id intent thread reply text send draft-id)))

(defun speak-drafts (kernel)
  "Dispatch ready drafts and resume interrupted scheduling through the one work queue."
  (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
    (let ((n 0) (db (kernel-db kernel)))
      (loop for (draft-id thought-id peer-id thread reply intent text source)
              in (sql-rows db
                           "SELECT draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,source_event_id FROM fleet_drafts WHERE state=?1 ORDER BY created_at LIMIT 10"
                           "ready")
            do (if (fleet-peer kernel peer-id)
                   (progn
                     ;; The draft projection consumes this request and links its
                     ;; speaking thought atomically. No pre-claim can strand it.
                     (request-thought kernel "work" (%speaking-prompt kernel peer-id draft-id thread reply intent text)
                                      :audience (format nil "peer:~a" peer-id) :source-event-id source)
                     (incf n))
                   (append! kernel "fleet.draft.settled"
                            (obj "draft_id" draft-id "fate" "dropped" "note" "that peer is no longer known"))))
      ;; request-thought records before queuing work. A crash in that gap is
      ;; recovered by queuing the same thought, never by making another one.
      (dolist (row (sql-rows db
                            "SELECT d.speaking_thought_id FROM fleet_drafts d JOIN thoughts t ON t.id=d.speaking_thought_id WHERE d.state=?1 AND t.status IN (?2,?3) LIMIT 10"
                            "speaking" "queued" "running"))
        (let ((id (first row)))
          (unless (work-pending-p kernel "thought" id)
            (request-work kernel "thought" id (obj) :priority :work :caused-by id))))
      n)))

(pushnew 'speak-drafts *periodic-hooks*)
