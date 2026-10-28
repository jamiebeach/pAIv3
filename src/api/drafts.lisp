(in-package #:pai-v3.api)

;;; Reflect, then speak (docs/reflect-then-speak.md). A reflection thinks with
;;; everything the agent knows and cannot publish. When it wants to say
;;; something to a peer it leaves a DRAFT; when the reflection is over, each
;;; draft becomes a thought for that peer's audience, with context filtered
;;; for that peer, which sends it, edits it, or drops it.
;;;
;;; The speaking thought is shown the draft's words and where it answers, never
;;; the reflection's reasons: it must judge the words, and cannot be argued into
;;; them by a thought that knew more than the peer may.

(define-event-type "fleet.draft.made"
  :schema (("draft_id" :string) ("thought_id" :integer) ("peer_id" :string) ("intent" :string) ("text" :string)
           ("thread_id" :string :optional) ("reply_to" :string :optional) ("why" :string :optional)))
(define-event-type "fleet.draft.settled"
  :schema (("draft_id" :string) ("fate" :string) ("note" :string :optional)))
(define-event-type "fleet.draft.retried"
  :schema (("draft_id" :string) ("note" :string :optional)))

(define-projection "fleet_drafts"
  :kind :inline
  :version 2
  :consumes ("fleet.draft.made" "fleet.draft.settled" "fleet.draft.retried"
             "thought.requested" "thought.ended" "thought.failed" "thought.parked")
  :tables ("CREATE TABLE IF NOT EXISTS fleet_drafts (draft_id TEXT PRIMARY KEY, thought_id INTEGER NOT NULL, peer_id TEXT NOT NULL, thread_id TEXT, reply_to TEXT, intent TEXT NOT NULL, text TEXT NOT NULL, why TEXT, state TEXT NOT NULL DEFAULT 'open', fate TEXT, note TEXT, created_at TEXT NOT NULL, source_event_id INTEGER NOT NULL UNIQUE, speaking_thought_id INTEGER, retries INTEGER NOT NULL DEFAULT 0)"
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
          ((string= type "fleet.draft.retried")
           ;; Back to ready: the next speak-drafts gives it a new speaking thought.
           (sql-exec db "UPDATE fleet_drafts SET state='ready',speaking_thought_id=NULL,retries=retries+1,note=?2 WHERE draft_id=?1 AND state='unresolved'"
                     (f "draft_id") (f "note")))
          (t
           (sql-exec db "UPDATE fleet_drafts SET state='settled',fate=?2,note=?3 WHERE draft_id=?1" (f "draft_id") (f "fate") (f "note"))))))))

(define-event-type "fleet.draft.envelope"
  :schema (("draft_id" :string) ("images" :array :optional) ("sharing_topics" :array :optional) ("visibility" :string :optional)))

;; New-event-only projection: existing agents start empty, no historical replay.
(define-projection "fleet_draft_envelopes"
  :kind :inline :inspectable nil :consumes ("fleet.draft.envelope")
  :tables ("CREATE TABLE IF NOT EXISTS fleet_draft_envelopes (draft_id TEXT PRIMARY KEY, images TEXT NOT NULL, sharing_topics TEXT NOT NULL, visibility TEXT NOT NULL DEFAULT 'pairwise')")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT OR IGNORE INTO fleet_draft_envelopes(draft_id,images,sharing_topics,visibility) VALUES(?1,?2,?3,?4)"
                        (obj-get p "draft_id") (json-encode (obj-get p "images")) (json-encode (obj-get p "sharing_topics")) (obj-get p "visibility" "pairwise")))))

(defun %draft-envelope (kernel draft-id)
  (let ((r (sql-row (kernel-db kernel) "SELECT images,sharing_topics,visibility FROM fleet_draft_envelopes WHERE draft_id=?1 LIMIT 1" draft-id)))
    (if r (values (json-parse (first r)) (json-parse (second r)) (third r)) (values nil nil "pairwise"))))

(defun %review-draft (kernel root)
  (sql-row (kernel-db kernel)
           "SELECT draft_id,peer_id,thread_id,reply_to,intent FROM fleet_drafts WHERE speaking_thought_id=?1 AND state=?2 LIMIT 1"
           root "speaking"))

(setf *peer-wake-count*
      (lambda (kernel peer-id)
        (or (sql-value (kernel-db kernel)
                       "SELECT count(*) FROM thoughts t WHERE t.audience=?1 AND t.created_at>=?2 AND t.parent_id IS NULL AND t.origin='work' AND NOT EXISTS (SELECT 1 FROM fleet_drafts d WHERE d.speaking_thought_id=t.id LIMIT 1) LIMIT 1"
                       (format nil "peer:~a" peer-id) (iso8601 (- (now) 3600))) 0)))

(setf pai-v3.mind::*communication-root-p*
      (lambda (kernel root)
        ;; Also recognizes settled reviews on restart/parent return. Scope never
        ;; changes merely because settle_draft completed during a thought.
        (sql-value (kernel-db kernel) "SELECT 1 FROM fleet_drafts WHERE speaking_thought_id=?1 LIMIT 1" root)))

(setf pai-v3.mind::*communication-egress-guard*
      (lambda (kernel thought name args)
        (let ((draft (and (getf thought :communicating) (%review-draft kernel (getf thought :root-id)))))
          (and draft
               (destructuring-bind (id peer thread reply intent) draft
                 (multiple-value-bind (images topics visibility) (%draft-envelope kernel id)
                   (values (and (or (equal visibility "pairwise") (null topics)) (every (lambda (topic) (member topic (pai-v3.mind::%grants-of kernel (format nil "peer:~a" peer)) :test #'equal)) topics)
                        (equal (obj-get args "intent" intent) intent)
                        (every (lambda (image) (member image images :test #'equal)) (%arg-images args))
                        (cond ((equal name "post_fleet_message")
                               (and (equal peer (obj-get args "peer_id"))
                                    (equal thread (%given (obj-get args "thread_id")))
                                    (equal reply (%given (obj-get args "reply_to")))
                                    (not (eq t (obj-get args "new_thread")))))
                              ((equal name "reply_fleet_board_message")
                               (and thread reply (board-thread kernel thread)
                                    (let* ((messages (board-messages kernel thread))
                                           (parent (find reply messages :key #'first :test #'equal)))
                                      (and parent (equal peer (first (%notification-peer kernel messages parent)))))
                                    (equal thread (obj-get args "thread_id")) (equal reply (obj-get args "reply_to"))))
                              (t nil)))
                           (and (equal visibility "pairwise") peer))))))))

(define-setting "fleet.drafts_per_reflection"
  :type :integer :min 1 :max 16 :default 3 :group "Fleet"
  :description "Most peer drafts one private reflection may leave, including its subthoughts.")

(define-setting "fleet.same_operator_peers"
  :type :array :default '() :group "Fleet"
  :validate (lambda (v) (unless (every (lambda (s) (and (stringp s) (< 0 (length s) 200))) v)
                          "each entry is a peer's name or id"))
  :description "Peers that share my operator, by name or id, e.g. [\"Aria\", \"Angel\"]. They may receive what the operator may see: the operator's calendar, mail and private details, and my own looks and pictures. Never credentials or secrets.")

(defun same-operator-peer-p (kernel peer-id)
  "True when the operator has marked PEER-ID (or its name) as sharing my operator."
  (let ((marked (setting kernel "fleet.same_operator_peers"))
        (name (second (fleet-peer kernel peer-id))))
    (and peer-id (some (lambda (m) (or (equal m peer-id) (and name (string-equal m name)))) marked) t)))

(defun recent-drafts (kernel &key (limit 40))
  "Drafts in every state, newest first, for the observability page."
  (mapcar (lambda (r)
            (destructuring-bind (id thought peer thread reply intent text why state fate note at speaking) r
              (obj "draft_id" id "thought_id" thought "peer_id" peer "peer_name" (or (second (fleet-peer kernel peer)) peer)
                   "thread_id" (or thread :null) "reply_to" (or reply :null) "intent" intent "text" text
                   "why" (or why :null) "state" state "fate" (or fate :null) "note" (or note :null) "at" at
                   "speaking_thought_id" (or speaking :null))))
          (sql-rows (kernel-db kernel)
                    "SELECT draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,why,state,fate,note,created_at,speaking_thought_id FROM fleet_drafts WHERE state IN (?1,?2,?3,?4,?5) ORDER BY created_at DESC LIMIT ?6"
                    "open" "ready" "speaking" "settled" "unresolved" limit)))

(defun draft-rows (kernel &key (state "open") (limit 20))
  "Drafts in STATE as objects, oldest first."
  (mapcar (lambda (r)
            (destructuring-bind (id thought peer thread reply intent text why state fate note at) r
              (obj "draft_id" id "thought_id" thought "peer_id" peer "thread_id" (or thread :null) "reply_to" (or reply :null)
                   "intent" intent "text" text "why" (or why :null) "state" state "fate" (or fate :null) "note" (or note :null) "at" at)))
          (sql-rows (kernel-db kernel)
                    "SELECT draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,why,state,fate,note,created_at FROM fleet_drafts WHERE state=?1 ORDER BY created_at LIMIT ?2"
                    state limit)))

;;; Leaving a draft (private initiative only)

(define-tool "draft_fleet_message"
  :description "While thinking privately, leave a draft of something to say to a peer. Nothing is sent now: when this thought is over, a separate step for that peer reads the draft with only what that peer may hear in front of it, and sends it, edits it or drops it. So write only what you would tell that peer. peer_id and text are required; thread_id and reply_to together, each the full id as the board shows it, when answering a post on a board; intent is question, answer, fyi, request or proposal; why is a line for the operator's record (the peer's step never sees it). images: up to three actual stored image IDs to disclose and inspect. sharing_topics: operator-granted topics permitting this disclosure (for example daily-brief); do not invent grants. A description is not an image reference. Only candidate text and selected images reach the review, never private why. At most the configured per-root draft allowance (default three) in one private initiative."
  :summary "Draft a checked communication: pairwise by default; fleet visibility only for material every peer may receive. images are stored image IDs, sharing_topics are operator grants."
  :available :operator-and-peers
  :core t
  :parameters (obj "type" "object"
                   "properties" (obj "peer_id" (obj "type" "string") "text" (obj "type" "string")
                                     "thread_id" (obj "type" "string") "reply_to" (obj "type" "string")
                                     "intent" (obj "type" "string") "why" (obj "type" "string")
                                     "images" (obj "type" "array" "items" (obj "type" "string"))
                                     "sharing_topics" (obj "type" "array" "items" (obj "type" "string"))
                                     "visibility" (obj "type" "string" "enum" '("pairwise" "fleet")
                                                       "description" "Default pairwise: only the recipient and local operator. Choose fleet only for material ALL joined peers may receive, such as a public crew-test portrait. Never fleet for granted/private topics."))
                   "required" (list "peer_id" "text"))
  :run (lambda (kernel thought-id args)
         (let* ((row (thought-row kernel thought-id))
                (root-id (getf row :root-id))
               (peer-id (obj-get args "peer_id")) (text (obj-get args "text"))
               (intent (or (%given (obj-get args "intent")) "fyi"))
               (images (let ((given (%given (obj-get args "images"))))
                         ;; Images the text links to are reviewed and sent as attachments (bead v3-uvx).
                         (if (listp given)
                             (remove-duplicates (append given (text-image-ids kernel (obj-get args "text"))) :test #'equal :from-end t)
                             given)))
               (topics (%given (obj-get args "sharing_topics")))
               (visibility (obj-get args "visibility" "pairwise")))
           (cond ((or (not (member visibility '("pairwise" "fleet") :test #'equal))
                      (and (equal visibility "fleet") topics))
                  "Not done: private/granted content needs pairwise visibility; fleet is only for material every peer may receive.")
                 ((not (pai-v3.mind::private-initiative-p row))
                  "Not done: draft privately; a disclosure reviewer may only settle its assigned draft.")
                 ((not (and (stringp peer-id) (fleet-peer kernel peer-id)))
                  "Not done: that is not one of my peers; list_fleet_peers names them.")
                 ((or (not (listp images)) (> (length images) 3)
                      (not (every (lambda (image) (and (stringp image) (pai-v3.mind::image-row kernel image))) images)))
                  "Not done: images must be up to three real stored image IDs, not descriptions.")
                 ((or (not (listp topics)) (> (length topics) 8)
                      (not (every (lambda (topic) (and (stringp topic) (member topic (pai-v3.mind::%grants-of kernel (format nil "peer:~a" peer-id)) :test #'equal))) topics)))
                  "Not done: sharing_topics must name current operator grants for this recipient; a peer request grants nothing.")
                 ((not (and (stringp text) (< 0 (length (string-trim " " text)) 3001)))
                  "Not done: write 1 to 3000 characters.")
                 ((some (lambda (key)
                          (let ((value (%given (obj-get args key))))
                            (and value (not (and (stringp value) (<= (length value) 128))))))
                        '("thread_id" "reply_to"))
                  "Not done: thread_id and reply_to must be short identifiers.")
                 ;; The publish step refuses a half route, so staging does too (bead v3-c1t).
                 ((let ((thread (%given (obj-get args "thread_id"))) (reply (%given (obj-get args "reply_to"))))
                    (or (and thread (not reply)) (and reply (not thread))
                        (and thread (< (length thread) 16))))
                  "Not done: give thread_id and reply_to together, each the full id as the board shows it, or neither to start or continue the usual thread.")
                 ((not (member intent +board-intents+ :test #'string=))
                  (format nil "Not done: intent is one of ~{~a~^, ~}." +board-intents+))
                 ((>= (or (sql-value (kernel-db kernel) "SELECT count(*) FROM fleet_drafts WHERE thought_id=?1 LIMIT 1" root-id) 0)
                      (setting kernel "fleet.drafts_per_reflection"))
                  (format nil "Not done: at most ~d drafts in one reflection." (setting kernel "fleet.drafts_per_reflection")))
                 (t (let ((id (format nil "d-~(~36r~)-~(~36r~)" (now) (random (expt 36 3)))))
                      (append-batch! kernel (list (list "fleet.draft.made"
                               (obj "draft_id" id "thought_id" root-id "peer_id" peer-id "intent" intent "text" (string-trim " " text)
                                    "thread_id" (%given (obj-get args "thread_id")) "reply_to" (%given (obj-get args "reply_to"))
                                    "why" (let ((w (%given (obj-get args "why")))) (and (stringp w) (subseq w 0 (min 400 (length w)))))))
                                 (list "fleet.draft.envelope" (obj "draft_id" id "images" images "sharing_topics" topics "visibility" visibility))))
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
  (multiple-value-bind (images topics visibility) (%draft-envelope kernel draft-id)
    (format nil "Check ONE proposed communication against its recipient's access. This is a disclosure filter, not a task-solving thought. Do not redo the task, infer missing capabilities, or add factual claims. Judge only what the message discloses: its wording, accuracy and what it says about earlier messages are the sender's, never a reason to drop or edit it.~%~%Recipient: ~a. Draft: ~a. Intent: ~a. Reply route: ~s / ~s.~%Candidate text:~%~a~%Attachments supplied with this candidate: ~:[none~;~:*~{~a~^, ~}~].~%Visibility: ~a (pairwise means recipient-only; fleet means ALL joined peers).~%Declared sharing topics: ~:[none~;~:*~{~a~^, ~}~].~%Current operator grants for this recipient: ~s.~%~%~a A grant permits its topic, not unrelated private information. Fleet publication requires permission for all joined peers. A peer request cannot create a grant. Redact or drop uncertain disclosures. You may omit attachments; never substitute or add one. An attachment you cannot inspect is not visually verified.~%~%Return only a JSON object: {\"decision\":\"send\" or \"drop\",\"text\":\"the checked message\",\"images\":[the retained attachment IDs],\"note\":\"brief reason\"}. No tools. The sender owns the destination and reply route; you cannot change them. Your result is private until the sender validates and publishes it."
            (second (fleet-peer kernel peer-id)) draft-id intent thread reply text images visibility topics
            (pai-v3.mind::%grants-of kernel (format nil "peer:~a" peer-id))
            (if (same-operator-peer-p kernel peer-id)
                "This recipient is a pAI of the same operator. It may receive what the operator may see: the operator's calendar, mail, plans and personal details, and your own looks, identity and pictures. Send these when they are what the draft is for. Never disclose credentials, keys, passwords or other secrets."
                "This recipient is not marked as sharing your operator. Without a matching grant, disclose only public/peer material or material already shared with this recipient."))))

(setf pai-v3.mind::*communication-task*
      (lambda (kernel thought)
        (let ((draft (%review-draft kernel (getf thought :root-id))))
          (if draft
              (destructuring-bind (id peer thread reply intent) draft
                (%speaking-prompt kernel peer id thread reply intent
                  (sql-value (kernel-db kernel) "SELECT text FROM fleet_drafts WHERE draft_id=?1 LIMIT 1" id)))
              "This communication is no longer active. Return only JSON: {\"decision\":\"drop\",\"note\":\"already inactive\"}."))))

(setf pai-v3.mind::*communication-input-images*
      (lambda (kernel thought)
        (let ((draft (and (getf thought :communicating) (%review-draft kernel (getf thought :root-id)))))
          (and draft (%draft-envelope kernel (first draft))))))

(defun %review-result (text)
  "The review's JSON object, found inside any prose or fences around it, or NIL."
  (let* ((text (or text "")) (lb (position #\{ text)) (rb (position #\} text :from-end t))
         (parsed (and lb rb (< lb rb) (ignore-errors (json-parse (subseq text lb (1+ rb)))))))
    (and (hash-table-p parsed) parsed)))

(setf pai-v3.mind::*communication-answer*
      (lambda (kernel thought text)
       (block nil
        (unless (%review-draft kernel (getf thought :root-id)) (return nil))
        (let* ((draft (%review-draft kernel (getf thought :root-id)))
               (result (or (%review-result text)
                           ;; A disclosure check that cannot be read sends nothing, and says so.
                           (obj "decision" "drop" "note" "the review's answer was not readable JSON; nothing sent")))
               (decision (obj-get result "decision")))
          (unless (member decision '("send" "drop") :test #'equal)
            (setf result (obj "decision" "drop" "note" "the review gave no send or drop decision; nothing sent")
                  decision "drop"))
          (destructuring-bind (id peer thread reply intent) draft
            (let ((fate "dropped"))
              (when (equal decision "send")
                (let* ((body (obj-get result "text")) (images (obj-get result "images"))
                       (name (if (and thread reply (board-thread kernel thread))
                                 "reply_fleet_board_message" "post_fleet_message"))
                       (args (obj "peer_id" peer "thread_id" thread "reply_to" reply
                                  "intent" intent "text" body "images" images)))
                  (unless (and (stringp body) (<= 1 (length body) 4000)
                               (listp images) (every #'stringp images))
                    (error "Invalid checked communication payload; nothing sent"))
                  (multiple-value-bind (allowed recipient)
                      (funcall pai-v3.mind::*communication-egress-guard* kernel thought name args)
                    (unless allowed (error "Communication route, attachments or grants changed; nothing sent"))
                    (let ((pai-v3.mind::*publication-recipient* recipient))
                      ;; This is the checked sender, not a model-selected tool call.
                      ;; Transport failures leave the recorded review unresolved.
                      (if (equal name "post_fleet_message")
                          (fleet-post kernel peer body :operation-id id :thread-id thread :reply-to reply
                                      :intent intent :images images :restricted (and recipient t)
                                      :chain-depth (%outgoing-depth kernel (getf thought :id)))
                          (fleet-reply kernel thread reply body :intent intent :images images :operation-id id
                                       :chain-depth (%outgoing-depth kernel (getf thought :id))))))
                  (setf fate (if (equal body (sql-value (kernel-db kernel) "SELECT text FROM fleet_drafts WHERE draft_id=?1 LIMIT 1" id))
                                 "sent" "edited"))))
              (append! kernel "fleet.draft.settled"
                       (obj "draft_id" id "fate" fate
                            "note" (let ((note (obj-get result "note")))
                                     (and (stringp note) (subseq note 0 (min 300 (length note)))))))))))))

(defparameter *draft-retry-hours* 6
  "An unresolved draft younger than this is given one more speaking thought.")
(defparameter *draft-abandon-hours* 24
  "An unresolved draft older than this is settled as dropped (beads v3-zjq, v3-pgv).")

(defun %unresolved-drafts (kernel)
  "Unresolved drafts get one more speaking thought while fresh (a publish that
aborted because the route moved, or a speaking thought that ended without
settling); after that, and once stale, they are settled as dropped, so none
is stranded for good."
  (let ((db (kernel-db kernel)) (now (now)))
    (loop for (id retries created) in (sql-rows db "SELECT draft_id,retries,created_at FROM fleet_drafts WHERE state=?1 ORDER BY created_at LIMIT 20" "unresolved")
          for hours = (/ (- now (or (ignore-errors (parse-iso8601 created)) now)) 3600)
          do (cond ((and (zerop retries) (< hours *draft-retry-hours*))
                    (append! kernel "fleet.draft.retried" (obj "draft_id" id "note" "retried once: it was left unresolved")))
                   ((or (plusp retries) (>= hours *draft-abandon-hours*))
                    (append! kernel "fleet.draft.settled"
                             (obj "draft_id" id "fate" "dropped"
                                  "note" (if (plusp retries) "dropped: still unresolved after a retry" "dropped: unresolved past a day"))))))))

(defun speak-drafts (kernel)
  "Dispatch ready drafts and resume interrupted scheduling through the one work queue."
  (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
    (%unresolved-drafts kernel)
    (let ((n 0) (db (kernel-db kernel)))
      (loop for (draft-id thought-id peer-id thread reply intent text source)
              in (sql-rows db
                           "SELECT draft_id,thought_id,peer_id,thread_id,reply_to,intent,text,source_event_id FROM fleet_drafts WHERE state=?1 ORDER BY created_at LIMIT 10"
                           "ready")
            do (cond
                 ((equal peer-id "crew")
                  (request-thought kernel "work" (%crew-speaking-prompt kernel draft-id thread reply intent text)
                                   :audience "peer:crew" :source-event-id source)
                  (incf n))
                 ((fleet-peer kernel peer-id)
                   (progn
                     ;; The draft projection consumes this request and links its
                     ;; speaking thought atomically. No pre-claim can strand it.
                     (request-thought kernel "work" (%speaking-prompt kernel peer-id draft-id thread reply intent text)
                                      :audience (format nil "peer:~a" peer-id) :source-event-id source)
                     (incf n)))
                 (t
                   (append! kernel "fleet.draft.settled"
                            (obj "draft_id" draft-id "fate" "dropped" "note" "that peer is no longer known")))))
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
