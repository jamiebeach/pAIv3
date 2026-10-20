(in-package #:pai-v3.mind)

;;; Dialogue (agent doc §10; spec §5.2). Conversation is channel-neutral: one
;;; relationship across many surfaces. Each turn is stored with its rendered
;;; text and token estimate at write time, and with its audience, so context
;;; assembly is a bounded indexed read filtered by who may see it (spec §16.5).

(define-event-type "turn.received"
  :schema (("channel" :string)         ; web, app, telegram, voice, ...
           ("audience" :string)        ; "operator" or "person:<id>" or "peer:<id>"
           ("speaker" :string)         ; display name of the verified principal
           ("text" :string)
           ("origin" :object)          ; verified origin attestation (spec §16.5)
           ("images" :array :optional)
           ("semantic_affect" :boolean :optional))) ; ids of images kept in the blob store

(define-event-type "reply.delivered"
  :schema (("channel" :string)
           ("audience" :string)
           ("text" :string)
           ("in_reply_to" :integer :optional)
           ("interrupted" :boolean :optional)    ; partial reply kept (EXT-3)
           ("interim" :boolean :optional)))      ; said between steps; the turn goes on

(define-event-type "notice.posted"
  :schema (("channel" :string)
           ("audience" :string)
           ("text" :string)
           ("importance" :string)))

(defun %clock-label (iso)
  "2026-10-12T08:05:00Z -> 2026-10-12 08:05"
  (format nil "~a ~a" (subseq iso 0 10) (subseq iso 11 16)))

(defun %render (event speaker text)
  (format nil "[~a ~a] ~a: ~a"
          (%clock-label (event-at event))
          (obj-get (event-payload event) "channel")
          speaker text))

(defun %reduce-dialogue (db event)
  (let* ((p (event-payload event))
         (type (event-type event))
         (speaker (cond ((string= type "turn.received") (obj-get p "speaker"))
                        ((string= type "notice.posted") "Notice")
                        (t "Me")))
         (text (cond ((eq t (obj-get p "interrupted"))
                      (concatenate 'string (obj-get p "text") " [interrupted]"))
                     ((obj-get p "images")
                      (format nil "~a [~d image~:p attached]" (obj-get p "text") (length (obj-get p "images"))))
                     (t (obj-get p "text"))))
         (rendered (%render event speaker text)))
    (sql-exec db "INSERT INTO working_dialogue(event_id,audience,channel,speaker,kind,rendered,tokens,at,reply_to) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
              (event-id event) (obj-get p "audience") (obj-get p "channel") speaker
              (subseq type 0 (position #\. type)) rendered (estimate-tokens rendered)
              (event-at event) (obj-get p "in_reply_to"))))

(define-projection "working_dialogue"
  :kind :inline
  :consumes ("turn.received" "reply.delivered" "notice.posted")
  :tables ("CREATE TABLE IF NOT EXISTS working_dialogue (event_id INTEGER PRIMARY KEY, audience TEXT NOT NULL, channel TEXT NOT NULL, speaker TEXT NOT NULL, kind TEXT NOT NULL, rendered TEXT NOT NULL, tokens INTEGER NOT NULL, at TEXT NOT NULL, reply_to INTEGER)"
           "CREATE INDEX IF NOT EXISTS working_dialogue_audience_idx ON working_dialogue(audience,event_id)")
  :entity (lambda (e) (obj-get (event-payload e) "audience"))
  :reduce #'%reduce-dialogue)

;;; Who the operator is, by name: what their turns and their board posts
;;; are signed with. One setting for every place the name is shown.

(define-setting "operator.name"
  :type :string :default "Operator" :group "Operator"
  :description "The operator's name, as their messages and posts are signed.")

;;; Writing

(defvar *received-turn-hook* nil
  "Registered post-recording seam for inbound message perception; never paid HTTP here.")

(defun receive-turn (kernel &key channel audience speaker text origin images)
  "Record a verified inbound turn. ORIGIN is the attestation from the
channel adapter; it is stored, never inferred from TEXT."
  (let* ((semantic (and (eq t (setting kernel "affect.appraisal.enabled"))
                        (equal audience "operator") (equal "operator" (obj-get origin "principal_type"))))
         (event (append! kernel "turn.received"
                         (obj "channel" channel "audience" audience "speaker" speaker "text" text
                              "origin" (or origin (obj)) "images" images "semantic_affect" (and semantic t)))))
    (when (and semantic *received-turn-hook*) (funcall *received-turn-hook* kernel event))
    event))

(defun deliver-reply (kernel &key channel audience text in-reply-to interrupted interim)
  (append! kernel "reply.delivered"
           (obj "channel" channel "audience" audience "text" text
                "in_reply_to" in-reply-to "interrupted" (and interrupted t) "interim" (and interim t))
           :caused-by in-reply-to))

(defun post-notice (kernel &key channel audience text (importance "normal"))
  (append! kernel "notice.posted"
           (obj "channel" channel "audience" audience "text" text "importance" importance)))

;;; Reading

(defstruct dialogue-row event-id channel speaker text tokens)

(defun recent-dialogue (kernel audience &key (token-budget 4000) (max-rows 200) before-id)
  "Most recent dialogue visible to AUDIENCE across all channels, oldest first,
fitted to TOKEN-BUDGET. One indexed read of at most MAX-ROWS rows."
  (let* ((rows (sql-rows (kernel-db kernel)
                         "SELECT event_id,channel,speaker,rendered,tokens FROM working_dialogue WHERE audience=?1 AND event_id<?2 ORDER BY event_id DESC LIMIT ?3"
                         audience (or before-id most-positive-fixnum) max-rows))
         (kept '())
         (used 0))
    (loop for (id channel speaker text tokens) in rows
          while (<= (+ used tokens) token-budget)
          do (incf used tokens)
             (push (make-dialogue-row :event-id id :channel channel :speaker speaker
                                      :text (localize-dialogue kernel text) :tokens tokens)
                   kept))
    (values kept used)))
