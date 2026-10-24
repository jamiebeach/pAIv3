(in-package #:pai-v3.recall)

;;; Prospective memory (design note interaction-recall §6): "when I next
;;; see her, tell her X" is a memory that fires on a cue, not at a time. An
;;; intention is attached to an anchor; when that anchor next comes to mind,
;;; the intention comes with it, in its impression's "open" lines, until the
;;; agent marks it done or lets it go. Intentions are never silently dropped,
;;; and the operator sees them as the agent does.

(define-event-type "intention.formed"
  :schema (("intention_id" :string) ("anchor" :string) ("label" :string) ("text" :string)
           ("until" :string :optional) ("thought_id" :integer :optional)))
(define-event-type "intention.settled"
  :schema (("intention_id" :string) ("how" :string) ("note" :string :optional)))
(define-event-type "intention.prompted"
  :schema (("intention_id" :string) ("thought_id" :integer)))

(define-projection "intentions"
  :kind :inline
  :version 2
  :consumes ("intention.formed" "intention.settled" "intention.prompted")
  :tables ("CREATE TABLE IF NOT EXISTS intentions (id TEXT PRIMARY KEY, anchor TEXT NOT NULL, label TEXT NOT NULL, text TEXT NOT NULL, until TEXT, status TEXT NOT NULL, note TEXT, formed_at TEXT NOT NULL, settled_at TEXT, prompted INTEGER NOT NULL DEFAULT 0)"
           "CREATE INDEX IF NOT EXISTS intentions_status_idx ON intentions(status,formed_at)")
  :entity (lambda (e) (obj-get (event-payload e) "intention_id"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (cond ((string= (event-type e) "intention.formed")
                     (sql-exec db "INSERT OR IGNORE INTO intentions(id,anchor,label,text,until,status,formed_at) VALUES(?1,?2,?3,?4,?5,'open',?6)"
                               (obj-get p "intention_id") (obj-get p "anchor") (obj-get p "label") (obj-get p "text")
                               (obj-get p "until") (event-at e)))
                    ((string= (event-type e) "intention.prompted")
                     (sql-exec db "UPDATE intentions SET prompted=1 WHERE id=?1" (obj-get p "intention_id")))
                    (t
                     (sql-exec db "UPDATE intentions SET status=?2,note=?3,settled_at=?4 WHERE id=?1 AND status='open'"
                               (obj-get p "intention_id") (obj-get p "how") (obj-get p "note") (event-at e)))))))

(defparameter *intentions-open-most* 30 "Most open intentions an agent may hold.")

(defun open-intentions (kernel)
  "Open intentions, oldest first, as (id anchor label text until formed-at)."
  (sql-rows (kernel-db kernel)
            "SELECT id,anchor,label,text,until,formed_at FROM intentions WHERE status=?1 ORDER BY formed_at LIMIT ?2"
            "open" *intentions-open-most*))

(defun %resolve-anchor (kernel ref)
  "(anchor label) for REF: a graph entity's id or exact name, or a known identifier."
  (let ((e (graph-entity kernel ref)))
    (cond (e (list (first e) (third e)))
          (t (let ((found (find-entities kernel ref)))
               (cond ((= 1 (length found)) (list (first (first found)) (third (first found))))
                     (found (error "~a could be ~{~a~^ or ~}; give the id" ref
                                   (mapcar (lambda (x) (format nil "~a (~a)" (third x) (first x))) found)))
                     (t (let ((handle (first (handles-in kernel ref))))
                          (if handle
                              (list (format nil "h:~a" (first handle)) (second handle))
                              (error "I know no one and nothing called ~a. `about` is a person or thing I already know (a name or id from my graph), or an identifier; for something not tied to anyone, use the carry tool" ref))))))))))

(defun form-intention (kernel about text &key until thought-id)
  "Attach TEXT (what to do) to ABOUT (a name, entity id or identifier).
Returns the intention's id."
  (unless (and (stringp text) (< 2 (length (string-trim " " text)) 501))
    (error "say what to do in 3 to 500 characters"))
  (unless (and (stringp about) (plusp (length (string-trim " " about))))
    (error "an intention needs `about`: the person or thing it should come back with (a name or id from my graph, or an identifier). If it is not tied to anyone or anything, use the carry tool, which keeps something in mind for the operator until it is done"))
  (when (>= (length (open-intentions kernel)) *intentions-open-most*)
    (error "I already hold ~d open intentions; settle some first" *intentions-open-most*))
  (destructuring-bind (anchor label) (%resolve-anchor kernel about)
    (let ((id (format nil "i-~a" (pai-v3.kernel:base64url-encode (ironclad:random-data 9)))))
      (append! kernel "intention.formed"
               (obj "intention_id" id "anchor" anchor "label" label "text" (string-trim " " text)
                    "until" until "thought_id" thought-id))
      id)))

(defun settle-intention (kernel id how &optional note)
  (unless (member how '("done" "dropped") :test #'string=) (error "an intention is done or dropped"))
  (unless (find id (open-intentions kernel) :key #'first :test #'equal) (error "no open intention ~a" id))
  (append! kernel "intention.settled" (obj "intention_id" id "how" how "note" note))
  id)

;;; Intentions come back with what they are about: through the open-items
;;; registry, so impressions need nothing new.

(define-open-items "intentions" (kernel terms audience)
  (loop for (id nil label text until) in (open-intentions kernel)
        when (or (mentions-any-p label terms) (some (lambda (term) (string-equal term label)) terms))
          collect (format nil "I meant to: ~a (~a~@[, by ~a~])" text id until)))

(define-tool "remember_to"
  :description "Prospective memory: keep something to do or say for the next time a particular person or thing comes to
mind, such as \"next time I talk to Nova, ask how the portrait came out\". It is tied to what it is about, so it needs
that: action add needs about (a name or id of someone or something I know, or an identifier; required) and do (what to do),
optionally until (a date it stops mattering). done (id, optional note) when it is done, drop (id, optional note) when it
no longer matters, list. It comes back whenever its subject comes to mind, until done or dropped. For something not tied
to anyone or anything, such as a wish, a plan or a deadline of the operator's, use the carry tool instead."
  :summary "Keep something to do for the next time someone or something comes to mind."
  :available :operator-and-peers
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "add" "done" "drop" "list"))
                                     "about" (obj "type" "string") "do" (obj "type" "string")
                                     "until" (obj "type" "string") "id" (obj "type" "string")
                                     "note" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (handler-case
             (let ((action (obj-get args "action")))
               (cond ((equal action "add")
                      (format nil "I will remember: ~a (~a)." (obj-get args "do")
                              (form-intention kernel (obj-get args "about") (obj-get args "do")
                                              :until (obj-get args "until") :thought-id thought-id)))
                     ((member action '("done" "drop") :test #'equal)
                      (settle-intention kernel (obj-get args "id") (if (equal action "done") "done" "dropped")
                                        (obj-get args "note"))
                      (format nil "Settled ~a." (obj-get args "id")))
                     ((equal action "list")
                      (let ((open (open-intentions kernel)))
                        (if open
                            (format nil "~{~a~^~%~}" (mapcar (lambda (r) (format nil "~a about ~a: ~a~@[ (by ~a)~]"
                                                                                (first r) (third r) (fourth r) (fifth r)))
                                                             open))
                            "I hold no open intentions.")))
                     (t "action is add, done, drop or list.")))
           (error (e) (format nil "Not done: ~a" e)))))

;;; A pressing intention may interrupt (operator decision 2026-10-17, design
;;; note §10): when something comes to mind for the operator and an open
;;; intention about it is due (its date has come), the agent gets a short
;;; thought of its own about it, once. Like any unbidden thought it stays
;;; internal unless the agent decides to speak.

(defun %due-p (kernel until)
  "Whether UNTIL, a date (YYYY-MM-DD...), has come in the operator's time zone."
  (and (stringp until) (>= (length until) 10)
       (every #'digit-char-p (remove #\- (subseq until 0 10)))
       (string<= (subseq until 0 10)
                 (subseq (local-slot (now) (local-time:find-timezone-by-location-name (setting kernel "time.zone"))) 0 10))))

(defun prompt-due-intentions (kernel anchors audience)
  "For each open, due, not yet prompted intention about ANCHORS, start an
unbidden thought. Only for the operator's thoughts."
  (when (equal audience "operator")
    (loop for (id anchor label text until) in (sql-rows (kernel-db kernel)
                                                       "SELECT id,anchor,label,text,until FROM intentions WHERE status=?1 AND prompted=?2 LIMIT 50"
                                                       "open" 0)
          when (and (member anchor anchors :test #'equal) (%due-p kernel until))
            do (let ((thought (request-thought kernel "unbidden"
                                               (format nil "I just remembered: I meant to ~a (about ~a), and it is due (by ~a). ~
Do it, or settle it with remember_to, or decide it can wait." text label until)
                                               :audience "operator")))
                 (append! kernel "intention.prompted" (obj "intention_id" id "thought_id" thought))))))
