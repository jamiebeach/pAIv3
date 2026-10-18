(in-package #:pai-v3.mind)

;;; Context assembly (spec §5, §14). One assembler; profiles are data. Each
;;; slot is filled by a registered source with a bounded indexed read of rows
;;; that already carry rendered text and token counts. A source that is
;;; missing or fails reports itself unavailable; assembly never fails and
;;; never silently empties a slot. The identity block comes first as the
;;; stable prefix so provider prompt caching hits across turns.

(defvar *context-profiles* (make-hash-table :test 'equal))
(defvar *slot-sources* (make-hash-table :test 'eq)
  "Slot source keyword -> (lambda (kernel request max-tokens)) returning a
list of (text tokens) rows, most relevant first.")

(defmacro define-context-profile (name &key slots)
  "SLOTS: list of (slot-name :max tokens :source keyword :title string)."
  `(setf (gethash ,name *context-profiles*) ',slots))

(defmacro define-slot-source (keyword (kernel request max) &body body)
  `(setf (gethash ,keyword *slot-sources*)
         (lambda (,kernel ,request ,max) (declare (ignorable ,kernel ,request ,max)) ,@body)))

(define-event-type "context.assembled"
  :schema (("profile" :string) ("thought_id" :integer :optional)
           ("persona_fingerprint" :string) ("digest" :string)
           ("total_tokens" :integer) ("milliseconds" :integer)
           ("slots" :array)))

;;; Profiles (engineering spec §5.1 and §14: activity-first for the operator)

(define-context-profile "operator-turn"
  :slots ((:clock     :max 80    :source :clock     :title "Now")
          (:state     :max 300   :source :state     :title "How I am")
          (:summary   :max 3000  :source :summary   :title "Earlier, in summary")
          (:conclusions :max 1200 :source :conclusions :title "What I have concluded lately (newer than the summary)")
          (:activity  :max 24000 :source :activity  :title "What I have been doing")
          (:dialogue  :max 4000  :source :dialogue  :title "Conversation" :order :oldest-first)
          (:recollection :max 6000 :source :recollection :title "What comes to mind")
          (:seen      :max 400   :source :seen      :title "Images I have seen lately")
          (:scheduled :max 500   :source :scheduled :title "Coming up")
          (:present   :max 300   :source :present   :title "The operator's day")
          (:stream    :max 600   :source :stream    :title "What I noticed between thoughts")
          (:concerns  :max 600   :source :concerns  :title "What I am looking into")
          (:docket    :max 600   :source :docket    :title "Work I am keeping")
          (:carried   :max 700   :source :carried   :title "Things I am carrying for the operator")
          (:restricted :max 200  :source :restricted :title "Not for this audience")))

(define-context-profile "private-cognition"
  :slots ((:clock     :max 80    :source :clock     :title "Now")
          (:state     :max 300   :source :state     :title "How I am")
          (:summary   :max 2000  :source :summary   :title "Earlier, in summary")
          (:conclusions :max 800 :source :conclusions :title "What I have concluded lately (newer than the summary)")
          (:activity  :max 8000  :source :activity  :title "What I have been doing")
          (:dialogue  :max 2000  :source :dialogue  :title "Recent conversation" :order :oldest-first)
          (:recollection :max 4000 :source :recollection :title "What comes to mind")
          (:seen      :max 300   :source :seen      :title "Images I have seen lately")
          (:present   :max 300   :source :present   :title "The operator's day")
          (:stream    :max 600   :source :stream    :title "What I noticed between thoughts")
          (:concerns  :max 600   :source :concerns  :title "What I am looking into")
          (:docket    :max 600   :source :docket    :title "Work I am keeping")
          (:carried   :max 700   :source :carried   :title "Things I am carrying for the operator")))

;;; Sources available in phase 3. Others (state, summary, recall, knowledge,
;;; scheduled, restricted) arrive with their modules; until then they report
;;; themselves unavailable.

(define-slot-source :dialogue (kernel request max)
  (mapcar (lambda (row) (list (dialogue-row-text row) (dialogue-row-tokens row)))
          (reverse (recent-dialogue kernel (getf request :audience) :token-budget max))))

(define-slot-source :activity (kernel request max)
  ;; Recent thoughts visible to this audience, newest first.
  (loop for (reason outcome status at) in
        (sql-rows (kernel-db kernel)
                  "SELECT reason,outcome,status,updated_at FROM thoughts WHERE audience=?1 AND id<>?2 ORDER BY id DESC LIMIT 40"
                  (getf request :audience) (or (getf request :thought-id) 0))
        for text = (format nil "[~a UTC] ~a -> ~a~@[: ~a~]" (subseq at 0 16) reason status outcome)
        collect (list text (estimate-tokens text))))

;;; Assembly

(defun %fill (kernel source request max)
  "Rows that fit MAX tokens, and a status string."
  (let ((fn (gethash source *slot-sources*)))
    (if (null fn)
        (values nil 0 "unavailable")
        (handler-case
            (let ((kept '()) (used 0))
              ;; A row is (text tokens), or plain text whose tokens are
              ;; estimated here: a source returning bare lines once kept
              ;; three slots out of every context, marked unavailable.
              (loop for row in (funcall fn kernel request max)
                    for (text tokens) = (if (stringp row) (list row (estimate-tokens row)) row)
                    while (<= (+ used tokens) max)
                    do (incf used tokens) (push text kept))
              (values (nreverse kept) used (if kept "ok" "empty")))
          (error (e)
            (values nil 0 (format nil "unavailable: ~a"
                                  (let ((m (princ-to-string e))) (subseq m 0 (min 120 (length m)))))))))))

(defun assemble-context (kernel profile-name request)
  "Build messages for REQUEST, a plist with :audience, :task, and optionally
:thought-id and :tools-text (the tool index, kept in the stable prefix). Returns the list of message objects."
  (let* ((start (get-internal-real-time))
         (profile (or (gethash profile-name *context-profiles*)
                      (error "No context profile ~s" profile-name)))
         (operator-facing (equal "operator" (getf request :audience))))
    (multiple-value-bind (identity fingerprint) (render-identity kernel :operator-facing operator-facing)
      (let ((sections '()) (manifest '()) (total (estimate-tokens identity)))
        (when (getf request :tools-text)
          (let ((tokens (estimate-tokens (getf request :tools-text))))
            (incf total tokens)
            (push (obj "slot" "tools" "count" 1 "tokens" tokens "status" "ok") manifest)))
        (dolist (slot profile)
          (destructuring-bind (name &key max source title order) slot
            (multiple-value-bind (texts used status) (%fill kernel source request max)
              ;; A source gives its newest rows first, so the budget drops the
              ;; oldest; a transcript still reads oldest to newest.
              (when (eq order :oldest-first) (setf texts (reverse texts)))
              (incf total used)
              (push (obj "slot" (string-downcase (symbol-name name)) "count" (length texts)
                         "tokens" used "status" status)
                    manifest)
              (when texts
                (push (format nil "## ~a~%~{~a~^~%~}" title texts) sections)))))
        (let* ((now-text (format nil "~{~a~^~%~%~}" (nreverse sections)))
               (task (getf request :task))
               (tools-text (getf request :tools-text))
               (messages (list (obj "role" "system"
                                    "content" (if tools-text
                                                  (format nil "~a~%~%~a" identity tools-text)
                                                  identity))
                               (obj "role" "system" "content" now-text)
                               (obj "role" "user" "content" task))))
          (append! kernel "context.assembled"
                   (obj "profile" profile-name "thought_id" (getf request :thought-id)
                        "persona_fingerprint" fingerprint
                        "digest" (pai-v3.kernel::sha256-hex (json-encode messages))
                        "total_tokens" (+ total (estimate-tokens task))
                        "milliseconds" (round (* 1000 (- (get-internal-real-time) start))
                                              internal-time-units-per-second)
                        "slots" (nreverse manifest)))
          messages)))))

;;; Thoughts use the assembler: operator-facing responses get the operator
;;; profile; everything else private cognition.

(defparameter +speaking-while-working+
  "## Speaking while you work
Anything you write alongside a tool call reaches the operator at once, and the turn goes on. On a task that needs several steps, say in a sentence or two what you are about to do before you start, give a short update when something changes, and finish with a summary. A quick question needs none of this: just answer.
If something comes up that is worth looking into later, raise a concern with the concern tool; in quiet time you will come back to it.
A message with no tool call ends your turn. Never end one by saying what you will do next: if there is a next step, make the tool call in the same message; if not, give your answer."
  "How a reply turn talks between steps. Sent with the tool index, so it stays in the stable prefix.")

(setf *assemble-thought-context*
      (lambda (kernel thought tools-text)
        (assemble-context kernel
                          (if (string= "responding" (getf thought :origin))
                              "operator-turn" "private-cognition")
                          (list :audience (getf thought :audience)
                                :thought-id (getf thought :id)
                                :tools-text (if (string= "responding" (getf thought :origin))
                                                (format nil "~a~@[~%~%~a~]" +speaking-while-working+ tools-text)
                                                (format nil "A message with no tool call ends this thought. Never end one by saying what you will do next: make the tool call in the same message, or finish.~@[~%~%~a~]" tools-text))
                                ;; Answering a turn, the task is the operator's own words, as
                                ;; the last user message, where any chat model looks for them.
                                :task (or (and (string= "responding" (getf thought :origin))
                                               (getf thought :source-event-id)
                                               (let ((turn (sql-value (kernel-db kernel) "SELECT rendered FROM working_dialogue WHERE event_id=?1 LIMIT 1"
                                                                      (getf thought :source-event-id))))
                                                 (and turn (localize-dialogue kernel turn))))
                                          (format nil "~a" (getf thought :reason)))))))
