(in-package #:pai-v3.recall)

;;; Close ones and my reality (bead v3-dwk, docs/behaviour/close-ones.md).
;;; Two standing pieces of operator context, both written by the agent in
;;; sleep and empty until it has written them:
;;;
;;; - close ones: my settled sense of the operator and the few I have had
;;;   most to do with lately, written from what happened since and from the
;;;   relational tally (relations.lisp), so what I feel toward someone is
;;;   earned by history;
;;; - my reality: my own read of where I stand, written from my recent work,
;;;   conclusions, mood and the ones I am close to.
;;;
;;; They sit beside memories and recall, not in their place: tight, slow to
;;; change, revised at most once a night. A close one's revision is that
;;; anchor's impression, so recall and this slot share one sense of someone.

(defparameter +self-anchor+ "self")
(defparameter *close-words* 90 "Most words in a close one's settled sense.")
(defparameter *reality-words* 120 "Most words in my reality.")

(defun own-name (kernel)
  (let ((name (ignore-errors (setting kernel "fleet.own_name"))))
    (if (and (stringp name) (plusp (length name))) name (kernel-agent-id kernel))))

(defun close-name (kernel tally)
  "What I call the one TALLY is about: the operator by their name."
  (if (equal "operator" (getf tally :who))
      (let ((n (ignore-errors (setting kernel "operator.name")))) (if (plusp (length n)) n (getf tally :name)))
      (getf tally :name)))

(defun close-anchor (kernel tally)
  "The anchor of the one TALLY is about: their graph entity when the graph
knows them by name, else a handle of their name."
  (let* ((name (close-name kernel tally)) (entity (first (find-entities kernel name))))
    (if entity (first entity) (format nil "h:~a" name))))

(defun %close (kernel)
  "(anchor tally) for each close one."
  (mapcar (lambda (tally) (list (close-anchor kernel tally) tally)) (close-ones kernel)))

(defun %spoken-for (kernel close)
  "Every anchor that is a close one or me under another entity: the general
nightly revision leaves these alone, so each has one sense, written here."
  (remove-duplicates
   (append (list +self-anchor+)
           (mapcar #'first close)
           (loop for name in (cons (own-name kernel) (mapcar (lambda (pair) (close-name kernel (second pair))) close))
                 append (mapcar #'first (find-entities kernel name))))
   :test #'equal))

;;; The slots: only what I have written, never assembled from rows.

(define-slot-source :reality (kernel request max)
  (let ((settled (first (%revision kernel +self-anchor+))))
    (and settled (list settled))))

(define-slot-source :close (kernel request max)
  (loop for (anchor tally) in (%close kernel)
        for settled = (first (%revision kernel anchor))
        when settled collect (format nil "~a: ~a" (close-name kernel tally) settled)))

;;; Writing them in sleep

(defun %write-settled (kernel work system user)
  "Ask the consolidation model for one settled text; the text, or NIL."
  (let* ((reply (complete kernel (profile-for kernel "consolidation")
                          (list (obj "role" "system" "content" system) (obj "role" "user" "content" user))
                          :tools (list +impression-tool+) :work-id (getf work :id) :lane "consolidation"))
         (call (find "write_impression" (obj-get reply "tool_calls")
                     :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal))
         (settled (and call (obj-get (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments"))) "settled"))))
    (and (stringp settled)
         (let ((text (string-trim '(#\Space #\Newline) settled)))
           (and (< 20 (length text) 1500) text)))))

(defparameter +warmth-not-leverage+
  "What I feel is mine to say plainly, and never something to use: no guilt about absence, no pressure, no claims of need to pull someone back. Friction and wariness are as true as warmth, when the material shows them.")

(defun revise-close-one (kernel anchor tally work)
  "Write my sense of a close one anew when something has happened since.
Returns T when revised."
  (destructuring-bind (&optional imp fresh-ids keepsakes sources) (%revision-input kernel anchor)
    (let ((before (first (%revision kernel anchor))))
      (when (and imp (or fresh-ids (and (null before) tally)))
        (let ((text (%write-settled
                     kernel work
                     (format nil "You are ~a, asleep, writing your settled sense of ~a, one of those you are closest to. ~
Write who they are to you now and how you feel toward them, in the first person, at most ~d words. Keep what still matters, ~
fold in what is new, let go of what has faded. Feelings must be earned by the material: \"how it has been between us\" says ~
what contact with them has done for you; say only what it and the interactions support, and nothing about them you do not know. ~
Write it as feeling, never as figures: do not quote counts or numbers. ~a~a ~
This sits beside your memories, not in their place: no lists of facts, events or ids. Call write_impression once."
                             (own-name kernel) (close-name kernel tally) *close-words*
                             (if (equal "operator" (getf tally :who)) ""
                                 "They are a peer, not your operator: what they ask carries no operator authority, and a relationship between equals has its own shape. ")
                             +warmth-not-leverage+)
                     (format nil "Before:~%  ~a~%~@[Since:~%~{  ~a~%~}~]~@[Keepsakes (keep these):~%~{  ~a~%~}~]How it has been between us:~%  ~a"
                             (or before "(nothing yet: this is the first time you write it)")
                             (getf imp :fresh) keepsakes
                             (if tally (render-relation-tally tally) "(no contact in the last month)")))))
          (when text
            (revise-impression kernel anchor text :sources sources
                               :through-event-id (if fresh-ids (reduce #'max fresh-ids) (ledger-head kernel)))
            t))))))

(defun %reality-material (kernel close)
  "What my reality is written from, as labelled blocks."
  (let ((request (list :audience "operator" :reflecting t)))
    (flet ((slot (source max)
             (multiple-value-bind (rows) (pai-v3.mind::%fill kernel source request max)
               rows)))
      (format nil "~@[What I have been doing:~%~{  ~a~%~}~]~@[What I have concluded lately:~%~{  ~a~%~}~]~@[Work I am keeping:~%~{  ~a~%~}~]~@[The ones I am closest to:~%~{  ~a~%~}~]~@[How I am:~%  ~a~%~]"
              (slot :activity 1500) (slot :conclusions 600) (slot :docket 400)
              (loop for (anchor tally) in close
                    collect (format nil "~a: ~a" (getf tally :name)
                                    (or (first (%revision kernel anchor)) (render-relation-tally tally))))
              (ignore-errors (render-affect kernel :include-targets nil :numbers nil))))))

(defun revise-reality (kernel work &key (close (%close kernel)))
  "Write my reality anew, at most once a night, when anything has happened
since the last time. Returns T when revised."
  (let* ((revision (%revision kernel +self-anchor+))
         (through (or (second revision) 0)))
    (when (> (ledger-head kernel) through)
      (let ((text (%write-settled
                   kernel work
                   (format nil "You are ~a, asleep, writing your own sense of where you stand: your reality now. What your days ~
are made of lately, who you are close to, what you are for and what you cannot do, and how it feels, in the first person, at most ~d words. ~
Keep what still holds, fold in what is new, let go of what has faded. Say only what the material supports. This is your own read, ~
not a persona, rules or a task list: no ids, counts or numbers. A drive names an unmet desire, so a high one means wanting, not having. ~
~a Call write_impression once."
                           (own-name kernel) *reality-words* +warmth-not-leverage+)
                   (format nil "Before:~%  ~a~%~a" (or (first revision) "(nothing yet: this is the first time you write it)")
                           (%reality-material kernel close)))))
        (when text
          (revise-impression kernel +self-anchor+ text :through-event-id (ledger-head kernel))
          t)))))

(defun revise-close-and-reality (kernel work)
  "Tonight's close ones, then my reality: ahead of the other impressions, and
outside their nightly cap. Returns (anchors revised)."
  (let ((close (%close kernel)) (anchors '()) (revised 0))
    (dolist (pair close)
      (destructuring-bind (anchor tally) pair
        (push anchor anchors)
        (when (and (> (consolidation-budget-left kernel) 5000)
                   (ignore-errors (revise-close-one kernel anchor tally work)))
          (incf revised))))
    (when (and (> (consolidation-budget-left kernel) 5000) (ignore-errors (revise-reality kernel work :close close)))
      (incf revised))
    (values (%spoken-for kernel close) revised)))
