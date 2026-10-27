(in-package #:pai-v3.recall)

;;; Impressions revised in sleep (design note interaction-recall §5a). For
;;; what has come to mind lately and has gathered enough new interactions
;;; since its last revision, a model writes the agent's settled sense of it
;;; anew: keeping what still matters, folding in what is new, letting go of
;;; what has faded, and always keeping what is significant. Consolidation
;;; owns when and how much; this is one of its tasks.

(defparameter *revisions-per-night* 6 "Most impressions one night revises.")
(defparameter *fresh-to-revise* 3 "New interactions an impression needs before it is revised again.")
(defparameter *keepsake-strength* 0.8d0 "Memories this strong are kept in the impression for good.")

(defparameter +impression-tool+
  (obj "type" "function"
       "function"
       (obj "name" "write_impression"
            "description" "Record my settled sense of this one."
            "parameters"
            (obj "type" "object"
                 "properties" (obj "settled" (obj "type" "string"
                                                  "description" "Two to six sentences, in the first person, of who or what this is to me now."))
                 "required" (list "settled")))))

(defun %revision-input (kernel anchor)
  "(impression fresh-event-ids keepsakes sources) for revising ANCHOR, or NIL."
  (let* ((imp (impression kernel anchor "operator"))
         (terms (third (%anchor kernel anchor)))
         (through (or (second (%revision kernel anchor)) 0))
         (found (and imp (experience-mentioning kernel terms "operator" :limit 30)))
         (fresh-ids (loop for (kind nil nil nil id) in found
                          when (and (member kind +fresh-kinds+ :test #'string=) (> id through)) collect id))
         (memories (loop for (kind nil text ref) in found
                         when (string= kind "memory") collect (list ref text (or (memory-strength kernel ref) 0)))))
    (when imp
      (list imp fresh-ids
            (loop for (nil text strength) in memories when (>= strength *keepsake-strength*) collect text)
            (mapcar #'first memories)))))

(defun revise-in-sleep (kernel anchor work)
  "Write ANCHOR's impression anew when it is due. Returns T when revised."
  (destructuring-bind (&optional imp fresh-ids keepsakes sources) (%revision-input kernel anchor)
    (when (and imp (or (>= (length fresh-ids) *fresh-to-revise*)
                       (and (null (getf imp :revision)) (getf imp :settled) fresh-ids)))
      (let* ((profile (profile-for kernel "consolidation"))
             (reply (complete kernel profile
                              (list (obj "role" "system"
                                         "content" (format nil "You are ~a, asleep, revising your settled sense of ~a. ~
From what you made of them before and what has happened since, write who or what this is to you now: keep what ~
still matters, fold in what is new, and let go of what has faded or no longer holds. Always keep the keepsakes. ~
Say only what the material supports; a story stays a story. Call write_impression once."
                                                           (kernel-agent-id kernel) (getf imp :name)))
                                    (obj "role" "user"
                                         "content" (format nil "Before:~%~{  ~a~%~}~@[Since:~%~{  ~a~%~}~]~@[Keepsakes (keep these):~%~{  ~a~%~}~]~@[Still open:~%~{  ~a~%~}~]"
                                                           (or (getf imp :settled) (list "(nothing yet)"))
                                                           (getf imp :fresh) keepsakes (getf imp :open))))
                              :tools (list +impression-tool+) :work-id (getf work :id) :lane "consolidation"))
             (call (find "write_impression" (obj-get reply "tool_calls")
                         :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal))
             (settled (and call (obj-get (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments"))) "settled"))))
        (when (and (stringp settled) (< 20 (length (string-trim " " settled)) 2001))
          (revise-impression kernel anchor (string-trim '(#\Space #\Newline) settled)
                             :sources sources
                             :through-event-id (if fresh-ids (reduce #'max fresh-ids) (or (second (%revision kernel anchor)) 0)))
          t)))))

(define-consolidation "impressions" (kernel work)
  ;; Close ones and my reality first, outside the cap (close.lisp); their
  ;; impressions are written only there, with what they need.
  (multiple-value-bind (close close-revised) (revise-close-and-reality kernel work)
  (let ((looked 0) (revised 0))
    (dolist (anchor (set-difference (recently-recalled kernel) close :test #'equal))
      (when (and (< revised *revisions-per-night*) (> (consolidation-budget-left kernel) 5000))
        (incf looked)
        (when (ignore-errors (revise-in-sleep kernel anchor work)) (incf revised))))
    (obj "looked" looked "revised" revised "close" (length close) "close_revised" close-revised))))
