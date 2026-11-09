(in-package #:pai-v3.mind)

;;; Overheard (operator, 2026-11-09). The avatar listening hands-free through
;;; a two-hour meeting recorded every phrase in the room as the operator's
;;; words to me. Marking a span overheard keeps the turns in the ledger and
;;; changes what they are taken for: the conversation, its summary, episodes,
;;; search and the graph read them as heard, not said to me, and nothing the
;;; operator is held to rests on them. Only spoken turns are marked; anything
;;; typed in the span stays theirs.

(defun overheard-candidates (kernel from to)
  "Event ids of the operator's spoken turns between ISO times FROM and TO."
  (loop for (id) in (sql-rows (kernel-db kernel)
                              "SELECT event_id FROM working_dialogue WHERE audience=?1 AND kind=?2 AND at>=?3 AND at<=?4 ORDER BY event_id LIMIT 5000"
                              "operator" "turn" from to)
        for e = (ignore-errors (event-by-id kernel id))
        when (and e (equal "voice" (obj-get (obj-get (event-payload e) "origin") "mode"))
                  (not (overheard-p kernel id)))
          collect id))

(defun mark-overheard (kernel from to note)
  "Mark the operator's spoken turns between local times FROM and TO as
overheard. Returns the number marked."
  (let* ((zone (%zone kernel nil))
         (from-iso (iso8601 (local-to-universal from zone)))
         (to-iso (iso8601 (local-to-universal to zone)))
         (ids (overheard-candidates kernel from-iso to-iso)))
    (when (> (parse-iso8601 from-iso) (parse-iso8601 to-iso)) (error "~a is after ~a" from to))
    (when ids
      (append! kernel "dialogue.overheard"
               (obj "audience" "operator" "event_ids" ids "from" from-iso "to" to-iso "note" note)))
    (length ids)))

(define-tool "mark_overheard"
  :description "Mark spoken turns as overheard: words the microphone picked up that were not said to me, such as a meeting the operator sat in while I listened hands-free. Use it only when the operator tells me so, for the span they name. from and to are local times like 2026-11-09T14:00. Only spoken turns in the span are marked; typed messages stay theirs. The turns are kept, and from then on read as heard, not said to me: in the conversation, its summary, episodes, search, and as grounds for anything about the operator."
  :summary "Mark spoken turns in a span as overheard, not said to me."
  :available '("operator")
  :core '("responding")
  :parameters (obj "type" "object"
                   "properties" (obj "from" (obj "type" "string") "to" (obj "type" "string") "note" (obj "type" "string"))
                   "required" (list "from" "to" "note"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (handler-case
             (let ((n (mark-overheard kernel (obj-get args "from" "") (obj-get args "to" "") (obj-get args "note" ""))))
               (if (zerop n)
                   (format nil "No spoken turns from the operator between ~a and ~a are left to mark." (obj-get args "from") (obj-get args "to"))
                   (format nil "Marked ~d spoken turn~:p between ~a and ~a as overheard. They stay in my record, read as heard, not said to me."
                           n (obj-get args "from") (obj-get args "to"))))
           (error (e) (format nil "Not marked: ~a" e)))))
