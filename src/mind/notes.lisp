(in-package #:pai-v3.mind)

;;; Notes to my next thought (continuity piece 2, first part; designed with
;;; Angel and Aria in the forum thread "Having a body", 2026-11-17). At the end
;;; of a thought an agent may leave a few lines for her next one, in her own
;;; words. They reach the next thought of the same audience under "Left from
;;; last time", with when they were written. This is the write path a later
;;; trial builds on: a note is recorded as hers where it is written, never
;;; inferred from how it reads.

(defparameter *note-max-chars* 600 "Longest note a thought may leave.")
(defparameter *note-shown-hours* 24 "A note older than this is no longer shown.")

(define-event-type "note.left"
  :schema (("thought_id" :integer) ("audience" :string) ("text" :string)))

(define-projection "notes_left"
  :kind :inline
  :consumes ("note.left")
  :tables ("CREATE TABLE IF NOT EXISTS notes_left (event_id INTEGER PRIMARY KEY, thought_id INTEGER NOT NULL, audience TEXT NOT NULL, text TEXT NOT NULL, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS notes_left_audience_idx ON notes_left(audience,event_id)")
  :entity (lambda (e) (event-id e))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO notes_left(event_id,thought_id,audience,text,at) VALUES(?1,?2,?3,?4,?5)"
                        (event-id e) (obj-get p "thought_id") (obj-get p "audience") (obj-get p "text") (event-at e)))))

(define-tool "leave_note"
  :description "Leave a few lines for my next thought, in my own words: what is still open, what surprised me, what I would do differently. It reaches my next thought with this audience under \"Left from last time\". One note per thought; leaving another replaces it. Leave nothing when there is nothing to carry."
  :parameters (obj "type" "object"
                   "properties" (obj "text" (obj "type" "string"
                                                 "description" (format nil "At most ~d characters." *note-max-chars*)))
                   "required" (list "text"))
  :run (lambda (kernel id args)
         (let ((text (string-trim '(#\Space #\Newline #\Tab #\Return) (or (obj-get args "text") "")))
               (row (thought-row kernel id)))
           (cond ((zerop (length text)) "Nothing left: the note was empty.")
                 ((null row) "Nothing left: this thought has no record to attach the note to.")
                 (t (let ((kept (subseq text 0 (min *note-max-chars* (length text)))))
                      (append! kernel "note.left" (obj "thought_id" id "audience" (getf row :audience) "text" kept))
                      (format nil "Left for my next thought~:[~; (cut to ~d characters)~]."
                              (> (length text) *note-max-chars*) *note-max-chars*)))))))

(defun latest-note (kernel audience &key exclude-thought)
  "The newest note left for AUDIENCE within *NOTE-SHOWN-HOURS*, not by
EXCLUDE-THOUGHT: (text at thought-id), or NIL. A thought's latest note
replaces its earlier ones."
  (sql-row (kernel-db kernel)
           "SELECT text,at,thought_id FROM notes_left WHERE audience=?1 AND thought_id<>?2 AND at>=?3 ORDER BY event_id DESC LIMIT 1"
           audience (or exclude-thought -1)
           (iso8601 (- (now) (* 3600 *note-shown-hours*)))))

(define-slot-source :left-note (kernel request max)
  ;; Filtered by audience: a note reaches only thoughts with the audience of
  ;; the thought that left it.
  (let ((note (latest-note kernel (getf request :audience) :exclude-thought (getf request :thought-id))))
    (when note
      (destructuring-bind (text at thought-id) note
        (declare (ignore thought-id))
        (let ((line (format nil "A note I left for myself (~a): ~a" (or (written-ago kernel at) at) text)))
          (list (list line (estimate-tokens line))))))))
