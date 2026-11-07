(in-package #:pai-v3.mind)

;;; What I understand the operator needs (2026-11-07; the crew's design thread,
;;; with the operator's two decisions). Each agent keeps her own view, from her
;;; own interactions, seen by no other agent: divergence is signal, not drift.
;;; She writes and edits entries with her own judgment, in her own words, with no
;;; categories and no one's sign-off. What the substrate holds her to is the
;;; evidence: an entry names the operator's own messages it rests on, or it is
;;; not kept, and it is shown with those words quoted from the record by the
;;; system, how old they are, and when it is due to be checked again. The prose
;;; is her claim; the quote and the ages are the check (the memory-notes lesson).

(defparameter *need-recheck-days* 30 "Days before an entry is due to be checked again, unless given.")
(defparameter *needs-most* 12 "Most entries kept at once.")

(define-event-type "operator.need.set"
  :schema (("id" :string) ("text" :string) ("record_ids" :array) ("recheck_on" :string)
           ("thought_id" :integer :optional)))
(define-event-type "operator.need.removed"
  :schema (("id" :string) ("why" :string :optional)))

(define-projection "operator_needs"
  :kind :inline
  :consumes ("operator.need.set" "operator.need.removed")
  :tables ("CREATE TABLE IF NOT EXISTS operator_needs (id TEXT PRIMARY KEY, text TEXT NOT NULL, record_ids TEXT NOT NULL, recheck_on TEXT NOT NULL, set_at TEXT NOT NULL, first_at TEXT NOT NULL, state TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS operator_needs_state_idx ON operator_needs(state,set_at)")
  :entity (lambda (e) (obj-get (event-payload e) "id"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (string= (event-type e) "operator.need.removed")
                  (sql-exec db "UPDATE operator_needs SET state='removed' WHERE id=?1" (obj-get p "id"))
                  (sql-exec db "INSERT INTO operator_needs(id,text,record_ids,recheck_on,set_at,first_at,state) VALUES(?1,?2,?3,?4,?5,?5,'open') ON CONFLICT(id) DO UPDATE SET text=excluded.text,record_ids=excluded.record_ids,recheck_on=excluded.recheck_on,set_at=excluded.set_at,state='open'"
                            (obj-get p "id") (obj-get p "text") (json-encode (obj-get p "record_ids")) (obj-get p "recheck_on") (event-at e))))))

(defun %operator-words (kernel event-id)
  "(at text) of EVENT-ID when it is something the operator said: a turn they gave,
or a forum post of theirs; else NIL."
  (let ((e (ignore-errors (event-by-id kernel event-id))))
    (when e
      (let ((p (event-payload e)) (type (event-type e)))
        (cond ((and (string= type "turn.received") (equal "operator" (obj-get (obj-get p "origin") "principal_type")))
               (list (event-at e) (obj-get p "text")))
              ((and (string= type "forum.posted") (equal "operator" (obj-get p "author_id")))
               (list (event-at e) (obj-get p "text"))))))))

(defun operator-needs (kernel)
  "Open entries, newest first, as (id text record-ids recheck-on set-at)."
  (mapcar (lambda (r) (list (first r) (second r) (or (ignore-errors (json-parse (third r))) '()) (fourth r) (fifth r)))
          (sql-rows (kernel-db kernel)
                    "SELECT id,text,record_ids,recheck_on,set_at FROM operator_needs WHERE state=?1 ORDER BY set_at DESC LIMIT ?2"
                    "open" *needs-most*)))

(defun %need-line (kernel row)
  (destructuring-bind (id text records recheck set-at) row
    (declare (ignore set-at))
    (let* ((words (remove nil (mapcar (lambda (r) (%operator-words kernel r)) records)))
           (latest (first (sort (mapcar #'first words) #'string>)))
           (due (and (stringp recheck) (string< recheck (iso8601 (now))))))
      (format nil "- [~a; from ~d message~:p of the operator's, the latest ~a~@[: \"~a\"~]~:[; check again by ~a~;; due to be checked again since ~a: look at the record, then keep, amend or remove it~]] ~a"
              id (length words)
              (if latest (or (ignore-errors (written-ago kernel latest)) latest) "no longer found")
              (and words (let ((s (substitute #\Space #\Newline (or (second (find latest words :key #'first :test #'equal)) ""))))
                           (subseq s 0 (min 160 (length s)))))
              due (subseq recheck 0 10) text))))

(define-slot-source :needs (kernel request max)
  (when (equal "operator" (getf request :audience))
    (let ((used 0) (rows '()))
      (dolist (row (operator-needs kernel) (nreverse rows))
        (let* ((text (%need-line kernel row)) (tokens (estimate-tokens text)))
          (when (> (+ used tokens) max) (return (nreverse rows)))
          (incf used tokens)
          (push (list text tokens) rows))))))

(defun set-need (kernel &key id text record-ids recheck-days thought-id)
  "Keep or amend an entry. (values id nil), or (values nil why-not)."
  (let ((records (remove-if-not #'integerp (mapcar (lambda (r) (if (stringp r) (ignore-errors (parse-integer r)) r)) record-ids))))
    (cond ((not (and (stringp text) (< 3 (length (string-trim " " text)) 600)))
           (values nil "say the need in 4 to 600 characters"))
          ;; The one thing held in code: the operator's own words behind it.
          ((null records)
           (values nil "name the operator's messages it rests on in record_ids (event ids of what they said, as recall and search_experience show them)"))
          ((find-if-not (lambda (r) (%operator-words kernel r)) records)
           (values nil (format nil "event ~a is not something the operator said; record_ids are their turns or forum posts"
                               (find-if-not (lambda (r) (%operator-words kernel r)) records))))
          ((and (null id) (>= (length (operator-needs kernel)) *needs-most*))
           (values nil (format nil "I keep at most ~d; remove or fold one first" *needs-most*)))
          ((and id (not (sql-value (kernel-db kernel) "SELECT 1 FROM operator_needs WHERE id=?1 AND state=?2 LIMIT 1" id "open")))
           (values nil "no open entry has that id"))
          (t (let ((id (or id (format nil "n-~a" (pai-v3.kernel:base64url-encode (ironclad:random-data 6))))))
               (append! kernel "operator.need.set"
                        (obj "id" id "text" (string-trim " " text) "record_ids" records "thought_id" thought-id
                             "recheck_on" (iso8601 (+ (now) (* 86400 (if (integerp recheck-days) (max 1 (min 365 recheck-days)) *need-recheck-days*))))))
               (values id nil))))))

(define-tool "operator_needs"
  :description "My own view of what the operator needs: sleep, food, rest, work boundaries, whatever I judge serves them, in my own words. It is mine alone; no other agent sees it. action: list; add (text, record_ids: the event ids of the operator's own messages it rests on, required; optional recheck_days, default 30); amend (id, text, record_ids); remove (id, optional why). Each entry is shown to me with their words quoted from the record and how old they are, and comes due to be checked again."
  :summary "Keep my view of what the operator needs, with their own words behind it."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "list" "add" "amend" "remove"))
                                     "id" (obj "type" "string") "text" (obj "type" "string")
                                     "record_ids" (obj "type" "array" "items" (obj "type" "integer"))
                                     "recheck_days" (obj "type" "integer") "why" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (let ((action (obj-get args "action")))
           (cond ((equal action "list")
                  (let ((rows (operator-needs kernel)))
                    (if rows (format nil "~{~a~^~%~}" (mapcar (lambda (r) (%need-line kernel r)) rows)) "I keep no entries yet.")))
                 ((member action '("add" "amend") :test #'equal)
                  (multiple-value-bind (id why) (set-need kernel :id (and (equal action "amend") (obj-get args "id"))
                                                                 :text (obj-get args "text") :record-ids (obj-get args "record_ids")
                                                                 :recheck-days (obj-get args "recheck_days") :thought-id thought-id)
                    (if id (format nil "Kept ~a." id) (format nil "Not kept: ~a." why))))
                 ((equal action "remove")
                  (if (sql-value (kernel-db kernel) "SELECT 1 FROM operator_needs WHERE id=?1 AND state=?2 LIMIT 1" (obj-get args "id" "") "open")
                      (progn (append! kernel "operator.need.removed" (obj "id" (obj-get args "id") "why" (obj-get args "why")))
                             (format nil "Removed ~a." (obj-get args "id")))
                      "Not done: no open entry has that id."))
                 (t "action is list, add, amend or remove.")))))
