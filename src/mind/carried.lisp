(in-package #:pai-v3.mind)

;;; Carried items (docs/charges-and-attention.md): things the agent keeps in
;;; mind for the operator and returns to, until they are done. A commitment
;;; found in mail ("timesheet due tomorrow by 10"), a thing the operator is
;;; waiting for, a standing interest with a condition ("when real material on
;;; X appears"). Three neighbours do other jobs: the schedule fires exact
;;; text at a fixed time, concerns are the agent's own curiosities, and the
;;; docket makes things. A carried item remembers; when it is time, or its
;;; condition shows up, it brings a thought, and that thought may notify the
;;; operator, open a docket item to make something, carry it on, or close it.
;;;
;;; Authority: outside content (mail, calendar) can make a thought that
;;; carries something or asks for an item to be looked at, never one that
;;; changes or closes what is carried. The thought that looks at an item is
;;; the operator's own, so that is where work and messages follow.

(define-setting "carried.max_open"
  :type :integer :min 1 :max 200 :default 30 :group "Carried items"
  :description "Most items carried at once.")
(define-setting "carried.max_per_day"
  :type :integer :min 1 :max 200 :default 10 :group "Carried items"
  :description "Most items I may add on my own in a UTC day (the operator's own, and those from a conversation with them, are not counted).")

(define-event-type "carried.added"
  :schema (("id" :string) ("what" :string) ("why" :string) ("by" :string)
           ("source" :string :optional) ("consider_at" :string :optional) ("due_at" :string :optional)
           ("condition" :string :optional) ("thought_id" :integer :optional)))
(define-event-type "carried.revised"
  :schema (("id" :string) ("what" :string :optional) ("why" :string :optional)
           ("consider_at" :string :optional) ("due_at" :string :optional) ("condition" :string :optional)
           ("work_id" :string :optional) ("note" :string :optional)))
(define-event-type "carried.considered" :schema (("id" :string) ("thought_id" :integer :optional)))
(define-event-type "carried.closed" :schema (("id" :string) ("state" :string) ("note" :string :optional)))

(defun %carry-given (value)
  "VALUE, or NIL for JSON null, false or a missing value."
  (if (member value '(:null :false nil)) nil value))

(defun %carry-normalise (text)
  (string-downcase (format nil "~{~a~^ ~}" (remove "" (uiop:split-string (string-trim " " (or text "")) :separator '(#\Space #\Tab #\Newline)) :test #'string=))))

(define-projection "carried"
  :kind :inline
  :consumes ("carried.added" "carried.revised" "carried.considered" "carried.closed")
  :tables ("CREATE TABLE IF NOT EXISTS carried_items (id TEXT PRIMARY KEY, what TEXT NOT NULL, normalised TEXT NOT NULL, why TEXT NOT NULL, by TEXT NOT NULL, source TEXT, consider_at TEXT, due_at TEXT, condition_text TEXT, work_id TEXT, note TEXT, state TEXT NOT NULL DEFAULT 'open', created_at TEXT NOT NULL, last_considered_at TEXT, times_considered INTEGER NOT NULL DEFAULT 0, closed_at TEXT)"
           "CREATE INDEX IF NOT EXISTS carried_items_due_idx ON carried_items(state,consider_at)"
           "CREATE INDEX IF NOT EXISTS carried_items_norm_idx ON carried_items(state,normalised)"
           "CREATE INDEX IF NOT EXISTS carried_items_day_idx ON carried_items(by,created_at)")
  :entity (lambda (e) (obj-get (event-payload e) "id"))
  :reduce
  (lambda (db e)
    (let ((p (event-payload e)) (type (event-type e)))
      (flet ((f (k) (%carry-given (obj-get p k))))
        (cond
          ((string= type "carried.added")
           (sql-exec db "INSERT INTO carried_items(id,what,normalised,why,by,source,consider_at,due_at,condition_text,created_at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
                     (f "id") (f "what") (%carry-normalise (f "what")) (f "why") (f "by") (f "source")
                     (f "consider_at") (f "due_at") (f "condition") (event-at e)))
          ((string= type "carried.revised")
           (sql-exec db "UPDATE carried_items SET what=COALESCE(?2,what),normalised=COALESCE(?3,normalised),why=COALESCE(?4,why),consider_at=COALESCE(?5,consider_at),due_at=COALESCE(?6,due_at),condition_text=COALESCE(?7,condition_text),work_id=COALESCE(?8,work_id),note=COALESCE(?9,note) WHERE id=?1 AND state='open'"
                     (f "id") (f "what") (and (f "what") (%carry-normalise (f "what"))) (f "why") (f "consider_at")
                     (f "due_at") (f "condition") (f "work_id") (f "note")))
          ((string= type "carried.considered")
           (sql-exec db "UPDATE carried_items SET consider_at=NULL,last_considered_at=?2,times_considered=times_considered+1 WHERE id=?1"
                     (f "id") (event-at e)))
          (t
           (sql-exec db "UPDATE carried_items SET state=?2,note=COALESCE(?3,note),consider_at=NULL,closed_at=?4 WHERE id=?1"
                     (f "id") (f "state") (f "note") (event-at e))))))))

;;; Reading

(defparameter +carried-columns+
  "id,what,why,by,source,consider_at,due_at,condition_text,work_id,note,state,created_at,last_considered_at,times_considered")

(defun %carried-object (kernel r)
  (destructuring-bind (id what why by source consider due condition work note state created last times) r
    (flet ((local (iso) (if iso (or (ignore-errors (local-stamp kernel iso)) iso) :null)))
      (obj "id" id "what" what "why" why "by" by "source" (or source :null)
           "consider_at" (or consider :null) "consider_local" (local consider)
           "due_at" (or due :null) "due_local" (local due)
           "condition" (or condition :null) "work_id" (or work :null) "note" (or note :null)
           "state" state "created_at" created "last_considered_at" (or last :null) "times_considered" times))))

(defun carried-items (kernel &key (state "open") (limit 40))
  "Items in STATE as objects, those waiting on a condition first, then by when they will be looked at."
  (mapcar (lambda (r) (%carried-object kernel r))
          (sql-rows (kernel-db kernel)
                    (format nil "SELECT ~a FROM carried_items WHERE state=?1 ORDER BY consider_at,created_at LIMIT ?2" +carried-columns+)
                    state limit)))

(defun carried-due (kernel &key (limit 3))
  "Open items whose time to be looked at has come, soonest first."
  (mapcar (lambda (r) (%carried-object kernel r))
          (sql-rows (kernel-db kernel)
                    (format nil "SELECT ~a FROM carried_items WHERE state=?1 AND consider_at<=?2 ORDER BY consider_at LIMIT ?3" +carried-columns+)
                    "open" (now-iso8601) limit)))

(defun carried-item (kernel id)
  (let ((r (sql-row (kernel-db kernel) (format nil "SELECT ~a FROM carried_items WHERE id=?1 LIMIT 1" +carried-columns+) id)))
    (and r (%carried-object kernel r))))

;;; Writing. Each returns (values result nil) or (values nil why-not).

(defun %carry-time (kernel text)
  "ISO UTC for the operator's local wall time TEXT (2026-10-19T09:00); NIL when none given."
  (when (and (stringp text) (plusp (length (string-trim " " text))))
    (let ((at (handler-case (local-to-universal (string-trim " " text) (%zone kernel nil))
                (error () (error "~a is not a date and time such as 2026-10-19T09:00" text)))))
      (iso8601 at))))

(defun %carry-ok (s n) (and (stringp s) (< 0 (length (string-trim " " s)) (1+ n))))

(defun carry-add (kernel &key what why source consider-at due-at condition (by "self") thought-id)
  (handler-case
      (let ((open (or (sql-value (kernel-db kernel) "SELECT count(*) FROM carried_items WHERE state=?1 LIMIT 1" "open") 0))
            (existing (and (stringp what)
                           (sql-value (kernel-db kernel) "SELECT id FROM carried_items WHERE state=?1 AND normalised=?2 LIMIT 1" "open" (%carry-normalise what)))))
        (cond ((not (and (%carry-ok what 240) (%carry-ok why 600)))
               (values nil "say what is being carried (up to 240 characters) and why it matters to the operator (up to 600)"))
              (existing (values existing "already carrying that"))
              ((>= open (setting kernel "carried.max_open"))
               (values nil "I am carrying as many things as I may; close one first"))
              ((and (equal by "self")
                    (>= (or (sql-value (kernel-db kernel) "SELECT count(*) FROM carried_items WHERE by=?1 AND created_at>=?2 LIMIT 1"
                                       "self" (format nil "~aT00:00:00Z" (subseq (now-iso8601) 0 10)))
                            0)
                        (setting kernel "carried.max_per_day")))
               (values nil "I have added enough on my own today; it can wait until tomorrow"))
              (t (let ((consider (%carry-time kernel consider-at)) (due (%carry-time kernel due-at)) (id (format nil "c-~(~36r~)-~(~36r~)" (now) (random (expt 36 3)))))
                   (when (and consider (< (parse-iso8601 consider) (- (now) 60))) (error "~a is already past" consider-at))
                   (append! kernel "carried.added"
                            (obj "id" id "what" (string-trim " " what) "why" (string-trim " " why) "by" by
                                 "source" (and (%carry-ok source 300) source) "consider_at" consider "due_at" due
                                 "condition" (and (%carry-ok condition 300) condition) "thought_id" thought-id))
                   (values id nil)))))
    (error (e) (values nil (princ-to-string e)))))

(defun %carry-open-p (kernel id)
  (sql-value (kernel-db kernel) "SELECT 1 FROM carried_items WHERE id=?1 AND state=?2 LIMIT 1" id "open"))

(defun carry-revise (kernel id &key what why consider-at due-at condition work-id note)
  (handler-case
      (cond ((not (%carry-open-p kernel id)) (values nil "nothing open is carried as that"))
            ((and what (not (%carry-ok what 240))) (values nil "what is up to 240 characters"))
            (t (let ((consider (%carry-time kernel consider-at)))
                 (when (and consider (< (parse-iso8601 consider) (- (now) 60))) (error "~a is already past" consider-at))
                 (append! kernel "carried.revised"
                          (obj "id" id "what" what "why" why "consider_at" consider "due_at" (%carry-time kernel due-at)
                               "condition" condition "work_id" work-id "note" (and note (subseq note 0 (min 600 (length note))))))
                 (values id nil))))
    (error (e) (values nil (princ-to-string e)))))

(defun carry-consider-now (kernel id note)
  "Ask for ID to be looked at now, with NOTE saying what turned up."
  (if (%carry-open-p kernel id)
      (progn (append! kernel "carried.revised"
                      (obj "id" id "consider_at" (now-iso8601) "note" (subseq (or note "") 0 (min 600 (length (or note ""))))))
             (values id nil))
      (values nil "nothing open is carried as that")))

(defun carry-close (kernel id state note)
  (cond ((not (member state '("done" "dropped") :test #'equal)) (values nil "state is done or dropped"))
        ((not (%carry-open-p kernel id)) (values nil "nothing open is carried as that"))
        (t (append! kernel "carried.closed" (obj "id" id "state" state "note" (and note (subseq note 0 (min 600 (length note))))))
           (values id nil))))

;;; What a thought sees

(define-slot-source :carried (kernel request max)
  (when (member (getf request :audience) '("operator" "external") :test #'equal)
    (let ((used 0) (rows '()))
      (dolist (i (carried-items kernel :limit 12))
        (let* ((text (format nil "- [~a] ~a~@[ — due ~a~]~@[; waiting for: ~a~]~@[; look again ~a~]~@[; work ~a~]"
                             (obj-get i "id") (obj-get i "what")
                             (let ((d (obj-get i "due_local"))) (and (stringp d) d))
                             (let ((c (obj-get i "condition"))) (and (stringp c) c))
                             (let ((c (obj-get i "consider_local"))) (and (stringp c) c))
                             (let ((w (obj-get i "work_id"))) (and (stringp w) w))))
               (tokens (estimate-tokens text)))
          (when (> (+ used tokens) max) (return))
          (incf used tokens)
          (push (list text tokens) rows)))
      (nreverse rows))))

(define-open-items "carried" (kernel terms audience)
  (when (member audience '("operator" "external") :test #'equal)
    (loop for i in (carried-items kernel :limit 40)
          when (or (mentions-any-p (obj-get i "what") terms) (mentions-any-p (obj-get i "condition") terms))
            collect (format nil "Carrying: ~a" (obj-get i "what")))))

;;; The agent's tool

(define-tool "carry"
  :description "Carry things the operator needs kept in mind and returned to until they are done: a deadline found in mail, something they are waiting for, a standing interest with a condition. Not for exact-time reminders (schedule), your own curiosities (concern) or making something (work_docket). action: add (what: a short line; why: why it matters to the operator; optionally source: where it came from; consider_at: the local time to look at it again, like 2026-10-19T08:30; due_at: when it is actually due; condition: what to watch for, like new material on a topic or a reply from someone), list, revise (id, plus any of what, why, consider_at, due_at, condition, work_id, note), consider (id, note: look at this now, for something that turned up), close (id, state done or dropped, note). When an item's time comes, or you ask for it to be looked at, a thought of the operator's own takes it up and may tell them, open work for it, carry it on or close it. From outside content (mail, calendar) only add, list and consider are allowed."
  :summary "Carry things the operator needs kept in mind until done."
  :available '("operator" "external")
  :core '("responding" "noticing" "introspection" "concern")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "add" "list" "revise" "consider" "close"))
                                     "id" (obj "type" "string") "what" (obj "type" "string") "why" (obj "type" "string")
                                     "source" (obj "type" "string") "consider_at" (obj "type" "string")
                                     "due_at" (obj "type" "string") "condition" (obj "type" "string")
                                     "work_id" (obj "type" "string") "note" (obj "type" "string")
                                     "state" (obj "type" "string" "enum" (list "done" "dropped")))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (let* ((row (thought-row kernel thought-id))
                (audience (getf row :audience))
                (action (obj-get args "action" "list"))
                (id (obj-get args "id")))
           (flet ((say (id why-not ok)
                    (if id (format nil ok id) (format nil "Not done: ~a." why-not))))
             (cond
               ((and (equal audience "external") (not (member action '("add" "list" "consider") :test #'string=)))
                "Not done: from outside content I may only add, list or ask for an item to be looked at.")
               ((string= action "add")
                (multiple-value-bind (id why-not)
                    (carry-add kernel :what (obj-get args "what") :why (obj-get args "why") :source (obj-get args "source")
                                      :consider-at (obj-get args "consider_at") :due-at (obj-get args "due_at")
                                      :condition (obj-get args "condition") :thought-id thought-id
                                      :by (if (and (equal audience "operator") (equal (getf row :origin) "responding")) "conversation" "self"))
                  (cond ((and id why-not) (format nil "Already carrying that as ~a." id))
                        (id (format nil "Carrying ~a." id))
                        (t (format nil "Not done: ~a." why-not)))))
               ((string= action "revise")
                (multiple-value-bind (id why-not)
                    (carry-revise kernel (or id "") :what (obj-get args "what") :why (obj-get args "why")
                                                    :consider-at (obj-get args "consider_at") :due-at (obj-get args "due_at")
                                                    :condition (obj-get args "condition") :work-id (obj-get args "work_id")
                                                    :note (obj-get args "note"))
                  (say id why-not "Updated ~a.")))
               ((string= action "consider")
                (multiple-value-bind (id why-not) (carry-consider-now kernel (or id "") (obj-get args "note"))
                  (say id why-not "~a will be looked at shortly.")))
               ((string= action "close")
                (multiple-value-bind (id why-not) (carry-close kernel (or id "") (obj-get args "state") (obj-get args "note"))
                  (say id why-not "Closed ~a.")))
               (t (let ((items (carried-items kernel)))
                    (if items
                        (format nil "~{~a~^~%~}"
                                (mapcar (lambda (i) (format nil "[~a] ~a — ~a~@[ (due ~a)~]~@[ (waiting for: ~a)~]~@[ (look again ~a)~]~@[ (work ~a)~]"
                                                            (obj-get i "id") (obj-get i "what") (obj-get i "why")
                                                            (let ((d (obj-get i "due_local"))) (and (stringp d) d))
                                                            (let ((c (obj-get i "condition"))) (and (stringp c) c))
                                                            (let ((c (obj-get i "consider_local"))) (and (stringp c) c))
                                                            (let ((w (obj-get i "work_id"))) (and (stringp w) w))))
                                        items))
                        "I am not carrying anything."))))))))
