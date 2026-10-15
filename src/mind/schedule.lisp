(in-package #:pai-v3.mind)

;;; Scheduled items (spec §5.2 "scheduled"; v2 reference
;;; src/mind/ticks/scheduler.lisp). One-off and cron reminders in the
;;; operator's time zone, as data, never code. A schedule is events; the
;;; scheduled_items projection is the list, ordered by when each is next due.
;;; The writer computes the next firing time and puts it in the event, so the
;;; reducer stays pure. Firing claims the item first (schedule.fired carries
;;; the following time), which gives at-most-once delivery as in v2.
;;;
;;; When an item is due: notify mode posts its exact text to its audience as
;;; a notice (the operator's own words, not a model's); context mode leaves
;;; it in the agent's "Coming up" slot as just happened, for the next turn to
;;; take up. Either mode also starts a thought (origin noticing) when the
;;; item asks for one.

;;; Time zones come from local-time's own IANA copy, loaded into the image so
;;; a standalone build carries them and no host path is assumed.
(local-time:reread-timezone-repository)

(define-setting "time.zone"
  :type :string :default "UTC" :group "Time"
  :validate (lambda (v) (unless (local-time:find-timezone-by-location-name v)
                          (format nil "~a is not a known time zone (use a name such as Europe/London)" v)))
  :description "The operator's time zone, for reminders and for showing times. An IANA name such as America/New_York.")

(define-setting "schedule.fired_visible_hours"
  :type :integer :min 1 :max 168 :default 24 :group "Time"
  :description "How long a reminder that has gone off stays in the Coming up slot.")

(define-event-type "schedule.added"
  :schema (("id" :string) ("kind" :string)            ; once | cron
           ("text" :string) ("mode" :string)          ; notify | context
           ("audience" :string) ("next_at" :string)
           ("cron" :string :optional) ("timezone" :string :optional)  ; "user" follows time.zone
           ("wake" :boolean :optional)                ; also start a thought when due
           ("origin" :object :optional)))
(define-event-type "schedule.fired"
  :schema (("id" :string) ("scheduled_for" :string) ("local_slot" :string)
           ("next_at" :string :optional)))            ; absent when nothing follows
(define-event-type "schedule.cancelled"
  :schema (("id" :string) ("reason" :string :optional)))
(define-event-type "schedule.rescheduled"
  :schema (("id" :string) ("next_at" :string) ("reason" :string)))

(define-projection "scheduled_items"
  :kind :inline
  :consumes ("schedule.added" "schedule.fired" "schedule.cancelled" "schedule.rescheduled")
  :tables ("CREATE TABLE IF NOT EXISTS scheduled_items (id TEXT PRIMARY KEY, kind TEXT NOT NULL, text TEXT NOT NULL, mode TEXT NOT NULL, audience TEXT NOT NULL, cron TEXT, timezone TEXT, wake INTEGER NOT NULL DEFAULT 0, status TEXT NOT NULL, next_at TEXT, last_fired_at TEXT, last_local_slot TEXT, created_at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS scheduled_items_due_idx ON scheduled_items(status,next_at)"
           "CREATE INDEX IF NOT EXISTS scheduled_items_fired_idx ON scheduled_items(audience,last_fired_at)")
  :entity (lambda (e) (obj-get (event-payload e) "id"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (flet ((f (k) (obj-get p k)))
                (cond
                  ((string= type "schedule.added")
                   (sql-exec db "INSERT INTO scheduled_items(id,kind,text,mode,audience,cron,timezone,wake,status,next_at,created_at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,'active',?9,?10)"
                             (f "id") (f "kind") (f "text") (f "mode") (f "audience") (f "cron")
                             (f "timezone") (if (eq t (f "wake")) 1 0) (f "next_at") (event-at e)))
                  ((string= type "schedule.fired")
                   (sql-exec db "UPDATE scheduled_items SET last_fired_at=?2,last_local_slot=?3,next_at=?4,status=CASE WHEN ?4 IS NULL THEN 'completed' ELSE status END WHERE id=?1"
                             (f "id") (f "scheduled_for") (f "local_slot") (f "next_at")))
                  ((string= type "schedule.rescheduled")
                   (sql-exec db "UPDATE scheduled_items SET next_at=?2 WHERE id=?1 AND status='active'"
                             (f "id") (f "next_at")))
                  (t
                   (sql-exec db "UPDATE scheduled_items SET status='cancelled',next_at=NULL WHERE id=?1"
                             (f "id"))))))))

;;; Time

(defun %zone (kernel name)
  (local-time:find-timezone-by-location-name
   (if (or (null name) (string= name "user")) (setting kernel "time.zone") name)))

(defun %local (universal zone)
  "(minute hour day month year weekday) of UNIVERSAL in ZONE; weekday 0 is Sunday."
  (multiple-value-bind (ns ss mi h d mo y wd)
      (local-time:decode-timestamp (local-time:universal-to-timestamp universal) :timezone zone)
    (declare (ignore ns ss))
    (list mi h d mo y wd)))

(defun local-slot (universal zone)
  "Wall-clock minute of UNIVERSAL in ZONE, as YYYY-MM-DD HH:MM."
  (destructuring-bind (mi h d mo y wd) (%local universal zone)
    (declare (ignore wd))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h mi)))

(defun local-to-universal (text zone)
  "Universal time of local wall time TEXT (YYYY-MM-DDTHH:MM[:SS], or with a
space) in ZONE."
  (flet ((n (from to) (parse-integer text :start from :end to)))
    (unless (and (>= (length text) 16) (member (char text 10) '(#\T #\Space)))
      (error "~a is not a date and time such as 2026-10-12T09:30" text))
    (local-time:timestamp-to-universal
     (local-time:encode-timestamp 0 (if (>= (length text) 19) (n 17 19) 0) (n 14 16) (n 11 13)
                                  (n 8 10) (n 5 7) (n 0 4) :timezone zone))))

;;; Cron: five fields, minute hour day month weekday, as in v2.

(defun %split (text char)
  (loop with start = 0
        for pos = (position char text :start start)
        collect (subseq text start pos)
        while pos do (setf start (1+ pos))))

(defun %cron-field (text minimum maximum &key weekday)
  "A membership vector for one field, and whether it was a bare *."
  (let ((allowed (make-array (1+ maximum) :initial-element nil)))
    (dolist (part (%split text #\,))
      (let* ((slash (position #\/ part))
             (base (if slash (subseq part 0 slash) part))
             (step (if slash (parse-integer part :start (1+ slash)) 1))
             (dash (position #\- base))
             (start (cond ((string= base "*") minimum)
                          (dash (parse-integer base :end dash))
                          (t (parse-integer base))))
             (end (cond ((string= base "*") maximum)
                        (dash (parse-integer base :start (1+ dash)))
                        (t start))))
        (when (or (<= step 0) (< start minimum) (> end maximum) (> start end))
          (error "cron field out of range: ~a" text))
        (loop for v from start to end by step
              do (setf (aref allowed (if (and weekday (= v 7)) 0 v)) t))))
    (values allowed (string= text "*"))))

(defun parse-cron (expression)
  (let ((fields (remove "" (%split (string-trim " " expression) #\Space) :test #'string=)))
    (unless (= 5 (length fields))
      (error "a cron expression has five fields: minute hour day month weekday"))
    (handler-case
        (multiple-value-bind (days day-any) (%cron-field (third fields) 1 31)
          (multiple-value-bind (weekdays weekday-any) (%cron-field (fifth fields) 0 7 :weekday t)
            (list (%cron-field (first fields) 0 59) (%cron-field (second fields) 0 23)
                  days (%cron-field (fourth fields) 1 12) weekdays day-any weekday-any)))
      (parse-error () (error "~a is not a cron expression" expression)))))

(defun cron-next (expression after zone &optional last-slot)
  "The first whole minute after universal time AFTER whose wall time in ZONE
matches EXPRESSION. LAST-SLOT, the wall-clock minute last fired, prevents a
second firing when clocks go back."
  (destructuring-bind (minutes hours days months weekdays day-any weekday-any) (parse-cron expression)
    (let ((candidate (* 60 (1+ (floor after 60))))
          (limit (+ after (* 5 366 86400))))
      (loop
        (when (> candidate limit) (error "~a never comes within five years" expression))
        (destructuring-bind (mi h d mo y wd) (%local candidate zone)
          (declare (ignore y))
          (let ((calendar (cond ((and day-any weekday-any) t)
                                (day-any (aref weekdays wd))
                                (weekday-any (aref days d))
                                (t (or (aref days d) (aref weekdays wd))))))
            (cond ((not (and calendar (aref months mo) (aref hours h)))
                   ;; Nothing this local hour can match: go to the next one.
                   (incf candidate (* 60 (- 60 mi))))
                  ((and (aref minutes mi)
                        (not (equal last-slot (local-slot candidate zone))))
                   (return candidate))
                  (t (incf candidate 60)))))))))

;;; Adding and firing

(defun %schedule-id ()
  (format nil "s-~(~36r~)-~(~36r~)" (now) (random (expt 36 4))))

(defun add-schedule (kernel text &key at in cron (mode "notify") (audience "operator") timezone wake origin)
  "Schedule TEXT: AT a local wall time, IN seconds from now, or on CRON. Returns the item's id."
  (unless (plusp (length text)) (error "a reminder needs text"))
  (unless (member mode '("notify" "context") :test #'string=)
    (error "mode is notify or context"))
  (let* ((zone (or (%zone kernel timezone) (error "~a is not a known time zone" timezone)))
         (next (cond (cron (cron-next cron (now) zone))
                     (in (if (and (integerp in) (plusp in)) (+ (now) in) (error "in must be a positive number of seconds")))
                     (at (local-to-universal at zone))
                     (t (error "say when: at, in or cron")))))
    (when (<= next (now)) (error "~a is already past" (local-slot next zone)))
    (let ((id (%schedule-id)))
      (append! kernel "schedule.added"
               (obj "id" id "kind" (if cron "cron" "once") "text" text "mode" mode
                    "audience" audience "next_at" (iso8601 next) "cron" cron
                    "timezone" (or timezone "user") "wake" (if wake t :false) "origin" origin))
      (values id next))))

(defun cancel-schedule (kernel id &optional reason)
  (unless (sql-value (kernel-db kernel) "SELECT 1 FROM scheduled_items WHERE id=?1 AND status='active' LIMIT 1" id)
    (error "nothing active is scheduled as ~a" id))
  (append! kernel "schedule.cancelled" (obj "id" id "reason" reason)))

(defun run-due-schedules (kernel &key (max 16))
  "Fire up to MAX items that are due. Each is claimed (its next time
recorded) and its delivery appended in one transaction. Returns the count."
  (let ((due (sql-rows (kernel-db kernel)
                       "SELECT id,kind,text,mode,audience,cron,timezone,wake,next_at,last_local_slot FROM scheduled_items WHERE status='active' AND next_at<=?1 ORDER BY next_at LIMIT ?2"
                       (now-iso8601) max)))
    (dolist (row due (length due))
      (destructuring-bind (id kind text mode audience cron timezone wake next-at last-slot) row
        (declare (ignore last-slot))
        (let* ((zone (%zone kernel timezone))
               (scheduled (parse-iso8601 next-at))
               (slot (local-slot scheduled zone))
               (following (and (string= kind "cron")
                               (ignore-errors (cron-next cron (max scheduled (now)) zone slot))))
               (specs (list (list "schedule.fired"
                                  (obj "id" id "scheduled_for" next-at "local_slot" slot
                                       "next_at" (and following (iso8601 following)))))))
          (when (string= mode "notify")
            (push (list "notice.posted" (obj "channel" "reminder" "audience" audience
                                             "text" text "importance" "reminder"))
                  specs))
          (let ((fired (first (append-batch! kernel (reverse specs)))))
            ;; A reminder going off is something noticed; the claim above
            ;; already holds, so a crash here loses the thought, never repeats it.
            (when (eql wake 1)
              (request-thought kernel "noticing" (format nil "It is ~a: ~a" slot text)
                               :audience audience :source-event-id (event-id fired)))))))))

;;; The Coming up slot: what has just gone off (context mode), then what is next.

(define-slot-source :scheduled (kernel request max)
  (let* ((audience (getf request :audience))
         (zone (%zone kernel nil))
         (since (iso8601 (- (now) (* 3600 (setting kernel "schedule.fired_visible_hours")))))
         (fired (sql-rows (kernel-db kernel)
                          "SELECT text,last_fired_at FROM scheduled_items WHERE audience=?1 AND last_fired_at>=?2 AND mode='context' ORDER BY last_fired_at DESC LIMIT 5"
                          audience since))
         (next (sql-rows (kernel-db kernel)
                         "SELECT text,next_at,kind FROM scheduled_items WHERE status='active' AND audience=?1 ORDER BY next_at LIMIT 10"
                         audience)))
    (loop for line in (append
                       (mapcar (lambda (r) (format nil "Went off at ~a: ~a" (local-slot (parse-iso8601 (second r)) zone) (first r)))
                               fired)
                       (mapcar (lambda (r) (format nil "~a~:[~; (repeats)~]: ~a" (local-slot (parse-iso8601 (second r)) zone)
                                                   (string= "cron" (third r)) (first r)))
                               next))
          collect (list line (estimate-tokens line)))))

;;; The agent's tool

(define-tool "schedule"
  :description "Set, list or cancel reminders in the operator's time zone. action: add (text, and one of at: a local time like 2026-10-12T09:30, in: seconds from now, cron: five fields minute hour day month weekday; mode: notify sends the text as is, context keeps it in mind for my next turn; wake: also start a thought then), list, or cancel (id)."
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "add" "list" "cancel"))
                                     "text" (obj "type" "string") "at" (obj "type" "string")
                                     "in" (obj "type" "integer") "cron" (obj "type" "string")
                                     "mode" (obj "type" "string" "enum" (list "notify" "context"))
                                     "wake" (obj "type" "boolean") "id" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel id args)
         (let ((audience (getf (thought-row kernel id) :audience))
               (action (obj-get args "action" "list")))
           (handler-case
               (cond
                 ((string= action "add")
                  (multiple-value-bind (sid next)
                      (add-schedule kernel (obj-get args "text" "") :at (obj-get args "at") :in (obj-get args "in")
                                                                    :cron (obj-get args "cron") :mode (obj-get args "mode" "notify")
                                                                    :audience audience :wake (eq t (obj-get args "wake"))
                                                                    :origin (obj "principal_type" "self" "thought_id" id))
                    (format nil "Scheduled ~a for ~a (~a)." sid (local-slot next (%zone kernel nil)) (setting kernel "time.zone"))))
                 ((string= action "cancel")
                  (cancel-schedule kernel (obj-get args "id" "") "cancelled by me")
                  "Cancelled.")
                 (t
                  (let ((rows (sql-rows (kernel-db kernel)
                                        "SELECT id,text,next_at,kind,mode FROM scheduled_items WHERE status='active' AND audience=?1 ORDER BY next_at LIMIT 50"
                                        audience)))
                    (if rows
                        (format nil "~{~a~^~%~}"
                                (mapcar (lambda (r) (format nil "~a ~a~:[~; (repeats)~] [~a]: ~a" (first r)
                                                            (local-slot (parse-iso8601 (third r)) (%zone kernel nil))
                                                            (string= "cron" (fourth r)) (fifth r) (second r)))
                                        rows))
                        "Nothing is scheduled."))))
             (error (e) (format nil "Not scheduled: ~a" e))))))
