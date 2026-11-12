(in-package #:pai-v3.api)

;;; Noticing change (bead v3-wsd; operator and crew, 2026-11-11/12). The model
;;; is the orienting response: a sense registers a change, its size and its
;;; surprise decide whether it earns attention, a significant change starts a
;;; thought the agent thinks herself, and repetition habituates. Nothing here
;;; maps a change to an action. What she is watching for (watch_for) raises a
;;; matching change's salience and is put in front of her; it triggers nothing.
;;; Senses are opt-in per agent (senses.attend): the first are location and
;;; calendar. Mail and the camera keep their own paths for now.

(define-setting "senses.attend"
  :type :array :default '() :group "Senses"
  :description "The senses whose changes may start a thought (orienting): any of \"location\", \"calendar\". Empty: none.")
(define-setting "orienting.threshold"
  :type :number :min 0 :max 2 :default 0.45d0 :group "Senses"
  :description "A change scoring at least this (size times surprise, plus a match with something watched for) starts a thought.")
(define-setting "orienting.per_sense_per_hour"
  :type :integer :min 0 :max 30 :default 3 :group "Senses"
  :description "Most thoughts one sense may start in an hour. A change matching something watched for is exempt.")
(define-setting "orienting.dwell_minutes"
  :type :integer :min 0 :max 60 :default 3 :group "Senses"
  :description "A new place counts as arrived at only when the fixes have stayed there this long.")

(define-event-type "sense.changed"
  :schema (("sense" :string) ("from" :string) ("to" :string) ("size" :number) ("surprise" :number)
           ("score" :number) ("seen_before" :integer) ("watch_id" :string :optional)
           ("thought_id" :integer :optional) ("held_back" :string :optional)))
(define-event-type "watch.set"
  :schema (("watch_id" :string) ("what" :string) ("why" :string) ("until" :string)))
(define-event-type "watch.cleared"
  :schema (("watch_id" :string) ("reason" :string)))

(define-projection "orienting"
  :kind :inline
  :consumes ("sense.changed" "watch.set" "watch.cleared")
  :tables ("CREATE TABLE IF NOT EXISTS sense_changes (event_id INTEGER PRIMARY KEY, sense TEXT NOT NULL, transition TEXT NOT NULL, to_state TEXT NOT NULL, thought_id INTEGER, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS sense_changes_sense_idx ON sense_changes(sense,at)"
           "CREATE INDEX IF NOT EXISTS sense_changes_latest_idx ON sense_changes(sense,event_id)"
           "CREATE INDEX IF NOT EXISTS sense_changes_transition_idx ON sense_changes(transition,at)"
           "CREATE TABLE IF NOT EXISTS watches (watch_id TEXT PRIMARY KEY, what TEXT NOT NULL, why TEXT NOT NULL, until TEXT NOT NULL, open INTEGER NOT NULL)"
           "CREATE INDEX IF NOT EXISTS watches_open_idx ON watches(open,until)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (cond ((string= type "sense.changed")
                     (sql-exec db "INSERT OR IGNORE INTO sense_changes(event_id,sense,transition,to_state,thought_id,at) VALUES(?1,?2,?3,?4,?5,?6)"
                               (event-id e) (obj-get p "sense") (%transition (obj-get p "sense") (obj-get p "from") (obj-get p "to"))
                               (obj-get p "to") (obj-get p "thought_id") (event-at e)))
                    ((string= type "watch.set")
                     (sql-exec db "INSERT OR REPLACE INTO watches(watch_id,what,why,until,open) VALUES(?1,?2,?3,?4,1)"
                               (obj-get p "watch_id") (obj-get p "what") (obj-get p "why") (obj-get p "until")))
                    (t (sql-exec db "UPDATE watches SET open=0 WHERE watch_id=?1" (obj-get p "watch_id")))))))

(defun %transition (sense from to) (format nil "~a:~a>~a" sense (string-downcase from) (string-downcase to)))

(defun %attends-p (kernel sense)
  (member sense (setting kernel "senses.attend") :test #'equal))

;;; Surprise: the same change seen often lately surprises less (habituation).
;;; The baseline is this agent's own record, so each agent's is her own.

(defun %seen-before (kernel transition)
  (or (sql-value (kernel-db kernel) "SELECT count(*) FROM sense_changes WHERE transition=?1 AND at>=?2 LIMIT 1"
                 transition (iso8601 (- (now) (* 30 86400))))
      0))

(defun %surprise (seen) (/ 1d0 (1+ (* 0.5d0 seen))))

;;; What she is watching for

(defun open-watches (kernel)
  "(watch_id what why until) of each open, unexpired watch."
  (sql-rows (kernel-db kernel) "SELECT watch_id,what,why,until FROM watches WHERE open=1 AND until>=?1 LIMIT 20" (now-iso8601)))

(defparameter +watch-stopwords+
  '("the" "and" "for" "has" "his" "her" "from" "with" "was" "are" "that" "this" "into" "operator" "jamie"
    "arriving" "arrives" "arrived" "when" "gets" "get" "home" "there")
  "Words a watch and a change share without meaning the same thing. Home is
named as a place when it is one.")

(defun %words (text)
  (remove-if (lambda (w) (or (< (length w) 3) (member w +watch-stopwords+ :test #'string=)))
             (uiop:split-string (string-downcase (substitute-if #\Space (lambda (c) (not (alphanumericp c))) (or text ""))))))

(defun %matching-watch (kernel description)
  "The open watch sharing a word (three letters or more, not a common one) with DESCRIPTION, or NIL."
  (let ((words (%words description)))
    (find-if (lambda (w) (intersection (%words (second w)) words :test #'string=)) (open-watches kernel))))

;;; A change, scored, and perhaps a thought

(defun %sense-thoughts-last-hour (kernel sense)
  (or (sql-value (kernel-db kernel) "SELECT count(*) FROM sense_changes WHERE sense=?1 AND at>=?2 AND thought_id IS NOT NULL LIMIT 1"
                 sense (iso8601 (- (now) 3600)))
      0))

(defun register-change (kernel sense from to size detail)
  "Score a change in SENSE from FROM to TO (SIZE 0..1, DETAIL a line for the
thought) and, when it earns attention, start an orienting thought. Returns the
recorded payload."
  (let* ((transition (%transition sense from to))
         (seen (%seen-before kernel transition))
         (surprise (%surprise seen))
         (watch (%matching-watch kernel (format nil "~a ~a" to detail)))
         (score (+ (* size surprise) (if watch 1 0)))
         (payload (obj "sense" sense "from" from "to" to "size" size "surprise" surprise
                       "score" score "seen_before" seen)))
    (when watch (setf (gethash "watch_id" payload) (first watch)))
    (cond ((< score (setting kernel "orienting.threshold"))
           (setf (gethash "held_back" payload) "below threshold"))
          ((and (not watch) (>= (%sense-thoughts-last-hour kernel sense) (setting kernel "orienting.per_sense_per_hour")))
           (setf (gethash "held_back" payload) "hourly cap"))
          (t (setf (gethash "thought_id" payload)
                   (request-thought kernel "noticing" (%orienting-prompt kernel sense from to detail surprise seen watch)
                                    :audience "operator"))))
    (append! kernel "sense.changed" payload)
    payload))

(defun %orienting-prompt (kernel sense from to detail surprise seen watch)
  (format nil "Noticed (~a, ~a): ~a.~%~a~%Surprise ~,2f: ~:[a change I have not seen in the last month~;~:*seen ~d time~:p in the last month~].~@[~%I was watching for this: ~a (~a).~]~@[~%Also open, that I am watching for: ~{~a~^; ~}.~]~%~%~
This is a change I noticed, not a message: no one asked me anything. What does it most likely mean? Say it as an assumption, labelled as one, with its reason. Act on it if it calls for something: quietly, or by telling the operator if it matters to them now. Ask the operator only before something consequential when I am not sure what the change means; otherwise record the assumption and let a later thought correct it. If I was watching for this, do what I meant to and clear the watch with watch_for clear. End with what I did, and what I chose not to do and why."
          sense (pai-v3.mind::local-stamp kernel (now-iso8601)) detail
          (format nil "From: ~a. To: ~a." from to)
          surprise (and (plusp seen) seen)
          (and watch (format nil "~a" (second watch))) (and watch (third watch))
          (let ((others (remove (first watch) (open-watches kernel) :key #'first :test #'equal)))
            (and others (mapcar #'second others)))))

;;; Location: a place counts once it has held for the dwell time; driving
;;; once two fixes in a row say so. A red light or a wobble starts nothing.

(defvar *location-pending* (make-hash-table :test 'equal)
  "Kernel agent id -> (state since): a new state not yet held long enough.")

(defun %location-state (payload)
  "Driving; a named place by its name; anywhere else by its town, so walking a
few streets is not a change."
  (let ((place (obj-get payload "place")))
    (cond ((eq t (obj-get payload "driving")) "driving")
          ((eq t (obj-get payload "named")) place)
          (t (let ((comma (position #\, place :from-end t)))
               (format nil "somewhere in ~a" (string-trim " " (if comma (subseq place (1+ comma)) place))))))))

(defun %settled-location (kernel)
  (sql-value (kernel-db kernel) "SELECT to_state FROM sense_changes WHERE sense=?1 ORDER BY event_id DESC LIMIT 1" "location"))

(defun sense-location-fix (kernel payload)
  "Called with each recorded fix."
  (when (%attends-p kernel "location")
    (let* ((state (%location-state payload))
           (settled (%settled-location kernel))
           (key (kernel-agent-id kernel))
           (pending (gethash key *location-pending*)))
      (cond ((null settled)                 ; the first fix ever: where things stand, not a change
             (append! kernel "sense.changed" (obj "sense" "location" "from" "unknown" "to" state "size" 0 "surprise" 0
                                                  "score" 0 "seen_before" 0 "held_back" "first fix")))
            ((equal state settled) (remhash key *location-pending*))
            ((or (null pending) (not (equal (first pending) state)))
             (setf (gethash key *location-pending*) (list state (now))))
            ((or (equal state "driving") (equal settled "driving")
                 (>= (- (now) (second pending)) (* 60 (setting kernel "orienting.dwell_minutes"))))
             (remhash key *location-pending*)
             (register-change kernel "location" settled state
                              (cond ((eq t (obj-get payload "named")) 0.8d0)
                                    ((or (equal state "driving") (equal settled "driving")) 0.6d0)
                                    (t 0.6d0))
                              (cond ((equal state "driving")
                                     (format nil "The operator has started driving~@[ (about ~d km/h)~], leaving ~a"
                                             (and (realp (obj-get payload "speed_kmh")) (round (obj-get payload "speed_kmh"))) settled))
                                    ((equal settled "driving") (format nil "The operator has stopped driving and is at ~a" (obj-get payload "place")))
                                    (t (format nil "The operator has moved from ~a to ~a (~a)" settled state (obj-get payload "place"))))))))))

;;; Calendar: something new, moved or cancelled in the next few hours.

(defun sense-calendar-change (kernel kind summary start)
  "Called when an event is first seen, moves, or is cancelled. KIND is new, moved or cancelled."
  (when (%attends-p kernel "calendar")
    (let ((at (%event-time start (%zone-of kernel))))
      (when (and at (< (now) at (+ (now) (* 6 3600))))
        (register-change kernel "calendar" "planned" (format nil "~a: ~a" kind summary)
                         (if (equal kind "cancelled") 0.7d0 0.6d0)
                         (format nil "On the operator's calendar, ~a in the next few hours: ~a at ~a"
                                 (cond ((equal kind "new") "a new event") ((equal kind "moved") "an event moved") (t "an event cancelled"))
                                 summary (pai-v3.mind::local-stamp kernel start)))))))

;;; watch_for

(define-tool "watch_for"
  :description "Note something I expect, so that when a change I notice matches it I am reminded of it: \"Jamie arriving at his mom's: pin it\". It does not make anything happen; it makes a matching change stand out and puts what I meant in front of me then. what: the change, in words the change will share (a place, an event); why: what I mean to do; hours: how long to watch (default 24). action clear (id, reason) stops watching; list shows what I am watching for."
  :summary "Note an expected change and what I mean to do when it comes."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "set" "clear" "list"))
                                     "what" (obj "type" "string") "why" (obj "type" "string")
                                     "hours" (obj "type" "number") "id" (obj "type" "string") "reason" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((action (obj-get args "action")))
           (cond ((equal action "list")
                  (let ((w (open-watches kernel)))
                    (if w (format nil "~{~{~a: ~a (~a), until ~a~}~^~%~}" w) "I am not watching for anything.")))
                 ((equal action "clear")
                  (if (find (obj-get args "id") (open-watches kernel) :key #'first :test #'equal)
                      (progn (append! kernel "watch.cleared" (obj "watch_id" (obj-get args "id") "reason" (or (obj-get args "reason") "done")))
                             "Stopped watching.")
                      "No open watch has that id."))
                 ((equal action "set")
                  (let ((what (string-trim " " (or (obj-get args "what") ""))) (why (string-trim " " (or (obj-get args "why") ""))))
                    (if (or (zerop (length what)) (zerop (length why)))
                        "Say what change to watch for and what I mean to do when it comes."
                        (let ((wid (format nil "wf-~a" (subseq (pai-v3.kernel::sha256-hex (format nil "~a~a" what (now))) 0 8)))
                              (hours (let ((h (obj-get args "hours"))) (if (realp h) (max 1 (min 168 h)) 24))))
                          (append! kernel "watch.set" (obj "watch_id" wid "what" what "why" why
                                                           "until" (iso8601 (+ (now) (round (* 3600 hours))))))
                          (format nil "Watching for ~s (~a) for ~d hours." what wid (round hours))))))
                 (t "action is set, clear or list.")))))
