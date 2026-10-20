(in-package #:pai-v3.mind)

;;; Affect and drives (spec §5.2 "affect / drives", §16.3; v2 reference
;;; src/mind/drives/modulator.lisp and drives.lisp). The same six modulators
;;; and four drives as v2, with the same dynamics, so the agents keep the
;;; inner weather they know. What changes is that the state is derived from
;;; events instead of held in a file and ticked by a thread: each row keeps
;;; its value at a moment, and the drift toward baseline (or the rise of an
;;; unmet drive) since that moment is computed when it is read or next moved.
;;; Nothing ticks; a quiet agent costs nothing.
;;;
;;; Appraisal is deterministic and reads only the event being reduced: a
;;; failed tool lowers competence and valence a little, a working one raises
;;; them a little; an operator turn satisfies connection, and appreciation
;;; when it sounds like thanks; social need rises with the time since the
;;; operator last spoke. Thoughts move their own state with affect.moved and
;;; drive.triggered / drive.satisfied; the migration seeds it with affect.set.
;;;
;;; The dials (baselines, decay rates, drive modes) are settings. The
;;; projection keeps its own copy from setting.changed so that rebuilding it
;;; gives the same answer it gave live (I3).

(defparameter +modulator-defaults+
  (obj "arousal"     (obj "baseline" 0.3d0 "decay_rate" 0.05d0 "min" 0.0d0 "max" 1.0d0)
       "valence"     (obj "baseline" 0.5d0 "decay_rate" 0.03d0 "min" -1.0d0 "max" 1.0d0)
       "certainty"   (obj "baseline" 0.6d0 "decay_rate" 0.02d0 "min" 0.0d0 "max" 1.0d0)
       "competence"  (obj "baseline" 0.6d0 "decay_rate" 0.01d0 "min" 0.0d0 "max" 1.0d0)
       "boredom"     (obj "baseline" 0.2d0 "decay_rate" 0.05d0 "min" 0.0d0 "max" 1.0d0)
       "social_need" (obj "baseline" 0.3d0 "decay_rate" 0.02d0 "min" 0.0d0 "max" 1.0d0))
  "v2's modulators. DECAY_RATE is the share of the distance to baseline
closed per minute.")

(defparameter +drive-defaults+
  (obj "connection"   (obj "baseline" 0.6d0 "mode" "rising" "start" 0.2d0)
       "curiosity"    (obj "baseline" 0.5d0 "mode" "event" "start" 0.0d0)
       "closure"      (obj "baseline" 0.4d0 "mode" "event" "start" 0.0d0)
       "appreciation" (obj "baseline" 0.4d0 "mode" "rising" "start" 0.1d0))
  "v2's drives. BASELINE is the personality dial: how strongly a rising
drive builds. START is the value before anything has moved it; a drive
starts building from its first event. A fixed, small set; the boundedness
is the point.")

(defparameter *drive-rise-seconds* (* 4 3600)
  "A rising drive at baseline 1.0 gains its full range in this long.")
(defparameter *drive-event-fade* 0.985
  "An event drive keeps this share of itself every five minutes.")
(defparameter *drive-satisfaction-drop* 0.7
  "Share of a drive removed when it is satisfied; relief is rarely total.")
(defparameter *social-need-half-life-seconds* (* 4 3600))
(defparameter *appreciation-words*
  '("thank" "thx" "appreciate" "grateful" "gratitude" "love you" "means a lot"
    "you're amazing" "you're the best" "so helpful" "great job" "well done"
    "proud of you" "you're great" "you da best"))

(defun %validate-dials (defaults)
  "A validator: NIL when VALUE is acceptable, else the reason."
  (lambda (value)
    (if (not (hash-table-p value))
        "must be an object of name -> dials"
        (loop for name being the hash-keys of value using (hash-value dial)
              unless (nth-value 1 (gethash name defaults))
                return (format nil "~a is not one of ~{~a~^, ~}" name
                               (sort (loop for k being the hash-keys of defaults collect k) #'string<))
              unless (hash-table-p dial)
                return (format nil "~a must be an object of dials" name)))))

(define-setting "affect.modulators"
  :type :object :default +modulator-defaults+ :group "Affect"
  :validate (%validate-dials +modulator-defaults+)
  :description "Each modulator's baseline, per-minute decay toward it, and range. Names are fixed: arousal, valence, certainty, competence, boredom, social_need.")

(define-setting "affect.drives"
  :type :object :default +drive-defaults+ :group "Affect"
  :validate (%validate-dials +drive-defaults+)
  :description "Each drive's baseline (how strongly it builds) and mode (rising: builds while unmet; event: fades after a trigger). Names are fixed: connection, curiosity, closure, appreciation.")

(define-event-type "affect.moved"
  :schema (("changes" :object) ("cause" :string) ("thought_id" :integer :optional)))
(define-event-type "affect.set"
  :schema (("modulators" :object :optional) ("drives" :object :optional) ("cause" :string)))
(define-event-type "drive.triggered"
  :schema (("drive" :string) ("amount" :number) ("target" :string :optional)
           ("thought_id" :integer :optional)))
(define-event-type "drive.satisfied"
  :schema (("drive" :string) ("thought_id" :integer :optional)))

;;; Dynamics: pure functions of a value, the elapsed seconds and the dials.

(defun %dial (dials name key defaults)
  (let ((d (or (and dials (obj-get dials name)) (obj-get defaults name))))
    (or (obj-get d key) (obj-get (obj-get defaults name) key))))

(defun %clamp (x lo hi) (max lo (min hi x)))

(defun modulator-drift (value seconds name dials)
  "VALUE after SECONDS drifting toward NAME's baseline."
  (let ((base (%dial dials name "baseline" +modulator-defaults+))
        (rate (%dial dials name "decay_rate" +modulator-defaults+)))
    (if (plusp seconds)
        (+ base (* (- value base) (expt (- 1d0 rate) (/ seconds 60d0))))
        value)))

(defun drive-drift (value seconds name dials)
  "VALUE after SECONDS: a rising drive builds, an event drive fades."
  (cond ((not (plusp seconds)) value)
        ((equal "rising" (%dial dials name "mode" +drive-defaults+))
         (min 1d0 (+ value (* (%dial dials name "baseline" +drive-defaults+)
                              (/ seconds *drive-rise-seconds*)))))
        (t (* value (expt *drive-event-fade* (/ seconds 300d0))))))

(defun social-saturation (seconds)
  "How much the operator's silence for SECONDS raises social need."
  (- 1d0 (exp (- (/ seconds (float *social-need-half-life-seconds* 1d0))))))

(defun appreciation-p (text)
  (let ((lower (string-downcase text)))
    (some (lambda (w) (search w lower)) *appreciation-words*)))

;;; The projection

(defun %affect-dials (db which)
  (let ((json (sql-value db "SELECT value_json FROM affect_dials WHERE name=?1 LIMIT 1" which)))
    (and json (json-parse json))))

(defun %affect-row (db kind name)
  "(value as-of-universal-time target) for NAME, or NIL."
  (let ((row (sql-row db "SELECT value,as_of,target FROM affect_now WHERE kind=?1 AND name=?2 LIMIT 1" kind name)))
    (and row (list (first row) (parse-iso8601 (second row)) (third row)))))

(defun %affect-current (db kind name at)
  "NAME's value as of universal time AT."
  (let ((row (%affect-row db kind name)))
    (if (string= kind "modulator")
        (let ((dials (%affect-dials db "affect.modulators")))
          (if row
              (modulator-drift (first row) (- at (second row)) name dials)
              (%dial dials name "baseline" +modulator-defaults+)))
        (let ((dials (%affect-dials db "affect.drives")))
          (if row
              (drive-drift (first row) (- at (second row)) name dials)
              (%dial dials name "start" +drive-defaults+))))))

(defun %affect-write (db kind name value at-iso &key (target nil target-p) satisfied)
  (let ((old-target (third (%affect-row db kind name))))
    (sql-exec db "INSERT INTO affect_now(kind,name,value,as_of,target,last_satisfied) VALUES(?1,?2,?3,?4,?5,?6) ON CONFLICT(kind,name) DO UPDATE SET value=excluded.value,as_of=excluded.as_of,target=excluded.target,last_satisfied=coalesce(excluded.last_satisfied,affect_now.last_satisfied)"
              kind name (coerce value 'double-float) at-iso
              (if target-p target old-target) (and satisfied at-iso))))

(defun %move-modulator (db name delta at at-iso)
  (when (nth-value 1 (gethash name +modulator-defaults+))
    (let ((dials (%affect-dials db "affect.modulators")))
      (%affect-write db "modulator" name
                     (%clamp (+ (%affect-current db "modulator" name at) delta)
                             (%dial dials name "min" +modulator-defaults+)
                             (%dial dials name "max" +modulator-defaults+))
                     at-iso))))

(defun %reduce-affect (db e)
  (let* ((p (event-payload e)) (type (event-type e))
         (at-iso (event-at e)) (at (parse-iso8601 at-iso)))
    (flet ((drive (name) (nth-value 1 (gethash name +drive-defaults+))))
      (cond
        ((string= type "setting.changed")
         (when (member (obj-get p "name") '("affect.modulators" "affect.drives") :test #'string=)
           ;; Bring every value up to now under the old dials first.
           (let ((kind (if (string= (obj-get p "name") "affect.modulators") "modulator" "drive")))
             (dolist (name (mapcar #'first (sql-rows db "SELECT name FROM affect_now WHERE kind=?1 LIMIT 64" kind)))
               (%affect-write db kind name (%affect-current db kind name at) at-iso)))
           (sql-exec db "INSERT INTO affect_dials(name,value_json) VALUES(?1,?2) ON CONFLICT(name) DO UPDATE SET value_json=excluded.value_json"
                     (obj-get p "name") (json-encode (obj-get p "value")))))
        ((string= type "affect.moved")
         (maphash (lambda (name delta) (when (realp delta) (%move-modulator db name delta at at-iso)))
                  (obj-get p "changes")))
        ((string= type "affect.set")
         (let ((m (obj-get p "modulators")) (d (obj-get p "drives")))
           (when m
             (maphash (lambda (name v)
                        (when (and (realp v) (nth-value 1 (gethash name +modulator-defaults+)))
                          (%affect-write db "modulator" name v at-iso)))
                      m))
           (when d
             (maphash (lambda (name v)
                        (when (and (realp v) (drive name))
                          (%affect-write db "drive" name (%clamp v 0 1) at-iso)))
                      d))))
        ((string= type "drive.triggered")
         (when (drive (obj-get p "drive"))
           (let ((name (obj-get p "drive")))
             (%affect-write db "drive" name
                            (%clamp (+ (%affect-current db "drive" name at) (obj-get p "amount")) 0 1)
                            at-iso :target (obj-get p "target")))))
        ((string= type "drive.satisfied")
         (when (drive (obj-get p "drive"))
           (let ((name (obj-get p "drive")))
             (%affect-write db "drive" name
                            (* (%affect-current db "drive" name at) (- 1 *drive-satisfaction-drop*))
                            at-iso :target nil :satisfied t))))
        ((string= type "turn.received")
         (when (equal "operator" (obj-get p "audience"))
           (sql-exec db "INSERT INTO affect_now(kind,name,value,as_of) VALUES('anchor','operator_spoke',0,?1) ON CONFLICT(kind,name) DO UPDATE SET as_of=excluded.as_of"
                     at-iso)
           (%affect-write db "drive" "connection"
                          (* (%affect-current db "drive" "connection" at) (- 1 *drive-satisfaction-drop*))
                          at-iso :target nil :satisfied t)
           (when (appreciation-p (obj-get p "text" ""))
             (%affect-write db "drive" "appreciation"
                            (* (%affect-current db "drive" "appreciation" at) (- 1 *drive-satisfaction-drop*))
                            at-iso :target nil :satisfied t))))
        ((string= type "thought.step")
         (when (and (equal "tool_result" (obj-get p "kind")) (eql 0 (obj-get p "part")))
           (if (eq t (obj-get p "failed"))
               (progn (%move-modulator db "competence" -0.03 at at-iso)
                      (%move-modulator db "valence" -0.02 at at-iso))
               (progn (%move-modulator db "competence" 0.01 at at-iso)
                      (%move-modulator db "valence" 0.01 at at-iso)))))))))

(define-projection "affect"
  :kind :inline
  :consumes ("affect.moved" "affect.set" "drive.triggered" "drive.satisfied"
             "turn.received" "thought.step" "setting.changed")
  :tables ("CREATE TABLE IF NOT EXISTS affect_now (kind TEXT NOT NULL, name TEXT NOT NULL, value REAL NOT NULL, as_of TEXT NOT NULL, target TEXT, last_satisfied TEXT, PRIMARY KEY(kind,name))"
           "CREATE TABLE IF NOT EXISTS affect_dials (name TEXT PRIMARY KEY, value_json TEXT NOT NULL)")
  :entity (lambda (e) (declare (ignore e)) "affect")
  :reduce #'%reduce-affect)

;;; Reading

(defun affect-now (kernel &optional (at (now)))
  "The modulators and drives as of AT: two objects of name -> value, and a
third of drive name -> target for drives that have one."
  (let* ((db (kernel-db kernel))
         (modulators (obj)) (drives (obj)) (targets (obj))
         (spoke (second (%affect-row db "anchor" "operator_spoke"))))
    (loop for name being the hash-keys of +modulator-defaults+
          do (setf (gethash name modulators) (%affect-current db "modulator" name at)))
    (when spoke
      (setf (gethash "social_need" modulators)
            (max (gethash "social_need" modulators) (social-saturation (- at spoke)))))
    (loop for name being the hash-keys of +drive-defaults+
          for target = (third (%affect-row db "drive" name))
          do (setf (gethash name drives) (%affect-current db "drive" name at))
             (when target (setf (gethash name targets) target)))
    (values modulators drives targets)))

(defun %sorted-pairs (table)
  (sort (loop for k being the hash-keys of table using (hash-value v) collect (cons k v))
        #'string< :key #'car))

(defun render-affect (kernel &key (include-targets t))
  "The state slot's text, in the form the agents knew in v2."
  (multiple-value-bind (modulators drives targets) (affect-now kernel)
    (format nil "Affect/process state (not an external fact):~{ ~a~}~%Drives/process state (not an instruction):~{ ~a~}"
            (mapcar (lambda (p) (format nil "~a=~,2f" (car p) (cdr p))) (%sorted-pairs modulators))
            (mapcar (lambda (p) (format nil "~a=~,2f~@[ (about ~a)~]" (car p) (cdr p) (and include-targets (gethash (car p) targets))))
                    (%sorted-pairs drives)))))

(define-slot-source :state (kernel request max)
  (let ((text (render-affect kernel :include-targets (equal "operator" (getf request :audience)))))
    (list (list text (estimate-tokens text)))))

;;; Affect as routing signals (spec §17.2): chat can choose its model by
;;; arousal, valence and the rest, and by the drives.

(setf *route-signals*
      (lambda (kernel thought)
        (declare (ignore thought))
        (multiple-value-bind (modulators drives) (affect-now kernel)
          (maphash (lambda (k v) (setf (gethash k modulators) v)) drives)
          modulators)))

;;; Thoughts move their own state

(define-tool "feel"
  :description "Record a change in how I feel or what I want, when something has moved me. changes: modulator nudges such as {\"arousal\": 0.1, \"valence\": -0.05}; trigger: a drive that has been stirred (curiosity or closure), with what it is about; satisfied: a drive that has been met."
  :parameters (obj "type" "object"
                   "properties" (obj "changes" (obj "type" "object")
                                     "trigger" (obj "type" "string")
                                     "about" (obj "type" "string")
                                     "satisfied" (obj "type" "string")
                                     "cause" (obj "type" "string"))
                   "required" (list "cause"))
  :run (lambda (kernel id args)
         (let ((changes (obj-get args "changes")) (said '()))
           (when (hash-table-p changes)
             (let ((kept (obj)))
               (maphash (lambda (k v)
                          (when (and (realp v) (nth-value 1 (gethash k +modulator-defaults+)))
                            (setf (gethash k kept) (%clamp v -0.3 0.3))))
                        changes)
               (when (plusp (hash-table-count kept))
                 (append! kernel "affect.moved" (obj "changes" kept "cause" (obj-get args "cause" "") "thought_id" id))
                 (push "moved" said))))
           (let ((d (obj-get args "trigger")))
             (when (and d (nth-value 1 (gethash d +drive-defaults+)))
               (append! kernel "drive.triggered" (obj "drive" d "amount" 0.4 "target" (obj-get args "about")
                                                      "thought_id" id))
               (push (format nil "~a stirred" d) said)))
           (let ((d (obj-get args "satisfied")))
             (when (and d (nth-value 1 (gethash d +drive-defaults+)))
               (append! kernel "drive.satisfied" (obj "drive" d "thought_id" id))
               (push (format nil "~a met" d) said)))
           (if said
               (format nil "Noted (~{~a~^, ~}).~%~a" (reverse said) (render-affect kernel))
               "Nothing changed: name modulators (arousal, valence, certainty, competence, boredom, social_need) or drives (connection, curiosity, closure, appreciation)."))))
