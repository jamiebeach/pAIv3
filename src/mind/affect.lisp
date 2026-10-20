(in-package #:pai-v3.mind)

;;; Affect and drives (spec §5.2 "affect / drives", §16.3; v2 reference
;;; src/mind/drives/modulator.lisp and drives.lisp). The same six modulators
;;; and four drives as v2, with legacy dynamics by default, so the agents keep the
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
  "Validate partial per-agent objects, including each dial and its units/range."
  (lambda (value)
    (block invalid
      (flet ((bad (text) (return-from invalid text)))
        (unless (hash-table-p value) (bad "must be an object of name -> dials"))
        (maphash
         (lambda (name dial)
           (unless (nth-value 1 (gethash name defaults)) (bad (format nil "unknown affect name ~a" name)))
           (unless (hash-table-p dial) (bad (format nil "~a must be an object of dials" name)))
           (let* ((modulator (eq defaults +modulator-defaults+))
                  (lo (if (and modulator (equal name "valence")) -1 0))
                  (keys (if modulator
                            (append '("baseline" "decay_rate" "min" "max")
                                    (and (equal name "social_need") '("silence_seconds")))
                            '("baseline" "start" "mode" "resting_level" "rise_per_hour"
                              "retention_per_five_minutes" "trigger_amount" "satisfaction_drop"))))
             (maphash
              (lambda (key v)
                (unless (member key keys :test #'equal) (bad (format nil "unknown dial ~a.~a" name key)))
                (if (equal key "mode")
                    (unless (member v '("rising" "event") :test #'equal) (bad "mode must be rising or event"))
                    (unless (and (realp v)
                                 (if (equal key "silence_seconds")
                                     (<= 1 v 31536000)
                                     (<= (if (member key '("baseline" "min" "max") :test #'equal) lo 0) v 1)))
                      (bad (format nil "~a.~a has an invalid numeric range" name key)))))
              dial)
             (when modulator
               (let* ((d (obj-get defaults name)) (min (obj-get dial "min" (obj-get d "min")))
                      (max (obj-get dial "max" (obj-get d "max")))
                      (base (obj-get dial "baseline" (obj-get d "baseline"))))
                 (unless (<= min base max) (bad (format nil "~a needs min <= baseline <= max" name)))))))
         value)
        nil))))

(define-setting "affect.modulators"
  :type :object :default +modulator-defaults+ :group "Affect"
  :validate (%validate-dials +modulator-defaults+)
  :description "Per-agent baseline, decay_rate (share of distance closed per minute), min/max. social_need also accepts silence_seconds: time without operator contact to reach 63% social need (default 14400). Partial objects use legacy defaults.")

(define-setting "affect.drives"
  :type :object :default +drive-defaults+ :group "Affect"
  :validate (%validate-dials +drive-defaults+)
  :description "Per-agent drives: resting_level (initial desire and event settling level), rise_per_hour (0-1), retention_per_five_minutes (0-1), trigger_amount (0-1), satisfaction_drop (fraction relieved,0-1), mode rising/event. Legacy baseline/start remain valid; absent new dials preserve existing dynamics. High drive means unmet desire, not satisfaction.")

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

(defun %drive-option (dials name key fallback)
  (let ((dial (and dials (obj-get dials name))))
    (if dial (obj-get dial key fallback) fallback)))

(defun %drive-resting (dials name)
  (%drive-option dials name "resting_level" 0d0))

(defun %drive-start (dials name)
  (%drive-option dials name "resting_level" (%dial dials name "start" +drive-defaults+)))

(defun %legacy-drive-drift (value seconds name dials)
  ;; Retain original operation order and floating-point behavior for old state.
  (cond ((not (plusp seconds)) value)
        ((equal "rising" (%dial dials name "mode" +drive-defaults+))
         (min 1d0 (+ value (* (%dial dials name "baseline" +drive-defaults+)
                              (/ seconds *drive-rise-seconds*)))))
        (t (* value (expt *drive-event-fade* (/ seconds 300d0))))))

(defun drive-drift (value seconds name dials)
  "Per-agent persistence and build rate; missing new dials preserve legacy math."
  (let ((dial (and dials (obj-get dials name))))
    (unless (and dial (some (lambda (key) (nth-value 1 (gethash key dial)))
                           '("resting_level" "rise_per_hour" "retention_per_five_minutes")))
      (return-from drive-drift (%legacy-drive-drift value seconds name dials))))
  (if (not (plusp seconds)) value
      (let* ((rising (equal "rising" (%dial dials name "mode" +drive-defaults+)))
             (rate (%drive-option dials name "rise_per_hour"
                                  (if rising (* (%dial dials name "baseline" +drive-defaults+) (/ 3600d0 *drive-rise-seconds*)) 0d0)))
             (keep (%drive-option dials name "retention_per_five_minutes" (if rising 1d0 *drive-event-fade*)))
             (rest (%drive-resting dials name)))
        (%clamp
         (cond ((= keep 1) (+ value (* rate (/ seconds 3600d0))))
               ((zerop keep) rest)
               (t (let* ((k (/ (- (log keep)) 300d0)) (fade (exp (- (* k seconds)))))
                    (+ rest (* (- value rest) fade) (* (/ rate (* 3600d0 k)) (- 1d0 fade))))))
         0 1))))

(defun social-saturation (seconds &optional dials)
  "Operator silence floor; silence_seconds is an e-folding time, not a half-life."
  (- 1d0 (exp (- (/ (max 0 seconds)
                       (float (%drive-option dials "social_need" "silence_seconds"
                                             *social-need-half-life-seconds*) 1d0))))))

(defun %satisfy-drive (db name at at-iso)
  (let* ((dials (%affect-dials db "affect.drives"))
         (drop (%drive-option dials name "satisfaction_drop" *drive-satisfaction-drop*)))
    (%affect-write db "drive" name (* (%affect-current db "drive" name at) (- 1 drop))
                   at-iso :target nil :satisfied t)))

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
              (%drive-start dials name))))))

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
           (%satisfy-drive db (obj-get p "drive") at at-iso)))
        ((string= type "turn.received")
         (when (equal "operator" (obj-get p "audience"))
           (sql-exec db "INSERT INTO affect_now(kind,name,value,as_of) VALUES('anchor','operator_spoke',0,?1) ON CONFLICT(kind,name) DO UPDATE SET as_of=excluded.as_of"
                     at-iso)
           ;; Old events keep their historical interpretation during rebuild.
           (unless (eq t (obj-get p "semantic_affect"))
             (%satisfy-drive db "connection" at at-iso)
             (when (appreciation-p (obj-get p "text" ""))
               (%satisfy-drive db "appreciation" at at-iso)))))
        ((string= type "affect.appraisal.finished")
         (unless (sql-value db "SELECT 1 FROM affect_appraisal_seen WHERE source_key=?1 LIMIT 1" (obj-get p "source_key"))
           (when (equal "applied" (obj-get p "status"))
           ;; The event contains bounded effects, not mutable future policy or text.
           (maphash (lambda (name delta) (when (realp delta) (%move-modulator db name (%clamp delta -0.3 0.3) at at-iso)))
                    (obj-get p "changes"))
           (maphash (lambda (name amount)
                      (when (and (drive name) (realp amount))
                        (%affect-write db "drive" name
                                       (%clamp (+ (%affect-current db "drive" name at) (%clamp amount 0 1)) 0 1)
                                       at-iso :target (format nil "message ~d" (obj-get p "source_id")))))
                    (obj-get p "triggers"))
           (maphash (lambda (name fraction)
                      (when (and (drive name) (realp fraction))
                        (%affect-write db "drive" name
                                       (* (%affect-current db "drive" name at) (- 1 (%clamp fraction 0 1)))
                                       at-iso :target nil :satisfied t)))
                    (obj-get p "satisfactions")))
           (sql-exec db "INSERT INTO affect_appraisal_seen(source_key,event_id) VALUES(?1,?2)" (obj-get p "source_key") (event-id e))))
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
             "turn.received" "thought.step" "setting.changed" "affect.appraisal.finished")
  :tables ("CREATE TABLE IF NOT EXISTS affect_now (kind TEXT NOT NULL, name TEXT NOT NULL, value REAL NOT NULL, as_of TEXT NOT NULL, target TEXT, last_satisfied TEXT, PRIMARY KEY(kind,name))"
           "CREATE TABLE IF NOT EXISTS affect_dials (name TEXT PRIMARY KEY, value_json TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS affect_appraisal_seen (source_key TEXT PRIMARY KEY, event_id INTEGER NOT NULL)")
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
            (max (gethash "social_need" modulators) (social-saturation (- at spoke) (%affect-dials db "affect.modulators")))))
    (loop for name being the hash-keys of +drive-defaults+
          for target = (third (%affect-row db "drive" name))
          do (setf (gethash name drives) (%affect-current db "drive" name at))
             (when target (setf (gethash name targets) target)))
    (values modulators drives targets)))

(defun %sorted-pairs (table)
  (sort (loop for k being the hash-keys of table using (hash-value v) collect (cons k v))
        #'string< :key #'car))

(defun %level-word (value)
  (cond ((< value 0.2d0) "very low") ((< value 0.4d0) "low")
        ((< value 0.6d0) "moderate") ((< value 0.8d0) "high") (t "very high")))

(defun %affect-meaning (name value)
  (if (equal name "valence")
      (cond ((<= value -0.6d0) "very negative") ((< value -0.2d0) "negative")
            ((<= value 0.2d0) "neutral") ((< value 0.6d0) "positive") (t "very positive"))
      (format nil "~a ~a" (%level-word value)
              (cdr (assoc name '(("arousal" . "activation") ("boredom" . "boredom")
                                ("certainty" . "felt certainty") ("competence" . "felt competence")
                                ("social_need" . "need for connection")) :test #'equal)))))

(defun %affect-trend (value future)
  ;; The derivative sign, not a claim about a remembered emotional episode.
  (cond ((> (- future value) 0.00001d0) "rising")
        ((< (- future value) -0.00001d0) "settling") (t "steady")))

(defun render-affect (kernel &key (include-targets t))
  "Numbers plus deterministic meaning and passive one-minute direction."
  (let* ((at (now)) (db (kernel-db kernel))
         (md (%affect-dials db "affect.modulators")) (dd (%affect-dials db "affect.drives"))
         (spoke (second (%affect-row db "anchor" "operator_spoke"))))
    (multiple-value-bind (modulators drives targets) (affect-now kernel at)
      (format nil "Affect/process state (not an external fact): ~{~a~^; ~}~%Drives (unmet desires, not instructions): ~{~a~^; ~}"
              (mapcar (lambda (p)
                        (let ((future (modulator-drift (cdr p) 60 (car p) md)))
                          (when (and spoke (equal "social_need" (car p)))
                            (setf future (max future (social-saturation (- (+ at 60) spoke) md))))
                          (format nil "~a=~,2f (~a; ~a)" (car p) (cdr p) (%affect-meaning (car p) (cdr p))
                                  (%affect-trend (cdr p) future))))
                      (%sorted-pairs modulators))
              (mapcar (lambda (p)
                        (let ((target (and include-targets (gethash (car p) targets))))
                          (format nil "~a=~,2f~@[ (about ~a)~] (~a unmet desire; ~a)"
                                  (car p) (cdr p) (and target (subseq target 0 (min 80 (length target))))
                                  (%level-word (cdr p))
                                  (%affect-trend (cdr p) (drive-drift (cdr p) 60 (car p) dd)))))
                      (%sorted-pairs drives))))))

(define-slot-source :state (kernel request max)
  (let ((text (render-affect kernel :include-targets (equal "operator" (getf request :audience)))))
    ;; Keep the ten meanings in hand even when several private targets are long.
    (when (> (estimate-tokens text) max)
      (setf text (render-affect kernel :include-targets nil)))
    (when (<= (estimate-tokens text) max)
      (list (list text (estimate-tokens text))))))

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
  :description "Record a change in how I feel or what I want, when something has moved me. Incoming messages may already have been semantically appraised; do not repeat the same adjustment merely by rereading them. changes: modulator nudges such as {\"arousal\": 0.1, \"valence\": -0.05}; trigger: a drive that has been stirred (curiosity or closure), with what it is about; satisfied: a drive that has been met."
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
               (append! kernel "drive.triggered" (obj "drive" d "amount" (%drive-option (%affect-dials (kernel-db kernel) "affect.drives") d "trigger_amount" 0.4) "target" (obj-get args "about")
                                                      "thought_id" id))
               (push (format nil "~a stirred" d) said)))
           (let ((d (obj-get args "satisfied")))
             (when (and d (nth-value 1 (gethash d +drive-defaults+)))
               (append! kernel "drive.satisfied" (obj "drive" d "thought_id" id))
               (push (format nil "~a met" d) said)))
           (if said
               (format nil "Noted (~{~a~^, ~}).~%~a" (reverse said) (render-affect kernel :include-targets (equal "operator" (getf (thought-row kernel id) :audience))))
               "Nothing changed: name modulators (arousal, valence, certainty, competence, boredom, social_need) or drives (connection, curiosity, closure, appreciation)."))))
