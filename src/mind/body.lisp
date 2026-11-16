(in-package #:pai-v3.mind)

;;; Body state (bead v3-baw.3.1; design docs/architecture/body-state.md). What
;;; a body is doing, kept by the agent's side, so a page draws it instead of
;;; deciding it. Nothing ticks. What is stored is an ANCHOR, written only when
;;; something happens (v3-baw.3.2): what the agent is doing, where its body
;;; was, and how it felt, at that moment. Everything since is DERIVED on read
;;; by BODY-AT, a pure function of the anchor and the time: no clock, no
;;; database, no randomness but a generator seeded by the anchor's own start.
;;; So every reader at the same moment gets the same body, a page closed for
;;; an hour finds what an hour would have done, and nothing ran meanwhile.
;;;
;;; The crew, 2026-11-15: the state when nobody looks is coarse and true
;;; (working, resting, away, unfinished, with where it got to), never dice
;;; for its own sake; a body at rest stays at rest.

(defparameter +activities+ '("talking" "working" "unfinished" "idle" "resting" "away")
  "What the agent is doing, as far as its body is concerned.")
(defparameter +postures+ '("standing" "walking" "sitting" "reading" "writing" "thinking" "away")
  "Named classes, not clips: how a view shows each is the view's business.")
(defparameter +gazes+ '("visitor" "aside" "task" "waypoint"))
(defparameter +work-details+ '("reading" "writing" "thinking"))

(defparameter *places*
  '(("home" . (("centre" 0.0d0 0.0d0) ("window" -1.2d0 -0.6d0) ("desk" 1.1d0 -0.4d0)
               ("chair" 0.9d0 0.6d0 :seat) ("shelf" -0.9d0 0.8d0))))
  "Each place's named spots: (name x z [:seat]), metres on its floor. One place
until places become records of their own (Phase D, v3-baw.4).")

(defparameter *talk-lingers* (* 10 60) "A conversation with no event for this long has ended: idle.")
(defparameter *idle-before-rest* (* 90 60) "Idle this long, the body settles: resting.")
(defparameter *walk-speed* 0.7d0 "Metres a second.")
(defparameter *most-moves* 64 "Most moves one read works out; idle is bounded long before this.")

;;; A generator of its own, so the answer does not depend on the Lisp's.

(defun %next-seed (seed) (mod (+ (* seed 1103515245) 12345) 2147483648))
(defun %unit (seed) (/ (float seed 1d0) 2147483648d0))

;;; The anchor

(defun %spots (place) (cdr (assoc place *places* :test #'equal)))
(defun %spot (place name) (assoc name (%spots place) :test #'equal))

(defun make-body-anchor (&key (activity "idle") detail (place "home") (spot "centre") (facing 0d0)
                              since feeling wearing)
  "The record kept for a body: what it is doing (ACTIVITY, with DETAIL such as
reading, or what an unfinished thing is), where (PLACE, SPOT, FACING in
radians), what it wears, since when (universal time), and the two numbers of
FEELING (an object, as BODY-FEELING gives) its time alone depends on:
boredom and arousal at that moment."
  (assert (member activity +activities+ :test #'equal) () "~s is not an activity" activity)
  (assert (%spot place spot) () "~s is not a spot of ~s" spot place)
  (obj "activity" activity "detail" (or detail :null) "place" place "spot" spot
       "facing" (coerce facing 'double-float) "wearing" (or wearing "")
       "since" (or since (now))
       "boredom" (coerce (or (and feeling (obj-get feeling "boredom")) 0.2d0) 'double-float)
       "arousal" (coerce (or (and feeling (obj-get feeling "arousal")) 0.3d0) 'double-float)
       "valence" (coerce (or (and feeling (obj-get feeling "valence")) 0.5d0) 'double-float)))

(defun body-anchor-of (state)
  "The anchor a state was derived from (a state carries it), or STATE itself."
  (or (obj-get state "anchor") state))

;;; Phases: what an activity becomes with no event, at fixed times from the anchor

(defun %phase (anchor elapsed)
  "(activity phase-start) for ELAPSED seconds after ANCHOR: talking lapses to
idle, idle settles to resting; working, unfinished, resting and away hold
until an event changes them."
  (let ((a (obj-get anchor "activity")))
    (cond ((and (equal a "talking") (>= elapsed *talk-lingers*))
           (if (>= elapsed (+ *talk-lingers* *idle-before-rest*))
               (list "resting" (+ *talk-lingers* *idle-before-rest*))
               (list "idle" *talk-lingers*)))
          ((and (equal a "idle") (>= elapsed *idle-before-rest*)) (list "resting" *idle-before-rest*))
          (t (list a 0)))))

;;; Moves: only while idle, the one activity with nothing to hold the body still

(defun %dwell (seed boredom)
  "Seconds to stay before moving on: about 20 minutes when nothing bores it,
about 4 when much does; drawn from SEED."
  (let ((mean (+ 240d0 (* 960d0 (- 1d0 (max 0d0 (min 1d0 boredom)))))))
    (* mean (+ 0.5d0 (%unit seed)))))

(defun %moves (anchor from-x from-z from-spot start end)
  "The walks an idle body takes between START and END seconds after the
anchor: a list of (leave arrive spot x z), from FROM-SPOT. Seeded by the
anchor's start, so the same anchor always walks the same way."
  (let* ((seed (%next-seed (mod (+ (obj-get anchor "since") 7919) 2147483648)))
         (boredom (obj-get anchor "boredom"))
         (at start) (x from-x) (z from-z) (here from-spot) (moves '()))
    (loop repeat *most-moves*
          do (setf seed (%next-seed seed))
             (incf at (%dwell seed boredom))
             (when (>= at end) (return))
             (setf seed (%next-seed seed))
             (let* ((choices (remove here (%spots (obj-get anchor "place")) :key #'first :test #'equal))
                    (to (nth (floor (* (%unit seed) (length choices))) choices))
                    (walk (/ (sqrt (+ (expt (- (second to) x) 2) (expt (- (third to) z) 2))) *walk-speed*)))
               (push (list at (+ at walk) (first to) (second to) (third to)) moves)
               (setf at (+ at walk) x (second to) z (third to) here (first to))))
    (nreverse moves)))

;;; Reading the body

(defun %face (valence)
  (cond ((> valence 0.35d0) (list "relaxed" 0.3d0))
        ((< valence -0.35d0) (list "sad" 0.3d0))
        (t (list "neutral" 0d0))))

(defun %work-posture (anchor)
  (let ((d (obj-get anchor "detail")))
    (if (member d +work-details+ :test #'equal) d "thinking")))

(defun body-at (state now)
  "What the body of STATE (an anchor, or a state derived from one) is doing at
NOW. Pure: the same anchor and NOW give the same answer, and deriving in two
steps gives what deriving in one does. A time before the anchor is read as the
anchor itself. Returns an object: activity, detail, place, spot, x, z,
facing, posture, gaze, face, strength, wearing, since (when the activity
shown began), for (seconds in it), next_change (seconds until this answer
next changes, or null), and the anchor."
  (let* ((anchor (body-anchor-of state))
         (since (obj-get anchor "since"))
         (elapsed (max 0 (- now since)))
         (place (obj-get anchor "place"))
         (start (%spot place (obj-get anchor "spot"))))
    (destructuring-bind (activity phase-start) (%phase anchor elapsed)
      (let ((x (second start)) (z (third start)) (spot (first start)) (facing (obj-get anchor "facing"))
            (posture nil) (gaze nil) (next nil))
        (flet ((until (at) (let ((d (- at elapsed))) (when (and (plusp d) (or (null next) (< d next))) (setf next d)))))
          ;; when this phase ends with no event
          (cond ((equal (obj-get anchor "activity") "talking")
                 (if (equal activity "talking") (until *talk-lingers*)
                     (when (equal activity "idle") (until (+ *talk-lingers* *idle-before-rest*)))))
                ((equal activity "idle") (until *idle-before-rest*)))
          (cond
            ((equal activity "away") (setf posture "away" gaze "aside"))
            ((equal activity "talking") (setf posture "standing" gaze "visitor"))
            ((member activity '("working" "unfinished") :test #'equal)
             (setf posture (if (equal activity "working") (%work-posture anchor) "thinking") gaze "task"))
            ((equal activity "resting")
             ;; at rest where idling left it; seated if that spot is a seat
             (let* ((idle-from (if (equal (obj-get anchor "activity") "talking") *talk-lingers* 0))
                    (walked (and (member (obj-get anchor "activity") '("idle" "talking") :test #'equal)
                                 (car (last (%moves anchor x z spot idle-from phase-start))))))
               (when walked (setf spot (third walked) x (fourth walked) z (fifth walked)))
               (setf posture (if (member :seat (%spot place spot)) "sitting" "standing") gaze "aside")))
            ((equal activity "idle")
             (let ((moves (%moves anchor x z spot phase-start (+ phase-start *idle-before-rest*))))
               (setf posture "standing" gaze "aside")
               (loop for (leave arrive to tx tz) in moves
                     do (cond ((< elapsed leave) (until leave) (return))
                              ((< elapsed arrive)
                               (let ((k (/ (- elapsed leave) (- arrive leave))))
                                 (setf facing (atan (- tx x) (- tz z))
                                       x (+ x (* k (- tx x))) z (+ z (* k (- tz z)))
                                       posture "walking" gaze "waypoint")
                                 (until arrive) (return)))
                              (t (setf facing (atan (- tx x) (- tz z)) spot to x tx z tz)))))))   ; it faces the way it came
          (destructuring-bind (face strength) (%face (obj-get anchor "valence"))
            (obj "activity" activity "detail" (obj-get anchor "detail")
                 "place" place "spot" (if (equal posture "walking") :null spot)
                 "x" (/ (round (* 1000 x)) 1000d0) "z" (/ (round (* 1000 z)) 1000d0)
                 "facing" (/ (round (* 1000 facing)) 1000d0)
                 "posture" posture "gaze" gaze "face" face "strength" strength
                 "wearing" (obj-get anchor "wearing")
                 "since" (+ since phase-start) "for" (- elapsed phase-start)
                 "next_change" (if next (/ (round (* 10 next)) 10d0) :null)
                 "anchor" anchor)))))))

;;; Keeping the anchor (bead v3-baw.3.2). One row per body, working state that
;;; was never in the ledger (routine body changes are not experience; spec §6.2),
;;; so the projection consumes nothing and is never rebuilt, like percepts. A new
;;; anchor is written only when what the body is doing changes: the same
;;; activity again (a storm of tool calls while reading) writes nothing. It
;;; starts where the body is at that moment, with how the agent feels then.

(define-projection "body"
  :kind :inline
  :derivation :scratch
  :consumes ()
  :tables ("CREATE TABLE IF NOT EXISTS body_state (body TEXT PRIMARY KEY, anchor TEXT NOT NULL, at TEXT NOT NULL)")
  :reduce (lambda (db e) (declare (ignore db e))))

(defun %stored-anchor (kernel body)
  (let ((row (sql-row (kernel-db kernel) "SELECT anchor FROM body_state WHERE body=?1 LIMIT 1" body)))
    (and row (ignore-errors (json-parse (first row))))))

(defun body-now (kernel &key (body "avatar") (at (now)))
  "BODY as it is at AT: BODY-AT of its stored anchor. With none stored, a body
idle at the centre of home since AT, so a body that was never told anything
still has a sensible state (and nothing is written for reading it)."
  (body-at (or (%stored-anchor kernel body)
               (make-body-anchor :since at :feeling (body-feeling kernel at)))
           at))

(defun note-body (kernel activity &key detail (body "avatar") wearing (at (now)))
  "What BODY is now doing: ACTIVITY (one of +activities+) with DETAIL. Writes a
new anchor only when ACTIVITY or DETAIL differs from what the body is doing
now (talking stays talking until it lapses by its own rule). The new anchor
starts at the spot the body is at, facing as it faces, at AT (when the change
happened, if it is told late). Returns T when it wrote."
  (let* ((current (body-now kernel :body body :at at))
         (detail (or detail :null)))
    (unless (and (equal activity (obj-get current "activity")) (equal detail (obj-get current "detail")))
      (let* ((spot (let ((s (obj-get current "spot"))) (if (stringp s) s (obj-get (body-anchor-of current) "spot"))))
             (anchor (make-body-anchor :activity activity :detail (and (stringp detail) detail)
                                       :place (obj-get current "place") :spot spot
                                       :facing (obj-get current "facing") :since at
                                       :feeling (body-feeling kernel at)
                                       :wearing (or wearing (obj-get (body-anchor-of current) "wearing")))))
        (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
          (sql-exec (kernel-db kernel)
                    "INSERT INTO body_state(body,anchor,at) VALUES(?1,?2,?3) ON CONFLICT(body) DO UPDATE SET anchor=excluded.anchor,at=excluded.at"
                    body (json-encode anchor) (iso8601 at)))
        t))))

(defun body-words (state)
  "A few plain words for where the body is and what it is doing."
  (let ((posture (obj-get state "posture")) (spot (obj-get state "spot"))
        (minutes (floor (or (obj-get state "for") 0) 60)))
    (if (equal posture "away")
        "away from your place"
        (format nil "~a~:[~; at the ~:*~a~]~:[~;, for ~d minute~:p~]"
                (cond ((equal posture "walking") "walking across the room")
                      ((member posture '("reading" "writing" "thinking") :test #'equal) posture)
                      (t posture))
                (and (stringp spot) (not (equal spot "centre")) spot)
                (>= minutes 2) minutes))))
