(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Body state (bead v3-baw.3.1): the anchor, and the pure function that
;;; derives the body from it. Property tests over many anchors and times.

(defparameter *t0* (encode-universal-time 0 0 12 15 11 2026 0))

(defun anchor (&rest args &key (since *t0*) (boredom 0.2d0) &allow-other-keys)
  (let ((args (loop for (k v) on args by #'cddr unless (member k '(:since :boredom)) append (list k v))))
    (apply #'make-body-anchor :since since :feeling (obj "boredom" boredom "arousal" 0.3d0 "valence" 0.5d0) args)))

(defun same-body-p (a b)
  "The derived fields agree (the anchor they carry is the same object or equal)."
  (every (lambda (k) (equalp (obj-get a k) (obj-get b k)))
         '("activity" "detail" "place" "spot" "x" "z" "facing" "posture" "gaze" "face" "strength" "since" "for" "next_change")))

(defun at (state minutes) (body-at state (+ *t0* (round (* 60 minutes)))))

(deftest the-same-anchor-and-moment-give-the-same-body
  (dotimes (i 40)
    (let ((a (anchor :since (+ *t0* (* i 977)) :boredom (/ i 40d0))))
      (dolist (m '(0 1 7 33 61 89 95 600))
        (let ((t1 (+ *t0* (* i 977) (* 60 m))))
          (is (same-body-p (body-at a t1) (body-at (anchor :since (+ *t0* (* i 977)) :boredom (/ i 40d0)) t1))))))))

(deftest deriving-in-two-steps-is-deriving-in-one
  (dotimes (i 30)
    (dolist (activity '("idle" "talking" "working" "resting" "away" "unfinished"))
      (let* ((a (anchor :activity activity :detail (and (equal activity "working") "reading")
                        :since (+ *t0* (* i 1013)) :boredom (/ (mod (* i 7) 30) 30d0)))
             (t1 (+ *t0* (* i 1013) (* 37 i))) (t2 (+ t1 (* 191 i) 60)))
        (is (same-body-p (body-at (body-at a t1) t2) (body-at a t2)) (list activity i))))))

(deftest nothing-moves-while-working-talking-resting-or-away
  (dolist (activity '("working" "unfinished" "resting" "away"))
    (let ((a (anchor :activity activity :detail "writing" :spot "desk")))
      (dolist (m '(0 30 300 3000))
        (let ((b (at a m)))
          (is (equal activity (obj-get b "activity")))
          (is (equal "desk" (obj-get b "spot")))
          (is (eq :null (obj-get b "next_change")) "it holds until an event changes it")))))
  (is (equal "writing" (obj-get (at (anchor :activity "working" :detail "writing") 5) "posture")))
  (is (equal "thinking" (obj-get (at (anchor :activity "unfinished" :detail "mirror-arithmetic") 5) "posture")))
  (is (equal "task" (obj-get (at (anchor :activity "unfinished") 180) "gaze")))
  (is (= (* 180 60) (obj-get (at (anchor :activity "unfinished") 180) "for")) "how long it has been at it")
  (is (equal "away" (obj-get (at (anchor :activity "away") 1) "posture"))))

(deftest talking-lapses-to-idle-and-idle-settles-to-rest
  (let ((a (anchor :activity "talking")))
    (is (equal '("talking" "standing" "visitor") (list (obj-get (at a 9) "activity") (obj-get (at a 9) "posture") (obj-get (at a 9) "gaze"))))
    (is (= 60 (obj-get (at a 9) "next_change")))
    (is (equal "idle" (obj-get (at a 11) "activity")))
    (is (= (+ *t0* 600) (obj-get (at a 11) "since")) "idle since the conversation lapsed")
    (is (equal "resting" (obj-get (at a 101) "activity"))))
  (is (equal "resting" (obj-get (at (anchor :activity "idle") 91) "activity"))))

(deftest an-idle-body-walks-between-spots-and-a-bored-one-more
  (flet ((walks (boredom)
           (loop with a = (anchor :boredom boredom)
                 for s from 0 below (* 89 60) by 20
                 for b = (body-at a (+ *t0* s))
                 count (equal "walking" (obj-get b "posture")) into walking
                 collect (obj-get b "spot") into spots
                 finally (return (values walking (remove-duplicates (remove :null spots) :test #'equal))))))
    (multiple-value-bind (calm-walking calm-spots) (walks 0d0)
      (multiple-value-bind (bored-walking bored-spots) (walks 1d0)
        (is (> bored-walking calm-walking))
        (is (> (length bored-spots) 1))
        (is (every (lambda (s) (member s '("centre" "window" "desk" "chair" "shelf") :test #'equal))
                   (append calm-spots bored-spots)) "only spots of its place")))))

(deftest a-walk-is-continuous
  (let ((a (anchor :boredom 1d0)) (prev nil))
    (loop for s from 0 below (* 89 60) by 1
          for b = (body-at a (+ *t0* s))
          do (when prev
               (is (<= (sqrt (+ (expt (- (obj-get b "x") (obj-get prev "x")) 2) (expt (- (obj-get b "z") (obj-get prev "z")) 2)))
                       (+ pai-v3.mind::*walk-speed* 0.01))
                   "no jump faster than walking"))
             (setf prev b))))

(deftest at-rest-it-sits-where-a-seat-is
  ;; Some anchor ends its idle at the chair; resting there, it sits.
  (let ((sat (loop for i below 200
                   for b = (body-at (anchor :since (+ *t0* i) :boredom 1d0) (+ *t0* i (* 100 60)))
                   thereis (and (equal "chair" (obj-get b "spot")) (equal "sitting" (obj-get b "posture"))))))
    (is sat))
  (is (equal "standing" (obj-get (at (anchor :activity "resting" :spot "window") 10) "posture"))))

(deftest a-long-absence-costs-one-short-read
  (let ((a (anchor :boredom 1d0)) (start (get-internal-real-time)))
    (dotimes (i 200) (body-at a (+ *t0* (* 86400 365) i)))
    (is (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second)) "200 reads a year on")
    (is (equal "resting" (obj-get (body-at a (+ *t0* (* 86400 365))) "activity")))))

(deftest a-time-before-the-anchor-is-the-anchor
  (let ((a (anchor :activity "working" :detail "reading" :spot "desk")))
    (is (= 0 (obj-get (body-at a (- *t0* 500)) "for")))
    (is (equal "desk" (obj-get (body-at a (- *t0* 500)) "spot")))))

(deftest feeling-shows-in-the-resting-face
  (is (equal "relaxed" (obj-get (body-at (make-body-anchor :since *t0* :feeling (obj "valence" 0.8d0)) *t0*) "face")))
  (is (equal "sad" (obj-get (body-at (make-body-anchor :since *t0* :feeling (obj "valence" -0.8d0)) *t0*) "face")))
  (is (equal "neutral" (obj-get (body-at (make-body-anchor :since *t0* :feeling (obj "valence" 0d0)) *t0*) "face"))))

(deftest an-anchor-names-only-what-exists
  (signals error (make-body-anchor :activity "dancing"))
  (signals error (make-body-anchor :spot "kitchen")))

(defun stored (k) (sql-value (kernel-db k) "SELECT count(*) FROM body_state LIMIT 1"))

(deftest a-body-never-told-anything-is-idle-at-home-and-nothing-is-written
  (with-clock
    (with-temp-kernel (k)
      (let ((b (body-now k)))
        (is (equal "idle" (obj-get b "activity")))
        (is (equal "home" (obj-get b "place"))))
      (is (zerop (stored k)) "reading writes nothing"))))

(deftest what-she-does-moves-her-body-and-repeats-write-nothing
  (with-clock
    (with-temp-kernel (k)
      (is (note-body k "working" :detail "reading"))
      (is (not (note-body k "working" :detail "reading")) "the same again writes nothing")
      (advance 600)
      (let ((b (body-now k)))
        (is (equal "reading" (obj-get b "posture")))
        (is (= 600 (obj-get b "for"))))
      (is (search "reading" (body-words (body-now k))))
      (is (search "for 10 minutes" (body-words (body-now k))))
      (is (note-body k "talking"))
      (is (equal "visitor" (obj-get (body-now k) "gaze")))
      (advance 700)
      (is (equal "idle" (obj-get (body-now k) "activity")) "a conversation lapses by its own rule")
      (is (= 1 (stored k)) "one row per body"))))

(deftest a-new-anchor-starts-where-the-body-is
  (with-clock
    (with-temp-kernel (k)
      (note-body k "working" :detail "reading")     ; idle on an idle body writes nothing, so stop something first
      (note-body k "idle")
      ;; a bored idle body walks; find a moment it stands at a spot that is not its start
      (let ((moved (loop for s from 60 to 5400 by 30
                         for b = (body-at (pai-v3.mind::%stored-anchor k "avatar") (+ (now) s))
                         when (and (stringp (obj-get b "spot")) (not (equal "centre" (obj-get b "spot"))))
                           return (cons s (obj-get b "spot")))))
        (when moved
          (advance (car moved))
          (note-body k "working" :detail "writing")
          (is (equal (cdr moved) (obj-get (body-now k) "spot")) "she starts writing where she was standing"))))))

(run-suite "mind-body")
