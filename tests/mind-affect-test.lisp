(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Affect and drives (bead v3-clp.8): v2's dynamics, derived from events.

(defparameter *t0* (encode-universal-time 0 0 12 1 6 2026 0))

(defmacro at-time ((seconds) &body body)
  `(let ((*clock* (lambda () (+ *t0* ,seconds)))) ,@body))

(defun mood (k name &optional (at (now)))
  (gethash name (affect-now k at)))
(defun want (k name &optional (at (now)))
  (gethash name (nth-value 1 (affect-now k at))))

(defun near (a b) (< (abs (- a b)) 1d-3))

(deftest a-fresh-agent-is-at-baseline
  (with-temp-kernel (k)
    (at-time (0)
      (is (near 0.3 (mood k "arousal")))
      (is (near 0.5 (mood k "valence")))
      (is (near 0.2 (want k "connection")))
      (is (search "Affect/process state (not an external fact): arousal=0.30" (render-affect k))))))

(deftest moods-drift-back-to-baseline
  (with-temp-kernel (k)
    (at-time (0) (append! k "affect.moved" (obj "changes" (obj "arousal" 0.5) "cause" "test")))
    (at-time (0) (is (near 0.8 (mood k "arousal"))))
    ;; v2: 5% of the distance per minute; after an hour about 95% of the way back.
    (at-time (3600)
      (is (near (+ 0.3 (* 0.5 (expt 0.95 60))) (mood k "arousal")))
      (is (< (mood k "arousal") 0.33)))
    (at-time (0) (append! k "affect.moved" (obj "changes" (obj "valence" -5) "cause" "test")))
    (at-time (0) (is (near -1 (mood k "valence")) "clamped to the range"))))

(deftest drives-build-fade-and-are-met
  (with-temp-kernel (k)
    (at-time (7200) (is (near 0.2 (want k "connection")) "v2's starting value"))
    (at-time (7200)
      (receive-turn k :channel "app" :audience "operator" :speaker "operator" :text "hello"
                      :origin (obj "principal_type" "operator")))
    (at-time (7200) (is (near 0.06 (want k "connection")) "an operator turn satisfies connection"))
    ;; Rising: connection at baseline 0.6 gains 0.6 over four hours unmet.
    (at-time ((+ 7200 3600)) (is (near 0.21 (want k "connection"))))
    ;; Event: curiosity jumps when stirred, then fades 1.5% every five minutes.
    (at-time (0) (append! k "drive.triggered" (obj "drive" "curiosity" "amount" 0.4 "target" "tides")))
    (at-time (3000) (is (near (* 0.4 (expt 0.985 10)) (want k "curiosity"))))
    (at-time (3000) (is (search "curiosity=0.34 (about tides)" (render-affect k))))))

(deftest appreciation-and-silence
  (with-temp-kernel (k)
    (at-time (3600)
      (receive-turn k :channel "app" :audience "operator" :speaker "operator" :text "Thanks, that was so helpful"
                      :origin (obj "principal_type" "operator")))
    (at-time (3600) (is (near 0.03 (want k "appreciation"))))
    (at-time (3600) (is (near 0.3 (mood k "social_need"))))
    (at-time ((+ 3600 (* 4 3600))) (is (near (- 1 (exp -1)) (mood k "social_need"))
                                     "social need rises with the operator's silence"))))

(deftest tool-outcomes-move-competence
  (with-temp-kernel (k)
    (at-time (0)
      (append! k "thought.step" (obj "thought_id" 1 "kind" "tool_result" "part" 0 "text" "x" "failed" t))
      (append! k "thought.step" (obj "thought_id" 1 "kind" "tool_result" "part" 0 "text" "x" "failed" t))
      (is (near 0.54 (mood k "competence")))
      (append! k "thought.step" (obj "thought_id" 1 "kind" "tool_result" "part" 0 "text" "ok"))
      (is (near 0.55 (mood k "competence"))))))

(deftest dials-are-settings-and-rebuilds-agree
  (with-temp-kernel (k)
    (at-time (0)
      (change-setting k "affect.modulators" (obj "arousal" (obj "baseline" 0.6))
                      :origin (obj "principal_type" "operator")))
    (at-time (0) (is (near 0.6 (mood k "arousal")) "the baseline is a dial"))
    (signals setting-refused (change-setting k "affect.drives" (obj "hunger" (obj "baseline" 1))
                                             :origin (obj "principal_type" "operator")))
    (at-time (60) (append! k "affect.set" (obj "modulators" (obj "certainty" 0.9) "drives" (obj "closure" 0.5)
                                               "cause" "imported from v2")))
    (at-time (60) (is (near 0.9 (mood k "certainty"))) (is (near 0.5 (want k "closure"))))))

(deftest chat-routes-by-arousal
  (with-temp-kernel (k)
    (let ((*thought-provider* nil))
      (change-setting k "llm.profiles"
                      (list (obj "name" "main" "base_url" "https://a.example/v1" "model" "big")
                            (obj "name" "calm" "base_url" "https://b.example/v1" "model" "steady"))
                      :origin (obj "principal_type" "operator"))
      (change-setting k "llm.routes"
                      (obj "chat" (list (obj "profile" "calm" "when" (obj "arousal" (obj ">" 0.7)))
                                        (obj "profile" "main")))
                      :origin (obj "principal_type" "operator"))
      (let ((signals (funcall *route-signals* k nil)))
        (is (equal "big" (pai-v3.mind::provider-profile-model (profile-for k "chat" signals)))))
      (append! k "affect.moved" (obj "changes" (obj "arousal" 0.3) "cause" "startled"))
      (append! k "affect.moved" (obj "changes" (obj "arousal" 0.3) "cause" "startled"))
      (let ((signals (funcall *route-signals* k nil)))
        (is (equal "steady" (pai-v3.mind::provider-profile-model (profile-for k "chat" signals))))))))

(deftest the-feel-tool
  (with-temp-kernel (k)
    (let ((run (pai-v3.mind::tool-run (gethash "feel" pai-v3.mind::*tools*))))
      (is (search "curiosity stirred" (funcall run k 1 (obj "trigger" "curiosity" "about" "moths" "cause" "a question"))))
      (is (search "Nothing changed" (funcall run k 1 (obj "changes" (obj "hunger" 1) "cause" "x"))))
      (funcall run k 1 (obj "changes" (obj "valence" 2) "cause" "big news"))
      (is (< (mood k "valence") 0.81) "one nudge is bounded"))))

(defparameter *affect-operator* (obj "principal_type" "operator"))

(deftest agent-dynamics-are-independent-and-legacy-dials-still-work
  (let ((fast (obj "connection" (obj "resting_level" 0.35 "rise_per_hour" 0.25)
                   "curiosity" (obj "retention_per_five_minutes" 0.99)))
        (slow (obj "connection" (obj "resting_level" 0.1 "rise_per_hour" 0.05)
                   "curiosity" (obj "retention_per_five_minutes" 0.9))))
    (is (near 0.45 (drive-drift 0.2 3600 "connection" fast)))
    (is (near 0.25 (drive-drift 0.2 3600 "connection" slow)))
    (is (> (drive-drift 0.8 3600 "curiosity" fast) (drive-drift 0.8 3600 "curiosity" slow)))
    (is (near 0.4 (drive-drift 0.2 3600 "connection" (obj "connection" (obj "baseline" 0.8)))))
    (is (near (* 0.8 (expt 0.985 12)) (drive-drift 0.8 3600 "curiosity" nil))))
  (with-temp-kernel (k)
    (at-time (0)
      (change-setting k "affect.drives" (obj "connection" (obj "resting_level" 0.35 "rise_per_hour" 0.25))
                      :origin *affect-operator*)
      (is (near 0.35 (want k "connection")))
      (receive-turn k :channel "app" :audience "operator" :speaker "Operator" :text "Hello"
                      :origin *affect-operator*)
      (is (near 0.105 (want k "connection"))))
    (at-time (3600) (is (near 0.355 (want k "connection"))))))

(deftest persistence-resting-level-and-growth-compose-across-time
  (let* ((d (obj "curiosity" (obj "resting_level" 0.2 "retention_per_five_minutes" 0.5 "rise_per_hour" 0.12)))
         (half (drive-drift 0.8 300 "curiosity" d)))
    (is (near (drive-drift 0.8 600 "curiosity" d) (drive-drift half 300 "curiosity" d)))
    (is (near 0.5 (drive-drift 0.8 300 "curiosity" (obj "curiosity" (obj "resting_level" 0.2 "retention_per_five_minutes" 0.5)))))
    (is (near 1 (drive-drift 0.9 100000 "connection" nil)))
    (is (near 0.2 (drive-drift 0.9 300 "curiosity" (obj "curiosity" (obj "resting_level" 0.2 "retention_per_five_minutes" 0)))))
    (is (near 0.8 (drive-drift 0.8 0 "curiosity" d)))))

(deftest per-agent-trigger-and-satisfaction-apply-to-feel-and-operator-contact
  (with-temp-kernel (k)
    (at-time (0)
      (change-setting k "affect.drives"
                      (obj "curiosity" (obj "trigger_amount" 0.7 "satisfaction_drop" 0.25)
                           "connection" (obj "satisfaction_drop" 0.5)
                           "appreciation" (obj "satisfaction_drop" 1)) :origin *affect-operator*)
      (let ((run (pai-v3.mind::tool-run (gethash "feel" *tools*))))
        (funcall run k 1 (obj "trigger" "curiosity" "cause" "Synthetic interest"))
        (is (near 0.7 (want k "curiosity")))
        (funcall run k 1 (obj "satisfied" "curiosity" "cause" "Synthetic answer"))
        (is (near 0.525 (want k "curiosity"))))
      (append! k "affect.set" (obj "drives" (obj "connection" 0.8 "appreciation" 0.8) "cause" "Fixture"))
      (receive-turn k :channel "app" :audience "operator" :speaker "Operator" :text "Thank you"
                      :origin *affect-operator*)
      (is (near 0.4 (want k "connection"))) (is (near 0 (want k "appreciation"))))))

(deftest social-silence-and-modulator-recovery-have-personal-dials
  (with-temp-kernel (k)
    (at-time (0)
      (change-setting k "affect.modulators"
                      (obj "social_need" (obj "silence_seconds" 3600)
                           "arousal" (obj "baseline" 0.4 "decay_rate" 0.1)) :origin *affect-operator*)
      (receive-turn k :channel "app" :audience "operator" :speaker "Operator" :text "Hello" :origin *affect-operator*)
      (append! k "affect.moved" (obj "changes" (obj "arousal" 0.3) "cause" "Fixture")))
    (at-time (60) (is (near 0.67 (mood k "arousal"))))
    (at-time (3600) (is (near (- 1 (exp -1)) (mood k "social_need"))))))

(deftest settings-change-materializes-old-dynamics-before-new-rate
  (with-temp-kernel (k)
    (at-time (0) (append! k "affect.set" (obj "drives" (obj "connection" 0.2) "cause" "Fixture")))
    (at-time (3600)
      (change-setting k "affect.drives" (obj "connection" (obj "rise_per_hour" 0.3)) :origin *affect-operator*)
      (is (near 0.35 (want k "connection"))))
    (at-time (7200) (is (near 0.65 (want k "connection"))))))

(deftest invalid-dials-are-refused-before-they-can-break-cognition
  (with-temp-kernel (k)
    (dolist (d (list (obj "connection" (obj "rise_per_hour" -0.1))
                     (obj "curiosity" (obj "trigger_amount" 2))
                     (obj "curiosity" (obj "retention_per_five_minutes" "fast"))
                     (obj "connection" (obj "mode" "urgent"))
                     (obj "closure" (obj "resting_level" 2))
                     (obj "closure" (obj "satisfaction_drop" -1))
                     (obj "closure" (obj "rise_per_huor" 0.2))))
      (signals setting-refused (change-setting k "affect.drives" d :origin *affect-operator*)))
    (dolist (d (list (obj "social_need" (obj "silence_seconds" 0))
                     (obj "arousal" (obj "baseline" 0.8 "max" 0.5))
                     (obj "arousal" (obj "decay_rate" 2))
                     (obj "arousal" (obj "silence_seconds" 3600))))
      (signals setting-refused (change-setting k "affect.modulators" d :origin *affect-operator*)))
    (signals setting-refused (change-setting k "affect.drives" (obj "connection" (obj "rise_per_hour" 0.3))
                                            :origin (obj "principal_type" "self")))))

(deftest semantic-state-is-accurate-budgeted-and-private
  (with-temp-kernel (k)
    (at-time (0)
      (append! k "affect.set" (obj "modulators" (obj "boredom" 0.95 "valence" -0.8)
                                 "drives" (obj "appreciation" 0.9 "curiosity" 0.8) "cause" "Fixture"))
      (append! k "drive.triggered" (obj "drive" "curiosity" "amount" 0 "target" "SYNTHETIC_PRIVATE_INTEREST"))
      (let ((text (render-affect k)))
        (is (search "very high boredom; settling" text))
        (is (search "very negative; rising" text))
        (is (search "appreciation=0.90 (very high unmet desire; rising)" text))
        (is (not (search "feeling appreciated" text)))
        (is (search "SYNTHETIC_PRIVATE_INTEREST" text)))
      (let* ((peer (request-thought k "work" "A public topic" :audience "peer:beta"))
             (run (pai-v3.mind::tool-run (gethash "feel" *tools*)))
             (reply (funcall run k peer (obj "changes" (obj "arousal" 0.01) "cause" "Fixture"))))
        (is (search "SYNTHETIC_PRIVATE_INTEREST" reply)))
      (dolist (name '("connection" "curiosity" "closure" "appreciation"))
        (append! k "drive.triggered" (obj "drive" name "amount" 0 "target" (make-string 2000 :initial-element #\x))))
      (multiple-value-bind (rows used status)
          (pai-v3.mind::%fill k :state '(:audience "operator") 300)
        (is (equal "ok" status)) (is (<= used 300)) (is (= 1 (length rows)))
        (let ((text (first rows)))
          (dolist (name '("arousal=" "valence=" "certainty=" "competence=" "boredom=" "social_need="
                          "connection=" "curiosity=" "closure=" "appreciation="))
            (is (search name text))))))))

(deftest legacy-drive-math-retains-exact-floating-point-results
  (dolist (name '("connection" "curiosity" "closure" "appreciation"))
    (dolist (seconds '(1 300 1234 86400))
      (let* ((dials (obj name (obj "baseline" 0.63 "trigger_amount" 0.5)))
             (old (if (member name '("connection" "appreciation") :test #'equal)
                      (min 1d0 (+ 0.123d0 (* 0.63 (/ seconds pai-v3.mind::*drive-rise-seconds*))))
                      (* 0.123d0 (expt pai-v3.mind::*drive-event-fade* (/ seconds 300d0))))))
        (is (= old (drive-drift 0.123d0 seconds name dials)))))))

(run-suite "mind-affect")
