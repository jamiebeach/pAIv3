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

(run-suite "mind-affect")
