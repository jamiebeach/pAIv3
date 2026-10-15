(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.self)

(defun ev (k text) (lisp-eval k 1 text))

(deftest evaluates-and-shows-output-and-values
  (with-temp-kernel (k)
    (let ((result (ev k "(format t \"hello\") (+ 1 2)")))
      (is (search "hello" result))
      (is (search "=> 3" result)))
    (is (search "Error:" (ev k "(car 5)")) "errors come back as text")
    (is (search "Error:" (ev k "(+ 1")) "so do reader errors")))

(deftest introspection-sees-the-running-image
  (with-temp-kernel (k)
    (is (search "THINK" (ev k "(documentation 'think 'function) 'think")))
    (is (search "=> 1" (ev k "(sql-value (kernel-db *kernel*) \"SELECT 1\")"))
        "the agent can read its own state through *kernel*")))

(deftest skills-are-kept-and-restored-at-boot
  (with-temp-kernel (k)
    (ev k "(defun greet (name) (format nil \"Hello, ~a\" name))")
    (is (equal '(("GREET" "skill")) (mapcar (lambda (r) (subseq r 0 2)) (live-changes k))))
    (is (search "Hello, Sam" (ev k "(greet \"Sam\")")))
    (fmakunbound (find-symbol "GREET" '#:pai-v3.self))           ; as after a restart
    (is (equal '(1 0) (restore-self k)))
    (is (search "Hello, Sam" (ev k "(greet \"Sam\")")) "the skill is back")
    (ev k "(defun greet (name) (format nil \"Hi, ~a\" name))")
    (is (= 1 (length (live-changes k))) "a redefinition supersedes the older one")))

(defun pai-v3.mind::test-dial () 1)

(deftest repairs-last-until-restart
  (with-temp-kernel (k)
    (ev k "(defun pai-v3.mind::test-dial () 2)")
    (is (= 2 (pai-v3.mind::test-dial)) "the repair is live")
    (is (equal "repair" (second (first (live-changes k)))))
    (is (equal '(0 1) (restore-self k)))
    (is (null (live-changes k)))
    (is (= 1 (length (live-changes k :status "lost-on-restart"))) "and is then marked lost, to be proposed")))

(deftest the-kernel-and-trust-are-locked
  (with-temp-kernel (k)
    (is (search "Refused" (ev k "(defun pai-v3.kernel:append! (&rest x) x)")))
    (is (search "Refused and put back" (ev k "(setf pai-v3.kernel:*budget* nil)")))
    (is pai-v3.kernel:*budget* "the budget is put back")
    (is (search "Refused" (ev k "(defun pai-v3.trust:login (&rest x) x)")))
    (is (search "Refused" (ev k "'pai-v3.kernel::brand-new-symbol")))
    (is (search "Refused" (ev k "(sb-ext:unlock-package :pai-v3.kernel)")))
    (is (search "Refused" (ev k "(funcall *egress-guard* \"https://x\" \"x\" nil nil)"))
        "the guard that fills in secrets is out of reach")
    (is (search "Refused" (ev k "(pai-v3.trust::secret-names)")))
    (is (null (live-changes k)) "nothing refused is recorded as a change")
    (is (not (sb-ext:package-locked-p (find-package '#:pai-v3.kernel))) "locks are only held while agent code runs")
    (is (plusp (event-id (append! k "self.change.lost" (obj "digest" "x" "reason" "y")))) "the kernel still works")))

(deftest runaway-code-is-stopped
  (with-temp-kernel (k)
    (let ((pai-v3.self::*eval-seconds* 1))
      (is (search "Stopped after 1 seconds" (ev k "(loop)"))))))

(deftest agent-made-tools-are-operator-trust
  (with-temp-kernel (k)
    (ev k "(define-tool \"weather\" :description \"Say the weather.\" :run (lambda (k id a) (declare (ignore k id a)) \"sunny\"))")
    (let ((guest (list :origin "responding" :audience "person:sam"))
          (own (list :origin "private" :audience "operator")))
      (is (find "weather" (tools-for own) :key #'tool-name :test #'equal))
      (is (null (find "weather" (tools-for guest) :key #'tool-name :test #'equal)))
      (is (null (find "lisp_eval" (tools-for guest) :key #'tool-name :test #'equal))
          "a guest's thought never has lisp_eval"))
    (is (equal "skill" (second (first (live-changes k)))))))

(run-suite "self-eval")
