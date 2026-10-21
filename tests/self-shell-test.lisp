(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.self)

;;; The shell (bead v3-clp.23): off until a workspace is set, operator-trust
;;; only, runs in the workspace with an empty environment, and is stopped
;;; and cut when it runs long or says too much.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun shell-tool (k command)
  (funcall (pai-v3.mind::tool-run (gethash "shell" pai-v3.mind::*tools*)) k 1 (obj "command" command)))

(defmacro with-workspace ((k root) &body body)
  `(with-temp-kernel (,k)
     (let ((,root (format nil "/tmp/ws-~d/" (random 1000000000))))
       (unwind-protect
            (progn (change-setting ,k "shell.workspace" ,root :origin *operator*)
                   ,@body)
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,root) :validate t :if-does-not-exist :ignore)))))

(deftest the-shell-is-off-until-a-workspace-is-set
  (with-temp-kernel (k)
    (is (search "not set" (shell-tool k "echo hi")))))

(deftest only-operator-trust-thoughts-have-it
  (let ((tool (gethash "shell" pai-v3.mind::*tools*)))
    (is (pai-v3.mind::tool-available-p tool '(:audience "operator")))
    (is (not (pai-v3.mind::tool-available-p tool '(:audience "coding-agent" :communicating t))))
    (is (not (pai-v3.mind::tool-available-p tool '(:audience "guest" :communicating t))))))

(deftest it-runs-in-the-workspace-and-reports-the-exit-code
  (with-workspace (k root)
    (let ((said (shell-tool k "pwd && echo oops >&2 && exit 3")))
      (is (search "exit 3" said))
      (is (search (string-right-trim "/" root) said) "runs in the workspace")
      (is (search "[stderr]" said)))
    (is (search "exit 0" (shell-tool k "echo kept > note.txt")))
    (is (search "kept" (shell-tool k "cat note.txt")) "files stay in the workspace")))

(deftest no-environment-is-inherited
  (with-workspace (k root)
    (sb-posix:setenv "PAI_TEST_SECRET" "hunter2" 1)
    (is (not (search "hunter2" (shell-tool k "env"))) "keys in the agent's environment do not reach commands")))

(deftest a-long-command-is-stopped-and-long-output-cut
  (with-workspace (k root)
    (change-setting k "shell.timeout_seconds" 1 :origin *operator*)
    (is (search "Stopped after 1 seconds" (shell-tool k "sleep 30")))
    (change-setting k "shell.timeout_seconds" 60 :origin *operator*)
    (change-setting k "shell.output_chars" 500 :origin *operator*)
    (let ((said (shell-tool k "seq 1 5000")))
      (is (search "characters cut" said))
      (is (search "5000" said) "the end is kept")
      (is (< (length said) 600)))))

(run-suite "self-shell")
