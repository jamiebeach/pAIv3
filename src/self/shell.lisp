(in-package #:pai-v3.self)

;;; The shell (bead v3-clp.23): v2's most used tool, ported. Commands run in
;;; the agent's own container only, in a workspace folder, with an empty
;;; environment (no keys or tokens are inherited), under GNU timeout, which
;;; owns the command's process group and kills what outlives it (v2's
;;; lesson: a descendant holding a pipe open can swallow a Lisp timeout).
;;; Only operator-trust thoughts have it; each call is recorded as a
;;; thought step like any tool.

(define-setting "shell.workspace"
  :type :string :default "" :group "Self"
  :description "The folder shell commands run in, such as /home/pai/workspace/. Empty: no shell tool.")
(define-setting "shell.timeout_seconds"
  :type :integer :min 1 :max 600 :default 60 :group "Self"
  :description "How long one shell command may run before it is stopped.")
(define-setting "shell.output_chars"
  :type :integer :min 500 :max 100000 :default 12000 :group "Self"
  :description "Most characters of a shell command's output the agent sees; the middle is cut.")

(defparameter *shell-container-marker* "/.dockerenv"
  "Present only inside a container. The shell never runs on a host.")

(defun %shell-root (kernel)
  (let ((dir (setting kernel "shell.workspace")))
    (and (stringp dir) (plusp (length dir)) (uiop:ensure-directory-pathname dir))))

(defun %clip-middle (text limit)
  "TEXT, or its start and end with the middle cut when longer than LIMIT."
  (if (<= (length text) limit)
      text
      (let ((half (floor limit 2)))
        (format nil "~a~%[... ~d characters cut ...]~%~a" (subseq text 0 half)
                (- (length text) (* 2 half)) (subseq text (- (length text) half))))))

(defun run-shell (kernel command)
  "Run COMMAND with bash in the workspace. Returns the exit code and output as text."
  (let ((root (%shell-root kernel))
        (seconds (setting kernel "shell.timeout_seconds"))
        (limit (setting kernel "shell.output_chars")))
    (cond ((null root) "There is no shell here: shell.workspace is not set.")
          ((not (probe-file *shell-container-marker*))
           "Refused: the shell runs only inside my own container.")
          ((zerop (length (string-trim '(#\Space #\Tab #\Newline) command))) "Give a command.")
          (t
           (ensure-directories-exist root)
           (handler-case
               (sb-ext:with-timeout (+ seconds 5)
                 (multiple-value-bind (out err code)
                     (uiop:run-program
                      (list "timeout" "--signal=TERM" "--kill-after=2s" (format nil "~ds" seconds)
                            "env" "-i" (format nil "HOME=~a" (namestring root))
                            "PATH=/usr/local/bin:/usr/bin:/bin" "LANG=C.UTF-8"
                            "bash" "--noprofile" "--norc" "-c" command)
                      :directory root :input nil :output :string :error-output :string
                      :ignore-error-status t)
                   (if (member code '(124 137))
                       (format nil "Stopped after ~d seconds.~@[~%~a~]" seconds
                               (let ((o (concatenate 'string out err)))
                                 (and (plusp (length o)) (%clip-middle o limit))))
                       (%clip-middle
                        (format nil "exit ~d~@[~%~a~]~@[~%[stderr]~%~a~]" code
                                (and (plusp (length out)) out) (and (plusp (length err)) err))
                        limit))))
             (sb-ext:timeout () (format nil "Stopped after ~d seconds." seconds))
             (error (e) (format nil "The shell failed: ~a" e)))))))

(define-tool "shell"
  :description "Run a bash command in my own container, in my workspace folder, and see its exit code and output.
Nothing from the host is mounted and no keys are in its environment. Each command starts fresh: cd and variables do
not carry over, so chain with && in one command. Long output is cut in the middle."
  :summary "Run a bash command in my own container."
  :parameters (obj "type" "object"
                   "properties" (obj "command" (obj "type" "string" "description" "The bash command."))
                   "required" (list "command"))
  :available :operator-trust
  :core '("introspection" "work" "docket")
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((command (obj-get args "command")))
           (run-shell kernel (if (stringp command) command "")))))
