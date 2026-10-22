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

(defun %shell-path (root)
  "Software I installed comes first: my tools environment, then ~/.local/bin."
  (format nil "~a:~a:/usr/local/bin:/usr/bin:/bin"
          (namestring (merge-pathnames ".local/tools/bin/" root))
          (namestring (merge-pathnames ".local/bin/" root))))

(defun run-shell (kernel command &key seconds)
  "Run COMMAND with bash in the workspace. Returns the exit code and output as text."
  (let ((root (%shell-root kernel))
        (seconds (or seconds (setting kernel "shell.timeout_seconds")))
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
                            (format nil "PATH=~a" (%shell-path root)) "LANG=C.UTF-8"
                            (format nil "MAMBA_ROOT_PREFIX=~a" (namestring (merge-pathnames ".local/mamba/" root)))
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

;;; Installing software (operator request 2026-10-22): the container has no
;;; root and almost nothing installed, so an agent installs into her own
;;; persistent home with micromamba from conda-forge: Python, Node, git,
;;; ffmpeg and thousands more, with pip and npm working inside. Nothing
;;; system-wide changes; every install is a recorded tool step.

(defparameter +micromamba-url+
  "https://github.com/mamba-org/micromamba-releases/releases/latest/download/micromamba-linux-64")

(defun %package-name-p (name)
  (and (stringp name) (< 0 (length name) 80)
       (every (lambda (c) (or (alphanumericp c) (find c "-_.=<>*"))) name)))

(defun install-software (kernel action packages)
  "ACTION install, remove or list conda-forge PACKAGES in my tools environment."
  (cond ((not (member action '("install" "remove" "list") :test #'equal))
         "action is install, remove or list.")
        ((and (not (equal action "list"))
              (or (null packages) (> (length packages) 10) (notevery #'%package-name-p packages)))
         "Name 1 to 10 conda-forge packages, such as python, nodejs, git or ffmpeg (a version like python=3.12 is fine).")
        (t
         (let ((mamba "$HOME/.local/bin/micromamba") (env "$HOME/.local/tools"))
           (run-shell kernel
                      (format nil "set -e; mkdir -p $HOME/.local/bin; [ -x ~a ] || { curl -fsSL -o ~a.part ~a && chmod +x ~a.part && mv ~a.part ~a; }; ~a"
                              mamba mamba +micromamba-url+ mamba mamba mamba
                              (cond ((equal action "list") (format nil "[ -d ~a ] && ~a list -p ~a || echo 'Nothing installed yet.'" env mamba env))
                                    ((equal action "remove") (format nil "~a remove -y -p ~a ~{'~a'~^ ~}" mamba env packages))
                                    (t (format nil "if [ -d ~a ]; then ~a install -y -q -p ~a -c conda-forge ~{'~a'~^ ~}; else ~a create -y -q -p ~a -c conda-forge ~{'~a'~^ ~}; fi && echo Installed: ~{~a~^ ~}"
                                               env mamba env packages mamba env packages packages))))
                      :seconds 540)))))

(define-tool "install_software"
  :description "Install software for myself, without root: conda-forge packages (python, nodejs, git, ffmpeg, imagemagick, pandoc, and thousands more) go into my own tools environment, which my shell finds first. pip install and npm install -g then work inside it. action: install, remove or list; packages: names, optionally with versions. Installing can take a few minutes."
  :summary "Install conda-forge packages for my own shell."
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "install" "remove" "list"))
                                     "packages" (obj "type" "array" "items" (obj "type" "string")))
                   "required" (list "action"))
  :available :operator-trust
  :core '("introspection" "work" "docket")
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((packages (obj-get args "packages")))
           (install-software kernel (obj-get args "action" "list") (if (listp packages) packages nil)))))
