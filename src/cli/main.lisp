(defpackage #:pai-v3.cli
  (:use #:cl #:pai-v3.kernel #:pai-v3.trust #:pai-v3.host)
  (:export #:main #:run-command))
(in-package #:pai-v3.cli)

;;; The pai command (spec §17; NFR PORT-1, ENC-8). One executable per OS;
;;; no container needed.
;;;
;;;   pai init <dir> <agent-id> [--passphrase] [--keyfile PATH] [--no-keychain]
;;;   pai run <dir>
;;;   pai backup-init <dir> <backup-dir> [--passphrase]
;;;   pai backup <dir>
;;;   pai restore <backup-dir> <stamp> <target-dir>
;;;   pai backups <backup-dir>
;;;   pai secret-set <dir> <name> <host>[,<host>...]     (value read from stdin)
;;;   pai unlock-methods <dir>
;;;   pai settings <dir>                       list every setting
;;;   pai settings-export <dir>                print them as JSON
;;;   pai settings-import <dir> <file> [--apply]  show the changes; apply with --apply
;;;
;;; The command does lifecycle only. Configuration is settings in the store
;;; (changed from the phone, by file import, or by the agent within bounds),
;;; never flags. Secrets, passphrases and recovery codes are read from
;;; standard input, never from arguments or the environment.

(defparameter +console-origin+
  (obj "principal_type" "operator" "principal_id" "operator" "auth_method" "host-console")
  "Someone at the host with the agent's keys is the operator.")

(defun %say (fmt &rest args) (apply #'format t fmt args) (terpri) (finish-output))

(defun %ask (prompt)
  (format *error-output* "~a: " prompt) (finish-output *error-output*)
  (let ((line (read-line *standard-input* nil "")))
    (string-trim '(#\Return #\Newline #\Space) line)))

(defun %flag (args name) (member name args :test #'string=))
(defun %option (args name) (second (member name args :test #'string=)))
(defun %positional (args)
  (loop with skip = nil
        for a in args
        if skip do (setf skip nil)
        else if (member a '("--keyfile") :test #'string=) do (setf skip t)
        else unless (and (> (length a) 2) (string= "--" a :end2 2)) collect a))

(defun %methods (args)
  (append (unless (%flag args "--no-keychain") '("keychain"))
          (when (%flag args "--passphrase") '("passphrase"))
          (when (%option args "--keyfile") '("keyfile"))))

(defun %show-recovery (what code)
  (%say "")
  (%say "~a recovery code (shown once; keep it offline, away from this machine):" what)
  (%say "")
  (%say "    ~a" code)
  (%say ""))

(defun cmd-init (args)
  (destructuring-bind (dir agent-id &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let ((code (init-agent dir agent-id :methods (%methods args)
                                         :passphrase (and (%flag args "--passphrase") (%ask "Passphrase"))
                                         :keyfile (%option args "--keyfile"))))
      (%say "Created agent ~a in ~a." agent-id dir)
      (%show-recovery "Storage" code))))

(defun %open (dir)
  "Open DIR, asking for a passphrase or recovery code only if no unattended
method works."
  (handler-case (open-agent dir)
    (keyring-error ()
      (let ((answer (%ask "Passphrase or recovery code")))
        (if (find #\- answer)
            (open-agent dir :recovery-code answer)
            (open-agent dir :passphrase answer))))))

(defun cmd-run (args)
  (destructuring-bind (dir &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let* ((config (agent-config dir))
           (handle (%open dir)))
      (let ((agent (pai-v3.api:start-agent
                    (agent-handle-kernel handle)
                    :port (obj-get config "port" 8080)
                    :address (coerce (obj-get config "address" "127.0.0.1") 'simple-string)
                    ;; Passkeys need a name, not an address: a local agent is localhost.
                    :rp-id (obj-get config "rp_id" "localhost")
                    :origin (obj-get config "origin" (format nil "http://localhost:~a" (obj-get config "port" 8080)))
                    ;; Other names it answers at, such as a Tailscale address for a phone.
                    :sites (loop for site in (obj-get config "sites")
                                 collect (cons (obj-get site "rp_id") (obj-get site "origin")))
                    :on-idle (lambda () (maybe-backup handle)))))
        (%say "~a is running on ~a:~a." (obj-get config "agent_id")
              (obj-get config "address" "127.0.0.1") (pai-v3.api:agent-port agent))
        (let ((warning (not-ready-warning (agent-handle-kernel handle))))
          (when warning (%say "WARNING: ~a" warning)))
        (handler-case (loop (sleep 3600))
          (sb-sys:interactive-interrupt ()
            (pai-v3.api:stop-agent agent)
            (close-agent handle)
            (%say "Stopped.")))))))

(defun cmd-backup-init (args)
  (destructuring-bind (dir backup-dir &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let* ((config (agent-config dir))
           (code (init-backups backup-dir (obj-get config "agent_id")
                               :methods (%methods args)
                               :passphrase (and (%flag args "--passphrase") (%ask "Backup passphrase"))))
           (handle (%open dir)))
      (unwind-protect
           (change-setting (agent-handle-kernel handle) "backup.dir" (namestring backup-dir)
                           :origin +console-origin+ :reason "backups set up from the host")
        (close-agent handle))
      (%say "Backups for ~a will go to ~a, daily." (obj-get config "agent_id") backup-dir)
      (%show-recovery "Backup" code))))

(defun cmd-backup (args)
  (destructuring-bind (dir &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let* ((handle (%open dir)) (backup-dir (setting (agent-handle-kernel handle) "backup.dir")))
      (unwind-protect (%say "Backed up as ~a." (backup-agent handle backup-dir))
        (close-agent handle)))))

(defun cmd-restore (args)
  (destructuring-bind (backup-dir stamp target &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let ((code (handler-case (restore-agent backup-dir stamp target :methods (%methods args))
                  (keyring-error ()
                    (restore-agent backup-dir stamp target :methods (%methods args)
                                   :backup-recovery-code (%ask "Backup recovery code"))))))
      (%say "Restored ~a into ~a." stamp target)
      (%show-recovery "New storage" code))))

(defun cmd-backups (args)
  (dolist (stamp (list-backups (first (%positional args)))) (%say "~a" stamp)))

(defun cmd-secret-set (args)
  (destructuring-bind (dir name hosts &rest ignore) (%positional args)
    (declare (ignore ignore))
    (let ((handle (%open dir)))
      (unwind-protect
           (let ((hosts (loop for h in (uiop:split-string hosts :separator ",")
                              when (plusp (length h)) collect h)))
             (set-secret name (%ask (format nil "Value for ~a" name)) hosts)
             (append! (agent-handle-kernel handle) "secret.set"
                      (obj "name" name "hosts" hosts "origin" +console-origin+))
             (%say "Stored ~a for ~{~a~^, ~}." name hosts))
        (close-agent handle)))))

(defun cmd-unlock-methods (args)
  (loop for (id method) in (unlock-methods (first (%positional args)))
        do (%say "~a  ~a" method id)))

(defun %with-agent (dir fn)
  (let ((handle (%open dir)))
    (unwind-protect (funcall fn (agent-handle-kernel handle)) (close-agent handle))))

(defun cmd-settings (args)
  (%with-agent (first (%positional args))
               (lambda (k)
                 (dolist (s (settings-list k))
                   (%say "~a~:[~; (changed)~]  ~a  = ~a" (obj-get s "name") (eq t (obj-get s "changed"))
                         (obj-get s "group") (json-encode (obj-get s "value")))))))

(defun cmd-settings-export (args)
  (%with-agent (first (%positional args)) (lambda (k) (%say "~a" (json-encode (export-settings k))))))

(defun cmd-settings-import (args)
  (destructuring-bind (dir file &rest ignore) (%positional args)
    (declare (ignore ignore))
    (%with-agent dir
                 (lambda (k)
                   (let* ((values (json-parse (uiop:read-file-string file)))
                          (changes (import-settings k values :origin +console-origin+
                                                             :dry-run (not (%flag args "--apply")))))
                     (if (null changes)
                         (%say "Nothing to change.")
                         (dolist (c changes)
                           (%say "~a: ~a -> ~a" (first c) (json-encode (second c)) (json-encode (third c)))))
                     (when (and changes (not (%flag args "--apply")))
                       (%say "Not applied. Run again with --apply to apply these changes.")))))))

(defun cmd-persona-set (args)
  "Set the persona from a JSON file with persona_id, identity, voice and
optionally instructions and tone."
  (destructuring-bind (dir file &rest ignore) (%positional args)
    (declare (ignore ignore))
    (%with-agent dir
                 (lambda (k)
                   (let ((p (json-parse (uiop:read-file-string file))))
                     (flet ((need (key) (or (obj-get p key) (error "~a has no ~s" file key))))
                       (append! k "persona.profile.set"
                                (obj "persona_id" (need "persona_id") "revision" (obj-get p "revision" 1)
                                     "identity" (need "identity") "voice" (need "voice")))
                       (when (obj-get p "instructions")
                         (append! k "persona.instructions.set"
                                  (obj "revision" (obj-get p "revision" 1) "text" (obj-get p "instructions"))))
                       (when (obj-get p "tone")
                         (append! k "persona.tone.set" (obj "text" (obj-get p "tone"))))
                       (%say "Persona ~a set." (need "persona_id"))))))))

(defun cmd-graph-reset (args)
  "Start the agent's graph again; the ledger keeps what was there and the reason."
  (destructuring-bind (dir &rest reason) (%positional args)
    (when (null reason) (error "Give the reason for the reset."))
    (%with-agent dir (lambda (k) (pai-v3.mind::reset-graph k (format nil "~{~a~^ ~}" reason))
                       (%say "Graph reset. Health: ~a" (json-encode (pai-v3.mind:graph-health k)))))))

(defun cmd-graph-recover (args)
  "Recover unconnected entities from an export of the messages that mention them."
  (destructuring-bind (dir file source earlier &optional (limit "200")) (%positional args)
    (%with-agent dir
                 (lambda (k)
                   (%say "~a" (json-encode (pai-v3.mind::recover-orphans
                                            k (json-parse (uiop:read-file-string file :external-format :utf-8))
                                            :limit (parse-integer limit) :source-agent source :earlier-agent earlier)))))))

(defun cmd-graph-import (args)
  "Import a v2 graph export into the agent's graph, with lineage."
  (destructuring-bind (dir file source earlier &optional (generation "v1")) (%positional args)
    (%with-agent dir
                 (lambda (k)
                   (let ((result (pai-v3.mind::import-graph k (json-parse (uiop:read-file-string file :external-format :utf-8))
                                                            :source-agent source :earlier-agent earlier
                                                            :earlier-generation generation)))
                     (%say "~a" (json-encode result))
                     (%say "Health after import: ~a" (json-encode (pai-v3.mind:graph-health k))))))))

(defparameter +commands+
  '(("init" . cmd-init) ("run" . cmd-run) ("backup-init" . cmd-backup-init) ("backup" . cmd-backup)
    ("restore" . cmd-restore) ("backups" . cmd-backups) ("secret-set" . cmd-secret-set)
    ("unlock-methods" . cmd-unlock-methods) ("settings" . cmd-settings)
    ("settings-export" . cmd-settings-export) ("settings-import" . cmd-settings-import)
    ("persona-set" . cmd-persona-set) ("graph-import" . cmd-graph-import)
    ("graph-reset" . cmd-graph-reset) ("graph-recover" . cmd-graph-recover)))

(defun run-command (argv)
  "Run one command. Returns the process exit code."
  (let ((command (cdr (assoc (first argv) +commands+ :test #'string=))))
    (if (null command)
        (progn (%say "Usage: pai <command> ...  Commands: ~{~a~^, ~}." (mapcar #'car +commands+)) 2)
        (handler-case (progn (funcall command (rest argv)) 0)
          ;; Output cut off by the reader (pai settings | head): not an error.
          (sb-int:broken-pipe () 0)
          ((or keyring-error store-locked agent-locked setting-refused) (e) (%say "~a" e) 1)
          (error (e) (%say "Failed: ~a" e) 1)))))

(defun main ()
  "Entry point of the executable. Foreign libraries are found afresh on the
machine it runs on."
  (pai-v3.kernel::reload-sqlite-library)
  (ignore-errors (cl+ssl:reload))
  (sb-ext:exit :code (handler-case (run-command (rest sb-ext:*posix-argv*))
                       (sb-int:broken-pipe () 0))
              :abort t))
