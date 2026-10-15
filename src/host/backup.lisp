(in-package #:pai-v3.host)

;;; Encrypted backups and restore (spec §17.1; NFR ENC-3, REL-3, REL-4).
;;;
;;; A backup destination has its own keyring: a different root key with its
;;; own recovery code, so the live keys cannot open backups and losing the
;;; live machine does not lose them. Each backup is a directory named by its
;;; time holding an online copy of every store, re-encrypted under the
;;; backup keys, and a manifest written last, so a directory without one is
;;; an interrupted backup and is ignored. Restore builds a new agent
;;; directory with a new live keyring and never overwrites an agent.

(defparameter *backup-keep* 14 "Backups kept; older ones are deleted.")
(defparameter *backup-every-seconds* 86400)
(defparameter +backup-format+ 1)

(define-setting "backup.dir"
  :type :string :group "Backups" :default ""
  :description "Where daily backups go (its own keyring; set up with pai backup-init). Empty means no backups.")
(define-setting "backup.every_seconds"
  :type :integer :min 3600 :group "Backups" :default *backup-every-seconds*
  :description "How often to back up while the agent runs.")
(define-setting "backup.keep"
  :type :integer :min 1 :group "Backups" :default *backup-keep*
  :description "How many backups to keep; older ones are deleted.")

(define-event-type "backup.completed"
  :schema (("stamp" :string) ("ledger_head" :integer) ("bytes" :integer)))
(define-event-type "backup.failed"
  :schema (("reason" :string)))

(define-projection "backups"
  :kind :inline
  :consumes ("backup.completed" "backup.failed")
  :tables ("CREATE TABLE IF NOT EXISTS backups (event_id INTEGER PRIMARY KEY, outcome TEXT NOT NULL, stamp TEXT, ledger_head INTEGER, bytes INTEGER, reason TEXT, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS backups_outcome_idx ON backups(outcome,event_id)")
  :entity (lambda (e) (event-id e))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO backups(event_id,outcome,stamp,ledger_head,bytes,reason,at) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                        (event-id e)
                        (if (string= (event-type e) "backup.completed") "completed" "failed")
                        (obj-get p "stamp") (obj-get p "ledger_head") (obj-get p "bytes")
                        (obj-get p "reason") (event-at e)))))

(defun last-backup-at (kernel)
  "Universal time of the last completed backup, or NIL."
  (let ((at (sql-value (kernel-db kernel)
                       "SELECT at FROM backups WHERE outcome=?1 ORDER BY event_id DESC LIMIT 1" "completed")))
    (and at (parse-iso8601 at))))

(defun init-backups (backup-dir agent-id &key (methods '("keychain")) passphrase keyfile)
  "Prepare BACKUP-DIR with its own keyring. Returns its recovery code, which
the operator keeps apart from the live one."
  (keyring-init backup-dir (format nil "~a-backup" agent-id)
                :methods methods :passphrase passphrase :keyfile keyfile))

(defun %stamp ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (now) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" y mo d h mi s)))

(defun %file-sha256 (path)
  (ironclad:byte-array-to-hex-string (ironclad:digest-file :sha256 path)))

(defun %backup-dirs (backup-dir)
  "Completed backups, oldest first, as (stamp . directory)."
  (sort (loop for d in (uiop:subdirectories (%dir backup-dir))
              when (probe-file (merge-pathnames "manifest.json" d))
                collect (cons (car (last (pathname-directory d))) d))
        #'string< :key #'car))

(defun backup-agent (handle backup-dir &key passphrase recovery-code)
  "Back up every store of the open agent HANDLE into BACKUP-DIR. Returns the
backup's stamp. Records backup.completed, or backup.failed and re-signals."
  (let* ((kernel (agent-handle-kernel handle))
         (stamp (%stamp))
         (dir (merge-pathnames (format nil "~a/" stamp) (%dir backup-dir)))
         (root nil))
    (handler-bind ((error (lambda (e)
                            (append! kernel "backup.failed"
                                     (obj "reason" (let ((m (princ-to-string e))) (subseq m 0 (min 300 (length m)))))))))
      (unwind-protect
           (progn
             (setf root (unlock backup-dir :passphrase passphrase :recovery-code recovery-code))
             (ensure-directories-exist dir)
             (let ((files '()) (bytes 0))
               (loop for (store . file) in +store-files+
                     for source = (agent-store-db handle store)
                     for path = (merge-pathnames file dir)
                     do (let ((dest (db-open path :key (store-key backup-dir root store))))
                          (unwind-protect (db-backup source dest) (db-close dest)))
                        (incf bytes (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))
                        (push (obj "store" store "file" file "sha256" (%file-sha256 path)) files))
               ;; The manifest is written last: its presence marks a complete backup.
               (with-open-file (out (merge-pathnames "manifest.json" dir) :direction :output
                                                                          :external-format :utf-8)
                 (write-string (json-encode (obj "format" +backup-format+ "stamp" stamp
                                                 "agent_id" (kernel-agent-id kernel)
                                                 "ledger_head" (ledger-head kernel)
                                                 "config" (agent-config (agent-handle-dir handle))
                                                 "stores" (nreverse files)))
                               out))
               (append! kernel "backup.completed" (obj "stamp" stamp "ledger_head" (ledger-head kernel)
                                                       "bytes" bytes))
               (prune-backups backup-dir :keep (setting kernel "backup.keep"))
               stamp))
        (when root (fill root 0))))))

(defun prune-backups (backup-dir &key (keep *backup-keep*))
  (let ((all (%backup-dirs backup-dir)))
    (loop for (nil . d) in (butlast all keep)
          do (uiop:delete-directory-tree d :validate t))))

(defun maybe-backup (handle)
  "Back up to the backup.dir setting when the last backup is older than
backup.every_seconds. Called by the runtime when idle. A failure is recorded
and never stops the agent."
  (let* ((kernel (agent-handle-kernel handle))
         (dir (setting kernel "backup.dir"))
         (last (last-backup-at kernel)))
    (when (and (plusp (length dir))
               (or (null last) (> (- (now) last) (setting kernel "backup.every_seconds"))))
      (ignore-errors (backup-agent handle dir)))))

(defun list-backups (backup-dir)
  (mapcar #'car (%backup-dirs backup-dir)))

(defun restore-agent (backup-dir stamp target-dir &key backup-passphrase backup-recovery-code
                                                        (methods '("keychain")) passphrase keyfile)
  "Rebuild the agent saved as backup STAMP into TARGET-DIR, which must not
hold an agent. Every file is checked against the manifest first. The new
directory gets a new live keyring; returns its recovery code."
  (when (or (probe-file (%config-path target-dir)) (probe-file (keyring-path target-dir)))
    (error 'keyring-error :message (format nil "~a already holds an agent; restore never overwrites one." target-dir)))
  (let* ((dir (or (cdr (assoc stamp (%backup-dirs backup-dir) :test #'string=))
                  (error 'keyring-error :message (format nil "There is no complete backup ~a." stamp))))
         (manifest (json-parse (uiop:read-file-string (merge-pathnames "manifest.json" dir))))
         (backup-root nil) (live-root nil) (code nil))
    (dolist (f (obj-get manifest "stores"))
      (unless (equal (obj-get f "sha256") (%file-sha256 (merge-pathnames (obj-get f "file") dir)))
        (error 'keyring-error :message (format nil "~a in backup ~a is damaged." (obj-get f "file") stamp))))
    (unwind-protect
         (progn
           (setf backup-root (unlock backup-dir :passphrase backup-passphrase :recovery-code backup-recovery-code)
                 code (keyring-init target-dir (obj-get manifest "agent_id")
                                    :methods methods :passphrase passphrase :keyfile keyfile)
                 live-root (unlock target-dir :recovery-code code))
           (dolist (f (obj-get manifest "stores"))
             (let ((source (db-open (merge-pathnames (obj-get f "file") dir) :read-only t
                                    :key (store-key backup-dir backup-root (obj-get f "store"))))
                   (dest (db-open (store-path target-dir (obj-get f "store"))
                                  :key (store-key target-dir live-root (obj-get f "store")))))
               (unwind-protect (db-backup source dest)
                 (db-close source) (db-close dest))))
           (with-open-file (out (%config-path target-dir) :direction :output :external-format :utf-8)
             (write-string (json-encode (obj-get manifest "config")) out))
           code)
      (when backup-root (fill backup-root 0))
      (when live-root (fill live-root 0)))))
