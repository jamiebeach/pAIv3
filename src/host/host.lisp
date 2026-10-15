(defpackage #:pai-v3.host
  (:use #:cl #:pai-v3.kernel #:pai-v3.trust)
  (:export #:init-agent #:open-agent #:close-agent #:agent-config #:agent-locked
           #:store-path #:agent-handle-kernel #:agent-handle-dir #:agent-store-db
           #:init-backups #:backup-agent #:restore-agent #:list-backups #:maybe-backup
           #:last-backup-at #:prune-backups #:*backup-keep*))
(in-package #:pai-v3.host)

;;; Setting up and opening an agent (spec §17, §17.1; NFR ENC-8, PORT-1,
;;; REL-6). One data directory per agent:
;;;
;;;   config.json    agent id, listen address, relying party; no secrets
;;;   keyring.json   the root key, wrapped per unlock method
;;;   ledger.db      events and projections        (encrypted)
;;;   secrets.db     provider keys, tokens          (encrypted)
;;;   blobs.db       sensor data with expiry        (encrypted)
;;;   runtime.lock   held while a runtime has the agent open
;;;
;;; Nothing here depends on a container or an OS service manager.

(defparameter +store-files+ '(("ledger" . "ledger.db") ("secrets" . "secrets.db") ("blobs" . "blobs.db")))

(define-condition agent-locked (error)
  ((dir :initarg :dir :reader agent-locked-dir))
  (:report (lambda (c s) (format s "Another runtime already has the agent in ~a open." (agent-locked-dir c)))))

(defun %dir (dir) (uiop:ensure-directory-pathname dir))
(defun store-path (dir store) (merge-pathnames (cdr (assoc store +store-files+ :test #'string=)) (%dir dir)))
(defun %config-path (dir) (merge-pathnames "config.json" (%dir dir)))

(defun agent-config (dir)
  (let ((path (%config-path dir)))
    (unless (probe-file path)
      (error 'keyring-error :message (format nil "~a is not an agent directory (no config.json)." dir)))
    (json-parse (uiop:read-file-string path))))

(defun %zero (octets) (when octets (fill octets 0)))

(defun init-agent (dir agent-id &key (methods '("keychain")) passphrase keyfile (address "127.0.0.1") (port 8080))
  "Create agent AGENT-ID in DIR: keyring, config and empty encrypted stores.
Returns the recovery code, to be shown once. Refuses a directory that already
holds an agent."
  (when (or (probe-file (%config-path dir)) (probe-file (keyring-path dir))
            (some (lambda (s) (probe-file (store-path dir (car s)))) +store-files+))
    (error 'keyring-error :message (format nil "~a already holds an agent; it is never overwritten." dir)))
  (let ((code (keyring-init dir agent-id :methods methods :passphrase passphrase :keyfile keyfile))
        (root nil))
    (unwind-protect
         (progn
           (setf root (unlock dir :recovery-code code))
           (close-kernel (open-kernel (store-path dir "ledger") :agent-id agent-id
                                                                :key (store-key dir root "ledger")))
           (dolist (store '("secrets" "blobs"))
             (let ((db (db-open (store-path dir store) :key (store-key dir root store))))
               (sql-exec db "CREATE TABLE IF NOT EXISTS store_meta (k TEXT PRIMARY KEY, v TEXT NOT NULL)")
               (sql-exec db "INSERT OR REPLACE INTO store_meta(k,v) VALUES('store',?1)" store)
               (db-close db))))
      (%zero root))
    (with-open-file (out (%config-path dir) :direction :output :external-format :utf-8)
      (write-string (json-encode (obj "agent_id" agent-id "created_at" (now-iso8601)
                                      "address" address "port" port))
                    out))
    code))

;;; Opening

(defstruct agent-handle dir kernel lock-db secrets-db blobs-db)

(defun agent-store-db (handle store)
  (cond ((string= store "ledger") (kernel-db (agent-handle-kernel handle)))
        ((string= store "secrets") (agent-handle-secrets-db handle))
        ((string= store "blobs") (agent-handle-blobs-db handle))
        (t (error "No store ~a." store))))

(defun %take-lock (dir)
  "An open SQLite handle holding an exclusive lock on runtime.lock. The OS
releases it if the process dies, so a crash never leaves a stale lock."
  (let ((db nil))
    (handler-case
        (progn (setf db (db-open (merge-pathnames "runtime.lock" (%dir dir)) :busy-ms 0))
               (sql-exec db "PRAGMA locking_mode=EXCLUSIVE")
               (sql-exec db "BEGIN EXCLUSIVE")
               (sql-exec db "COMMIT")
               db)
      (sqlite-error ()
        (when db (db-close db))
        (error 'agent-locked :dir dir)))))

(defun open-agent (dir &key passphrase recovery-code)
  "Unlock and open the agent in DIR. Unattended methods are tried first;
PASSPHRASE or RECOVERY-CODE are used when given. Returns an AGENT-HANDLE."
  (let* ((config (agent-config dir))
         (lock (%take-lock dir))
         (root nil))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (db-close lock))))
      (unwind-protect
           (progn
             (setf root (unlock dir :passphrase passphrase :recovery-code recovery-code))
             (let ((secrets (db-open (store-path dir "secrets") :key (store-key dir root "secrets")
                                                                :create nil)))
               (open-secrets secrets)
               (make-agent-handle
                :dir dir :lock-db lock :secrets-db secrets
                :blobs-db (db-open (store-path dir "blobs") :key (store-key dir root "blobs") :create nil)
                :kernel (open-kernel (store-path dir "ledger") :agent-id (obj-get config "agent_id")
                                                              :key (store-key dir root "ledger")
                                                              :create nil))))
        (%zero root)))))

(defun close-agent (handle)
  (close-secrets)
  (db-close (agent-handle-secrets-db handle))
  (db-close (agent-handle-blobs-db handle))
  (close-kernel (agent-handle-kernel handle))
  (db-close (agent-handle-lock-db handle))
  nil)
