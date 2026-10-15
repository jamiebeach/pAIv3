(in-package #:pai-v3.test)
(use-package '#:pai-v3.trust)
(use-package '#:pai-v3.host)

;;; Setting up and opening an agent directory (spec §17.1).

(defvar *chain* (make-hash-table :test 'equal))

(defmacro with-dir ((dir) &body body)
  `(let ((,dir (format nil "/tmp/pai-v3-host-~d/" (random 100000000)))
         (*keychain* (list :put (lambda (n o) (setf (gethash n *chain*) o))
                           :get (lambda (n) (gethash n *chain*))
                           :delete (lambda (n) (remhash n *chain*))))
         (*scrypt-n* 1024))
     ,@body))

(define-event-type "test.note" :schema (("text" :string)))

(deftest init-then-open-and-continue
  (with-dir (dir)
    (let ((code (init-agent dir "beta")))
      (is (stringp code))
      (dolist (file '("config.json" "keyring.json" "ledger.db" "secrets.db" "blobs.db"))
        (is (probe-file (merge-pathnames file dir)) file))
      (let ((h (open-agent dir)))
        (append! (agent-handle-kernel h) "test.note" (obj "text" "first words"))
        (close-agent h))
      (let ((h (open-agent dir)))
        (is (= 1 (ledger-head (agent-handle-kernel h))) "history survives a restart")
        (close-agent h))
      (signals keyring-error (init-agent dir "beta")))))

(deftest one-runtime-per-agent
  (with-dir (dir)
    (init-agent dir "beta")
    (let ((h (open-agent dir)))
      (signals agent-locked (open-agent dir))
      (close-agent h))
    (let ((h (open-agent dir)))
      (is h "the lock is released on close")
      (close-agent h))))

(deftest a-lost-keychain-needs-the-recovery-code
  (with-dir (dir)
    (let ((code (init-agent dir "beta")))
      (clrhash *chain*)                                   ; a new machine
      (signals keyring-error (open-agent dir))
      (let ((h (open-agent dir :recovery-code code)))
        (is h)
        (close-agent h))
      (signals keyring-error (open-agent dir)))))           ; and the failed attempt released the lock

(deftest a-missing-ledger-is-never-recreated
  (with-dir (dir)
    (init-agent dir "beta")
    (delete-file (store-path dir "ledger"))
    (signals store-locked (open-agent dir))
    (is (null (probe-file (store-path dir "ledger"))))))

(deftest backup-and-restore-to-a-new-machine
  (with-dir (dir)
    (let* ((backups (format nil "/tmp/pai-v3-backups-~d/" (random 100000000)))
           (restored (format nil "/tmp/pai-v3-restored-~d/" (random 100000000)))
           (backup-code (progn (init-agent dir "beta")
                               (init-backups backups "beta" :methods '("passphrase")
                                                            :passphrase "a separate backup phrase")))
           (h (open-agent dir)))
      (dotimes (i 5) (append! (agent-handle-kernel h) "test.note" (obj "text" (format nil "note ~d" i))))
      (pai-v3.trust:set-secret "brave" "BSAverySecretValue123" '("api.search.brave.com"))
      (signals keyring-error (backup-agent h backups))   ; the live keys cannot open the backup keyring
      (let ((stamp (backup-agent h backups :passphrase "a separate backup phrase")))
        (is (equal (list stamp) (list-backups backups)))
        (is (last-backup-at (agent-handle-kernel h)))
        (let ((raw (uiop:read-file-string (format nil "~a~a/secrets.db" backups stamp) :external-format :latin-1)))
          (is (null (search "BSAvery" raw)) "backups are encrypted"))
        (close-agent h)
        (clrhash *chain*)                                 ; the old machine is gone
        (signals keyring-error (restore-agent backups stamp dir :backup-recovery-code backup-code))
        (let ((code (restore-agent backups stamp restored :backup-recovery-code backup-code)))
          (is (stringp code) "the restored agent has a new recovery code")
          (let ((h2 (open-agent restored)))
            (is (= (obj-get (json-parse (uiop:read-file-string (format nil "~a~a/manifest.json" backups stamp)))
                            "ledger_head")
                   (ledger-head (agent-handle-kernel h2)))
                "every event up to the backup is back")
            (is (= 6 (ledger-head (agent-handle-kernel h2))) "five notes and the refused attempt")
            (is (equal '("brave") (mapcar #'first (pai-v3.trust:secret-names))) "and the secrets")
            (close-agent h2)))))))

(deftest a-damaged-backup-is-refused
  (with-dir (dir)
    (let ((backups (format nil "/tmp/pai-v3-backups-~d/" (random 100000000))))
      (init-agent dir "beta")
      (let* ((code (init-backups backups "beta" :methods '("passphrase") :passphrase "a separate backup phrase"))
             (h (open-agent dir))
             (stamp (backup-agent h backups :passphrase "a separate backup phrase")))
        (close-agent h)
        (with-open-file (out (format nil "~a~a/ledger.db" backups stamp) :direction :io
                                                                         :element-type '(unsigned-byte 8)
                                                                         :if-exists :overwrite)
          (file-position out 5000) (write-byte 7 out))
        (signals keyring-error (restore-agent backups stamp (format nil "/tmp/pai-v3-r-~d/" (random 100000000))
                                              :backup-recovery-code code))))))

(deftest old-backups-are-pruned
  (with-dir (dir)
    (let ((backups (format nil "/tmp/pai-v3-backups-~d/" (random 100000000))))
      (init-agent dir "beta")
      (init-backups backups "beta" :methods '("passphrase") :passphrase "a separate backup phrase")
      (let ((h (open-agent dir)) (*backup-keep* 2))
        (with-clock
          (dotimes (i 4)
            (backup-agent h backups :passphrase "a separate backup phrase")
            (advance 86400)))
        (is (= 2 (length (list-backups backups))))
        (close-agent h)))))

(run-suite "host-agent")
