(in-package #:pai-v3.test)
(use-package '#:pai-v3.trust)

;;; Keyring and unlock methods (spec §17, ENC-8).

(defvar *fake-keychain* (make-hash-table :test 'equal))

(defmacro with-agent-dir ((dir) &body body)
  `(let ((,dir (format nil "/tmp/pai-v3-agent-~d/" (random 100000000)))
         (*keychain* (list :put (lambda (n o) (setf (gethash n *fake-keychain*) o))
                           :get (lambda (n) (gethash n *fake-keychain*))
                           :delete (lambda (n) (remhash n *fake-keychain*))))
         (*scrypt-n* 1024))                 ; fast in tests; the default is 32768
     (clrhash *fake-keychain*)
     ,@body))

(deftest setup-unlocks-unattended-from-the-keychain
  (with-agent-dir (dir)
    (let ((code (keyring-init dir "beta")))
      (is (= 64 (length code)) "a recovery code of 52 characters in groups of four")
      (let ((root (unlock dir)))
        (is (= 32 (length root)))
        (is (equalp root (unlock dir)) "the same root every time")
        (is (equalp root (unlock dir :recovery-code code)) "and from the recovery code")))
    (signals keyring-error (keyring-init dir "beta"))))

(deftest the-keyring-file-holds-no-key
  (with-agent-dir (dir)
    (keyring-init dir "beta")
    (let ((root (unlock dir))
          (text (uiop:read-file-string (keyring-path dir))))
      (is (null (search (ironclad:byte-array-to-hex-string root) text)))
      (is (null (search (base64url-encode root) text))))))

(deftest store-keys-are-distinct-and-stable
  (with-agent-dir (dir)
    (keyring-init dir "beta")
    (let* ((root (unlock dir))
           (ledger (store-key dir root "ledger"))
           (secrets (store-key dir root "secrets")))
      (is (not (equalp ledger secrets)))
      (is (equalp ledger (store-key dir root "ledger")))
      (is (= 2 (next-store-generation dir "ledger")))
      (is (not (equalp ledger (store-key dir root "ledger"))) "a new generation is a new key")
      (is (equalp secrets (store-key dir root "secrets")) "other stores are untouched"))))

(deftest without-the-keychain-only-a-secret-opens-it
  (with-agent-dir (dir)
    (let ((code (keyring-init dir "beta" :methods '("passphrase") :passphrase "correct horse battery")))
      (signals keyring-error (unlock dir))
      (signals keyring-error (unlock dir :passphrase "wrong horse battery"))
      (is (equalp (unlock dir :passphrase "correct horse battery") (unlock dir :recovery-code code)))
      (is (unlock dir :recovery-code (string-downcase (remove #\- code))) "codes are forgiving to type")
      (signals keyring-error (unlock dir :recovery-code (concatenate 'string "ZZZZ" (subseq code 4)))))))

(deftest a-key-file-elsewhere-unlocks-unattended
  (with-agent-dir (dir)
    (let ((keyfile (format nil "/tmp/pai-v3-keyfile-~d" (random 100000000))))
      (keyring-init dir "beta" :methods '("keyfile") :keyfile keyfile)
      (let ((root (unlock dir)))
        (is root)
        (delete-file keyfile)
        (signals keyring-error (unlock dir)) "without the file, it stays locked"))))

(deftest methods-change-without-touching-the-root
  (with-agent-dir (dir)
    (let* ((code (keyring-init dir "beta"))
           (root (unlock dir))
           (pass (add-unlock-method dir root "passphrase" :passphrase "a long enough phrase")))
      (is (= 3 (length (unlock-methods dir))))
      (remove-unlock-method dir (first (find "keychain" (unlock-methods dir) :key #'second :test #'equal)))
      (is (= 0 (hash-table-count *fake-keychain*)) "the keychain entry is removed too")
      (signals keyring-error (unlock dir))
      (is (equalp root (unlock dir :passphrase "a long enough phrase")) "the root never changed")
      (signals keyring-error (remove-unlock-method dir (first (find "recovery" (unlock-methods dir)
                                                                    :key #'second :test #'equal))))
      (multiple-value-bind (code2 id2) (new-recovery-code dir root)
        (remove-unlock-method dir (first (find "recovery" (unlock-methods dir) :key #'second :test #'equal)))
        (signals keyring-error (unlock dir :recovery-code code))
        (is (equalp root (unlock dir :recovery-code code2)))
        (is (find id2 (unlock-methods dir) :key #'first :test #'equal)))
      (is (stringp pass)))))

(deftest a-tampered-keyring-does-not-open
  (with-agent-dir (dir)
    (let* ((code (keyring-init dir "beta"))
           (text (uiop:read-file-string (keyring-path dir))))
      ;; Claim the recovery wrap belongs to another agent: the AAD no longer matches.
      (with-open-file (out (keyring-path dir) :direction :output :if-exists :supersede)
        (write-string (let ((at (search "\"agent\":\"beta\"" text)))
                        (concatenate 'string (subseq text 0 at) "\"agent\":\"eve\""
                                     (subseq text (+ at (length "\"agent\":\"beta\"")))))
                      out))
      (signals keyring-error (unlock dir :recovery-code code)))))

(run-suite "trust-keys")
