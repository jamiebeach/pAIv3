(in-package #:pai-v3.trust)

;;; Recovery codes for a lost device (NFR AUTH-8). Single-use codes, issued
;;; only to a session that has just verified with a passkey, stored only as
;;; hashes. Redeeming a code yields a short-lived grant whose one use is
;;; enrolling a new passkey; it gives no session and no other authority.
;;; Failed attempts are limited per hour. Every issue, redemption and
;;; failure is an event the operator can review (AUTH-9).
;;;
;;; Neither the agent nor a peer can reach this: it is only an HTTP route
;;; for the client, and lisp_eval refuses code that names the trust package.

(defparameter *recovery-code-count* 10)
(defparameter *recovery-grant-seconds* 600)
(defparameter *recovery-failures-per-hour* 5)

(define-event-type "auth.recovery.issued"
  :schema (("set_id" :string) ("code_hashes" :array)))
(define-event-type "auth.recovery.redeemed"
  :schema (("code_hash" :string) ("grant_hash" :string) ("grant_expires" :string)))
(define-event-type "auth.recovery.failed"
  :schema (("hour" :string)))
(define-event-type "auth.recovery.grant.used"
  :schema (("grant_hash" :string) ("credential_id" :string)))
;; Pairing: a signed-in device lets another enrol its own passkey. The code
;; is redeemed like a recovery code, but lasts minutes and leaves the
;; operator's recovery codes alone.
(define-event-type "auth.pairing.issued"
  :schema (("code_hash" :string) ("expires" :string)))

(define-projection "auth_recovery"
  :kind :inline
  :inspectable nil
  :consumes ("auth.recovery.issued" "auth.recovery.redeemed" "auth.recovery.failed"
             "auth.recovery.grant.used" "auth.pairing.issued")
  :tables ("CREATE TABLE IF NOT EXISTS auth_recovery_codes (code_hash TEXT PRIMARY KEY, set_id TEXT NOT NULL, status TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS auth_recovery_codes_status_idx ON auth_recovery_codes(status)"
           "CREATE TABLE IF NOT EXISTS auth_recovery_grants (grant_hash TEXT PRIMARY KEY, expires_at TEXT NOT NULL, status TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS auth_recovery_attempts (hour TEXT PRIMARY KEY, failures INTEGER NOT NULL)"
           "CREATE TABLE IF NOT EXISTS auth_pairing_codes (code_hash TEXT PRIMARY KEY, expires_at TEXT NOT NULL, status TEXT NOT NULL)")
  :entity (lambda (e) (or (obj-get (event-payload e) "set_id") (obj-get (event-payload e) "code_hash")
                          (obj-get (event-payload e) "grant_hash") (obj-get (event-payload e) "hour")))
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (flet ((f (k) (obj-get p k)))
                (cond
                  ((string= type "auth.recovery.issued")
                   ;; A new set replaces every earlier code.
                   (sql-exec db "UPDATE auth_recovery_codes SET status='replaced' WHERE status=?1" "unused")
                   (dolist (h (f "code_hashes"))
                     (sql-exec db "INSERT INTO auth_recovery_codes(code_hash,set_id,status) VALUES(?1,?2,'unused')"
                               h (f "set_id"))))
                  ((string= type "auth.pairing.issued")
                   (sql-exec db "INSERT INTO auth_pairing_codes(code_hash,expires_at,status) VALUES(?1,?2,'open')"
                             (f "code_hash") (f "expires")))
                  ((string= type "auth.recovery.redeemed")
                   (sql-exec db "UPDATE auth_recovery_codes SET status='used' WHERE code_hash=?1" (f "code_hash"))
                   (sql-exec db "UPDATE auth_pairing_codes SET status='used' WHERE code_hash=?1" (f "code_hash"))
                   (sql-exec db "INSERT INTO auth_recovery_grants(grant_hash,expires_at,status) VALUES(?1,?2,'open')"
                             (f "grant_hash") (f "grant_expires")))
                  ((string= type "auth.recovery.failed")
                   (sql-exec db "INSERT INTO auth_recovery_attempts(hour,failures) VALUES(?1,1) ON CONFLICT(hour) DO UPDATE SET failures=failures+1"
                             (f "hour")))
                  ((string= type "auth.recovery.grant.used")
                   (sql-exec db "UPDATE auth_recovery_grants SET status='used' WHERE grant_hash=?1"
                             (f "grant_hash"))))))))

(defun %code-hash (code)
  "Codes are typed by people: case, dashes and spaces do not matter."
  (%token-hash (string-upcase (remove-if (lambda (c) (member c '(#\- #\Space))) code))))

(defun %new-code ()
  (let ((text (%base32 (ironclad:random-data 7))))       ; 56 bits, 12 characters
    (format nil "~a-~a-~a" (subseq text 0 4) (subseq text 4 8) (subseq text 8 12))))

(defun issue-recovery-codes (kernel token)
  "New recovery codes for the operator, replacing any earlier ones. TOKEN's
session must have verified with a passkey in the last few minutes. Returns
the codes, which are shown once."
  (unless (stepped-up-p kernel token)
    (fail "recovery codes need a fresh passkey verification"))
  (let ((codes (loop repeat *recovery-code-count* collect (%new-code))))
    (append! kernel "auth.recovery.issued"
             (obj "set_id" (base64url-encode (ironclad:random-data 8))
                  "code_hashes" (mapcar #'%code-hash codes)))
    codes))

(defparameter *pairing-seconds* 600)

(defun issue-pairing-code (kernel token)
  "A one-use code another device can redeem, within *PAIRING-SECONDS*, to
enrol its own passkey. TOKEN's session must have verified with a passkey in
the last few minutes."
  (unless (stepped-up-p kernel token)
    (fail "pairing needs a fresh passkey verification: sign in again, then pair within five minutes"))
  (let ((code (%new-code)))
    (append! kernel "auth.pairing.issued"
             (obj "code_hash" (%code-hash code) "expires" (iso8601 (+ (now) *pairing-seconds*))))
    code))

(defun issue-console-pairing-code (kernel)
  "A one-use pairing code issued from the console. Opening the agent's own
stores is a stronger proof than a browser session, and it is the only way in
when no device is enrolled and the agent runs where a browser cannot reach
it as this machine (a container sees a forwarded port's requests as remote)."
  (let ((code (%new-code)))
    (append! kernel "auth.pairing.issued"
             (obj "code_hash" (%code-hash code) "expires" (iso8601 (+ (now) *pairing-seconds*))))
    code))

(defun %pairing-open-p (kernel hash)
  (destructuring-bind (&optional expires status)
      (sql-row (kernel-db kernel) "SELECT expires_at,status FROM auth_pairing_codes WHERE code_hash=?1 LIMIT 1" hash)
    (and (equal status "open") (< (now) (parse-iso8601 expires)))))

(defun recovery-codes-left (kernel)
  (or (sql-value (kernel-db kernel) "SELECT count(*) FROM auth_recovery_codes WHERE status=?1 LIMIT 1" "unused") 0))

(defun %hour () (subseq (now-iso8601) 0 13))

(defun redeem-recovery-code (kernel code)
  "Exchange an unused recovery CODE for a grant to enrol one new passkey.
Returns the grant token. Too many failures in an hour refuse every attempt
until the next hour."
  (when (>= (or (sql-value (kernel-db kernel)
                           "SELECT failures FROM auth_recovery_attempts WHERE hour=?1 LIMIT 1" (%hour))
                0)
            *recovery-failures-per-hour*)
    (fail "too many recovery attempts; try again next hour"))
  (let ((hash (%code-hash code)))
    (if (or (equal "unused" (sql-value (kernel-db kernel)
                                       "SELECT status FROM auth_recovery_codes WHERE code_hash=?1 LIMIT 1" hash))
            (%pairing-open-p kernel hash))
        (let ((grant (base64url-encode (ironclad:random-data 32))))
          (append! kernel "auth.recovery.redeemed"
                   (obj "code_hash" hash "grant_hash" (%token-hash grant)
                        "grant_expires" (iso8601 (+ (now) *recovery-grant-seconds*))))
          grant)
        (progn (append! kernel "auth.recovery.failed" (obj "hour" (%hour)))
               (fail "that recovery code is not valid")))))

(defun %recovery-grant-open-p (kernel grant)
  (destructuring-bind (&optional expires status)
      (sql-row (kernel-db kernel) "SELECT expires_at,status FROM auth_recovery_grants WHERE grant_hash=?1 LIMIT 1"
               (%token-hash grant))
    (and (equal status "open") (< (now) (parse-iso8601 expires)))))
