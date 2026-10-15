(in-package #:pai-v3.trust)

;;; Operator authentication (NFR AUTH-1..3, AUTH-6, AUTH-9; spec §16.5).
;;; Passkeys per enrolled device; device-bound sessions with idle and
;;; absolute expiry; step-up for sensitive actions; and the verified origin
;;; attestation that every inbound operator message carries. Every
;;; enrollment, login, step-up and revocation is an event (audit, AUTH-9).
;;; Session tokens are never stored, only their SHA-256.

(defvar *session-idle-seconds* (* 30 86400))
(defvar *session-absolute-seconds* (* 90 86400))
(defvar *challenge-seconds* 300)
(defvar *refresh-seconds* 86400
  "A session in use is rotated (new token, activity recorded) at most this often (AUTH-2).")
(defvar *rotation-grace-seconds* 60
  "How long a rotated token still works, for requests already in flight.")

(define-event-type "auth.device.enrolled"
  :schema (("credential_id" :string) ("name" :string) ("public_key" :string)
           ("sign_count" :integer)))
(define-event-type "auth.device.revoked"
  :schema (("credential_id" :string) ("reason" :string)))
(define-event-type "auth.assertion.verified"
  :schema (("credential_id" :string) ("sign_count" :integer) ("purpose" :string)))
(define-event-type "auth.session.started"
  :schema (("session_hash" :string) ("credential_id" :string) ("absolute_expires" :string)))
(define-event-type "auth.session.refreshed"
  :schema (("session_hash" :string)))
(define-event-type "auth.session.rotated"
  :schema (("session_hash" :string) ("new_hash" :string) ("grace_until" :string)))
(define-event-type "auth.session.ended"
  :schema (("session_hash" :string) ("reason" :string)))

(defun %reduce-auth (db e)
  (let ((p (event-payload e)) (at (event-at e)) (type (event-type e)))
    (flet ((f (k) (obj-get p k)))
      (cond
        ((string= type "auth.device.enrolled")
         (sql-exec db "INSERT INTO auth_devices(credential_id,name,public_key,sign_count,status,enrolled_at,last_stepup_at) VALUES(?1,?2,?3,?4,'active',?5,NULL)"
                   (f "credential_id") (f "name") (f "public_key") (f "sign_count") at))
        ((string= type "auth.device.revoked")
         (sql-exec db "UPDATE auth_devices SET status='revoked' WHERE credential_id=?1" (f "credential_id"))
         (sql-exec db "UPDATE auth_sessions SET status='ended' WHERE credential_id=?1" (f "credential_id")))
        ((string= type "auth.assertion.verified")
         (sql-exec db "UPDATE auth_devices SET sign_count=?2,last_stepup_at=?3 WHERE credential_id=?1"
                   (f "credential_id") (f "sign_count") at))
        ((string= type "auth.session.started")
         (sql-exec db "INSERT INTO auth_sessions(session_hash,credential_id,started_at,last_seen,absolute_expires,status) VALUES(?1,?2,?3,?3,?4,'active')"
                   (f "session_hash") (f "credential_id") at (f "absolute_expires")))
        ((string= type "auth.session.refreshed")
         (sql-exec db "UPDATE auth_sessions SET last_seen=?2 WHERE session_hash=?1" (f "session_hash") at))
        ((string= type "auth.session.rotated")
         (sql-exec db "INSERT INTO auth_sessions(session_hash,credential_id,started_at,last_seen,absolute_expires,status) SELECT ?2,credential_id,started_at,?3,absolute_expires,'active' FROM auth_sessions WHERE session_hash=?1"
                   (f "session_hash") (f "new_hash") at)
         (sql-exec db "UPDATE auth_sessions SET status='rotated',grace_until=?2 WHERE session_hash=?1"
                   (f "session_hash") (f "grace_until")))
        ((string= type "auth.session.ended")
         (sql-exec db "UPDATE auth_sessions SET status='ended' WHERE session_hash=?1" (f "session_hash")))))))

(define-projection "auth"
  :kind :inline
  :inspectable nil
  :consumes ("auth.device.enrolled" "auth.device.revoked" "auth.assertion.verified"
             "auth.session.started" "auth.session.refreshed" "auth.session.rotated" "auth.session.ended")
  :tables ("CREATE TABLE IF NOT EXISTS auth_devices (credential_id TEXT PRIMARY KEY, name TEXT NOT NULL, public_key TEXT NOT NULL, sign_count INTEGER NOT NULL, status TEXT NOT NULL, enrolled_at TEXT NOT NULL, last_stepup_at TEXT)"
           "CREATE INDEX IF NOT EXISTS auth_devices_status_idx ON auth_devices(status,enrolled_at)"
           "CREATE TABLE IF NOT EXISTS auth_sessions (session_hash TEXT PRIMARY KEY, credential_id TEXT NOT NULL, started_at TEXT NOT NULL, last_seen TEXT NOT NULL, absolute_expires TEXT NOT NULL, status TEXT NOT NULL, grace_until TEXT)"
           "CREATE INDEX IF NOT EXISTS auth_sessions_credential_idx ON auth_sessions(credential_id)")
  :entity (lambda (e) (or (obj-get (event-payload e) "credential_id")
                          (obj-get (event-payload e) "session_hash")))
  :reduce #'%reduce-auth)

;;; Challenges: short-lived, single use, held in memory only.

(defvar *challenges* (make-hash-table :test 'equal))
(defvar *challenge-lock* (bt:make-lock "webauthn challenges"))

(defun new-challenge (purpose)
  (bt:with-lock-held (*challenge-lock*)
    (let ((now (now)))
      (loop for c being the hash-keys of *challenges* using (hash-value v)
            when (< (cdr v) now) do (remhash c *challenges*))
      (when (> (hash-table-count *challenges*) 64) (error "Too many pending challenges"))
      (let ((challenge (base64url-encode (ironclad:random-data 32))))
        (setf (gethash challenge *challenges*) (cons purpose (+ now *challenge-seconds*)))
        challenge))))

(defun take-challenge (challenge purpose)
  "Consume CHALLENGE; error unless it was issued for PURPOSE and is fresh."
  (bt:with-lock-held (*challenge-lock*)
    (let ((entry (gethash challenge *challenges*)))
      (remhash challenge *challenges*)
      (unless (and entry (eq (car entry) purpose) (>= (cdr entry) (now)))
        (fail "unknown, reused or expired challenge"))
      challenge)))

;;; Devices

(defun %hex (octets) (ironclad:byte-array-to-hex-string octets))
(defun %unhex (string) (ironclad:hex-string-to-byte-array string))
(defun %token-hash (token) (%hex (sha256 (octets token))))

(defun active-devices (kernel)
  (sql-rows (kernel-db kernel)
            "SELECT credential_id,name,enrolled_at,last_stepup_at FROM auth_devices WHERE status=?1 ORDER BY enrolled_at LIMIT 32"
            "active"))

(defun enroll-device (kernel &key name client-data-json attestation-object challenge origin rp-id
                                  authorized-by)
  "Enroll a passkey. AUTHORIZED-BY is :bootstrap (allowed only while no
device is enrolled), (:recovery grant) from a redeemed recovery code, or a
session token that has stepped up recently."
  (cond ((eq authorized-by :bootstrap)
         (when (active-devices kernel) (fail "bootstrap enrollment closed: a device is enrolled")))
        ((and (consp authorized-by) (eq :recovery (first authorized-by)))
         (unless (%recovery-grant-open-p kernel (second authorized-by))
           (fail "that recovery grant is not valid or has expired")))
        ((not (and (stringp authorized-by) (stepped-up-p kernel authorized-by)))
         (fail "enrolling a device requires a recent step-up")))
  (take-challenge challenge :register)
  (destructuring-bind (&key credential-id public-key sign-count)
      (verify-registration :client-data-json client-data-json :attestation-object attestation-object
                           :challenge challenge :origin origin :rp-id rp-id)
    (append! kernel "auth.device.enrolled"
             (obj "credential_id" credential-id "name" name
                  "public_key" (%hex public-key) "sign_count" sign-count))
    (when (consp authorized-by)
      (append! kernel "auth.recovery.grant.used"
               (obj "grant_hash" (%token-hash (second authorized-by)) "credential_id" credential-id)))
    credential-id))

(defun revoke-device (kernel credential-id reason)
  (append! kernel "auth.device.revoked" (obj "credential_id" credential-id "reason" reason)))

(defun %device (kernel credential-id)
  (sql-row (kernel-db kernel)
           "SELECT public_key,sign_count,status,last_stepup_at FROM auth_devices WHERE credential_id=?1 LIMIT 1"
           credential-id))

(defun %assert (kernel purpose &key credential-id client-data-json authenticator-data signature
                                    challenge origin rp-id)
  (take-challenge challenge purpose)
  (destructuring-bind (&optional public-key sign-count status last) (%device kernel credential-id)
    (declare (ignore last))
    (unless (equal status "active") (fail "credential is not enrolled or was revoked"))
    (let ((new-count (verify-assertion :client-data-json client-data-json
                                       :authenticator-data authenticator-data :signature signature
                                       :challenge challenge :origin origin :rp-id rp-id
                                       :public-key (%unhex public-key)
                                       :stored-sign-count sign-count)))
      (append! kernel "auth.assertion.verified"
               (obj "credential_id" credential-id "sign_count" new-count
                    "purpose" (string-downcase (symbol-name purpose)))))))

;;; Sessions

(defun login (kernel &rest assertion &key credential-id &allow-other-keys)
  "Verify a passkey assertion and start a session. Returns the session token."
  (apply #'%assert kernel :login assertion)
  (let ((token (base64url-encode (ironclad:random-data 32))))
    (append! kernel "auth.session.started"
             (obj "session_hash" (%token-hash token) "credential_id" credential-id
                  "absolute_expires" (iso8601 (+ (now) *session-absolute-seconds*))))
    token))

(defun %session (kernel token)
  (sql-row (kernel-db kernel)
           "SELECT s.credential_id,s.last_seen,s.absolute_expires,s.status,d.status,s.grace_until FROM auth_sessions s JOIN auth_devices d ON d.credential_id=s.credential_id WHERE s.session_hash=?1 LIMIT 1"
           (%token-hash token)))

(defun session-origin (kernel token &key (channel "web"))
  "The verified origin attestation for TOKEN, or NIL when the session is
unknown, ended, expired or its device revoked. Never derived from content.
A session in use is rotated once per *REFRESH-SECONDS*: the second value is
then the new token, which the caller hands back to the client; the old one
keeps working for *ROTATION-GRACE-SECONDS* so requests in flight succeed."
  (destructuring-bind (&optional credential last-seen absolute status device-status grace-until)
      (and token (%session kernel token))
    (let ((now (now))
          (origin (obj "principal_type" "operator" "principal_id" "operator"
                       "auth_method" "passkey" "credential_id" credential
                       "channel" channel "verified_at" (now-iso8601))))
      (cond ((not (equal device-status "active")) nil)
            ((equal status "rotated")
             (and grace-until (< now (parse-iso8601 grace-until)) origin))
            ((not (equal status "active")) nil)
            ((> now (parse-iso8601 absolute))
             (end-session kernel token "absolute expiry") nil)
            ((> now (+ (parse-iso8601 last-seen) *session-idle-seconds*))
             (end-session kernel token "idle expiry") nil)
            ((> now (+ (parse-iso8601 last-seen) *refresh-seconds*))
             (let ((new (base64url-encode (ironclad:random-data 32))))
               (append! kernel "auth.session.rotated"
                        (obj "session_hash" (%token-hash token) "new_hash" (%token-hash new)
                             "grace_until" (iso8601 (+ now *rotation-grace-seconds*))))
               (values origin new)))
            (t origin)))))

(defun end-session (kernel token reason)
  (append! kernel "auth.session.ended" (obj "session_hash" (%token-hash token) "reason" reason)))

;;; Step-up

(defun step-up (kernel token &rest assertion &key credential-id &allow-other-keys)
  "Re-verify presence with a passkey assertion from the session's own device."
  (let ((session (%session kernel token)))
    (unless (and session (equal (first session) credential-id))
      (fail "step-up must use the session's own device"))
    (apply #'%assert kernel :step-up assertion)))

(defun stepped-up-p (kernel token &key (within 300))
  "True when TOKEN's session is valid and its device verified within WITHIN seconds."
  (and (session-origin kernel token)
       (let ((last (fourth (%device kernel (first (%session kernel token))))))
         (and last (<= (- (now) (parse-iso8601 last)) within)))))
