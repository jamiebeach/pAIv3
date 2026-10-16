(in-package #:pai-v3.trust)

;;; Google sign-in for mail and calendar (operator request 2026-10-15: built
;;; in, with credentials the agent cannot reach). Everything that touches a
;;; token is here, in the trust module:
;;;
;;; - The client secret is an ordinary secret, google-client-secret, set by the
;;;   operator and bound to the token host.
;;; - Connecting exchanges Google's one-time code for tokens. The refresh token
;;;   is stored as google-refresh-<account>, bound to the token host; it never
;;;   leaves this function.
;;; - An access token is minted from it when needed and stored as
;;;   google-access-<account>, bound to the API hosts, so a reader names it as
;;;   a header and the egress guard attaches it. Callers learn only whether it
;;;   worked.
;;;
;;; Token requests name the client secret and refresh token by placeholder; the
;;; egress guard fills them in only for the token host.

(defparameter +google-scopes+
  '("https://www.googleapis.com/auth/gmail.readonly" "https://www.googleapis.com/auth/calendar.readonly")
  "Read-only: even a leaked token can only read what the agent already may.")

(defvar *google-access-until* (make-hash-table :test 'equal) "account -> universal time its access token lapses")
(defvar *google-lock* (bt:make-lock "google tokens"))

(defun %account-ok (account)
  (and (stringp account) (< 0 (length account) 41)
       (every (lambda (c) (or (alphanumericp c) (find c "-_"))) account)))

(defun google-access-secret (account) (format nil "google-access-~a" account))
(defun %refresh-secret (account) (format nil "google-refresh-~a" account))

(defun %form (pairs)
  "PAIRS as a form body. A value that is a placeholder stays as it is."
  (format nil "~{~a~^&~}"
          (loop for (k v) on pairs by #'cddr
                collect (format nil "~a=~a" k (if (search "{{secret:" v) v (quri:url-encode v))))))

(defun %token-call (token-url pairs allow-private)
  "POST PAIRS to the token endpoint. Returns the parsed reply, or signals
with Google's error code only: a reply may hold tokens, so it is never shown."
  (multiple-value-bind (out status)
      (http-request :post token-url :headers (list (cons "Content-Type" "application/x-www-form-urlencoded"))
                                    :body (%form pairs) :allow-private allow-private
                                    :connect-timeout 10 :read-timeout 20 :max-redirects 0)
    (let ((parsed (ignore-errors (json-parse out))))
      (unless (and (eql status 200) (hash-table-p parsed))
        (error 'keyring-error :message (format nil "Google refused (~a~@[: ~a~])" status
                                               (and (hash-table-p parsed) (obj-get parsed "error")))))
      parsed)))

(defun %keep-access (account reply api-hosts)
  (let ((token (obj-get reply "access_token")) (life (obj-get reply "expires_in" 3600)))
    (unless (stringp token) (error 'keyring-error :message "Google gave no access token."))
    (set-secret (google-access-secret account) token api-hosts)
    (setf (gethash account *google-access-until*) (+ (get-universal-time) (- life 120)))))

(defun google-connect (account code &key client-id redirect-uri token-url api-hosts allow-private)
  "Finish connecting ACCOUNT with Google's one-time CODE. Returns the scopes
granted; the tokens stay in the secret store."
  (unless (%account-ok account) (error 'keyring-error :message "An account name is letters, digits, - and _."))
  (let* ((reply (%token-call token-url (list "code" code "client_id" client-id
                                             "client_secret" "{{secret:google-client-secret}}"
                                             "redirect_uri" redirect-uri "grant_type" "authorization_code")
                             allow-private))
         (refresh (obj-get reply "refresh_token")))
    (unless (stringp refresh)
      (error 'keyring-error :message "Google gave no refresh token. Remove this app's access in the Google account, then connect again."))
    (bt:with-lock-held (*google-lock*)
      (set-secret (%refresh-secret account) refresh (list (quri:uri-host (quri:uri token-url))))
      (%keep-access account reply api-hosts))
    (obj-get reply "scope" "")))

(defun google-access-ready (account &key client-id token-url api-hosts allow-private)
  "Make sure ACCOUNT has a live access token, minting one if needed. T, or signals."
  (bt:with-lock-held (*google-lock*)
    (unless (and (> (gethash account *google-access-until* 0) (get-universal-time))
                 (find (google-access-secret account) (secret-names) :key #'first :test #'string=))
      (unless (find (%refresh-secret account) (secret-names) :key #'first :test #'string=)
        (error 'keyring-error :message (format nil "~a is not connected to Google." account)))
      (%keep-access account
                    (%token-call token-url (list "refresh_token" (format nil "{{secret:~a}}" (%refresh-secret account))
                                                 "client_id" client-id
                                                 "client_secret" "{{secret:google-client-secret}}"
                                                 "grant_type" "refresh_token")
                                 allow-private)
                    api-hosts))
    t))

(defun google-disconnect (account)
  (bt:with-lock-held (*google-lock*)
    (remove-secret (%refresh-secret account))
    (remove-secret (google-access-secret account))
    (remhash account *google-access-until*))
  t)
