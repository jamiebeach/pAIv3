(defpackage #:pai-v3.trust
  (:use #:cl #:pai-v3.kernel)
  (:export
   ;; encoding
   #:base64url-encode #:base64url-decode #:cbor-decode
   ;; webauthn
   #:verify-registration #:verify-assertion #:webauthn-error #:es256-verify
   #:der-to-raw-signature
   ;; devices, sessions, origin
   #:new-challenge #:take-challenge #:enroll-device #:revoke-device #:active-devices
   #:login #:session-origin #:end-session #:step-up #:stepped-up-p
   #:*session-idle-seconds* #:*session-absolute-seconds*
   ;; keys
   #:keyring-init #:unlock #:store-key #:next-store-generation #:unlock-methods
   #:add-unlock-method #:remove-unlock-method #:new-recovery-code #:keyring-error
   #:keyring-path #:*keychain* #:*scrypt-n* #:hkdf
   ;; recovery
   #:issue-recovery-codes #:redeem-recovery-code #:recovery-codes-left #:issue-pairing-code
   ;; secrets
   #:open-secrets #:close-secrets #:set-secret #:remove-secret #:secret-names #:redact
   #:secret-hmac #:secret-hmac-valid-p #:ensure-es256-secret #:secret-es256-public #:secret-es256-sign
   ;; google
   #:+google-scopes+ #:google-connect #:google-access-ready #:google-disconnect #:google-access-secret))
