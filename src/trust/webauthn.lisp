(in-package #:pai-v3.trust)

;;; WebAuthn relying-party verification (W3C WebAuthn Level 2, §7.1 and §7.2)
;;; for ES256 (ECDSA P-256 with SHA-256), the algorithm platform passkeys use.
;;; Attestation statements are not verified: devices are enrolled by the
;;; operator, and "none" attestation is the platform default. User presence
;;; and user verification are required.

(define-condition webauthn-error (error)
  ((reason :initarg :reason :reader webauthn-error-reason))
  (:report (lambda (c s) (format s "WebAuthn verification failed: ~a" (webauthn-error-reason c)))))

(defun fail (fmt &rest args)
  (error 'webauthn-error :reason (apply #'format nil fmt args)))

(defun sha256 (octets) (ironclad:digest-sequence :sha256 octets))

(defun octets (string) (babel:string-to-octets string :encoding :utf-8))

(defun concat-octets (&rest parts)
  (let ((out (make-array (reduce #'+ parts :key #'length) :element-type '(unsigned-byte 8)))
        (pos 0))
    (dolist (p parts out)
      (replace out p :start1 pos)
      (incf pos (length p)))))

(defun der-to-raw-signature (der)
  "ECDSA-Sig-Value ::= SEQUENCE { r INTEGER, s INTEGER } -> 64-byte r||s."
  (let ((pos 0))
    (labels ((byte* () (prog1 (aref der pos) (incf pos)))
             (len () (let ((b (byte*)))
                       (if (< b #x80) b
                           (loop repeat (logand b #x7f) for v = (byte*) then (logior (ash v 8) (byte*))
                                 finally (return v)))))
             (int32 ()
               (unless (= (byte*) #x02) (fail "signature: expected INTEGER"))
               (let* ((n (len))
                      (bytes (subseq der pos (+ pos n))))
                 (incf pos n)
                 (let ((trimmed (subseq bytes (or (position-if #'plusp bytes) (length bytes)))))
                   (when (> (length trimmed) 32) (fail "signature: integer too long"))
                   (concat-octets (make-array (- 32 (length trimmed)) :element-type '(unsigned-byte 8)
                                                                      :initial-element 0)
                                  trimmed)))))
      (unless (= (byte*) #x30) (fail "signature: expected SEQUENCE"))
      (len)
      (concat-octets (int32) (int32)))))

(defun es256-verify (public-point message-digest raw-signature)
  "Verify RAW-SIGNATURE (r||s) over the 32-byte MESSAGE-DIGEST with the
uncompressed P-256 PUBLIC-POINT (0x04||x||y)."
  (ironclad:verify-signature (ironclad:make-public-key :secp256r1 :y public-point)
                             message-digest raw-signature))

(defun %client-data (client-data-json expected-type challenge origin)
  (let ((data (json-parse (babel:octets-to-string client-data-json :encoding :utf-8))))
    (unless (equal expected-type (obj-get data "type")) (fail "client data type ~s" (obj-get data "type")))
    (unless (equal challenge (obj-get data "challenge")) (fail "challenge does not match"))
    (unless (equal origin (obj-get data "origin")) (fail "origin ~s not allowed" (obj-get data "origin")))
    data))

(defun %auth-data (auth-data rp-id)
  "Parse authenticator data; check RP id hash, user presence and verification."
  (when (< (length auth-data) 37) (fail "authenticator data too short"))
  (unless (equalp (subseq auth-data 0 32) (sha256 (octets rp-id))) (fail "RP id hash mismatch"))
  (let ((flags (aref auth-data 32)))
    (unless (logbitp 0 flags) (fail "user not present"))
    (unless (logbitp 2 flags) (fail "user not verified"))
    (values flags
            (loop for i from 33 below 37 for v = (aref auth-data i) then (logior (ash v 8) (aref auth-data i))
                  finally (return v)))))

(defun %cose-es256-point (cose)
  "COSE_Key (alist) for EC2/P-256/ES256 -> uncompressed point."
  (flet ((key (k) (cdr (assoc k cose))))
    (unless (and (eql 2 (key 1)) (eql -7 (key 3)) (eql 1 (key -1)))
      (fail "only ES256 P-256 credentials are supported"))
    (concat-octets (make-array 1 :element-type '(unsigned-byte 8) :initial-element 4)
                   (key -2) (key -3))))

(defun verify-registration (&key client-data-json attestation-object challenge origin rp-id)
  "Verify a navigator.credentials.create() response. Inputs are octet
vectors except CHALLENGE (base64url), ORIGIN and RP-ID. Returns a plist
(:credential-id base64url :public-key octets :sign-count n)."
  (%client-data client-data-json "webauthn.create" challenge origin)
  (let* ((att (cbor-decode attestation-object))
         (auth-data (cdr (assoc "authData" att :test #'equal))))
    (unless auth-data (fail "attestation object has no authData"))
    (multiple-value-bind (flags sign-count) (%auth-data auth-data rp-id)
      (unless (logbitp 6 flags) (fail "no attested credential data"))
      (let* ((id-length (logior (ash (aref auth-data 53) 8) (aref auth-data 54)))
             (credential-id (subseq auth-data 55 (+ 55 id-length)))
             (cose (cbor-decode auth-data :start (+ 55 id-length))))
        (list :credential-id (base64url-encode credential-id)
              :public-key (%cose-es256-point cose)
              :sign-count sign-count)))))

(defun verify-assertion (&key client-data-json authenticator-data signature
                              challenge origin rp-id public-key stored-sign-count)
  "Verify a navigator.credentials.get() response against a stored key.
SIGNATURE is DER. Returns the new sign count."
  (%client-data client-data-json "webauthn.get" challenge origin)
  (multiple-value-bind (flags sign-count) (%auth-data authenticator-data rp-id)
    (declare (ignore flags))
    (unless (es256-verify public-key
                          (sha256 (concat-octets authenticator-data (sha256 client-data-json)))
                          (der-to-raw-signature signature))
      (fail "signature does not verify"))
    ;; Counters that stay 0 are allowed (many platform authenticators);
    ;; a counter that goes backwards indicates a cloned credential.
    (when (and (plusp sign-count) (<= sign-count stored-sign-count))
      (fail "sign count did not increase"))
    sign-count))
