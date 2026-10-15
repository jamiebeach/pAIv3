(in-package #:pai-v3.test)
(use-package '#:pai-v3.trust)

;;; Shared by tests that sign in.

;;; A software authenticator produces real WebAuthn structures: CBOR
;;; attestation objects, authenticator data and DER ECDSA signatures.

(defun u8 (&rest bytes) (coerce bytes '(simple-array (unsigned-byte 8) (*))))
(defun cat (&rest parts) (apply #'pai-v3.trust::concat-octets parts))
(defun utf8 (s) (babel:string-to-octets s :encoding :utf-8))
(defun sha (o) (ironclad:digest-sequence :sha256 o))

(defun cbor-head (major n)
  (cond ((< n 24) (u8 (logior (ash major 5) n)))
        ((< n 256) (u8 (logior (ash major 5) 24) n))
        (t (u8 (logior (ash major 5) 25) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))))
(defun cbor-int (n) (if (minusp n) (cbor-head 1 (- -1 n)) (cbor-head 0 n)))
(defun cbor-bytes (o) (cat (cbor-head 2 (length o)) o))
(defun cbor-text (s) (cat (cbor-head 3 (length (utf8 s))) (utf8 s)))
(defun cbor-map (&rest kv) (apply #'cat (cbor-head 5 (/ (length kv) 2)) kv))

(defun int-der (bytes)
  (let* ((trim (subseq bytes (or (position-if #'plusp bytes) 31)))
         (body (if (>= (aref trim 0) #x80) (cat (u8 0) trim) trim)))
    (cat (u8 2 (length body)) body)))

(defun raw-to-der (raw)
  (let ((body (cat (int-der (subseq raw 0 32)) (int-der (subseq raw 32)))))
    (cat (u8 #x30 (length body)) body)))

(defstruct authn private public cred-id (count 0))

(defun make-software-authenticator ()
  (multiple-value-bind (priv pub) (ironclad:generate-key-pair :secp256r1)
    (make-authn :private priv :public (getf (ironclad:destructure-public-key pub) :y)
                :cred-id (ironclad:random-data 16))))

(defparameter *rp* "agent.example")
(defparameter *origin* "https://agent.example")

(defun auth-data (a &key attested (flags #x05))
  (let ((count (incf (authn-count a))))
    (cat (sha (utf8 *rp*))
         (u8 (if attested (logior flags #x40) flags))
         (u8 (ldb (byte 8 24) count) (ldb (byte 8 16) count) (ldb (byte 8 8) count) (ldb (byte 8 0) count))
         (if attested
             (let ((pub (authn-public a)))
               (cat (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
                    (u8 0 (length (authn-cred-id a))) (authn-cred-id a)
                    (cbor-map (cbor-int 1) (cbor-int 2) (cbor-int 3) (cbor-int -7)
                              (cbor-int -1) (cbor-int 1)
                              (cbor-int -2) (cbor-bytes (subseq pub 1 33))
                              (cbor-int -3) (cbor-bytes (subseq pub 33 65)))))
             (u8)))))

(defun client-data (type challenge &optional (origin *origin*))
  (utf8 (json-encode (obj "type" type "challenge" challenge "origin" origin))))

(defun register (k a &key (authorized-by :bootstrap) (name "phone"))
  (let ((challenge (new-challenge :register)))
    (enroll-device k :name name :challenge challenge :origin *origin* :rp-id *rp*
                     :authorized-by authorized-by
                     :client-data-json (client-data "webauthn.create" challenge)
                     :attestation-object (cbor-map (cbor-text "fmt") (cbor-text "none")
                                                   (cbor-text "attStmt") (cbor-map)
                                                   (cbor-text "authData")
                                                   (cbor-bytes (auth-data a :attested t))))))

(defun assertion (a purpose &key (origin *origin*) (flags #x05) tamper)
  (let* ((challenge (new-challenge purpose))
         (cd (client-data "webauthn.get" challenge origin))
         (ad (auth-data a :flags flags))
         (sig (ironclad:sign-message (authn-private a) (sha (cat ad (sha cd))))))
    (when tamper (setf (aref ad 36) (logxor 1 (aref ad 36))))
    (list :credential-id (base64url-encode (authn-cred-id a)) :client-data-json cd
          :authenticator-data ad :signature (raw-to-der sig)
          :challenge challenge :origin *origin* :rp-id *rp*)))

