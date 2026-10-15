(in-package #:pai-v3.trust)

;;; Keys and unlock (spec §17, NFR ENC-6..8).
;;;
;;; Each agent has one random 256-bit root key. It is never stored in the
;;; clear: the keyring file holds it wrapped (AES-256-GCM) once per unlock
;;; method. Each store's key is derived from the root, the store's name and
;;; its generation, so rekeying one store bumps its generation and touches
;;; nothing else. Adding or removing an unlock method rewraps the root;
;;; no store is rewritten.
;;;
;;; Methods: a recovery code (always created, shown once), a passphrase
;;; (scrypt), a key file kept somewhere other than the data directory, and
;;; the OS keychain. Each method is an adapter that produces a wrapping key.

(defparameter +keyring-version+ 1)
(defparameter +stores+ '("ledger" "secrets" "blobs"))
(defparameter *scrypt-n* 32768 "scrypt cost: 32 MB and about 100 ms per unlock.")

(define-condition keyring-error (error)
  ((message :initarg :message :reader keyring-message))
  (:report (lambda (c s) (write-string (keyring-message c) s))))

(defun %fail (fmt &rest args)
  (error 'keyring-error :message (apply #'format nil fmt args)))

;;; Primitives: HKDF-SHA256 (RFC 5869) and AES-256-GCM, both from ironclad.

(defun %octets (x)
  (etypecase x
    ((vector (unsigned-byte 8)) x)
    (string (babel:string-to-octets x :encoding :utf-8))))

(defun %hmac (key data)
  (let ((mac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac mac (%octets data))
    (ironclad:hmac-digest mac)))

(defun hkdf (ikm info &key (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
  "32 bytes of HKDF-SHA256 output."
  (let ((prk (%hmac salt ikm)))
    (%hmac prk (concatenate '(vector (unsigned-byte 8)) (%octets info) #(1)))))

(defun %seal (kek plaintext aad)
  (let* ((nonce (ironclad:random-data 12))
         (mode (ironclad:make-authenticated-encryption-mode
                :gcm :cipher-name :aes :key kek :initialization-vector nonce)))
    (ironclad:process-associated-data mode (%octets aad))
    (let ((ct (ironclad:encrypt-message mode plaintext)))
      (values nonce ct (ironclad:produce-tag mode)))))

(defun %unseal (kek nonce ct tag aad)
  "The plaintext, or NIL when KEK is wrong or anything was altered."
  (let ((mode (ironclad:make-authenticated-encryption-mode
               :gcm :cipher-name :aes :key kek :initialization-vector nonce)))
    (ironclad:process-associated-data mode (%octets aad))
    (let ((pt (ironclad:decrypt-message mode ct)))
      (and (ironclad:constant-time-equal (ironclad:produce-tag mode) tag) pt))))

;;; Recovery codes: 32 random bytes as Crockford base32 in groups of four.

(defparameter +crockford+ "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(defun %base32 (octets)
  (let ((bits 0) (n 0) (out (make-string-output-stream)))
    (loop for b across octets
          do (setf bits (logior (ash bits 8) b)) (incf n 8)
             (loop while (>= n 5)
                   do (decf n 5)
                      (write-char (char +crockford+ (ldb (byte 5 n) bits)) out)))
    (when (plusp n) (write-char (char +crockford+ (ash (ldb (byte n 0) bits) (- 5 n))) out))
    (get-output-stream-string out)))

(defun %unbase32 (text count)
  (let ((bits 0) (n 0) (out '()))
    (loop for c across (string-upcase (remove-if (lambda (c) (member c '(#\- #\Space))) text))
          for v = (position (case c (#\O #\0) ((#\I #\L) #\1) (t c)) +crockford+)
          do (unless v (%fail "That recovery code has a character that cannot be in one."))
             (setf bits (logior (ash bits 5) v)) (incf n 5)
             (when (>= n 8) (decf n 8) (push (ldb (byte 8 n) bits) out)))
    (let ((octets (coerce (nreverse out) '(vector (unsigned-byte 8)))))
      (unless (= count (length octets)) (%fail "That recovery code is the wrong length."))
      octets)))

(defun %group (text) (format nil "~{~a~^-~}" (loop for i from 0 below (length text) by 4
                                                    collect (subseq text i (min (length text) (+ i 4))))))

;;; The OS keychain is an adapter: (put name octets), (get name) -> octets
;;; or NIL, (delete name). The default chooses by OS; tests install their own.

(defvar *keychain* nil
  "Plist (:put fn :get fn :delete fn), or NIL to use the OS default.")

(defun %run (program args &optional input)
  (with-output-to-string (out)
    (let ((process (sb-ext:run-program program args :search t :output out :error nil
                                                    :input (and input (make-string-input-stream input)))))
      (unless (zerop (sb-ext:process-exit-code process))
        (%fail "~a failed." program)))))

(defun %linux-keychain ()
  ;; Secret Service through secret-tool; the secret travels on stdin, never argv.
  (list :put (lambda (name octets)
               (%run "secret-tool" (list "store" "--label" name "service" "pai-v3" "name" name)
                     (ironclad:byte-array-to-hex-string octets)))
        :get (lambda (name)
               (let ((hex (string-trim '(#\Newline #\Space)
                                       (ignore-errors (%run "secret-tool" (list "lookup" "service" "pai-v3" "name" name))))))
                 (and (= 64 (length hex)) (ironclad:hex-string-to-byte-array hex))))
        :delete (lambda (name) (ignore-errors (%run "secret-tool" (list "clear" "service" "pai-v3" "name" name))))))

(defun keychain ()
  (or *keychain*
      (cond ((member :win32 *features*) (windows-keychain))
            ((member :darwin *features*)
             (%fail "The macOS keychain is not supported yet; use a passphrase or a key file."))
            (t (%linux-keychain)))))

#-win32
(defun windows-keychain ()
  (%fail "The Windows keychain adapter is only built on Windows."))

;;; Keyring file

(defun keyring-path (dir) (merge-pathnames "keyring.json" (uiop:ensure-directory-pathname dir)))

(defun %read-keyring (dir)
  (let ((path (keyring-path dir)))
    (unless (probe-file path)
      (%fail "There is no keyring in ~a. Run setup to create the agent, or point at the right directory." dir))
    (let ((ring (json-parse (uiop:read-file-string path))))
      (unless (eql +keyring-version+ (obj-get ring "version"))
        (%fail "The keyring in ~a is version ~a; this build reads version ~a."
               dir (obj-get ring "version") +keyring-version+))
      ring)))

(defun %write-keyring (dir ring)
  "Write atomically: a crash leaves the old keyring or the new one."
  (let* ((path (keyring-path dir))
         (tmp (make-pathname :type "tmp" :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (json-encode ring) out)
      (finish-output out))
    (rename-file tmp path)))

(defun %aad (agent method id) (format nil "pai-v3 keyring|~a|~a|~a" agent method id))

(defun %wrapping-key (method wrap &key passphrase recovery-code)
  "The key that unwraps WRAP, or NIL when this method cannot produce one now."
  (cond
    ((string= method "recovery")
     (and recovery-code (hkdf (%unbase32 recovery-code 32) "pai-v3 recovery")))
    ((string= method "passphrase")
     (and passphrase
          (ironclad:derive-key (ironclad:make-kdf :scrypt-kdf :n (obj-get wrap "n") :r 8 :p 1)
                               (%octets passphrase) (base64url-decode (obj-get wrap "salt")) 1 32)))
    ((string= method "keyfile")
     (let ((path (obj-get wrap "path")))
       (and (probe-file path)
            (hkdf (ironclad:hex-string-to-byte-array (string-trim '(#\Newline #\Space #\Return)
                                                                  (uiop:read-file-string path)))
                  "pai-v3 keyfile"))))
    ((string= method "keychain")
     (ignore-errors (funcall (getf (keychain) :get) (obj-get wrap "name"))))))

(defun %wrap (agent root method id &rest fields)
  (let ((wrap (apply #'obj "method" method "id" id "alg" "aes-256-gcm" fields)))
    (lambda (kek)
      (multiple-value-bind (nonce ct tag) (%seal kek root (%aad agent method id))
        (setf (gethash "nonce" wrap) (base64url-encode nonce)
              (gethash "ct" wrap) (base64url-encode ct)
              (gethash "tag" wrap) (base64url-encode tag))
        wrap))))

(defun %make-wrap (agent root method &key passphrase keyfile keychain-name recovery-octets)
  "A wrap entry for METHOD, creating what the method needs (salt, key file,
keychain entry)."
  (let ((id (format nil "~a-~a" method (subseq (ironclad:byte-array-to-hex-string (ironclad:random-data 4)) 0 8))))
    (cond
      ((string= method "recovery")
       (funcall (%wrap agent root method id) (hkdf recovery-octets "pai-v3 recovery")))
      ((string= method "passphrase")
       (when (< (length passphrase) 12) (%fail "Use a passphrase of at least 12 characters."))
       (let ((salt (ironclad:random-data 16)))
         (funcall (%wrap agent root method id "kdf" "scrypt" "n" *scrypt-n* "salt" (base64url-encode salt))
                  (ironclad:derive-key (ironclad:make-kdf :scrypt-kdf :n *scrypt-n* :r 8 :p 1)
                                       (%octets passphrase) salt 1 32))))
      ((string= method "keyfile")
       (let ((secret (ironclad:random-data 32)))
         (when (probe-file keyfile) (%fail "~a already exists; choose a new path for the key file." keyfile))
         (with-open-file (out keyfile :direction :output :external-format :utf-8)
           (write-line (ironclad:byte-array-to-hex-string secret) out))
         (funcall (%wrap agent root method id "path" (namestring keyfile)) (hkdf secret "pai-v3 keyfile"))))
      ((string= method "keychain")
       (let ((secret (ironclad:random-data 32)) (name (or keychain-name (format nil "pai-v3:~a" agent))))
         (funcall (getf (keychain) :put) name secret)
         (funcall (%wrap agent root method id "name" name) secret)))
      (t (%fail "Unknown unlock method ~a." method)))))

;;; API

(defun keyring-init (dir agent &key (methods '("keychain")) passphrase keyfile keychain-name)
  "Create the keyring for AGENT in DIR with a new root key, wrapped for a
recovery code and each of METHODS. Returns the recovery code, which is shown
once and never stored. Refuses to replace an existing keyring."
  (ensure-directories-exist (uiop:ensure-directory-pathname dir))
  (when (probe-file (keyring-path dir))
    (%fail "~a already has a keyring; it is never replaced." dir))
  (let* ((root (ironclad:random-data 32))
         (recovery (ironclad:random-data 32))
         (wraps (cons (%make-wrap agent root "recovery" :recovery-octets recovery)
                      (mapcar (lambda (m) (%make-wrap agent root m :passphrase passphrase :keyfile keyfile
                                                                   :keychain-name keychain-name))
                              methods))))
    (%write-keyring dir (obj "version" +keyring-version+ "agent" agent "created_at" (now-iso8601)
                             "cipher" "sqlite3mc chacha20-poly1305"
                             "stores" (let ((s (obj))) (dolist (n +stores+ s) (setf (gethash n s) 1)))
                             "wraps" wraps))
    (fill root 0)
    (%group (%base32 recovery))))

(defun unlock (dir &key passphrase recovery-code)
  "The root key, from the first unlock method that can produce it. Unattended
methods (keychain, key file) are tried first."
  (let* ((ring (%read-keyring dir)) (agent (obj-get ring "agent")))
    (dolist (method '("keychain" "keyfile" "passphrase" "recovery")
                    (%fail "No unlock method opened the keyring in ~a. Give the passphrase or recovery code." dir))
      (dolist (wrap (obj-get ring "wraps"))
        (when (string= method (obj-get wrap "method"))
          (let ((kek (%wrapping-key method wrap :passphrase passphrase :recovery-code recovery-code)))
            (when kek
              (let ((root (%unseal kek (base64url-decode (obj-get wrap "nonce"))
                                   (base64url-decode (obj-get wrap "ct"))
                                   (base64url-decode (obj-get wrap "tag"))
                                   (%aad agent method (obj-get wrap "id")))))
                (when root (return-from unlock root))))))))))

(defun store-key (dir root store)
  "The key for STORE (ledger, secrets, blobs) at its current generation."
  (let* ((ring (%read-keyring dir))
         (generation (or (obj-get (obj-get ring "stores") store)
                         (%fail "The keyring has no store named ~a." store))))
    (hkdf root (format nil "pai-v3 store|~a|~a|~a" (obj-get ring "agent") store generation))))

(defun next-store-generation (dir store)
  "Bump STORE's generation and return it; the caller rekeys the store to the
new key in the same maintenance step."
  (let ((ring (%read-keyring dir)))
    (incf (gethash store (obj-get ring "stores")))
    (%write-keyring dir ring)
    (gethash store (obj-get ring "stores"))))

(defun unlock-methods (dir)
  "Enrolled methods as (id method)."
  (mapcar (lambda (w) (list (obj-get w "id") (obj-get w "method"))) (obj-get (%read-keyring dir) "wraps")))

(defun add-unlock-method (dir root method &key passphrase keyfile keychain-name)
  "Wrap ROOT for METHOD too. Returns the new method id."
  (let* ((ring (%read-keyring dir))
         (wrap (%make-wrap (obj-get ring "agent") root method
                           :passphrase passphrase :keyfile keyfile :keychain-name keychain-name)))
    (setf (gethash "wraps" ring) (append (obj-get ring "wraps") (list wrap)))
    (%write-keyring dir ring)
    (obj-get wrap "id")))

(defun remove-unlock-method (dir id)
  "Remove one method. The last recovery code cannot be removed, so a way in
always remains that needs nothing but what the operator keeps offline."
  (let* ((ring (%read-keyring dir))
         (wraps (obj-get ring "wraps"))
         (target (find id wraps :key (lambda (w) (obj-get w "id")) :test #'equal)))
    (unless target (%fail "No unlock method ~a." id))
    (when (and (string= "recovery" (obj-get target "method"))
               (= 1 (count "recovery" wraps :key (lambda (w) (obj-get w "method")) :test #'equal)))
      (%fail "That is the only recovery code; create a new one before removing it."))
    (when (string= "keychain" (obj-get target "method"))
      (ignore-errors (funcall (getf (keychain) :delete) (obj-get target "name"))))
    (setf (gethash "wraps" ring) (remove target wraps))
    (%write-keyring dir ring)
    id))

(defun new-recovery-code (dir root)
  "Wrap ROOT for a fresh recovery code and return the code (shown once)."
  (let* ((ring (%read-keyring dir))
         (recovery (ironclad:random-data 32))
         (wrap (%make-wrap (obj-get ring "agent") root "recovery" :recovery-octets recovery)))
    (setf (gethash "wraps" ring) (append (obj-get ring "wraps") (list wrap)))
    (%write-keyring dir ring)
    (values (%group (%base32 recovery)) (obj-get wrap "id"))))
