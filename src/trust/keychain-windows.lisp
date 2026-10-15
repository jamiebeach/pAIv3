(in-package #:pai-v3.trust)

;;; Windows Credential Manager as the keychain (spec §17). Loaded only on
;;; Windows. A generic credential per agent holds the 32-byte wrapping
;;; secret; the user's Windows logon protects it (DPAPI).
;;; Verified by the native Windows test run (bead v3-fu8.6), not in the
;;; Linux container.

(cffi:define-foreign-library advapi32 (:windows "advapi32.dll"))
(cffi:use-foreign-library advapi32)

(cffi:defcstruct credential
  (flags :uint32) (type :uint32) (target-name :pointer) (comment :pointer)
  (last-written-low :uint32) (last-written-high :uint32)
  (blob-size :uint32) (blob :pointer) (persist :uint32)
  (attribute-count :uint32) (attributes :pointer) (target-alias :pointer) (user-name :pointer))

(cffi:defcfun ("CredWriteW" %cred-write) :int (credential :pointer) (flags :uint32))
(cffi:defcfun ("CredReadW" %cred-read) :int (target :pointer) (type :uint32) (flags :uint32) (out :pointer))
(cffi:defcfun ("CredDeleteW" %cred-delete) :int (target :pointer) (type :uint32) (flags :uint32))
(cffi:defcfun ("CredFree" %cred-free) :void (buffer :pointer))

(defconstant +cred-type-generic+ 1)
(defconstant +cred-persist-local-machine+ 2)

(defmacro %with-wide ((var string) &body body)
  `(let ((,var (cffi:foreign-string-alloc ,string :encoding :utf-16le)))
     (unwind-protect (progn ,@body) (cffi:foreign-string-free ,var))))

(defun %win-put (name octets)
  (%with-wide (target name)
    (cffi:with-foreign-objects ((cred '(:struct credential)) (blob :uint8 (length octets)))
      (dotimes (i (length octets)) (setf (cffi:mem-aref blob :uint8 i) (aref octets i)))
      (dotimes (i (cffi:foreign-type-size '(:struct credential)))
        (setf (cffi:mem-aref cred :uint8 i) 0))
      (cffi:with-foreign-slots ((type target-name blob-size persist) cred (:struct credential))
        (setf type +cred-type-generic+ target-name target blob-size (length octets)
              persist +cred-persist-local-machine+))
      (setf (cffi:foreign-slot-value cred '(:struct credential) 'blob) blob)
      (prog1 (when (zerop (%cred-write cred 0))
               (%fail "Windows Credential Manager refused to store ~a." name))
        (dotimes (i (length octets)) (setf (cffi:mem-aref blob :uint8 i) 0))))))

(defun %win-get (name)
  (%with-wide (target name)
    (cffi:with-foreign-object (out :pointer)
      (unless (zerop (%cred-read target +cred-type-generic+ 0 out))
        (let ((cred (cffi:mem-ref out :pointer)))
          (unwind-protect
               (let* ((n (cffi:foreign-slot-value cred '(:struct credential) 'blob-size))
                      (blob (cffi:foreign-slot-value cred '(:struct credential) 'blob))
                      (octets (make-array n :element-type '(unsigned-byte 8))))
                 (dotimes (i n octets) (setf (aref octets i) (cffi:mem-aref blob :uint8 i))))
            (%cred-free cred)))))))

(defun %win-delete (name)
  (%with-wide (target name) (%cred-delete target +cred-type-generic+ 0)))

(defun windows-keychain ()
  (list :put #'%win-put :get #'%win-get :delete #'%win-delete))
