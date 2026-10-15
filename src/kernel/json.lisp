(in-package #:pai-v3.kernel)

;;; JSON objects are hash tables with string keys (equal). Arrays are lists.
;;; JSON null is :null, false is :false, true is t. NIL is the empty array,
;;; so (obj ...) omits NIL-valued keys; use :null for an explicit null.

(defun obj (&rest pairs)
  "Build a JSON object from alternating key/value PAIRS. NIL values are omitted."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          unless (null v) do (setf (gethash k h) v))
    h))

(defun obj-get (object key &optional default)
  (if (hash-table-p object)
      (multiple-value-bind (v found) (gethash key object)
        (if found v default))
      default))

(defun %normalize (value)
  "Convert shasht's decoded representation to ours."
  (cond ((hash-table-p value)
         (let ((h (make-hash-table :test 'equal)))
           (maphash (lambda (k v) (setf (gethash k h) (%normalize v))) value)
           h))
        ((and (vectorp value) (not (stringp value)))
         (map 'list #'%normalize value))
        (t value)))

(defun json-parse (string)
  (let ((shasht:*read-default-null-value* :null)
        (shasht:*read-default-false-value* :false)
        (shasht:*read-default-array-format* :vector)
        (shasht:*read-default-object-format* :hash-table))
    (%normalize (shasht:read-json string))))

(defun %write (value stream)
  (cond ((eq value t) (write-string "true" stream))
        ((eq value :false) (write-string "false" stream))
        ((eq value :null) (write-string "null" stream))
        ((null value) (write-string "[]" stream))
        ((stringp value) (shasht:write-json value stream))
        ((integerp value) (format stream "~d" value))
        ((realp value)
         (let ((*read-default-float-format* 'double-float))
           (format stream "~f" (coerce value 'double-float))))
        ((hash-table-p value)
         ;; Keys sorted so identical objects always encode identically.
         (let ((keys (sort (loop for k being the hash-keys of value collect k)
                           #'string<)))
           (write-char #\{ stream)
           (loop for (k . rest) on keys
                 do (shasht:write-json k stream)
                    (write-char #\: stream)
                    (%write (gethash k value) stream)
                    (when rest (write-char #\, stream)))
           (write-char #\} stream)))
        ((listp value)
         (write-char #\[ stream)
         (loop for (v . rest) on value
               do (%write v stream)
                  (when rest (write-char #\, stream)))
         (write-char #\] stream))
        (t (error "Cannot encode ~s as JSON" value))))

(defun json-encode (value)
  "Canonical compact JSON: sorted keys, no whitespace."
  (with-output-to-string (s) (%write value s)))
