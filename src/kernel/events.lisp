(in-package #:pai-v3.kernel)

;;; Event types and the event envelope (spec §4.2, I9).
;;;
;;; Every type is declared once with its current version, a payload schema and
;;; upcasters from older versions. Append validates; reads upcast, so
;;; consumers only ever see the current version of each type.

(defparameter *payload-limit-bytes* 16384
  "I9: maximum encoded payload size accepted at append.")

(define-condition event-type-error (error)
  ((type :initarg :type :reader event-type-error-type)
   (detail :initarg :detail :reader event-type-error-detail))
  (:report (lambda (c s)
             (format s "Event type ~s: ~a"
                     (event-type-error-type c) (event-type-error-detail c)))))

(defstruct event-type
  name version schema upcasters max-bytes)

(defvar *event-types* (make-hash-table :test 'equal))

(defun %fields (schema)
  "Normalize ((\"key\" :type [:optional]) ...) to (key type optional-p) lists."
  (mapcar (lambda (spec)
            (destructuring-bind (key type &rest flags) spec
              (check-type key string)
              (assert (member type '(:string :integer :number :boolean
                                     :object :array :any)))
              (list key type (and (member :optional flags) t))))
          schema))

(defmacro define-event-type (name &key (version 1) schema upcast max-bytes)
  "Declare event type NAME. UPCAST is an alist (FROM-VERSION . FUNCTION)
where FUNCTION maps a payload at FROM-VERSION to FROM-VERSION + 1. Legacy
v2 events have no version and are treated as version 0. MAX-BYTES raises the
payload limit for a type whose text is someone's whole message."
  `(setf (gethash ,name *event-types*)
         (make-event-type :name ,name :version ,version
                          :schema (%fields ',schema) :max-bytes ,max-bytes
                          :upcasters (list ,@(loop for (from . fn) in upcast
                                                   collect `(cons ,from ,fn))))))

(defun find-event-type (name &optional (errorp t))
  (or (gethash name *event-types*)
      (and errorp (error 'event-type-error :type name :detail "not declared"))))

(defun %value-matches-p (value type)
  (ecase type
    (:any t)
    (:string (stringp value))
    (:integer (integerp value))
    (:number (realp value))
    (:boolean (member value '(t :false)))
    (:object (hash-table-p value))
    (:array (listp value))))

(defun validate-payload (name payload)
  "Signal EVENT-TYPE-ERROR unless PAYLOAD matches NAME's current schema."
  (let ((type (find-event-type name)))
    (unless (hash-table-p payload)
      (error 'event-type-error :type name :detail "payload must be an object"))
    (let ((known (mapcar #'first (event-type-schema type))))
      (loop for (key kind optional) in (event-type-schema type)
            for (value found) = (multiple-value-list (gethash key payload))
            do (cond ((not found)
                      (unless optional
                        (error 'event-type-error :type name
                                                 :detail (format nil "missing ~s" key))))
                     ((not (%value-matches-p value kind))
                      (error 'event-type-error :type name
                                               :detail (format nil "~s is not ~(~a~)" key kind)))))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (unless (member key known :test #'string=)
                   (error 'event-type-error :type name
                                            :detail (format nil "unknown key ~s" key))))
               payload))
    type))

(defun upcast-payload (name from-version payload)
  "Bring PAYLOAD from FROM-VERSION to NAME's current version."
  (let ((type (find-event-type name)))
    (loop for v from from-version below (event-type-version type)
          for fn = (cdr (assoc v (event-type-upcasters type)))
          do (unless fn
               (error 'event-type-error :type name
                                        :detail (format nil "no upcaster from v~d" v)))
             (setf payload (funcall fn payload)))
    (values payload (event-type-version type))))

;;; Envelope

(defstruct event
  id seq type version at caused-by root-id agent-id payload
  (declared t)          ; NIL when the type is not declared (census gap)
  legacy)               ; v2-only envelope fields kept for importers

(defconstant +envelope+ 3
  "Envelope marker for events written by v3. v2 wrote schema_version 1.")

(defun encode-envelope (event)
  (json-encode
   (obj "envelope" +envelope+
        "id" (event-id event)
        "agent_id" (event-agent-id event)
        "at" (event-at event)
        "type" (event-type event)
        "v" (event-version event)
        "payload" (event-payload event)
        "caused_by" (event-caused-by event)
        "root_id" (event-root-id event))))

(defun decode-envelope (json seq)
  "Decode a stored envelope written by v2 or v3, upcasting the payload."
  (let* ((raw (json-parse json))
         (type (obj-get raw "type"))
         (v3-p (eql (obj-get raw "envelope") +envelope+))
         (version (if v3-p (obj-get raw "v" 1) 0))
         (payload (let ((p (obj-get raw "payload")))
                    (if (hash-table-p p) p (obj "value" p))))
         (declared (find-event-type type nil))
         (caused-by (let ((c (obj-get raw "caused_by")))
                      (and (integerp c) c))))
    (when declared
      (multiple-value-setq (payload version) (upcast-payload type version payload)))
    (make-event :id (obj-get raw "id") :seq seq :type type :version version
                :at (or (obj-get raw "at") (obj-get raw "timestamp"))
                :caused-by caused-by
                :root-id (and v3-p (obj-get raw "root_id"))
                :agent-id (obj-get raw "agent_id")
                :payload payload :declared (and declared t)
                :legacy (unless v3-p
                          (obj "tick_id" (obj-get raw "tick_id")
                               "affect_snapshot" (obj-get raw "affect_snapshot"))))))

(defvar *clock* #'get-universal-time
  "Function returning the current universal time. Tests bind a virtual clock.")

(defun now () (funcall *clock*))

(defun iso8601 (universal-time)
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h mi s)))

(defun parse-iso8601 (string)
  "Universal time of a UTC timestamp written by ISO8601 (seconds precision)."
  (flet ((n (start end) (parse-integer string :start start :end end)))
    (encode-universal-time (n 17 19) (n 14 16) (n 11 13) (n 8 10) (n 5 7) (n 0 4) 0)))

(defun now-iso8601 () (iso8601 (now)))

(defun sha256-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (babel:string-to-octets string :encoding :utf-8))))
