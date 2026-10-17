(in-package #:pai-v3.mind)

;;; Vectors: the one place text is embedded, vectors are encoded, compared
;;; and found nearest, and missing embeddings are backfilled (design note
;;; interaction-recall §11: one place for each thing). Memories, graph
;;; facts, interaction gists and impressions all go through it; each kind is
;;; an embedding source registered with DEFINE-EMBEDDING-SOURCE.
;;;
;;; Vectors are kept in pgvector's binary send format (big-endian float32
;;; with a 4-byte header), as v2 stored them. The distance functions are SQL
;;; functions reading each vector in place in SQLite's memory, so a search
;;; decodes no vectors into the Lisp heap. A half-precision unit copy of a
;;; vector, in a table of (id, scope, vec), makes a full scan small enough to
;;; stay in the page cache.

(defparameter *embedding-model* "nomic-embed-text")
(defvar *embedding-url* "http://localhost:11434")

(define-setting "embedding.url"
  :type :string :group "Memory" :default *embedding-url*
  :description "The embedding server (Ollama API). A configured local service, so it may be on a private address.")
(define-setting "embedding.model"
  :type :string :group "Memory" :default *embedding-model*
  :description "Embedding model. Changing it makes old vectors incomparable: re-embedding is a separately approved project (BUD-3).")

;;; Vector encoding (pgvector-send-v1)

(defun encode-vector (floats)
  (let* ((n (length floats))
         (out (make-array (+ 4 (* 4 n)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref out 0) (ldb (byte 8 8) n) (aref out 1) (ldb (byte 8 0) n))
    (loop for f in (coerce floats 'list) for pos from 4 by 4
          for bits = (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce f 'single-float)))
          do (loop for j below 4 do (setf (aref out (+ pos j)) (ldb (byte 8 (* 8 (- 3 j))) bits))))
    out))

(defun decode-vector (octets)
  (let ((n (logior (ash (aref octets 0) 8) (aref octets 1))))
    (loop for i below n
          for pos = (+ 4 (* 4 i))
          collect (sb-kernel:make-single-float
                   (let ((u (logior (ash (aref octets pos) 24) (ash (aref octets (+ pos 1)) 16)
                                    (ash (aref octets (+ pos 2)) 8) (aref octets (+ pos 3)))))
                     (if (logbitp 31 u) (- u (ash 1 32)) u))))))

;;; Distance functions registered on every connection

(cffi:defcfun ("sqlite3_create_function_v2" %create-function) :int
  (db :pointer) (name :string) (nargs :int) (flags :int) (app :pointer)
  (fn :pointer) (step :pointer) (final :pointer) (destroy :pointer))
(cffi:defcfun ("sqlite3_value_blob" %value-blob) :pointer (value :pointer))
(cffi:defcfun ("sqlite3_value_bytes" %value-bytes) :int (value :pointer))
(cffi:defcfun ("sqlite3_result_double" %result-double) :void (ctx :pointer) (value :double))
(cffi:defcfun ("sqlite3_result_null" %result-null) :void (ctx :pointer))

(declaim (inline %be-float))
(defun %be-float (sap offset)
  "Big-endian float32 at OFFSET of SAP, as a double. One 32-bit load and a
byte swap; nothing is allocated."
  (declare (optimize (speed 3) (safety 0)) (type fixnum offset) (type sb-sys:system-area-pointer sap))
  (let* ((le (sb-sys:sap-ref-32 sap offset))
         (be (logior (ash (ldb (byte 8 0) le) 24) (ash (ldb (byte 8 8) le) 16)
                     (ash (ldb (byte 8 16) le) 8) (ldb (byte 8 24) le))))
    (declare (type (unsigned-byte 32) le be))
    (coerce (sb-kernel:make-single-float (if (logbitp 31 be) (- be (ash 1 32)) be)) 'double-float)))

(cffi:defcallback cosine-distance :void ((ctx :pointer) (argc :int) (argv :pointer))
  (declare (ignore argc) (optimize (speed 3) (safety 0)))
  (let* ((a (cffi:mem-aref argv :pointer 0))
         (b (cffi:mem-aref argv :pointer 1))
         (na (%value-bytes a))
         (nb (%value-bytes b)))
    (declare (type fixnum na nb))
    (if (or (/= na nb) (< na 8))
        (%result-null ctx)
        (let ((pa (%value-blob a)) (pb (%value-blob b))  ; SAPs in SBCL
              (dot 0d0) (ma 0d0) (mb 0d0))
          (declare (type double-float dot ma mb))
          (loop for offset of-type fixnum from 4 below na by 4
                do (let ((x (%be-float pa offset)) (y (%be-float pb offset)))
                     (declare (type double-float x y))
                     (incf dot (* x y)) (incf ma (* x x)) (incf mb (* y y))))
          (if (or (zerop ma) (zerop mb))
              (%result-null ctx)
              (%result-double ctx (- 1d0 (/ dot (sqrt (* ma mb))))))))))

(defun %half-bits (x)
  "X (a double in [-1,1] or so) as IEEE 754 half-precision bits, rounded to
nearest."
  (let* ((bits (sb-kernel:single-float-bits (coerce x 'single-float)))
         (sign (if (minusp bits) #x8000 0))
         (e (ldb (byte 8 23) bits))
         (m (ldb (byte 23 0) bits))
         (he (+ (- e 127) 15)))
    (cond ((zerop e) sign)
          ((>= he 31) (logior sign #x7bff))                       ; clamp to the largest finite
          ((<= he 0)                                              ; subnormal half
           (if (< he -10)
               sign
               (logior sign (round (logior m #x800000) (ash 1 (- 14 he))))))
          (t (let ((half (+ (ash he 10) (round m (ash 1 13)))))   ; a mantissa carry rolls into the exponent
               (logior sign (min half #x7bff)))))))

(defun unit-vector-octets (floats)
  "FLOATS normalized to unit length (in double precision), as little-endian
half-precision floats: the compact scoring copy (spec §17; 1.5 KB for 768
dimensions). The pgvector original is kept for exact re-ranking."
  (let* ((norm (sqrt (loop for f in floats sum (* (coerce f 'double-float) (coerce f 'double-float)))))
         (out (make-array (* 2 (length floats)) :element-type '(unsigned-byte 8))))
    (loop for f in floats for pos from 0 by 2
          for bits = (%half-bits (if (zerop norm) 0d0 (/ f norm)))
          do (setf (aref out pos) (ldb (byte 8 0) bits)
                   (aref out (1+ pos)) (ldb (byte 8 8) bits)))
    out))

(declaim (type (simple-array double-float (65536)) +half-values+))
(sb-ext:defglobal +half-values+
    (let ((table (make-array 65536 :element-type 'double-float)))
      (dotimes (u 65536 table)
        (let ((s (if (logbitp 15 u) -1d0 1d0)) (e (ldb (byte 5 10) u)) (m (ldb (byte 10 0) u)))
          (setf (aref table u)
                (cond ((= e 0) (* s m (expt 2d0 -24)))
                      ((= e 31) 0d0)
                      (t (* s (+ 1d0 (/ m 1024d0)) (expt 2d0 (- e 15))))))))))

(cffi:defcallback unit-dot-distance :void ((ctx :pointer) (argc :int) (argv :pointer))
  (declare (ignore argc) (optimize (speed 3) (safety 0)))
  (let* ((a (cffi:mem-aref argv :pointer 0))
         (b (cffi:mem-aref argv :pointer 1))
         (n (%value-bytes a)))
    (declare (type fixnum n))
    (if (or (/= n (the fixnum (%value-bytes b))) (< n 2))
        (%result-null ctx)
        (let ((pa (%value-blob a)) (pb (%value-blob b)) (dot 0d0) (table +half-values+))
          (declare (type double-float dot) (type sb-sys:system-area-pointer pa pb))
          (loop for offset of-type fixnum from 0 below n by 2
                do (incf dot (* (aref table (sb-sys:sap-ref-16 pa offset))
                                (aref table (sb-sys:sap-ref-16 pb offset)))))
          (%result-double ctx (- 1d0 dot))))))

(defun %register-distance-functions (handle)
  "pai_unit_distance shortlists recall over the half-precision unit copies.
pai_cosine_distance is v2's exact definition over the original pgvector
blobs; it re-ranks the shortlist."
  ;; SQLITE_UTF8 | SQLITE_DETERMINISTIC
  (%create-function handle "pai_unit_distance" 2 (logior 1 #x800) (cffi:null-pointer)
                    (cffi:callback unit-dot-distance) (cffi:null-pointer) (cffi:null-pointer)
                    (cffi:null-pointer))
  (%create-function handle "pai_cosine_distance" 2 (logior 1 #x800) (cffi:null-pointer)
                    (cffi:callback cosine-distance) (cffi:null-pointer) (cffi:null-pointer)
                    (cffi:null-pointer)))

(pushnew '%register-distance-functions *connection-hooks*)

;;; Embedding (Ollama /api/embed); transport injectable for tests.

(defvar *embedder*
  (lambda (texts)
    ;; The embedding server is a configured local service, so it may be on
    ;; a private address; it goes through the one transport all the same.
    (multiple-value-bind (body status)
        (http-request :post (format nil "~a/api/embed" *embedding-url*)
                      :headers '(("Content-Type" . "application/json"))
                      :body (json-encode (obj "model" *embedding-model* "input" texts))
                      :connect-timeout 5 :read-timeout 60 :allow-private t :max-redirects 0)
      (unless (eql status 200) (error "the embedding service answered ~a" status))
      (obj-get (json-parse body) "embeddings")))
  "(lambda (list-of-strings)) -> list of float lists.")

(defvar *embedding-cache* '() "Recent (text . vector) pairs; at most 16.")
(defvar *embedding-cache-lock* (bt:make-lock "embedding cache"))

(defvar *embedder-resting-until* 0
  "After the embedding service fails, recall reports itself unavailable at
once until this time instead of waiting on the service every turn (REL-5).")
(defparameter *embedder-rest-seconds* 60)

(defun embed (text &optional kernel)
  "Encoded embedding of TEXT, using KERNEL's embedding settings when given.
Recent queries are cached so the slots of one assembly embed the query once."
  (or (bt:with-lock-held (*embedding-cache-lock*)
        (cdr (assoc text *embedding-cache* :test #'string=)))
      (let ((vector (progn
                      (when (< (get-universal-time) *embedder-resting-until*)
                        (error "the embedding service failed recently; retrying after ~d s"
                               (- *embedder-resting-until* (get-universal-time))))
                      (encode-vector
                       (first (handler-bind ((error (lambda (e)
                                                      (declare (ignore e))
                                                      (setf *embedder-resting-until*
                                                            (+ (get-universal-time) *embedder-rest-seconds*)))))
                                (let ((*embedding-url* (if kernel (setting kernel "embedding.url") *embedding-url*))
                                      (*embedding-model* (if kernel (setting kernel "embedding.model") *embedding-model*)))
                                  (funcall *embedder* (list text)))))))))
        (bt:with-lock-held (*embedding-cache-lock*)
          (push (cons text vector) *embedding-cache*)
          (when (> (length *embedding-cache*) 16)
            (setf *embedding-cache* (subseq *embedding-cache* 0 16))))
        vector)))

(defun %query-unit (query-vector) (unit-vector-octets (decode-vector query-vector)))

;;; Nearest neighbours

(defun %in-list (start n) (format nil "~{?~d~^,~}" (loop for i from start below (+ start n) collect i)))

(defun nearest-vectors (kernel table scope-column scopes query-vector &key (pool 48))
  "The POOL rows of TABLE (id, SCOPE-COLUMN, vec of unit half-precision
copies) nearest QUERY-VECTOR (pgvector-encoded) whose scope is one of
SCOPES, as (id distance), nearest first. TABLE and SCOPE-COLUMN are names
fixed in code, never input."
  (when scopes
    (apply #'sql-rows (kernel-db kernel)
           (format nil "SELECT id,pai_unit_distance(vec,?1) AS d FROM ~a WHERE ~a IN (~a) ORDER BY d LIMIT ?2"
                   table scope-column (%in-list 3 (length scopes)))
           (%query-unit query-vector) pool scopes)))

;;; Embedding sources and the backfill

(defvar *embedding-sources* (make-hash-table :test 'equal)
  "Source name -> plist (:missing :text :record).")

(defmacro define-embedding-source (name &key missing text record)
  "Register NAME as something whose items are embedded. MISSING (kernel
batch) lists ids not yet embedded; TEXT (kernel id) is the text to embed, or
NIL when the item is gone; RECORD (kernel id model vector) keeps the
vector, normally by appending an event. Defines the work handler
\"NAME.embed\", whose entity is the item's id."
  `(progn
     (setf (gethash ,name *embedding-sources*) (list :missing ,missing :text ,text :record ,record))
     (define-handler ,(format nil "~a.embed" name)
       :run (lambda (kernel work) (%embed-item kernel ,name (getf work :entity)) nil))))

(defun %embed-item (kernel source id)
  (destructuring-bind (&key text record &allow-other-keys) (gethash source *embedding-sources*)
    (let ((content (funcall text kernel id)))
      (unless content
        (error 'handler-failure :class :source :reason (format nil "~a ~a not found" source id)))
      (funcall record kernel id *embedding-model*
               (handler-case (embed content kernel)
                 (error (e) (error 'handler-failure :class :transient :reason (princ-to-string e))))))))

(defun request-embedding (kernel source id)
  "Queue the embedding of item ID of SOURCE."
  (request-work kernel (format nil "~a.embed" source) id (obj) :priority :maintenance))

(defvar *embedding-backfill-at* (make-hash-table :test 'eq :weakness :key)
  "Kernel -> when the backfill last looked.")

(defun request-missing-embeddings (kernel &key (every 600) (batch 20))
  "Queue embedding for items of every source that have none: those formed
while the embedding service could not be reached would otherwise never be
found. Looks at most every EVERY seconds, BATCH items per source at a time.
Returns how many were queued, or NIL when it was not yet time to look."
  (let ((last (gethash kernel *embedding-backfill-at* 0)))
    (when (>= (- (now) last) every)
      (setf (gethash kernel *embedding-backfill-at*) (now))
      (let ((n 0))
        (maphash (lambda (source spec)
                   (dolist (id (funcall (getf spec :missing) kernel batch))
                     (unless (work-pending-p kernel (format nil "~a.embed" source) id)
                       (request-embedding kernel source id)
                       (incf n))))
                 *embedding-sources*)
        n))))
