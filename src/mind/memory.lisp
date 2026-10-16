(in-package #:pai-v3.mind)

;;; Memory (agent doc §11; spec §13.2, §16.5, §16.7).
;;;
;;; Recall ranks by v2's exact cosine distance over nomic-embed-text
;;; vectors stored in pgvector's binary send format (big-endian float32 with
;;; a 4-byte header), so imported memories need no re-embedding. The
;;; distance is a SQL function that reads each vector in place in SQLite's
;;; memory, so recall decodes no vectors into the Lisp heap. A half-precision
;;; unit copy of every vector makes the full scan small enough to stay in the
;;; page cache; it picks the shortlist, and the exact cosine ranks it. Every query is
;;; filtered by what the audience may receive before anything is scored.

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

;;; Events and projection

(define-event-type "memory.formed"
  :schema (("memory_id" :string) ("kind" :string) ("content" :string)
           ("sensitivity" :string) ("owner" :string) ("strength" :number)
           ("topic" :string :optional) ("source_event_id" :integer :optional)))

(define-event-type "memory.embedded"
  :schema (("memory_id" :string) ("model" :string) ("vector" :string)))  ; base64url pgvector-send

(define-event-type "memory.recalled"
  :schema (("memory_ids" :array) ("thought_id" :integer :optional)))

(defun %reduce-memory (db e)
  (let ((p (event-payload e)))
    (flet ((f (k) (obj-get p k)))
      (let ((type (event-type e)))
        (cond
          ((string= type "memory.formed")
           (unless (valid-sensitivity-p (f "sensitivity"))
             (error "Unknown sensitivity ~s" (f "sensitivity")))
           (sql-exec db "INSERT INTO memory_nodes(id,kind,content,sensitivity,owner,topic,strength,created_at,source_event_id) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
                     (f "memory_id") (f "kind") (f "content") (f "sensitivity") (f "owner")
                     (f "topic") (coerce (f "strength") 'double-float) (event-at e) (f "source_event_id")))
          ((string= type "memory.embedded")
           (let ((vector (base64url-decode (f "vector"))))
             (sql-exec db "UPDATE memory_nodes SET retrieval_embedding=?2,embedding_model=?3 WHERE id=?1"
                       (f "memory_id") vector (f "model"))
             (sql-exec db "INSERT INTO memory_vectors(id,sensitivity,vec) SELECT id,sensitivity,?2 FROM memory_nodes WHERE id=?1 ON CONFLICT(id) DO UPDATE SET vec=excluded.vec"
                       (f "memory_id") (unit-vector-octets (decode-vector vector)))))
          ((string= type "memory.recalled")
           ;; Revisiting a memory strengthens it a little (§16.7).
           (dolist (id (f "memory_ids"))
             (sql-exec db "UPDATE memory_nodes SET strength=min(1.0,strength+0.02),recalled=recalled+1 WHERE id=?1" id))))))))

(define-projection "memory"
  :kind :inline
  :consumes ("memory.formed" "memory.embedded" "memory.recalled")
  :tables ("CREATE TABLE IF NOT EXISTS memory_nodes (id TEXT PRIMARY KEY, kind TEXT NOT NULL, content TEXT NOT NULL, sensitivity TEXT NOT NULL, owner TEXT NOT NULL, topic TEXT, strength REAL NOT NULL, recalled INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL, source_event_id INTEGER, retrieval_embedding BLOB, embedding_model TEXT)"
           "CREATE INDEX IF NOT EXISTS memory_nodes_sensitivity_idx ON memory_nodes(sensitivity,created_at)"
           ;; Memories not yet embedded, found cheaply by the backfill.
           "CREATE INDEX IF NOT EXISTS memory_nodes_unembedded_idx ON memory_nodes(coalesce(embedding_model,''))"
           ;; Scoring copy: unit-length native float32, one compact row per
           ;; memory so a recall scan reads only vectors.
           "CREATE TABLE IF NOT EXISTS memory_vectors (id TEXT PRIMARY KEY, sensitivity TEXT NOT NULL, vec BLOB NOT NULL)"
           "CREATE INDEX IF NOT EXISTS memory_vectors_sensitivity_idx ON memory_vectors(sensitivity)")
  :entity (lambda (e) (obj-get (event-payload e) "memory_id"))
  :reduce #'%reduce-memory)

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

(defun form-memory (kernel &key id kind content (sensitivity "operator-only") (owner "operator")
                                (strength 0.5) topic source-event-id)
  "Form a memory and queue its embedding. Sensitivity defaults to the most
restrictive class when the caller does not know better."
  (let ((id (or id (format nil "m-~36r" (random (expt 2 64))))))
    (append! kernel "memory.formed"
             (obj "memory_id" id "kind" kind "content" content "sensitivity" sensitivity
                  "owner" owner "strength" strength "topic" topic "source_event_id" source-event-id))
    (request-work kernel "memory.embed" id (obj) :priority :maintenance)
    id))

(defvar *embedding-backfill-at* (make-hash-table :test 'eq :weakness :key)
  "Kernel -> when the backfill last looked.")

(defun request-missing-embeddings (kernel &key (every 600) (batch 20))
  "Queue embedding for memories that have none: those formed while the
embedding service could not be reached would otherwise never be recalled.
Looks at most every EVERY seconds, BATCH memories at a time."
  (let ((last (gethash kernel *embedding-backfill-at* 0)))
    (when (>= (- (now) last) every)
      (setf (gethash kernel *embedding-backfill-at*) (now))
      (let ((n 0))
        (dolist (id (mapcar #'first (sql-rows (kernel-db kernel)
                                              "SELECT id FROM memory_nodes WHERE coalesce(embedding_model,'')=?1 LIMIT ?2" "" batch)))
          (unless (work-pending-p kernel "memory.embed" id)
            (request-work kernel "memory.embed" id (obj) :priority :maintenance)
            (incf n)))
        n))))

(define-handler "memory.embed"
  :run (lambda (kernel work)
         (let* ((id (getf work :entity))
                (content (sql-value (kernel-db kernel) "SELECT content FROM memory_nodes WHERE id=?1 LIMIT 1" id)))
           (unless content
             (error 'handler-failure :class :source :reason "memory not found"))
           (append! kernel "memory.embedded"
                    (obj "memory_id" id "model" *embedding-model*
                         "vector" (base64url-encode
                                   (handler-case (embed content kernel)
                                     (error (e) (error 'handler-failure :class :transient
                                                                        :reason (princ-to-string e)))))))
           nil)))

;;; Recall

(defun %in-list (start n) (format nil "~{?~d~^,~}" (loop for i from start below (+ start n) collect i)))

(defun %query-unit (query-vector) (unit-vector-octets (decode-vector query-vector)))

(defun recall (kernel query-vector audience &key (k 12) (strength-weight 0.1) (pool 48))
  "Top-K memories for QUERY-VECTOR (pgvector-encoded) that AUDIENCE may
receive, as (id content distance strength). Every eligible memory is scored
on its half-precision copy; the POOL nearest are re-scored with v2's exact
cosine on the original vectors and ranked with a small strength term. The
distances returned are exact."
  (let* ((allowed (sensitivities-for kernel audience))
         (nearest (apply #'sql-rows (kernel-db kernel)
                         (format nil "SELECT id,pai_unit_distance(vec,?1) AS d FROM memory_vectors WHERE sensitivity IN (~a) ORDER BY d LIMIT ?2"
                                 (%in-list 3 (length allowed)))
                         (%query-unit query-vector) pool allowed)))
    (subseq
     (sort (loop for (id d) in nearest
                 for (content strength exact) = (sql-row (kernel-db kernel)
                                                         "SELECT content,strength,pai_cosine_distance(retrieval_embedding,?2) FROM memory_nodes WHERE id=?1 LIMIT 1"
                                                         id query-vector)
                 collect (list id content (or exact d) strength))
           #'< :key (lambda (row) (- (third row) (* strength-weight (fourth row)))))
     0 (min k (length nearest)))))

(defparameter *withheld-threshold* 0.35d0
  "Cosine distance under which a withheld memory counts as relevant enough to
mention that restricted information exists. Tuned for nomic-embed-text.")

(define-setting "memory.withheld_threshold"
  :type :number :min 0 :max 1 :group "Memory" :changed-by (:agent :min 0.2 :max 0.6)
  :default *withheld-threshold*
  :description "How close a restricted memory must be to the question before I say restricted information exists (cosine distance; lower is stricter).")

(defun withheld-topics (kernel query-vector audience &key (threshold (setting kernel "memory.withheld_threshold")))
  "Topic labels of closely matching memories AUDIENCE may not receive."
  (let ((excluded (set-difference (sensitivities-for kernel "operator")
                                  (sensitivities-for kernel audience) :test #'string=)))
    (when excluded
      (remove-duplicates
       (mapcar #'first
               (apply #'sql-rows (kernel-db kernel)
                      (format nil "SELECT coalesce(n.topic,n.sensitivity) FROM memory_vectors v JOIN memory_nodes n ON n.id=v.id WHERE v.sensitivity IN (~a) AND pai_unit_distance(v.vec,?1) < ?2 LIMIT 5"
                              (%in-list 3 (length excluded)))
                      (%query-unit query-vector) threshold excluded))
       :test #'equal))))

;;; Context slot sources

(define-slot-source :recall (kernel request max)
  (let* ((query (or (getf request :query) (getf request :task)))
         (rows (recall kernel (embed query kernel) (getf request :audience))))
    (when rows
      (append! kernel "memory.recalled" (obj "memory_ids" (mapcar #'first rows)
                                             "thought_id" (getf request :thought-id))))
    (mapcar (lambda (row) (list (second row) (estimate-tokens (second row)))) rows)))

(define-slot-source :restricted (kernel request max)
  (let ((topics (withheld-topics kernel (embed (or (getf request :query) (getf request :task)) kernel)
                                 (getf request :audience))))
    (mapcar (lambda (topic)
              (let ((text (format nil "I hold information about ~a that is not for this audience." topic)))
                (list text (estimate-tokens text))))
            topics)))
