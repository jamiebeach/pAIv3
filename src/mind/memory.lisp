(in-package #:pai-v3.mind)

;;; Memory (agent doc §11; spec §13.2, §16.5, §16.7).
;;;
;;; Recall ranks by v2's exact cosine distance over nomic-embed-text
;;; vectors, kept in pgvector's binary send format so imported memories need
;;; no re-embedding. Embedding, vector encoding and the distance functions
;;; are the shared facility in vectors.lisp; memory is one of its sources.
;;; A half-precision unit copy of every vector picks the shortlist, and the
;;; exact cosine ranks it. Every query is filtered by what the audience may
;;; receive before anything is scored.

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

(defun form-memory (kernel &key id kind content (sensitivity "operator-only") (owner "operator")
                                (strength 0.5) topic source-event-id)
  "Form a memory and queue its embedding. Sensitivity defaults to the most
restrictive class when the caller does not know better."
  (let ((id (or id (format nil "m-~36r" (random (expt 2 64))))))
    (append! kernel "memory.formed"
             (obj "memory_id" id "kind" kind "content" content "sensitivity" sensitivity
                  "owner" owner "strength" strength "topic" topic "source_event_id" source-event-id))
    (request-embedding kernel "memory" id)
    id))

;;; Memories are embedded through the shared facility (vectors.lisp).

(define-embedding-source "memory"
  :missing (lambda (kernel batch)
             (mapcar #'first (sql-rows (kernel-db kernel)
                                       "SELECT id FROM memory_nodes WHERE coalesce(embedding_model,'')=?1 LIMIT ?2" "" batch)))
  :text (lambda (kernel id)
          (sql-value (kernel-db kernel) "SELECT content FROM memory_nodes WHERE id=?1 LIMIT 1" id))
  :record (lambda (kernel id model vector)
            (append! kernel "memory.embedded"
                     (obj "memory_id" id "model" model "vector" (base64url-encode vector)))))

;;; Recall



(defun recall (kernel query-vector audience &key (k 12) (strength-weight 0.1) (pool 48))
  "Top-K memories for QUERY-VECTOR (pgvector-encoded) that AUDIENCE may
receive, as (id content distance strength). Every eligible memory is scored
on its half-precision copy; the POOL nearest are re-scored with v2's exact
cosine on the original vectors and ranked with a small strength term. The
distances returned are exact."
  (let* ((allowed (sensitivities-for kernel audience))
         (nearest (nearest-vectors kernel "memory_vectors" "sensitivity" allowed query-vector :pool pool)))
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

(define-slot-source :restricted (kernel request max)
  (let ((topics (withheld-topics kernel (embed (or (getf request :query) (getf request :task)) kernel)
                                 (getf request :audience))))
    (mapcar (lambda (topic)
              (let ((text (format nil "I hold information about ~a that is not for this audience." topic)))
                (list text (estimate-tokens text))))
            topics)))
