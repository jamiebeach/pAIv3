(in-package #:pai-v3.mind)

;;; Fact embeddings (design note interaction-recall §7a). Each live graph
;;; fact's statement, as the agent reads it (render-fact), is embedded
;;; through the shared vector facility, so what the agent knows can come to
;;; mind by meaning as well as by name. The graph's own tables are not
;;; touched: this projection keeps only which facts are live and embedded,
;;; and their vectors. Graph facts are the operator's knowledge, so every
;;; vector is in the operator's scope.

(define-event-type "graph.fact.embedded"
  :schema (("fact_id" :string) ("model" :string) ("vector" :string)))  ; base64url pgvector-send

(define-projection "fact_vectors"
  :kind :inline
  :consumes ("graph.fact.admitted" "graph.fact.retracted" "graph.reset" "graph.fact.embedded")
  :tables ("CREATE TABLE IF NOT EXISTS fact_items (fact_id TEXT PRIMARY KEY, live INTEGER NOT NULL, embedded INTEGER NOT NULL DEFAULT 0, seq INTEGER NOT NULL)"
           "CREATE INDEX IF NOT EXISTS fact_items_pending_idx ON fact_items(live,embedded,seq)"
           "CREATE TABLE IF NOT EXISTS fact_vectors (id TEXT PRIMARY KEY, scope TEXT NOT NULL, vec BLOB NOT NULL)"
           "CREATE INDEX IF NOT EXISTS fact_vectors_scope_idx ON fact_vectors(scope)")
  :entity (lambda (e) (obj-get (event-payload e) "fact_id"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (cond ((string= type "graph.fact.admitted")
                     (sql-exec db "INSERT INTO fact_items(fact_id,live,embedded,seq) VALUES(?1,1,0,?2) ON CONFLICT(fact_id) DO UPDATE SET live=1,embedded=0,seq=excluded.seq"
                               (obj-get p "fact_id") (event-id e)))
                    ((string= type "graph.fact.retracted")
                     (sql-exec db "UPDATE fact_items SET live=0 WHERE fact_id=?1" (obj-get p "fact_id"))
                     (sql-exec db "DELETE FROM fact_vectors WHERE id=?1" (obj-get p "fact_id")))
                    ((string= type "graph.reset")
                     (sql-exec db "DELETE FROM fact_items")
                     (sql-exec db "DELETE FROM fact_vectors"))
                    ((string= type "graph.fact.embedded")
                     ;; A fact retracted while its embedding was made stays out.
                     (when (sql-value db "SELECT 1 FROM fact_items WHERE fact_id=?1 AND live=?2 LIMIT 1" (obj-get p "fact_id") 1)
                       (sql-exec db "INSERT INTO fact_vectors(id,scope,vec) VALUES(?1,'operator',?2) ON CONFLICT(id) DO UPDATE SET vec=excluded.vec"
                                 (obj-get p "fact_id")
                                 (unit-vector-octets (decode-vector (base64url-decode (obj-get p "vector")))))
                       (sql-exec db "UPDATE fact_items SET embedded=1 WHERE fact_id=?1" (obj-get p "fact_id"))))))))

(defun fact-statement (kernel fact-id)
  "FACT-ID as the agent reads it, while it is live; else NIL."
  (let ((row (live-fact kernel fact-id)))
    (and row (render-fact kernel row))))

(define-embedding-source "fact"
  :batch 100
  :missing (lambda (kernel batch)
             (mapcar #'first (sql-rows (kernel-db kernel)
                                       "SELECT fact_id FROM fact_items WHERE live=?1 AND embedded=?2 ORDER BY seq DESC LIMIT ?3" 1 0 batch)))
  :text #'fact-statement
  :record (lambda (kernel id model vector)
            (append! kernel "graph.fact.embedded"
                     (obj "fact_id" id "model" model "vector" (base64url-encode vector)))))

(defun facts-near (kernel query-vector &key (pool 12))
  "Live facts nearest QUERY-VECTOR in meaning, as (fact-id distance
statement), nearest first. The operator's knowledge: callers decide who may
receive it."
  (loop for (id d) in (nearest-vectors kernel "fact_vectors" "scope" (list "operator") query-vector :pool pool)
        for statement = (fact-statement kernel id)
        when statement collect (list id d statement)))
