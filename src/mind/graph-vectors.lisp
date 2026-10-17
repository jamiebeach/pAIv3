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

;;; Formation by meaning (design note interaction-recall §7a). A proposed
;;; fact that says what a live fact about the same two entities already
;;; says, in other words, supports it instead of standing beside it as a
;;; near copy. And the graph can point at its own vague relations and at
;;; entities that may be one thing, for the operator and the agent to decide.

(defparameter *duplicate-distance* 0.08d0
  "How close in meaning a proposed fact must be to a live one to support it instead.")
(defparameter *suggestion-distance* 0.15d0
  "How close a related_to fact must be to a precise fact, or two entities' facts to each other, to be suggested.")

(defun near-duplicate-fact (kernel statement subject object scope polarity)
  "A live fact about SUBJECT and OBJECT, with the same SCOPE and POLARITY,
saying what STATEMENT says; else NIL. Unembeddable now: NIL."
  (let ((v (ignore-errors (embed statement kernel))))
    (when v
      (loop for (id d) in (nearest-vectors kernel "fact_vectors" "scope" (list "operator") v :pool 5)
            for row = (live-fact kernel id)
            when (and row (< d *duplicate-distance*)
                      (equal (second row) subject) (equal (fourth row) object)
                      (equal (eighth row) scope) (equal (ninth row) polarity))
              return id))))

(defun %facts-near-fact (kernel fact-id &key (pool 4))
  "Other live facts nearest FACT-ID in meaning, as (fact-id distance)."
  (sql-rows (kernel-db kernel)
            "SELECT b.id,pai_unit_distance(b.vec,a.vec) AS d FROM fact_vectors a, fact_vectors b WHERE a.id=?1 AND b.scope=?2 AND b.id<>a.id ORDER BY d LIMIT ?3"
            fact-id "operator" pool))

(defun graph-suggestions (kernel &key (most 10))
  "Lines suggesting corrections: related_to facts that read like a fact with
a precise relation, and pairs of entities of one kind whose facts read
alike. Suggestions only: nothing is changed."
  (let ((lines '()) (seen (make-hash-table :test 'equal)))
    (dolist (row (sql-rows (kernel-db kernel)
                           "SELECT fact_id FROM fact_items WHERE live=?1 AND embedded=?2 ORDER BY seq DESC LIMIT 400" 1 1))
      (let* ((id (first row)) (fact (live-fact kernel id)))
        (when (and fact (< (length lines) most))
          (loop for (other d) in (%facts-near-fact kernel id)
                for near = (live-fact kernel other)
                when (and near (< d *suggestion-distance*))
                  do (cond
                       ;; A vague relation that reads like a precise one.
                       ((and (equal (third fact) "related_to") (not (equal (third near) "related_to"))
                             (not (gethash id seen)))
                        (setf (gethash id seen) t)
                        (push (format nil "~a reads like ~a: perhaps predicate ~a for \"~a\""
                                      id other (third near) (fact-statement kernel id))
                              lines))
                       ;; Two entities of one kind whose facts read alike.
                       ((and (not (equal (second fact) (second near)))
                             (equal (second (graph-entity kernel (second fact))) (second (graph-entity kernel (second near))))
                             (equal (fourth fact) (fourth near)))
                        (let ((pair (sort (list (second fact) (second near)) #'string<)))
                          (unless (gethash pair seen)
                            (setf (gethash pair seen) t)
                            (push (format nil "~a and ~a may be one ~a: \"~a\" / \"~a\""
                                          (third (graph-entity kernel (first pair))) (third (graph-entity kernel (second pair)))
                                          (second (graph-entity kernel (first pair)))
                                          (fact-statement kernel id) (fact-statement kernel other))
                                  lines)))))))))
    (nreverse lines)))
