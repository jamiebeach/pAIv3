(in-package #:pai-v3.recall)

;;; Impressions (design note interaction-recall §5, §5a): the agent's sense
;;; of who or what something is. Recall returns impressions and nothing
;;; else, so what an impression holds can deepen without changing anything
;;; that uses it.
;;;
;;; An anchor is a graph entity (its id) or, for something the graph does
;;; not hold, a handle ("h:" and the identifier). An impression has a
;;; settled part, a fresh part, and what is open:
;;;
;;; - settled: the latest revision's text when consolidation has written
;;;   one; until then it is assembled live from rows (the entity's summary,
;;;   its strongest facts, the memories that name it), which never go stale;
;;; - fresh: the interactions that name it since that revision, newest
;;;   first, read live from the experience index;
;;; - open: what concerns, the docket and schedules hold about it.
;;;
;;; Everything is filtered by the audience first: the graph's facts are the
;;; operator's knowledge, so another audience's impression has no settled
;;; facts, and only the interactions and memories that audience may see.

(define-event-type "impression.revised"
  :schema (("anchor" :string) ("settled" :string) ("sources" :array :optional)
           ("through_event_id" :integer)))

(define-event-type "impression.embedded"
  :schema (("anchor" :string) ("model" :string) ("vector" :string)))  ; base64url pgvector-send

(define-projection "impressions"
  :kind :inline
  :version 2
  :consumes ("impression.revised" "impression.embedded" "graph.reset")
  :tables ("CREATE TABLE IF NOT EXISTS impressions (anchor TEXT PRIMARY KEY, settled TEXT NOT NULL, sources TEXT NOT NULL, through_event_id INTEGER NOT NULL, revision INTEGER NOT NULL, revised_at TEXT NOT NULL, embedded INTEGER NOT NULL DEFAULT 0)"
           "CREATE INDEX IF NOT EXISTS impressions_embedded_idx ON impressions(embedded,revised_at)"
           "CREATE TABLE IF NOT EXISTS impression_vectors (id TEXT PRIMARY KEY, scope TEXT NOT NULL, vec BLOB NOT NULL)"
           "CREATE INDEX IF NOT EXISTS impression_vectors_scope_idx ON impression_vectors(scope)")
  :entity (lambda (e) (obj-get (event-payload e) "anchor"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (cond ((string= type "graph.reset")
                     (sql-exec db "DELETE FROM impressions")
                     (sql-exec db "DELETE FROM impression_vectors"))
                    ((string= type "impression.embedded")
                     (sql-exec db "INSERT INTO impression_vectors(id,scope,vec) VALUES(?1,'operator',?2) ON CONFLICT(id) DO UPDATE SET vec=excluded.vec"
                               (obj-get p "anchor") (unit-vector-octets (decode-vector (base64url-decode (obj-get p "vector")))))
                     (sql-exec db "UPDATE impressions SET embedded=1 WHERE anchor=?1" (obj-get p "anchor")))
                    (t
                     (sql-exec db "INSERT INTO impressions(anchor,settled,sources,through_event_id,revision,revised_at,embedded) VALUES(?1,?2,?3,?4,1,?5,0) ON CONFLICT(anchor) DO UPDATE SET settled=excluded.settled,sources=excluded.sources,through_event_id=excluded.through_event_id,revision=revision+1,revised_at=excluded.revised_at,embedded=0"
                               (obj-get p "anchor") (obj-get p "settled") (json-encode (or (obj-get p "sources") '()))
                               (obj-get p "through_event_id") (event-at e)))))))

;;; A written impression is embedded, so the sense of someone can come to
;;; mind by meaning (§5a); it is the operator's, like the graph it draws on.

(define-embedding-source "impression"
  :missing (lambda (kernel batch)
             (mapcar #'first (sql-rows (kernel-db kernel)
                                       "SELECT anchor FROM impressions WHERE embedded=?1 ORDER BY revised_at DESC LIMIT ?2" 0 batch)))
  :text (lambda (kernel anchor)
          (sql-value (kernel-db kernel) "SELECT settled FROM impressions WHERE anchor=?1 LIMIT 1" anchor))
  :record (lambda (kernel anchor model vector)
            (append! kernel "impression.embedded"
                     (obj "anchor" anchor "model" model "vector" (base64url-encode vector)))))

(defun impressions-near (kernel query-vector &key (pool 6))
  "Anchors whose written impression is nearest QUERY-VECTOR, as (anchor distance)."
  (nearest-vectors kernel "impression_vectors" "scope" (list "operator") query-vector :pool pool))

(defun revise-impression (kernel anchor settled &key sources (through-event-id (ledger-head kernel)))
  "Record a new settled text for ANCHOR, covering interactions through
THROUGH-EVENT-ID. Consolidation writes these (§5a); every revision is kept
in the ledger."
  (append! kernel "impression.revised"
           (obj "anchor" anchor "settled" settled "sources" (or sources '())
                "through_event_id" through-event-id)))

(defun %revision (kernel anchor)
  "(settled through-event-id revision) of ANCHOR's latest revision, or NIL."
  (sql-row (kernel-db kernel) "SELECT settled,through_event_id,revision FROM impressions WHERE anchor=?1 LIMIT 1" anchor))

;;; Anchors

(defun anchor-of-entity (entity-id) entity-id)

(defun %anchor (kernel anchor)
  "(name kind terms entity-id) for ANCHOR. TERMS are every name and
identifier it goes by, so interactions naming any of them are found."
  (if (and (> (length anchor) 2) (string= "h:" anchor :end2 2))
      (let* ((handle (subseq anchor 2))
             (label (second (first (handles-in kernel handle)))))
        ;; What it was bound to is a name for it too (a peer's id and its name).
        (list (or label handle) "thing" (remove nil (list handle label)) nil))
      (let ((e (graph-entity kernel anchor)))
        (when e
          (let ((names (entity-names kernel (first e))))
            (list (third e) (second e)
                  (remove-duplicates
                   (append (list (third e)) names (handles-labelled kernel (list (third e))))
                   :test #'string-equal)
                  (first e)))))))

;;; Assembling an impression

(defparameter *fresh-most* 5 "Most fresh interactions an impression carries.")
(defparameter *memories-most* 3 "Most memories a first-form settled part carries.")
(defparameter +fresh-kinds+ '("conversation" "board" "fleet" "made")
  "Kinds of experience that are interactions; thinking and memories are not.")

(defun %ago (kernel iso)
  "ISO as the operator's wall clock and how long ago it was."
  (let* ((seconds (- (now) (or (ignore-errors (parse-iso8601 iso)) (now))))
         (ago (cond ((< seconds 120) "just now")
                    ((< seconds 7200) (format nil "~d minutes ago" (round seconds 60)))
                    ((< seconds 172800) (format nil "~d hours ago" (round seconds 3600)))
                    (t (format nil "~d days ago" (round seconds 86400))))))
    (format nil "~a, ~a" (or (ignore-errors (local-stamp kernel iso)) iso) ago)))

(defun %clip (text most)
  (let ((text (substitute #\Space #\Newline (string-trim " " (or text "")))))
    (if (> (length text) most) (concatenate 'string (subseq text 0 most) "…") text)))

(defun impression (kernel anchor audience &key open-worlds)
  "The impression of ANCHOR that AUDIENCE may receive, as a plist
(:anchor :name :kind :settled lines :fresh lines :open lines :revision n), or
NIL when ANCHOR is unknown. OPEN-WORLDS are the story worlds the cue opened
(the real world, and a story's only when it or someone in it was named);
by default, those ANCHOR itself opens."
  (destructuring-bind (&optional name kind terms entity) (%anchor kernel anchor)
    (when name
      (let* ((revision (%revision kernel anchor))
             (through (or (second revision) 0))
             (operator (equal audience "operator"))
             (found (experience-mentioning kernel terms audience :limit 30))
             (fresh (loop for (k at text nil event-id) in found
                          when (and (member k +fresh-kinds+ :test #'string=) (> event-id through))
                            collect (format nil "~a: ~a" (%ago kernel at) (%clip text 240))))
             (memories (loop for (k nil text) in found
                             when (string= k "memory") collect (%clip text 240)))
             (settled
               (cond ((and revision operator) (list (first revision)))
                     (t (append
                         (and operator entity
                              (let ((summary (fourth (graph-entity kernel entity))))
                                (and (stringp summary) (plusp (length summary)) (list summary))))
                         ;; What the graph knows, as the graph tells it: worlds,
                         ;; ended facts and lineage are handled there, once.
                         (and operator entity
                              (let* ((row (graph-entity kernel entity))
                                     (known (entity-knowledge kernel row (or open-worlds (open-worlds-for kernel (list row))))))
                                (and known (list (string-right-trim '(#\Newline) known)))))
                         (subseq memories 0 (min *memories-most* (length memories))))))))
        (list :anchor anchor :name name :kind kind
              ;; Pictures of the operator's world are the operator's.
              :pictures (and operator entity (pictures-of kernel entity))
              :settled settled
              :fresh (subseq fresh 0 (min *fresh-most* (length fresh)))
              :open (open-items-about kernel terms audience)
              :revision (third revision))))))

(defun render-impression (impression)
  "IMPRESSION as the lines the agent reads, marked as recall."
  (destructuring-bind (&key name kind settled fresh open pictures &allow-other-keys) impression
    (format nil "Recalled about ~a (~a):~@[~%~{  ~a~^~%~}~]~@[~%  Pictures (stored image ids, to look at or use as references): ~{~a~^, ~}~]~@[~%  Lately:~%~{    ~a~^~%~}~]~@[~%  Open:~%~{    ~a~^~%~}~]"
            name kind settled pictures fresh open)))
