(in-package #:pai-v3.mind)

;;; Searching my own experience, and memory as tools (spec §5.3, §16.10).
;;;
;;; experience_fts is a full-text index over what was said, what I thought
;;; and what I remember, maintained inline as those events are written.
;;; Every row carries its scope: the audience a conversation or thought
;;; belonged to (a:<audience>) or a memory's sensitivity (s:<class>). A
;;; search returns only rows the asking audience may receive; the operator
;;; may receive everything.

(define-event-type "experience.embedded"
  :schema (("event_id" :integer) ("model" :string) ("vector" :string)))  ; base64url pgvector-send

;;; Interactions (design note interaction-recall §7): besides what was said,
;;; thought and remembered, what passed between the agent and its peers and
;;; what it made. Each carries the identifiers it names (keys), so it can be
;;; found by them, and handles bind those identifiers to what they mean (a
;;; peer id to its name, an image id to its prompt). Names of people and
;;; things stay in the graph; handles hold only identifiers the graph does
;;; not. Conversation and interactions are embedded, through the shared
;;; vector facility, so they can be found by meaning; memories have their
;;; own vectors and are not embedded twice.

(defparameter +gist-characters+ 600 "Most characters of an interaction that are embedded.")

(defun %ids (&rest values)
  "The strings among VALUES (and in lists among them), space-separated."
  (format nil "~{~a~^ ~}"
          (loop for v in values
                append (cond ((stringp v) (and (plusp (length v)) (list v)))
                             ((listp v) (remove-if-not (lambda (x) (and (stringp x) (plusp (length x)))) v))))))

(defun %interaction (type p)
  "For event TYPE with payload P: (text kind scope ref keys embed handles),
or NIL when it is not an interaction. HANDLES is a list of (handle label)."
  (flet ((body (k) (obj-get (obj-get p "body") k)))
    (cond
      ((member type '("turn.received" "reply.delivered" "notice.posted") :test #'string=)
       (list (format nil "~a: ~a"
                     (cond ((string= type "turn.received") (obj-get p "speaker"))
                           ((string= type "notice.posted") "Notice")
                           (t "Me"))
                     (obj-get p "text"))
             "conversation" (format nil "a:~a" (obj-get p "audience")) (obj-get p "channel")
             (%ids (obj-get p "images"))
             ;; A status line between steps is not worth finding by meaning.
             (and (not (string= type "notice.posted")) (not (eq t (obj-get p "interim"))))
             nil))
      ((string= type "fleet.peer.joined")
       (list (format nil "Joined the fleet with ~a." (obj-get p "name"))
             "fleet" (format nil "a:peer:~a" (obj-get p "peer_id")) "fleet"
             (%ids (obj-get p "peer_id")) t
             (list (list (obj-get p "peer_id") (obj-get p "name")))))
      ((string= type "fleet.board.posted")
       (list (format nil "~a, on my board~@[ (thread: ~a)~]: ~a"
                     (obj-get p "author_name") (obj-get p "thread_title") (obj-get p "text"))
             "board" (format nil "a:peer:~a" (obj-get p "author_id")) (obj-get p "thread_id")
             (%ids (obj-get p "author_id") (obj-get p "thread_id") (obj-get p "msg_id")
                   (obj-get p "reply_to") (obj-get p "images") (obj-get p "tags"))
             t
             (remove nil (list (list (obj-get p "author_id") (obj-get p "author_name"))
                               (and (stringp (obj-get p "thread_title"))
                                    (list (obj-get p "thread_id") (format nil "board thread \"~a\"" (obj-get p "thread_title"))))))))
      ((and (string= type "fleet.op") (equal "notification-received" (obj-get p "kind")))
       (list (format nil "A peer told me about its own board: ~a" (body "text"))
             "fleet" (format nil "a:peer:~a" (obj-get p "peer_id")) (body "thread_id")
             (%ids (obj-get p "peer_id") (body "thread_id") (body "message_id") (body "reply_to") (body "images"))
             t nil))
      ((and (string= type "fleet.op") (equal "publication-intent" (obj-get p "kind")))
       (let ((r (obj-get (obj-get p "body") "request")))
         (list (format nil "Me, on a peer's board: ~a" (obj-get r "text"))
               "fleet" (format nil "a:peer:~a" (obj-get p "peer_id")) (obj-get r "thread_id")
               (%ids (obj-get p "peer_id") (obj-get r "thread_id") (obj-get r "reply_to") (obj-get r "image_ids"))
               t nil)))
      ((string= type "image.generated")
       (list (format nil "I made an image: ~a" (obj-get p "prompt"))
             "made" "a:operator" (obj-get p "image_id")
             (%ids (obj-get p "image_id") (obj-get p "references"))
             t
             (list (list (obj-get p "image_id")
                         (let ((prompt (obj-get p "prompt")))
                           (format nil "image of: ~a" (subseq prompt 0 (min 120 (length prompt))))))))))))

(defun %canonical-handle (h) (string-downcase (string-trim " " h)))

(define-projection "experience"
  :kind :inline
  :version 3
  :consumes ("turn.received" "reply.delivered" "notice.posted" "thought.step" "memory.formed"
             "fleet.peer.joined" "fleet.board.posted" "fleet.op" "image.generated" "experience.embedded")
  :tables ("CREATE VIRTUAL TABLE IF NOT EXISTS experience_fts USING fts5(text, keys, kind UNINDEXED, scope UNINDEXED, at UNINDEXED, ref UNINDEXED, event_id UNINDEXED, tokenize='porter unicode61')"
           ;; Interactions to embed, newest first; embedded once their vector is kept.
           ;; The whole text, its kind and when; the first part of it is what is embedded.
           "CREATE TABLE IF NOT EXISTS experience_items (event_id INTEGER PRIMARY KEY, scope TEXT NOT NULL, kind TEXT NOT NULL, gist TEXT NOT NULL, at TEXT NOT NULL, embedded INTEGER NOT NULL DEFAULT 0)"
           "CREATE INDEX IF NOT EXISTS experience_items_embedded_idx ON experience_items(embedded,event_id)"
           "CREATE INDEX IF NOT EXISTS experience_items_scope_idx ON experience_items(scope,event_id)"
           "CREATE TABLE IF NOT EXISTS experience_vectors (id TEXT PRIMARY KEY, scope TEXT NOT NULL, vec BLOB NOT NULL)"
           "CREATE INDEX IF NOT EXISTS experience_vectors_scope_idx ON experience_vectors(scope)"
           ;; Every scope seen, so a search open to the operator names them all.
           "CREATE TABLE IF NOT EXISTS experience_scopes (scope TEXT PRIMARY KEY, kind TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS experience_scopes_kind_idx ON experience_scopes(kind,scope)"
           "CREATE TABLE IF NOT EXISTS experience_handles (handle TEXT PRIMARY KEY, label TEXT NOT NULL, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS experience_handles_label_idx ON experience_handles(label)")
  :entity (lambda (e) (event-id e))
  :reduce (lambda (db e)
            (let* ((p (event-payload e)) (type (event-type e)))
              (flet ((index (text kind scope ref &optional (keys ""))
                       (when (and (stringp text) (plusp (length text)))
                         (sql-exec db "INSERT INTO experience_fts(text,keys,kind,scope,at,ref,event_id) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                                   text keys kind scope (event-at e) ref (event-id e))
                         ;; A peer's scope is also listed as one, for sealing its episodes.
                         (sql-exec db "INSERT OR IGNORE INTO experience_scopes(scope,kind) VALUES(?1,?2)"
                                   scope (if (and (> (length scope) 7) (string= "a:peer:" scope :end2 7)) "peer" "scope")))))
                (cond ((string= type "experience.embedded")
                       (let ((id (obj-get p "event_id")))
                         (sql-exec db "INSERT INTO experience_vectors(id,scope,vec) SELECT ?1,scope,?2 FROM experience_items WHERE event_id=?3 ON CONFLICT(id) DO UPDATE SET vec=excluded.vec"
                                   (princ-to-string id) (unit-vector-octets (decode-vector (base64url-decode (obj-get p "vector")))) id)
                         (sql-exec db "UPDATE experience_items SET embedded=1 WHERE event_id=?1" id)))
                      ((string= type "thought.step")
                       (when (string= "model" (obj-get p "kind"))
                         (index (obj-get p "text") "thinking"
                                (format nil "a:~a" (or (sql-value db "SELECT audience FROM thoughts WHERE id=?1 LIMIT 1"
                                                                  (obj-get p "thought_id"))
                                                       "unknown"))
                                (format nil "thought ~d" (obj-get p "thought_id")))))
                      ((string= type "memory.formed")
                       (index (obj-get p "content") "memory" (format nil "s:~a" (obj-get p "sensitivity"))
                              (obj-get p "memory_id")))
                      (t
                       (let ((i (%interaction type p)))
                         (when i
                           (destructuring-bind (text kind scope ref keys embed handles) i
                             (index text kind scope ref keys)
                             (when (and embed (stringp text) (plusp (length text)))
                               (sql-exec db "INSERT OR IGNORE INTO experience_items(event_id,scope,kind,gist,at) VALUES(?1,?2,?3,?4,?5)"
                                         (event-id e) scope kind text (event-at e)))
                             (loop for (handle label) in handles
                                   when (and (stringp handle) (>= (length handle) 6) (stringp label) (plusp (length label)))
                                     do (sql-exec db "INSERT INTO experience_handles(handle,label,at) VALUES(?1,?2,?3) ON CONFLICT(handle) DO UPDATE SET label=excluded.label,at=excluded.at"
                                                  (%canonical-handle handle) label (event-at e))))))))))))

(defun %event-id (id) (if (stringp id) (parse-integer id) id))

(define-embedding-source "experience"
  :batch 100
  :missing (lambda (kernel batch)
             (mapcar #'first (sql-rows (kernel-db kernel)
                                       "SELECT event_id FROM experience_items WHERE embedded=?1 ORDER BY event_id DESC LIMIT ?2" 0 batch)))
  ;; Work entities are text; an item's id is its event id.
  :text (lambda (kernel id)
          (let ((text (sql-value (kernel-db kernel) "SELECT gist FROM experience_items WHERE event_id=?1 LIMIT 1" (%event-id id))))
            (and text (subseq text 0 (min +gist-characters+ (length text))))))
  :record (lambda (kernel id model vector)
            (append! kernel "experience.embedded"
                     (obj "event_id" (%event-id id) "model" model "vector" (base64url-encode vector)))))

;;; Finding interactions: by identifier, by name, by meaning

(defun handles-in (kernel text)
  "The known handles in TEXT, as (handle label): identifiers the agent has
seen bound to a meaning."
  (let ((tokens (remove-duplicates
                 (loop with start = 0
                       for end = (position-if-not (lambda (c) (or (alphanumericp c) (find c "-_"))) text :start start)
                       for token = (subseq text start (or end (length text)))
                       when (>= (length token) 6) collect (string-downcase token)
                       while end do (setf start (1+ end)))
                 :test #'string=)))
    (loop while tokens
          append (let ((batch (subseq tokens 0 (min 100 (length tokens)))))
                   (setf tokens (nthcdr (length batch) tokens))
                   (apply #'sql-rows (kernel-db kernel)
                          (format nil "SELECT handle,label FROM experience_handles WHERE handle IN (~a) LIMIT 100"
                                  (%in-list 1 (length batch)))
                          batch)))))

(defun handles-labelled (kernel labels)
  "The handles bound to any of LABELS (exact), such as a peer's id bound to its name."
  (when labels
    (mapcar #'first (apply #'sql-rows (kernel-db kernel)
                           (format nil "SELECT handle FROM experience_handles WHERE label IN (~a) LIMIT 50"
                                   (%in-list 1 (length labels)))
                           labels))))

(defun %experience-scopes (kernel audience)
  "The scopes AUDIENCE may see: every scope for the operator."
  (if (equal audience "operator")
      (mapcar #'first (sql-rows (kernel-db kernel) "SELECT scope FROM experience_scopes WHERE kind IN (?1,?2) LIMIT 1000" "scope" "peer"))
      (cons (format nil "a:~a" audience)
            (mapcar (lambda (s) (format nil "s:~a" s)) (sensitivities-for kernel audience)))))

(defun experience-mentioning (kernel terms audience &key (limit 6))
  "The latest experience AUDIENCE may see that mentions any of TERMS (names
or identifiers), newest first, as (kind at text ref event-id)."
  (let ((match (format nil "~{\"~a\"~^ OR ~}"
                       (loop for term in terms
                             for clean = (remove #\" (string-trim " " (or term "")))
                             when (plusp (length clean)) collect clean)))
        (scopes (%scopes kernel audience)))
    (unless (string= match "")
      (sql-rows (kernel-db kernel)
                "SELECT kind,at,text,ref,event_id FROM experience_fts WHERE experience_fts MATCH ?1 AND (?2 IS NULL OR instr(?2,'|'||scope||'|')>0) ORDER BY rowid DESC LIMIT ?3"
                match scopes limit))))

(defun peer-audiences (kernel)
  "The audiences (peer:<id>) of every peer something has passed with."
  (mapcar (lambda (r) (subseq (first r) 2))
          (sql-rows (kernel-db kernel) "SELECT scope FROM experience_scopes WHERE kind=?1 LIMIT 200" "peer")))

(defun experience-items-after (kernel audience after &key (limit 400))
  "Interactions with AUDIENCE after event AFTER, oldest first, as
(event-id kind speaker text at), the shape conversation rows have."
  (loop for (id kind text at) in (sql-rows (kernel-db kernel)
                                           "SELECT event_id,kind,gist,at FROM experience_items WHERE scope=?1 AND event_id>?2 ORDER BY event_id LIMIT ?3"
                                           (format nil "a:~a" audience) after limit)
        collect (list id kind audience text at)))

(defun experience-near (kernel query-vector audience &key (pool 12))
  "Interactions AUDIENCE may see nearest QUERY-VECTOR in meaning, as
(event-id distance gist), nearest first."
  (loop for (id d) in (nearest-vectors kernel "experience_vectors" "scope"
                                       (%experience-scopes kernel audience) query-vector :pool pool)
        collect (list (parse-integer id) d
                      (sql-value (kernel-db kernel) "SELECT gist FROM experience_items WHERE event_id=?1 LIMIT 1"
                                 (parse-integer id)))))

(defun %fts-query (text joiner)
  "TEXT as an FTS5 query of quoted words joined by JOINER, or NIL."
  (let ((words (loop with start = 0
                     for end = (position-if-not #'alphanumericp text :start start)
                     for word = (subseq text start (or end (length text)))
                     when (plusp (length word)) collect (format nil "\"~a\"" word)
                     while end do (setf start (1+ end)))))
    (and words (format nil (concatenate 'string "~{~a~^" joiner "~}") words))))

(defun %scopes (kernel audience)
  "The scope filter for AUDIENCE: NIL for the operator (everything)."
  (unless (equal audience "operator")
    (format nil "|a:~a|~{s:~a|~}" audience (sensitivities-for kernel audience))))

(defun search-experience (kernel query audience &key (limit 8))
  "What AUDIENCE may see of my experience matching QUERY, best first, as
(kind at snippet ref). All words must match; if nothing does, any word may."
  (let ((scopes (%scopes kernel audience)))
    (dolist (joiner '(" " " OR "))
      (let* ((match (%fts-query query joiner))
             (rows (and match
                        (sql-rows (kernel-db kernel)
                                  "SELECT kind,at,snippet(experience_fts,0,'[',']','…',32),ref FROM experience_fts WHERE experience_fts MATCH ?1 AND (?2 IS NULL OR instr(?2,'|'||scope||'|')>0) ORDER BY rank LIMIT ?3"
                                  match scopes limit))))
        (when rows (return rows))))))

(define-tool "search_experience"
  :description "Search my own experience: conversations, my thinking and my memories, by words. Use it for when something happened, what was said, or what I thought about it."
  :parameters (obj "type" "object"
                   "properties" (obj "query" (obj "type" "string") "limit" (obj "type" "integer"))
                   "required" (list "query"))
  :core '("responding" "introspection" "work" "concern" "reflection")
  :run (lambda (kernel id args)
         (let ((rows (search-experience kernel (obj-get args "query" "")
                                        (getf (thought-row kernel id) :audience)
                                        :limit (max 1 (min 25 (obj-get args "limit" 8))))))
           (if rows
               (format nil "~{~a~^~%~}"
                       (loop for (kind at snippet ref) in rows
                             collect (format nil "[~a ~a UTC] ~a (~a): ~a"
                                             (subseq at 0 10) (subseq at 11 16) kind ref snippet)))
               "Nothing in my experience matches that."))))

;;; Memory

(define-tool "recall"
  :description "Recall memories related to something, by meaning rather than exact words."
  :parameters (obj "type" "object"
                   "properties" (obj "about" (obj "type" "string") "count" (obj "type" "integer"))
                   "required" (list "about"))
  :run (lambda (kernel id args)
         (block recall-tool
         (let* ((audience (getf (thought-row kernel id) :audience))
                (query (handler-case (embed (obj-get args "about" "") kernel)
                         (error (e) (return-from recall-tool
                                      (format nil "Recall is unavailable just now: ~a" e)))))
                (rows (recall kernel query audience :k (max 1 (min 25 (obj-get args "count" 8))))))
           (when rows
             (append! kernel "memory.recalled" (obj "memory_ids" (mapcar #'first rows) "thought_id" id)))
           (if rows
               (format nil "~{~a~^~%~}"
                       (mapcar (lambda (r) (format nil "- ~a (~a)" (second r) (or (memory-origin kernel (first r)) "a memory")))
                               rows))
               "Nothing comes to mind.")))))

(define-tool "remember"
  :description "Keep something as a memory. Say who may hear it again: operator-only (the default), household, peers, public, or granted:<topic>. To keep a note about pictures, name their image ids in images: the ids go into the memory, so it leads back to them. A memory's own id (m-...) or a fact's (f-...) is not a picture's id."
  :parameters (obj "type" "object"
                   "properties" (obj "content" (obj "type" "string")
                                     "images" (obj "type" "array" "items" (obj "type" "string")
                                                   "description" "ids of pictures this memory is about")
                                     "kind" (obj "type" "string" "description" "fact, episode, preference or insight")
                                     "sensitivity" (obj "type" "string")
                                     "topic" (obj "type" "string" "description" "What it is about, for telling others it exists without saying it."))
                   "required" (list "content"))
  :run (lambda (kernel id args)
         (let* ((sensitivity (obj-get args "sensitivity" "operator-only"))
                (images (remove-if-not #'stringp (or (obj-get args "images") '())))
                (missing (find-if-not (lambda (i) (image-row kernel i)) images)))
           (cond
             ((not (valid-sensitivity-p sensitivity))
               (format nil "~a is not a sensitivity. Use operator-only, household, peers, public or granted:<topic>." sensitivity))
             (missing
              (format nil "~a is not an image I hold, so nothing was remembered. An image id is 32 hex digits (set_avatar with no image_id lists the pictures I made); a memory id (m-...) or fact id (f-...) is not one." missing))
             (t
               (let ((memory (form-memory kernel :kind (obj-get args "kind" "fact")
                                                 :content (format nil "~a~@[~%(pictures: ~{~a~^ ~})~]" (obj-get args "content" "") images)
                                                 :sensitivity sensitivity
                                                 :topic (obj-get args "topic")
                                                 :owner "self" :source-event-id id)))
                 (format nil "Remembered (~a, ~a)." memory sensitivity)))))))
