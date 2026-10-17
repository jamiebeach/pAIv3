(in-package #:pai-v3.mind)

;;; Searching my own experience, and memory as tools (spec §5.3, §16.10).
;;;
;;; experience_fts is a full-text index over what was said, what I thought
;;; and what I remember, maintained inline as those events are written.
;;; Every row carries its scope: the audience a conversation or thought
;;; belonged to (a:<audience>) or a memory's sensitivity (s:<class>). A
;;; search returns only rows the asking audience may receive; the operator
;;; may receive everything.

(define-projection "experience"
  :kind :inline
  :consumes ("turn.received" "reply.delivered" "notice.posted" "thought.step" "memory.formed")
  :tables ("CREATE VIRTUAL TABLE IF NOT EXISTS experience_fts USING fts5(text, kind UNINDEXED, scope UNINDEXED, at UNINDEXED, ref UNINDEXED, tokenize='porter unicode61')")
  :entity (lambda (e) (event-id e))
  :reduce (lambda (db e)
            (let* ((p (event-payload e)) (type (event-type e)))
              (flet ((index (text kind scope ref)
                       (when (plusp (length text))
                         (sql-exec db "INSERT INTO experience_fts(text,kind,scope,at,ref) VALUES(?1,?2,?3,?4,?5)"
                                   text kind scope (event-at e) ref))))
                (cond ((string= type "thought.step")
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
                       (index (format nil "~a: ~a"
                                      (cond ((string= type "turn.received") (obj-get p "speaker"))
                                            ((string= type "notice.posted") "Notice")
                                            (t "Me"))
                                      (obj-get p "text"))
                              "conversation" (format nil "a:~a" (obj-get p "audience"))
                              (obj-get p "channel"))))))))

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
  :core '("responding" "introspection" "work" "concern")
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
               (format nil "~{- ~a~^~%~}" (mapcar #'second rows))
               "Nothing comes to mind.")))))

(define-tool "remember"
  :description "Keep something as a memory. Say who may hear it again: operator-only (the default), household, peers, public, or granted:<topic>."
  :parameters (obj "type" "object"
                   "properties" (obj "content" (obj "type" "string")
                                     "kind" (obj "type" "string" "description" "fact, episode, preference or insight")
                                     "sensitivity" (obj "type" "string")
                                     "topic" (obj "type" "string" "description" "What it is about, for telling others it exists without saying it."))
                   "required" (list "content"))
  :run (lambda (kernel id args)
         (let ((sensitivity (obj-get args "sensitivity" "operator-only")))
           (if (not (valid-sensitivity-p sensitivity))
               (format nil "~a is not a sensitivity. Use operator-only, household, peers, public or granted:<topic>." sensitivity)
               (let ((memory (form-memory kernel :kind (obj-get args "kind" "fact")
                                                 :content (obj-get args "content" "")
                                                 :sensitivity sensitivity
                                                 :topic (obj-get args "topic")
                                                 :owner "self" :source-event-id id)))
                 (format nil "Remembered (~a, ~a)." memory sensitivity))))))
