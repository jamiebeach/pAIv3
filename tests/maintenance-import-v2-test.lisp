(handler-bind ((warning #'muffle-warning))
  (ql:quickload "pai-v3-maintenance" :silent t))
(in-package #:pai-v3.test)
(use-package '#:pai-v3.maintenance)
(use-package '#:pai-v3.trust)
(use-package '#:pai-v3.host)

;;; Importing a v2 agent (beads v3-0yk.5, v3-0yk.6): everything into the
;;; archive, verbatim; conversation, memories, open work and the summary as
;;; native v3 events at their own times; nothing v2-only declared in v3.

(defvar *chain* (make-hash-table :test 'equal))

(defun v2-event (db id type at payload)
  (let ((json (json-encode (obj "schema_version" 1 "id" id "agent_id" "old-agent" "timestamp" at "type" type "payload" payload))))
    (sql-exec db "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'native',?2,'verified',?3,?4,?5,?6)"
              id "old-agent" type at json (pai-v3.kernel::sha256-hex json))))

(defun v2-stores (dir)
  "A small v2 events store and derived store in DIR. Returns their paths."
  (let ((events (format nil "~aevents.sqlite3" dir)) (derived (format nil "~aderived.sqlite3" dir)))
    (with-temp-kernel (k :path events :agent-id "old-agent" :key nil)
      (let ((db (kernel-db k)))
        (v2-event db 10 "heap-health" "2026-07-27T10:00:00Z" (obj "rss" 1))
        (v2-event db 11 "user-message" "2026-07-27T10:01:00Z" (obj "text" "Hello from the old days"))
        (v2-event db 12 "historical-user-message-imported" "2026-07-27T10:01:00Z" (obj "text" "Hello from the old days" "metadata" (obj "source_agent_id" "older")))
        (v2-event db 13 "agent-message" "2026-07-27T10:01:05Z" (obj "text" "Let me look." "turn_id" "t1" "segment_sequence" 0 "final" :false))
        (v2-event db 14 "agent-message" "2026-07-27T10:01:20Z" (obj "text" "Hello again." "turn_id" "t1" "segment_sequence" 2 "final" t))
        (v2-event db 20 "user-message" "2026-09-08T16:00:00Z" (obj "text" "A newer question" "channel" "web" "metadata" (obj)))
        (v2-event db 21 "agent-message" "2026-09-08T16:00:30Z" (obj "text" "A newer answer" "channel" "web" "metadata" (obj)))
        (v2-event db 30 "conscious-work-docket-opened" "2026-09-10T00:00:00Z"
                  (obj "work_id" "w1" "title" "Plan the garden" "purpose" "a garden plan" "operator_benefit" "the operator plants in spring"
                       "next_step" "list the beds" "priority" "normal"))
        (v2-event db 31 "conscious-work-docket-opened" "2026-09-10T00:00:00Z"
                  (obj "work_id" "w2" "title" "Finished thing" "purpose" "x" "operator_benefit" "y" "next_step" "z" "priority" "normal"))
        (v2-event db 32 "conscious-work-docket-transitioned" "2026-09-11T00:00:00Z" (obj "work_id" "w2" "state" "completed"))
        (v2-event db 40 "user-timezone-changed" "2026-08-01T00:00:00Z" (obj "before" "UTC" "after" "America/New_York"))))
    (let ((d (db-open derived :key nil)))
      (sql-exec d "CREATE TABLE pai_memory_nodes (id TEXT PRIMARY KEY, source_ordinal INTEGER NOT NULL UNIQUE, scalar_json TEXT NOT NULL, embedding BLOB NOT NULL, retrieval_embedding BLOB NOT NULL, integrity_hash TEXT NOT NULL)")
      (sql-exec d "CREATE TABLE pai_working_context_summaries (agent_id TEXT NOT NULL, activity_id TEXT NOT NULL, policy_revision TEXT NOT NULL, model_revision TEXT NOT NULL, source_digest TEXT NOT NULL, source_event_ids_json TEXT NOT NULL, response_json TEXT NOT NULL, provenance_json TEXT NOT NULL, integrity_hash TEXT NOT NULL, created_at TEXT NOT NULL)")
      (let ((vec (pai-v3.mind::encode-vector (loop for i below 768 collect (/ (1+ (mod i 7)) 10.0)))))
        (loop for (id ord json) in (list (list "m-keep" 1 (json-encode (obj "content" "The operator likes tomatoes." "kind" "observation" "importance" 0.8 "created_at" "2026-08-02T00:00:00Z")))
                                         (list "m-old" 2 (json-encode (obj "content" "Old belief." "kind" "thought" "created_at" "2026-08-02T00:00:00Z")))
                                         (list "m-new" 3 (json-encode (obj "content" "Newer belief." "kind" "thought" "supersedes_node_id" "m-old" "created_at" "2026-08-03T00:00:00Z")))
                                         (list "m-bad" 4 (json-encode (obj "content" "Quarantined." "kind" "thought" "quarantined" t))))
              do (sql-exec d "INSERT INTO pai_memory_nodes(id,source_ordinal,scalar_json,embedding,retrieval_embedding,integrity_hash) VALUES(?1,?2,?3,?4,?4,'h')" id ord json vec)))
      (sql-exec d "INSERT INTO pai_working_context_summaries VALUES('old-agent','a','p','m','d','[]',?1,'{}','h','2026-10-11 00:00:00')"
                (json-encode (obj "claims" (list (obj "text" "The operator is planning a garden.") "They talk most evenings."))))
      (db-close d))
    (list events derived)))

(deftest a-v2-agent-is-carried-over-without-v2-in-v3
  (let ((dir (format nil "/tmp/pai-v3-import-~d/" (random 100000000)))
        (*keychain* (list :put (lambda (n o) (setf (gethash n *chain*) o)) :get (lambda (n) (gethash n *chain*))
                          :delete (lambda (n) (remhash n *chain*))))
        (*scrypt-n* 1024))
    (ensure-directories-exist (format nil "~av2/" dir))
    (destructuring-bind (events derived) (v2-stores (format nil "~av2/" dir))
      (init-agent (format nil "~av3/" dir) "beta")
      (import-v2-agent (format nil "~av3/" dir) events derived "old-agent")
      (import-v2-agent (format nil "~av3/" dir) events derived "old-agent")      ; resumable: a second run adds nothing
      (let* ((h (open-agent (format nil "~av3/" dir))) (k (agent-handle-kernel h)) (blobs (agent-store-db h "blobs")))
        (unwind-protect
             (progn
               (is (= 11 (sql-value blobs "SELECT count(*) FROM v2_archive WHERE storage_sequence>?1 LIMIT 1" 0)) "every v2 event archived, once")
               (is (sql-value blobs "SELECT 1 FROM v2_archive WHERE event_type=?1 LIMIT 1" "heap-health") "even what v3 will never use")
               (is (null (sql-value (kernel-db k) "SELECT 1 FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1" "beta" "heap-health"))
                   "and no v2 type enters the v3 ledger")
               (let ((rows (sql-rows (kernel-db k) "SELECT kind,rendered,at FROM working_dialogue WHERE audience=?1 ORDER BY event_id LIMIT 10" "operator")))
                 (is (= 5 (length rows)) "two turns, three replies (one an interim note)")
                 (is (search "Hello from the old days" (second (first rows))))
                 (is (equal "2026-07-27T10:01:00Z" (third (first rows))) "at its own time"))
               (is (sql-value (kernel-db k) "SELECT 1 FROM pai_events WHERE agent_id=?1 AND event_type=?2 AND event_json LIKE ?3 LIMIT 1"
                              "beta" "turn.received" "%earlier agent (v1) via this agent%")
                   "carried-in history keeps its lineage")
               (is (= 5 (sql-value blobs "SELECT count(*) FROM v2_map WHERE phase=?1 LIMIT 1" "conversation")) "each traced back to its v2 event")
               (is (equal '("m-keep" "m-new") (mapcar #'first (sql-rows (kernel-db k) "SELECT id FROM memory_nodes WHERE kind IN (?1,?2) ORDER BY id LIMIT 10" "observation" "thought")))
                   "current memories carried; superseded and quarantined left in the archive")
               (is (sql-value (kernel-db k) "SELECT 1 FROM memory_vectors WHERE id=?1 LIMIT 1" "m-keep") "with their embeddings, not re-embedded")
               (is (equal '("Plan the garden") (mapcar (lambda (i) (obj-get i "title")) (pai-v3.mind:docket-items k))) "only work still open")
               (is (equal "America/New_York" (setting k "time.zone")))
               (is (search "planning a garden" (sql-value (kernel-db k) "SELECT text FROM working_summary WHERE audience=?1 LIMIT 1" "operator"))
                   "the summary covers the imported conversation"))
          (close-agent h))))))

(run-suite "maintenance-import-v2")
