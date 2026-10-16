(in-package #:pai-v3.maintenance)

;;; Importing a v2 agent (beads v3-0yk.5, v3-0yk.6; operator decisions
;;; 2026-10-16). No v2 event type is declared in v3, and no v3 event type
;;; exists only for v2:
;;;
;;; - Archive: every v2 event, verbatim (ids, gaps, types, times, payloads),
;;;   goes into the agent's encrypted blob store, table v2_archive. It is
;;;   lossless and read-only; nothing in v3 replays it.
;;; - Native events: only what the agent lives by becomes v3 events, each at
;;;   its original time (the kernel clock is set to it while appending), with
;;;   v2_map recording which v2 event it came from.
;;;     conversation  user-message -> turn.received; agent-message ->
;;;                   reply.delivered (a streamed reply's progress segments are
;;;                   interim, its final segment the reply)
;;;     memories      v2's current memory nodes, with their embeddings as they
;;;                   are (the same vector format), operator-only; quarantined
;;;                   and superseded nodes stay behind, in the archive
;;;     work          docket items still open in v2
;;;     settings      the operator's time zone
;;;   The graph has its own importer; persona statements are left for the agent
;;;   to review, not imported as instructions.
;;; - Resumable: each phase starts after what it has already done.

(defparameter +archive-ddl+
  '("CREATE TABLE IF NOT EXISTS v2_archive (storage_sequence INTEGER PRIMARY KEY, event_id INTEGER NOT NULL, storage_origin TEXT NOT NULL, agent_id TEXT NOT NULL, partition_status TEXT NOT NULL, event_type TEXT NOT NULL, occurred_at TEXT NOT NULL, event_json TEXT NOT NULL, integrity_hash TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS v2_archive_type_idx ON v2_archive(event_type,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS v2_archive_event_idx ON v2_archive(event_id)"
    "CREATE TABLE IF NOT EXISTS v2_map (v3_event_id INTEGER PRIMARY KEY, v2_event_id INTEGER NOT NULL, v2_type TEXT NOT NULL, phase TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS v2_map_v2_idx ON v2_map(v2_event_id,phase)"
    "CREATE INDEX IF NOT EXISTS v2_map_phase_idx ON v2_map(phase,v2_event_id)")
  "Tables in the agent's blob store.")

(defun %say (fmt &rest args) (apply #'format t (concatenate 'string "~&" fmt "~%") args) (finish-output))

(defun %v2-universal (iso)
  "Universal time of a v2 timestamp (UTC, seconds precision or finer)."
  (parse-iso8601 (subseq iso 0 19)))

(defun %at (iso thunk)
  "Run THUNK with the kernel clock at ISO, so what it appends carries that time."
  (let ((*clock* (let ((u (%v2-universal iso))) (lambda () u))))
    (funcall thunk)))

(defun %done-through (blobs phase)
  (or (sql-value blobs "SELECT v2_event_id FROM v2_map WHERE phase=?1 ORDER BY v2_event_id DESC LIMIT 1" phase) 0))

(defun %map (blobs v3-event v2-event type phase)
  (sql-exec blobs "INSERT OR REPLACE INTO v2_map(v3_event_id,v2_event_id,v2_type,phase) VALUES(?1,?2,?3,?4)"
            (event-id v3-event) v2-event type phase))

;;; Archive

(defun archive-v2 (blobs v2 &key (page 500))
  "Copy every v2 event into the archive, after what is already there."
  (dolist (ddl +archive-ddl+) (sql-exec blobs ddl))
  (let ((after (or (sql-value blobs "SELECT storage_sequence FROM v2_archive WHERE storage_sequence>?1 ORDER BY storage_sequence DESC LIMIT 1" -1) -1))
        (n 0))
    (loop for rows = (sql-rows v2 "SELECT storage_sequence,event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash FROM pai_events WHERE storage_sequence>?1 ORDER BY storage_sequence LIMIT ?2"
                               after page)
          while rows
          do (with-transaction (blobs)
               (dolist (r rows)
                 (apply #'sql-exec blobs "INSERT OR IGNORE INTO v2_archive(storage_sequence,event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)" r)))
             (incf n (length rows))
             (setf after (first (car (last rows))))
             (when (zerop (mod n 50000)) (%say "  archived ~:d" n)))
    n))

;;; Conversation

(defparameter *text-bytes* 14000 "Most UTF-8 bytes of a message carried as a v3 event.")

(defun %fit (text v2-id)
  "TEXT as it fits a v3 event: a message too long keeps its start and says
where the whole of it is. The archive holds every word."
  (if (<= (length (babel:string-to-octets text :encoding :utf-8)) *text-bytes*)
      text
      (let ((end (length text)))
        (loop while (> (length (babel:string-to-octets (subseq text 0 end) :encoding :utf-8)) *text-bytes*)
              do (setf end (floor (* end 9) 10)))
        (format nil "~a~%[... ~:d more characters; the whole of it is in the v2 archive, ~a]"
                (subseq text 0 end) (- (length text) end) v2-id))))

(defun %payload (json) (obj-get (json-parse json) "payload"))

(defun import-conversation (kernel blobs v2 v2-agent &key (page 500))
  "Each operator message and each reply, at its time. Messages from the era a
predecessor's history was carried in are marked with that lineage."
  (let ((after (%done-through blobs "conversation")) (turns 0) (replies 0))
    (loop for rows = (sql-rows v2 "SELECT event_id,event_type,occurred_at,event_json FROM pai_events WHERE agent_id=?1 AND event_type IN (?2,?3) AND event_id>?4 ORDER BY event_id LIMIT ?5"
                               v2-agent "user-message" "agent-message" after page)
          while rows
          do (dolist (r rows)
               (destructuring-bind (id type at json) r
                 (let* ((p (%payload json))
                        (text (let ((x (obj-get p "text" ""))) (if (stringp x) (%fit x (format nil "event ~d" id)) x)))
                        ;; Messages without a channel are the carried-in history.
                        (carried (null (obj-get p "channel")))
                        (channel (or (obj-get p "channel") "web")))
                   (when (and (stringp text) (plusp (length text)))
                     (let ((event
                             (%at at (lambda ()
                                       (if (string= type "user-message")
                                           (append! kernel "turn.received"
                                                    (obj "channel" channel "audience" "operator" "speaker" "Operator" "text" text
                                                         "origin" (obj "principal_type" "operator" "imported_from" "v2"
                                                                      "lineage" (if carried "earlier agent (v1) via this agent" :null))))
                                           (append! kernel "reply.delivered"
                                                    (obj "channel" channel "audience" "operator" "text" text
                                                         "interim" (if (eq :false (obj-get p "final")) t :false))))))))
                       (%map blobs event id type "conversation")
                       (if (string= type "user-message") (incf turns) (incf replies)))))))
               (setf after (first (car (last rows)))))
    (values turns replies)))

;;; Memories

(defun import-memories (kernel blobs derived &key (page 500))
  "v2's current memory nodes, embeddings kept. Quarantined and superseded
nodes are not carried; the archive still has them."
  (let ((superseded (make-hash-table :test 'equal)) (n 0) (skipped 0)
        (done (make-hash-table :test 'equal)))
    (dolist (r (sql-rows derived "SELECT scalar_json FROM pai_memory_nodes WHERE source_ordinal>?1 ORDER BY source_ordinal LIMIT 100000" -1))
      (let ((s (obj-get (json-parse (first r)) "supersedes_node_id")))
        (when (stringp s) (setf (gethash s superseded) t))))
    (dolist (r (sql-rows (kernel-db kernel) "SELECT id FROM memory_nodes WHERE kind IN (?1,?2,?3,?4,?5,?6,?7) LIMIT 100000"
                         "observation" "thought" "episode" "reflection" "prediction" "worldview" "self-fact"))
      (setf (gethash (first r) done) t))
    (loop with after = -1
          for rows = (sql-rows derived "SELECT source_ordinal,id,scalar_json,retrieval_embedding FROM pai_memory_nodes WHERE source_ordinal>?1 ORDER BY source_ordinal LIMIT ?2" after page)
          while rows
          do (dolist (r rows)
               (destructuring-bind (ordinal id json vector) r
                 (declare (ignore ordinal))
                 (let* ((m (json-parse json))
                        (content (obj-get m "content")))
                   (cond ((or (gethash id done)) nil)
                         ((or (eq t (obj-get m "quarantined")) (gethash id superseded)
                              (not (and (stringp content) (plusp (length content)))))
                          (incf skipped))
                         (t
                          (%at (or (obj-get m "created_at") (now-iso8601))
                               (lambda ()
                                 (append! kernel "memory.formed"
                                          (obj "memory_id" id "kind" (or (obj-get m "kind") "observation")
                                               "content" (%fit content (format nil "in the memory-write events for node ~a" id))
                                               "sensitivity" "operator-only" "owner" "self"
                                               "strength" (let ((i (obj-get m "importance"))) (if (realp i) (float (max 0 (min 1 i)) 1d0) 0.5d0))))
                                 (append! kernel "memory.embedded"
                                          (obj "memory_id" id "model" "nomic-embed-text" "vector" (base64url-encode vector)))))
                          (incf n))))))
             (setf after (first (car (last rows)))))
    (values n skipped)))

;;; Work still open

(defun import-open-work (kernel v2 v2-agent)
  "Docket items v2 still held open, as v3 work items."
  (let ((items (make-hash-table :test 'equal)) (n 0))
    (dolist (r (sql-rows v2 "SELECT event_type,event_json FROM pai_events WHERE agent_id=?1 AND event_type IN (?2,?3,?4) ORDER BY storage_sequence LIMIT 5000"
                         v2-agent "conscious-work-docket-opened" "conscious-work-docket-transitioned" "conscious-work-docket-parked"))
      (let* ((p (%payload (second r))) (id (obj-get p "work_id")))
        (cond ((string= (first r) "conscious-work-docket-opened") (setf (gethash id items) (list p "active")))
              ((gethash id items)
               (setf (second (gethash id items))
                     (if (string= (first r) "conscious-work-docket-parked") "parked" (obj-get p "state" "active")))
               (when (obj-get p "next_step") (setf (gethash "next_step" (first (gethash id items))) (obj-get p "next_step")))))))
    (maphash (lambda (id v)
               (declare (ignore id))
               (destructuring-bind (p state) v
                 ;; Resumable: an item already carried over is not opened again.
                 (when (and (member state '("active" "waiting") :test #'equal)
                            (not (find (obj-get p "title") (pai-v3.mind:docket-items kernel)
                                       :key (lambda (i) (obj-get i "title")) :test #'equal)))
                   (when (ignore-errors
                          (pai-v3.mind::open-work kernel :title (obj-get p "title") :purpose (obj-get p "purpose")
                                                         :operator-benefit (obj-get p "operator_benefit")
                                                         :next-step (obj-get p "next_step")
                                                         :priority (or (obj-get p "priority") "normal")))
                     (incf n)))))
             items)
    n))

;;; The working summary

(defun %claim-text (c)
  (cond ((stringp c) c)
        ((hash-table-p c) (or (find-if #'stringp (mapcar (lambda (k) (obj-get c k)) '("text" "claim" "statement" "summary"))) ""))
        (t "")))

(defun import-summary (kernel derived v2-agent)
  "v2's latest working summary, as the summary of the operator's conversation
up to the last imported turn, so the agent does not fold the whole history
again at its first start."
  (let* ((row (sql-row derived "SELECT response_json,created_at FROM pai_working_context_summaries WHERE agent_id=?1 ORDER BY created_at DESC LIMIT 1" v2-agent))
         (last-turn (sql-value (kernel-db kernel) "SELECT event_id FROM working_dialogue WHERE audience=?1 ORDER BY event_id DESC LIMIT 1" "operator")))
    (when (and row last-turn)
      (let* ((r (json-parse (first row)))
             (claims (remove "" (mapcar #'%claim-text (obj-get r "claims")) :test #'string=))
             (text (format nil "~{- ~a~^~%~}" claims)))
        (when claims
          (append! kernel "summary.updated"
                   (obj "audience" "operator" "text" text "covers_to" last-turn
                        "turns" (or (sql-value (kernel-db kernel) "SELECT count(*) FROM working_dialogue WHERE audience=?1 LIMIT 1" "operator") 0)
                        "model" "v2 working summary, imported"))
          (length claims))))))

;;; Everything

(defun import-v2-agent (dir events-path derived-path v2-agent)
  "Import a v2 agent's stores (read-only copies) into the v3 agent at DIR."
  (let* ((handle (pai-v3.host:open-agent dir))
         (kernel (pai-v3.host:agent-handle-kernel handle))
         (blobs (pai-v3.host:agent-store-db handle "blobs"))
         (v2 (db-open events-path :read-only t))
         (derived (db-open derived-path :read-only t)))
    (unwind-protect
         (progn
           (%say "archive: ~:d events" (archive-v2 blobs v2))
           (multiple-value-bind (turns replies) (import-conversation kernel blobs v2 v2-agent)
             (%say "conversation: ~:d turns, ~:d replies" turns replies))
           (multiple-value-bind (n skipped) (import-memories kernel blobs derived)
             (%say "memories: ~:d carried, ~:d left in the archive" n skipped))
           (%say "open work: ~d" (import-open-work kernel v2 v2-agent))
           (%say "summary: ~a claims" (import-summary kernel derived v2-agent))
           (let ((tz (sql-row v2 "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1" v2-agent "user-timezone-changed")))
             (when tz
               (let ((after (obj-get (%payload (first tz)) "after")))
                 (when (ignore-errors (local-time:find-timezone-by-location-name after))
                   (change-setting kernel "time.zone" after :origin (obj "principal_type" "operator" "via" "v2 import"))
                   (%say "time zone: ~a" after))))))
      (db-close v2) (db-close derived)
      (pai-v3.host:close-agent handle))))
