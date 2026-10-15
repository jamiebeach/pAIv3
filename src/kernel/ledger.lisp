(in-package #:pai-v3.kernel)

;;; The ledger (spec §4.1). pai_events is the authority table, kept exactly as
;;; v2 laid it out so existing agents' history needs no copy. v3's own kernel
;;; tables are prefixed v3_. Only this package reads pai_events (I1).

(defparameter +ledger-ddl+
  '("CREATE TABLE IF NOT EXISTS pai_events (storage_sequence INTEGER PRIMARY KEY AUTOINCREMENT, event_id INTEGER NOT NULL, storage_origin TEXT NOT NULL, agent_id TEXT NOT NULL, partition_status TEXT NOT NULL, event_type TEXT NOT NULL, occurred_at TEXT NOT NULL, event_json TEXT NOT NULL, integrity_hash TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS pai_events_id_idx ON pai_events(event_id,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS pai_events_agent_id_idx ON pai_events(agent_id,event_id,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS pai_events_agent_type_id_idx ON pai_events(agent_id,event_type,event_id,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS pai_events_agent_seq_idx ON pai_events(agent_id,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS pai_events_agent_type_seq_idx ON pai_events(agent_id,event_type,storage_sequence)"
    ;; Kernel-assigned causal root, kept beside the authority row (spec §4.2).
    "CREATE TABLE IF NOT EXISTS v3_event_meta (storage_sequence INTEGER PRIMARY KEY, event_id INTEGER NOT NULL, root_id INTEGER NOT NULL)"
    "CREATE INDEX IF NOT EXISTS v3_event_meta_root_idx ON v3_event_meta(root_id,storage_sequence)"
    "CREATE INDEX IF NOT EXISTS v3_event_meta_id_idx ON v3_event_meta(event_id)"
    "CREATE TABLE IF NOT EXISTS v3_projection_state (name TEXT PRIMARY KEY, version INTEGER NOT NULL, kind TEXT NOT NULL, cursor_seq INTEGER NOT NULL, status TEXT NOT NULL, last_error TEXT, updated_at TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS v3_quarantine (projection TEXT NOT NULL, storage_sequence INTEGER NOT NULL, event_id INTEGER NOT NULL, error_class TEXT NOT NULL, message TEXT NOT NULL, attempts INTEGER NOT NULL, first_at TEXT NOT NULL, last_at TEXT NOT NULL, PRIMARY KEY(projection,storage_sequence))"
    "CREATE INDEX IF NOT EXISTS v3_quarantine_recent_idx ON v3_quarantine(last_at)"
    "CREATE TABLE IF NOT EXISTS v3_degraded (projection TEXT NOT NULL, entity TEXT NOT NULL, event_id INTEGER NOT NULL, reason TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(projection,entity,event_id))"
    "CREATE INDEX IF NOT EXISTS v3_degraded_recent_idx ON v3_degraded(projection,at)"))

(defstruct (kernel (:constructor %make-kernel))
  db agent-id (lock (bt:make-recursive-lock "pai-v3 kernel")) projections)

(defun open-kernel (path &key (agent-id "default") key (create t))
  "Open the agent's store at PATH, encrypted under KEY (octets), and install
every declared projection. With CREATE false the store must already exist.
Ordinary startup reads only projection state rows, never history (REL-2)."
  (let* ((db (db-open path :key key :create create))
         (kernel (%make-kernel :db db :agent-id agent-id)))
    (with-transaction (db)
      (dolist (ddl +ledger-ddl+) (sql-exec db ddl)))
    (install-projections kernel)
    kernel))

(defun close-kernel (kernel)
  (db-close (kernel-db kernel)))

(defun ledger-head (kernel)
  "Highest storage_sequence for this agent, or 0."
  (or (sql-value (kernel-db kernel)
                 "SELECT storage_sequence FROM pai_events WHERE agent_id=?1 ORDER BY storage_sequence DESC LIMIT 1"
                 (kernel-agent-id kernel))
      0))

(defun %next-event-id (db)
  (1+ (or (sql-value db "SELECT event_id FROM pai_events ORDER BY event_id DESC LIMIT 1") 0)))

(defun %root-of (db caused-by)
  (if caused-by
      (or (sql-value db "SELECT root_id FROM v3_event_meta WHERE event_id=?1 LIMIT 1" caused-by)
          caused-by)
      nil))

(defun %insert-event (kernel type payload caused-by)
  "Validate, write one event and its meta row. Caller holds the transaction."
  (let* ((db (kernel-db kernel))
         (event-type (validate-payload type payload))
         (encoded (json-encode payload)))
    (when (> (length (babel:string-to-octets encoded :encoding :utf-8))
             *payload-limit-bytes*)
      (error 'event-type-error :type type
                               :detail (format nil "payload exceeds ~d bytes"
                                               *payload-limit-bytes*)))
    (let* ((id (%next-event-id db))
           (event (make-event :id id :type type
                              :version (event-type-version event-type)
                              :at (now-iso8601) :caused-by caused-by
                              :agent-id (kernel-agent-id kernel)
                              :payload payload))
           (root (or (%root-of db caused-by) id))
           (json (progn (setf (event-root-id event) root)
                        (encode-envelope event))))
      (sql-exec db "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'native',?2,'verified',?3,?4,?5,?6)"
                id (kernel-agent-id kernel) type (event-at event) json (sha256-hex json))
      (let ((seq (sql-value db "SELECT last_insert_rowid()")))
        (setf (event-seq event) seq)
        (sql-exec db "INSERT INTO v3_event_meta(storage_sequence,event_id,root_id) VALUES(?1,?2,?3)"
                  seq id root))
      event)))

(defun append-batch! (kernel specs)
  "Append SPECS, each (TYPE PAYLOAD &key CAUSED-BY), in one transaction.
Inline projections apply in the same transaction (I4). A reducer failure
quarantines that event for that projection; the append still commits."
  (bt:with-recursive-lock-held ((kernel-lock kernel))
    (let ((db (kernel-db kernel)))
      (with-transaction (db)
        (loop for (type payload . options) in specs
              for event = (%insert-event kernel type payload
                                         (getf options :caused-by))
              do (apply-inline-projections kernel event)
              collect event)))))

(defun append! (kernel type payload &key caused-by)
  "Append one event; return it."
  (first (append-batch! kernel (list (list type payload :caused-by caused-by)))))

(defun %row->event (row)
  (destructuring-bind (seq json) row
    (decode-envelope json seq)))

(defun event-by-id (kernel id)
  "Point read for provenance and evidence display. Not an iteration primitive."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT storage_sequence,event_json FROM pai_events WHERE event_id=?1 AND agent_id=?2 ORDER BY storage_sequence DESC LIMIT 1"
                      id (kernel-agent-id kernel))))
    (and row (%row->event row))))

(defun latest-event-ids (kernel type limit)
  "Ids of the newest LIMIT events of TYPE, newest first. For inspection."
  (mapcar #'first (sql-rows (kernel-db kernel)
                            "SELECT event_id FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT ?3"
                            (kernel-agent-id kernel) type limit)))

(defun %events-after (kernel after-seq types limit)
  "Kernel-internal cursor read: events of TYPES after AFTER-SEQ, in order, at
most LIMIT. One indexed query per type, merged, so work stays bounded by
LIMIT however sparse the types are (spec §11)."
  (let ((rows '()))
    (dolist (type types)
      (setf rows
            (nconc rows
                   (sql-rows (kernel-db kernel)
                             "SELECT storage_sequence,event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 AND storage_sequence>?3 ORDER BY storage_sequence LIMIT ?4"
                             (kernel-agent-id kernel) type after-seq limit))))
    (mapcar #'%row->event
            (subseq (sort rows #'< :key #'first) 0 (min limit (length rows))))))
