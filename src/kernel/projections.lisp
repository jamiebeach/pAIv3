(in-package #:pai-v3.kernel)

;;; The projection registry (spec §4.3, §14). The only way to maintain
;;; derived state. The kernel owns cursors, status, quarantine and
;;; degradation; a projection supplies tables and a deterministic reducer.

(defparameter *inline-budget-ms* 5
  "Inline reducer time budget per event (spec §14). Overruns are counted.")

(defstruct projection
  name kind version consumes tables reduce entity (derivation :pure)
  (inspectable t) (applied 0) (over-budget 0))

(defvar *projections* (make-hash-table :test 'equal))

(defmacro define-projection (name &key (kind :inline) (version 1) consumes
                                       tables reduce entity (derivation :pure)
                                       (inspectable t))
  "Declare projection NAME.
KIND is :inline (applied in the append transaction) or :async (applied by
STEP-ASYNC-PROJECTION in bounded steps). CONSUMES lists event types.
TABLES is a list of DDL strings for tables this projection alone owns (I2).
REDUCE is (lambda (db event)): deterministic, no clock, network, model call
or other projection's tables (I3). ENTITY, when given, is (lambda (event))
returning the entity key to mark degraded if the reducer fails on EVENT.
DERIVATION is :pure (rebuildable from the ledger) or :model (built by paid
cognition; never rebuilt automatically). INSPECTABLE NIL keeps the rows
out of the agent's inspection tools (credentials); its status, counts and
digest stay visible."
  `(setf (gethash ,name *projections*)
         (make-projection :name ,name :kind ,kind :version ,version
                          :consumes ',consumes :tables ',tables
                          :reduce ,reduce :entity ,entity
                          :derivation ,derivation :inspectable ,inspectable)))

(defun find-projection (name)
  (or (gethash name *projections*)
      (error "Projection ~s is not declared" name)))

(defun %state (db name)
  (sql-row db "SELECT version,kind,cursor_seq,status FROM v3_projection_state WHERE name=?1 LIMIT 1"
           name))

(defun %set-state (db name version kind cursor status &optional error)
  (sql-exec db "INSERT INTO v3_projection_state(name,version,kind,cursor_seq,status,last_error,updated_at) VALUES(?1,?2,?3,?4,?5,?6,?7) ON CONFLICT(name) DO UPDATE SET version=excluded.version,kind=excluded.kind,cursor_seq=excluded.cursor_seq,status=excluded.status,last_error=excluded.last_error,updated_at=excluded.updated_at"
            name version (string-downcase (symbol-name kind)) cursor status error
            (now-iso8601)))

(defun %has-consumed-history-p (kernel projection)
  (loop for type in (projection-consumes projection)
          thereis (sql-value (kernel-db kernel)
                             "SELECT 1 FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1"
                             (kernel-agent-id kernel) type)))

(defun install-projections (kernel)
  "Create owned tables and reconcile each projection's state. A projection
that is new on a non-empty history, or whose version changed, is marked
needs-rebuild; it is never silently replayed (I7). A version change may
change a projection's tables, so its own tables are dropped first and made
again from its declaration: an index on a new column must not meet the old
table (the graph's worlds once kept an agent from opening)."
  (let ((db (kernel-db kernel)))
    (with-transaction (db)
      (loop for projection being the hash-values of *projections*
            for name = (projection-name projection)
            for state = (%state db name)
            do (when (and state (/= (first state) (projection-version projection)))
                 (dolist (table (owned-tables projection))
                   (sql-exec db (format nil "DROP TABLE IF EXISTS ~a" table))))
               (dolist (ddl (projection-tables projection)) (sql-exec db ddl))
               (cond ((null state)
                      (if (%has-consumed-history-p kernel projection)
                          (%set-state db name (projection-version projection)
                                      (projection-kind projection) 0 "needs-rebuild"
                                      "new projection over existing history")
                          (%set-state db name (projection-version projection)
                                      (projection-kind projection)
                                      (ledger-head kernel) "ready")))
                     ((/= (first state) (projection-version projection))
                      (%set-state db name (first state) (projection-kind projection)
                                  (third state) "needs-rebuild"
                                  (format nil "version ~d declared, ~d stored"
                                          (projection-version projection)
                                          (first state)))))
               (push projection (kernel-projections kernel))))))

(defun projection-status (kernel name)
  (fourth (%state (kernel-db kernel) name)))

(defun projections-not-ready (kernel)
  "Projections that are not applying events, as (name status reason) lists.
Such a projection serves stale or empty answers, so whatever shows the
agent's state must say so (v3-clp.19)."
  (sql-rows (kernel-db kernel)
            "SELECT name,status,last_error FROM v3_projection_state WHERE status IN (?1,?2) ORDER BY name LIMIT 100"
            "needs-rebuild" "rebuilding"))

(defun not-ready-warning (kernel)
  "One line naming the projections that are not ready, or nil."
  (let ((rows (projections-not-ready kernel)))
    (when rows
      (format nil "Not ready, so answering from stale or empty state: ~{~a~^, ~}. Rebuild: ~{~a~^ ~}"
              (mapcar (lambda (r) (format nil "~a (~a~@[: ~a~])" (first r) (second r) (third r))) rows)
              (mapcar #'first rows)))))

(defun projection-cursor (kernel name)
  (third (%state (kernel-db kernel) name)))

(defun %quarantine (db projection event condition)
  (let ((now (now-iso8601))
        (message (let ((*print-length* 8) (*print-level* 3))
                   (subseq (princ-to-string condition) 0
                           (min 500 (length (princ-to-string condition)))))))
    (sql-exec db "INSERT INTO v3_quarantine(projection,storage_sequence,event_id,error_class,message,attempts,first_at,last_at) VALUES(?1,?2,?3,?4,?5,1,?6,?6) ON CONFLICT(projection,storage_sequence) DO UPDATE SET attempts=attempts+1,last_at=excluded.last_at,message=excluded.message"
              (projection-name projection) (event-seq event) (event-id event)
              (string-downcase (symbol-name (type-of condition))) message now)
    (let ((entity (and (projection-entity projection)
                       (ignore-errors (funcall (projection-entity projection) event)))))
      (when entity
        (sql-exec db "INSERT OR IGNORE INTO v3_degraded(projection,entity,event_id,reason,at) VALUES(?1,?2,?3,?4,?5)"
                  (projection-name projection) (princ-to-string entity)
                  (event-id event) message now)))))

(defun %apply-one (db projection event)
  "Apply EVENT in a savepoint. On error, roll back this event's effects,
quarantine it and mark its entity degraded. Never signals."
  (let ((start (get-internal-real-time)))
    (handler-case
        (with-savepoint (db)
          (funcall (projection-reduce projection) db event))
      (error (condition)
        (%quarantine db projection event condition)))
    (incf (projection-applied projection))
    (when (> (/ (* 1000 (- (get-internal-real-time) start))
                internal-time-units-per-second)
             *inline-budget-ms*)
      (incf (projection-over-budget projection)))))

(defun %ready-p (db projection)
  (equal "ready" (fourth (%state db (projection-name projection)))))

(defun apply-inline-projections (kernel event)
  "Called by the ledger inside the append transaction."
  (let ((db (kernel-db kernel)))
    (when (event-declared event)
      (dolist (projection (kernel-projections kernel))
        (when (and (eq (projection-kind projection) :inline)
                   (member (event-type event) (projection-consumes projection)
                           :test #'string=)
                   (%ready-p db projection))
          (%apply-one db projection event))))
    (sql-exec db "UPDATE v3_projection_state SET cursor_seq=?1,updated_at=?2 WHERE kind='inline' AND status='ready'"
              (event-seq event) (event-at event))))

(defun step-async-projection (kernel name &key (max-events 64))
  "Apply at most MAX-EVENTS pending events to async projection NAME in one
transaction. Returns the number applied. Bounded work regardless of history."
  (let* ((db (kernel-db kernel))
         (projection (find-projection name)))
    (assert (eq (projection-kind projection) :async))
    (bt:with-recursive-lock-held ((kernel-lock kernel))
      (with-transaction (db)
        (let ((state (%state db name)))
          (if (not (equal "ready" (fourth state)))
              0
              (let* ((head (ledger-head kernel))
                     (events (%events-after kernel (third state)
                                            (projection-consumes projection)
                                            max-events)))
                (dolist (event events) (%apply-one db projection event))
                (%set-state db name (projection-version projection) :async
                            (if (< (length events) max-events)
                                head
                                (event-seq (car (last events))))
                            "ready")
                (length events))))))))

(defun quarantined-events (kernel &key projection (limit 50))
  (if projection
      (sql-rows (kernel-db kernel)
                "SELECT projection,event_id,error_class,message,attempts FROM v3_quarantine WHERE projection=?1 ORDER BY storage_sequence DESC LIMIT ?2"
                projection limit)
      (sql-rows (kernel-db kernel)
                "SELECT projection,event_id,error_class,message,attempts FROM v3_quarantine ORDER BY last_at DESC LIMIT ?1"
                limit)))

(defun degraded-entities (kernel projection &key (limit 50))
  (sql-rows (kernel-db kernel)
            "SELECT entity,event_id,reason FROM v3_degraded WHERE projection=?1 ORDER BY at DESC LIMIT ?2"
            projection limit))
;;; Inspection (spec §16.6). Read-only views over projection tables and
;;; kernel bookkeeping, used by the agents' inspection tool and by migration
;;; verification. Nothing here reads the ledger beyond its head.

(defun owned-tables (projection)
  "Table names created by PROJECTION's DDL."
  (loop for ddl in (projection-tables projection)
        for start = (search "TABLE IF NOT EXISTS " ddl)
        when start
          collect (let* ((from (+ start (length "TABLE IF NOT EXISTS ")))
                         (end (position-if (lambda (c) (member c '(#\Space #\())) ddl
                                           :start from)))
                    (subseq ddl from end))))

(defun %row-hash (row)
  (parse-integer (sha256-hex (prin1-to-string row)) :radix 16))

(defun projection-digest (kernel name &key (page-size 1000))
  "Order-independent digest of NAME's owned tables: the sum of per-row hashes
modulo 2^256, read in rowid pages. Rowids themselves are excluded."
  (let ((db (kernel-db kernel))
        (sum 0))
    (dolist (table (owned-tables (find-projection name)))
      (loop with after = -1
            for rows = (sql-rows db (format nil "SELECT rowid,* FROM ~a WHERE rowid>?1 ORDER BY rowid LIMIT ?2" table)
                                 after page-size)
            while rows
            do (dolist (row rows)
                 (setf sum (mod (+ sum (%row-hash (cons table (rest row)))) (expt 2 256))))
               (setf after (first (car (last rows))))))
    (format nil "~64,'0x" sum)))

(defun projection-counts (kernel name)
  "Rows in each of NAME's owned tables, as an object."
  (let ((counts (obj)))
    (dolist (table (owned-tables (find-projection name)) counts)
      (setf (gethash table counts)
            (sql-value (kernel-db kernel) (format nil "SELECT count(*) FROM ~a LIMIT 1" table))))))

(defun projections-overview (kernel)
  "Every declared projection with its state, in name order."
  (let ((db (kernel-db kernel))
        (head (ledger-head kernel)))
    (loop for name in (sort (loop for k being the hash-keys of *projections* collect k) #'string<)
          for p = (find-projection name)
          for state = (%state db name)
          collect (obj "name" name
                       "kind" (string-downcase (symbol-name (projection-kind p)))
                       "derivation" (string-downcase (symbol-name (projection-derivation p)))
                       "version" (projection-version p)
                       "status" (or (fourth state) "not-installed")
                       "cursor" (third state)
                       "behind" (and state (max 0 (- head (third state))))
                       "quarantined" (sql-value db "SELECT count(*) FROM v3_quarantine WHERE projection=?1 LIMIT 1" name)
                       "degraded" (sql-value db "SELECT count(*) FROM v3_degraded WHERE projection=?1 LIMIT 1" name)
                       "inspectable" (if (projection-inspectable p) t :false)))))

(define-condition not-inspectable (error)
  ((what :initarg :what :reader not-inspectable-what))
  (:report (lambda (c s) (format s "~a" (not-inspectable-what c)))))

(defun %columns (db table)
  (mapcar #'second (sql-rows db (format nil "PRAGMA table_info(~a)" table))))

(defun projection-rows (kernel name &key table column value (limit 20))
  "Up to LIMIT rows of one of NAME's owned tables (TABLE, default the
first), optionally where COLUMN equals VALUE, as objects. Signals
NOT-INSPECTABLE for an undeclared name, a table or column it does not own,
or a projection whose rows are kept out of inspection."
  (let* ((p (or (gethash name *projections*)
                (error 'not-inspectable :what (format nil "There is no projection ~a." name))))
         (tables (owned-tables p))
         (table (or table (first tables)))
         (db (kernel-db kernel)))
    (unless (projection-inspectable p)
      (error 'not-inspectable :what (format nil "The rows of ~a hold credentials and are not shown." name)))
    (unless (member table tables :test #'string=)
      (error 'not-inspectable :what (format nil "~a has no table ~a; it has ~{~a~^, ~}." name table tables)))
    (let ((columns (%columns db table)))
      (when (and column (not (member column columns :test #'string=)))
        (error 'not-inspectable :what (format nil "~a has no column ~a; it has ~{~a~^, ~}." table column columns)))
      (loop for row in (if column
                           (sql-rows db (format nil "SELECT * FROM ~a WHERE ~a=?1 LIMIT ?2" table column) value limit)
                           (sql-rows db (format nil "SELECT * FROM ~a ORDER BY rowid DESC LIMIT ?1" table) limit))
            collect (let ((o (obj)))
                      (loop for c in columns for v in row
                            do (setf (gethash c o)
                                     (if (typep v '(vector (unsigned-byte 8)))
                                         (format nil "<~d bytes>" (length v))
                                         v)))
                      o)))))
