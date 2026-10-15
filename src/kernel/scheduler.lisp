(in-package #:pai-v3.kernel)

;;; Handlers, the scheduler and budgets (engineering spec §4.4–4.5, §14;
;;; NFR BUD-1..3). Effects happen only in handlers; every request, outcome,
;;; deferral and spend is an event, and the work queue is an ordinary inline
;;; projection over those events, so it is rebuildable like any other.

;;; Priorities: the operator always comes first.
(defparameter +priorities+ '(:operator 0 :work 1 :concern 2 :private 3 :maintenance 4))

(defun priority-rank (priority)
  (or (getf +priorities+ priority) (error "Unknown priority ~s" priority)))

(defvar *budget* (list :daily-microusd 2000000 :operator-reserve 0.2d0)
  "Code defaults for the budget settings below (BUD-1).")

(define-setting "budget.daily_microusd"
  :type :integer :min 0 :group "Budgets"
  :default (getf *budget* :daily-microusd)
  :description "Paid model spend allowed per UTC day, in millionths of a dollar, paced across the day.")
(define-setting "budget.operator_reserve"
  :type :number :min 0 :max 1 :group "Budgets"
  :default (getf *budget* :operator-reserve)
  :description "Share of the daily budget kept for answering the operator; background work cannot use it.")

;;; Handlers

(defstruct handler
  name run (paid nil) (estimate-microusd 0) (on-dead nil)
  (transient-attempts 8) (transient-base-seconds 30) (transient-cap-seconds 3600)
  (model-attempts 3) (model-base-seconds 60) (recovery-cooldown-seconds 3600))

(defvar *handlers* (make-hash-table :test 'equal))

(defmacro define-handler (name &rest options &key run &allow-other-keys)
  "Declare handler NAME. RUN is (lambda (kernel work)) performing one bounded
step; WORK is a plist (:id :entity :payload :attempts). It may append events
as it goes; it returns a result object (or NIL) on success and signals
HANDLER-FAILURE to report a classified failure. PAID handlers are admitted
against the budget with ESTIMATE-MICROUSD before they run. ON-DEAD, if given,
is (lambda (kernel entity class reason)), called after the work is
dead-lettered so the owning module can record what that means for its entity."
  (declare (ignore run))
  `(setf (gethash ,name *handlers*) (make-handler :name ,name ,@options)))

(define-condition handler-failure (error)
  ((class :initarg :class :reader failure-class)     ; :source :transient :model
   (reason :initarg :reason :reader failure-reason)
   (retry-after :initarg :retry-after :initform nil :reader failure-retry-after))
  (:report (lambda (c s) (format s "~(~a~) failure: ~a" (failure-class c) (failure-reason c)))))

;;; Events

(define-event-type "work.requested"
  :schema (("handler" :string) ("entity" :string) ("priority" :integer) ("payload" :object)))
(define-event-type "work.completed"
  :schema (("work_id" :integer) ("result" :object :optional)))
(define-event-type "work.failed"
  :schema (("work_id" :integer) ("class" :string) ("reason" :string)
           ("retry_after_seconds" :integer :optional)))
(define-event-type "work.deferred"
  :schema (("work_id" :integer) ("reason" :string) ("next_at" :string)))
(define-event-type "work.dead"
  :schema (("work_id" :integer) ("handler" :string) ("entity" :string)
           ("class" :string) ("reason" :string)))
(define-event-type "spend.recorded"
  :schema (("microusd" :integer) ("lane" :string) ("work_id" :integer :optional)))

;;; The work queue projection

(defun %retry-plan (handler class attempts at &optional retry-after)
  "Status and next time after failure number ATTEMPTS of CLASS at AT
(universal time). Deterministic: a function of the event alone (I3)."
  (flet ((later (seconds) (values "queued" (iso8601 (+ at (max seconds (or retry-after 0))))))
         (dead () (values "dead" nil)))
    (cond ((null handler) (dead))
          ((string= class "source") (dead))
          ((string= class "model")
           (cond ((< attempts (handler-model-attempts handler))
                  (later (* (handler-model-base-seconds handler) attempts)))
                 ;; One recovery attempt after a cool-down, then dead-letter.
                 ((= attempts (handler-model-attempts handler))
                  (later (handler-recovery-cooldown-seconds handler)))
                 (t (dead))))
          (t ; transient, or anything unclassified
           (if (< attempts (handler-transient-attempts handler))
               (later (min (handler-transient-cap-seconds handler)
                           (* (handler-transient-base-seconds handler)
                              (expt 2 (1- attempts)))))
               (dead))))))

(defun %reduce-work (db event)
  (let ((p (event-payload event))
        (at (event-at event)))
    (flet ((field (k) (obj-get p k)))
      (cond
        ((string= (event-type event) "work.requested")
         (sql-exec db "INSERT INTO v3_work(id,handler,entity,priority,payload_json,status,attempts,next_at,updated_at) VALUES(?1,?2,?3,?4,?5,'queued',0,?6,?6)"
                   (event-id event) (field "handler") (field "entity") (field "priority")
                   (json-encode (field "payload")) at))
        ((string= (event-type event) "work.completed")
         (sql-exec db "UPDATE v3_work SET status='done',deferred_reason=NULL,updated_at=?2 WHERE id=?1"
                   (field "work_id") at))
        ((string= (event-type event) "work.deferred")
         (sql-exec db "UPDATE v3_work SET next_at=?2,deferred_reason=?3,updated_at=?4 WHERE id=?1"
                   (field "work_id") (field "next_at") (field "reason") at))
        ((string= (event-type event) "work.failed")
         (destructuring-bind (handler-name attempts)
             (sql-row db "SELECT handler,attempts FROM v3_work WHERE id=?1 LIMIT 1" (field "work_id"))
           (multiple-value-bind (status next)
               (%retry-plan (gethash handler-name *handlers*) (field "class")
                            (1+ attempts) (parse-iso8601 at)
                            (field "retry_after_seconds"))
             (sql-exec db "UPDATE v3_work SET status=?2,attempts=attempts+1,next_at=?3,last_class=?4,last_reason=?5,deferred_reason=NULL,updated_at=?6 WHERE id=?1"
                       (field "work_id") status next (field "class") (field "reason") at))))))))

(define-projection "kernel.work"
  :kind :inline
  :consumes ("work.requested" "work.completed" "work.failed" "work.deferred")
  :tables ("CREATE TABLE IF NOT EXISTS v3_work (id INTEGER PRIMARY KEY, handler TEXT NOT NULL, entity TEXT NOT NULL, priority INTEGER NOT NULL, payload_json TEXT NOT NULL, status TEXT NOT NULL, attempts INTEGER NOT NULL, next_at TEXT, last_class TEXT, last_reason TEXT, deferred_reason TEXT, updated_at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS v3_work_due_idx ON v3_work(status,priority,next_at)"
           "CREATE INDEX IF NOT EXISTS v3_work_entity_idx ON v3_work(handler,entity)")
  :entity (lambda (e) (or (obj-get (event-payload e) "work_id") (event-id e)))
  :reduce #'%reduce-work)

(define-projection "kernel.spend"
  :kind :inline
  :consumes ("spend.recorded")
  :tables ("CREATE TABLE IF NOT EXISTS v3_spend (day TEXT NOT NULL, lane TEXT NOT NULL, microusd INTEGER NOT NULL, PRIMARY KEY(day,lane))")
  :reduce (lambda (db e)
            (sql-exec db "INSERT INTO v3_spend(day,lane,microusd) VALUES(?1,?2,?3) ON CONFLICT(day,lane) DO UPDATE SET microusd=microusd+excluded.microusd"
                      (subseq (event-at e) 0 10) (obj-get (event-payload e) "lane")
                      (obj-get (event-payload e) "microusd"))))

;;; API

(defun request-work (kernel handler entity payload &key (priority :work) caused-by)
  "Queue a step of HANDLER for ENTITY. Returns the work id."
  (event-id (append! kernel "work.requested"
                     (obj "handler" handler "entity" (princ-to-string entity)
                          "priority" (priority-rank priority) "payload" (or payload (obj)))
                     :caused-by caused-by)))

(defun work-pending-p (kernel handler entity)
  "True when HANDLER has work for ENTITY that is queued and not finished."
  (and (sql-value (kernel-db kernel)
                  "SELECT 1 FROM v3_work WHERE handler=?1 AND entity=?2 AND status='queued' LIMIT 1"
                  handler (princ-to-string entity))
       t))

(defun work-status (kernel work-id)
  "Plist describing work WORK-ID, or NIL."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT status,attempts,next_at,last_class,last_reason,deferred_reason FROM v3_work WHERE id=?1 LIMIT 1"
                      work-id)))
    (and row (destructuring-bind (status attempts next class reason deferred) row
               (list :status status :attempts attempts :next-at next
                     :last-class class :last-reason reason :deferred-reason deferred)))))

(defun dead-letters (kernel &key (limit 50))
  (sql-rows (kernel-db kernel)
            "SELECT id,handler,entity,attempts,last_class,last_reason FROM v3_work WHERE status='dead' ORDER BY priority,next_at LIMIT ?1"
            limit))

(defun record-spend (kernel microusd &key work-id (lane "background"))
  (append! kernel "spend.recorded" (obj "microusd" microusd "lane" lane "work_id" work-id)))

(defun spent-today (kernel &optional lane)
  (let ((day (subseq (now-iso8601) 0 10)))
    (or (if lane
            (sql-value (kernel-db kernel) "SELECT microusd FROM v3_spend WHERE day=?1 AND lane=?2 LIMIT 1" day lane)
            (sql-value (kernel-db kernel) "SELECT sum(microusd) FROM v3_spend WHERE day=?1 LIMIT 2" day))
        0)))

;;; Budget admission (checked before anything is recorded)

(defun %day-start (ut) (- ut (mod ut 86400)))

(defun admit-spend (kernel priority estimate)
  "Return T, or NIL with a reason and the universal time it may next run."
  (let* ((now (now))
         (daily (setting kernel "budget.daily_microusd"))
         (background (* daily (- 1 (setting kernel "budget.operator_reserve"))))
         (tomorrow (+ (%day-start now) 86400)))
    (if (= priority (priority-rank :operator))
        (if (<= (+ (spent-today kernel) estimate) daily)
            t
            (values nil "daily budget exhausted" tomorrow))
        (let* ((spent (spent-today kernel "background"))
               (needed (+ spent estimate))
               (allowance (* background (/ (- now (%day-start now)) 86400))))
          (cond ((<= needed allowance) t)
                ((> needed background)
                 (values nil "background budget exhausted for today" tomorrow))
                (t (values nil "background spend ahead of the day's pace"
                           (+ (%day-start now) (ceiling (* 86400 (/ needed background)))))))))))

;;; The scheduler

(defun %next-due (kernel)
  (sql-row (kernel-db kernel)
           "SELECT id,handler,entity,priority,payload_json,attempts,deferred_reason FROM v3_work WHERE status='queued' AND next_at<=?1 ORDER BY priority,next_at LIMIT 1"
           (now-iso8601)))

(defun run-scheduler-step (kernel)
  "Run at most one bounded handler step: the most urgent due work, operator
first. Returns :IDLE, (:DEFERRED id reason) or (:RAN id outcome). No kernel
lock or transaction is held while the handler runs."
  (let ((row (%next-due kernel)))
    (unless row (return-from run-scheduler-step :idle))
    (destructuring-bind (id name entity priority payload attempts deferred) row
      (declare (ignore deferred))
      (let ((handler (gethash name *handlers*)))
        (unless handler
          (append! kernel "work.failed" (obj "work_id" id "class" "source"
                                             "reason" (format nil "no handler ~s" name)))
          (return-from run-scheduler-step (list :ran id :dead)))
        (when (handler-paid handler)
          (multiple-value-bind (ok reason next) (admit-spend kernel priority
                                                             (handler-estimate-microusd handler))
            (unless ok
              ;; Admission refused: nothing is requested or charged. One
              ;; deferral records why and the computed time it may next be
              ;; admitted, so the item is not re-examined before then (v2
              ;; re-tried every few minutes and logged each refusal).
              (append! kernel "work.deferred"
                       (obj "work_id" id "reason" reason "next_at" (iso8601 next)))
              (return-from run-scheduler-step (list :deferred id reason)))))
        (handler-case
            (let ((result (funcall (handler-run handler) kernel
                                   (list :id id :entity entity :attempts attempts
                                         :payload (json-parse payload)))))
              (append! kernel "work.completed"
                       (obj "work_id" id "result" (and (hash-table-p result) result)))
              (list :ran id :completed))
          (handler-failure (f)
            (%failed kernel id name entity (string-downcase (symbol-name (failure-class f)))
                     (princ-to-string (failure-reason f)) (failure-retry-after f)))
          (error (e)
            (%failed kernel id name entity "transient"
                     (subseq (princ-to-string e) 0 (min 500 (length (princ-to-string e))))
                     nil)))))))

(defun %failed (kernel id handler entity class reason retry-after)
  "Record a failure; if it dead-letters the work, announce that as an event
so the owning module's projection can mark its own entity (I3)."
  (append! kernel "work.failed" (obj "work_id" id "class" class "reason" reason
                                     "retry_after_seconds" retry-after))
  (when (equal "dead" (getf (work-status kernel id) :status))
    (append! kernel "work.dead" (obj "work_id" id "handler" handler "entity" entity
                                     "class" class "reason" reason))
    (let ((h (gethash handler *handlers*)))
      (when (and h (handler-on-dead h))
        (funcall (handler-on-dead h) kernel entity class reason))))
  (list :ran id :failed))
