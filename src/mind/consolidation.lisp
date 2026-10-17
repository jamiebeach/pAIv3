(in-package #:pai-v3.mind)

;;; Consolidation: the agent's sleep (agent doc §13; design note
;;; interaction-recall §5a, §11). Once a night, in the operator's early
;;; hours, each registered consolidation task runs once, within its own
;;; daily budget. All sleep-time writing by a model belongs here; a module
;;; that wants something revised in sleep registers a task, and nothing else
;;; schedules it.

(define-setting "consolidation.enabled"
  :type :boolean :default :false :group "Sleep"
  :description "Run consolidation (sleep) once a night: revising what I make of things, as registered tasks need.")
(define-setting "consolidation.hour"
  :type :integer :min 0 :max 23 :default 3 :group "Sleep"
  :description "The hour, in the operator's time zone, sleep starts; it may run until three hours later.")
(define-setting "consolidation.daily_usd"
  :type :number :min 0 :max 20 :default 0.25d0 :group "Sleep"
  :description "Most a night's consolidation may spend, across its tasks.")

(define-event-type "consolidation.ran"
  :schema (("day" :string) ("task" :string) ("result" :object :optional)))

(define-projection "consolidation_runs"
  :kind :inline
  :consumes ("consolidation.ran")
  :tables ("CREATE TABLE IF NOT EXISTS consolidation_runs (day TEXT NOT NULL, task TEXT NOT NULL, result TEXT, at TEXT NOT NULL, PRIMARY KEY(day,task))")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT OR REPLACE INTO consolidation_runs(day,task,result,at) VALUES(?1,?2,?3,?4)"
                        (obj-get p "day") (obj-get p "task") (json-encode (or (obj-get p "result") (obj))) (event-at e)))))

(defvar *consolidation-tasks* '()
  "(name . function) pairs; function is (lambda (kernel work)) doing one
night's share of a task within the remaining budget, returning a result
object for the record.")

(defmacro define-consolidation (name (kernel work) &body body)
  "Register consolidation task NAME, run once a night."
  `(let ((fn (lambda (,kernel ,work) (declare (ignorable ,kernel ,work)) ,@body)))
     (setf *consolidation-tasks*
           (append (remove ,name *consolidation-tasks* :key #'car :test #'equal) (list (cons ,name fn))))
     ,name))

(defun %sleep-day (kernel)
  "The operator's local date, and whether it is now within sleep hours."
  (let* ((slot (local-slot (now) (%zone kernel nil)))
         (hour (parse-integer slot :start 11 :end 13))
         (start (setting kernel "consolidation.hour")))
    (values (subseq slot 0 10) (< -1 (mod (- hour start) 24) 3))))

(defun consolidation-ran-p (kernel day task)
  (sql-value (kernel-db kernel) "SELECT 1 FROM consolidation_runs WHERE day=?1 AND task=?2 LIMIT 1" day task))

(defun consolidation-budget-left (kernel)
  "Micro-USD sleep may still spend today."
  (max 0 (- (round (* 1000000 (setting kernel "consolidation.daily_usd"))) (or (spent-today kernel "consolidation") 0))))

(defun request-consolidation (kernel)
  "In sleep hours, ask for each task not yet run tonight."
  (when (eq t (setting kernel "consolidation.enabled"))
    (multiple-value-bind (day asleep) (%sleep-day kernel)
      (when asleep
        (loop for (name) in *consolidation-tasks*
              unless (or (consolidation-ran-p kernel day name) (work-pending-p kernel "consolidate" name))
                do (request-work kernel "consolidate" name (obj "day" day) :priority :maintenance))))))

(defun consolidate (kernel work)
  (let* ((name (getf work :entity))
         (task (cdr (assoc name *consolidation-tasks* :test #'equal)))
         (day (or (obj-get (getf work :payload) "day") (%sleep-day kernel))))
    (unless task (error 'handler-failure :class :source :reason (format nil "no consolidation task ~a" name)))
    (let ((result (funcall task kernel work)))
      (append! kernel "consolidation.ran" (obj "day" day "task" name "result" result))
      result)))

(define-handler "consolidate" :run #'consolidate :paid t :estimate-microusd 20000)

(pushnew 'request-consolidation *periodic-hooks*)
