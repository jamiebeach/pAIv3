(in-package #:pai-v3.mind)

;;; The work docket (bead v3-clp.18): goals worth keeping beyond one
;;; conversation, worked on a step at a time in quiet time. Ported from v2's
;;; work docket (src/mind/conscious/work-docket.lisp) with its rules:
;;;  - an item has a title, a purpose, the operator benefit, a next useful
;;;    step and a priority, and is active, waiting, completed or cancelled;
;;;  - open items are capped; each item has a step and a cost cap across all
;;;    its runs, and spending either parks it: waiting, visible, and reopened
;;;    only by the operator (/docket-reopen), never silently dropped;
;;;  - quiet time picks one eligible item: high priority first, then the one
;;;    eligible longest, then the one least recently worked;
;;;  - a run grants no new authority: an ordinary private thought with the
;;;    agent's ordinary tools.
;;; Docket runs spend from the quiet budget. Concerns come before the docket
;;; in quiet time, and rumination only when neither has anything.

(define-setting "docket.max_open"
  :type :integer :min 1 :max 64 :default 3 :group "Work docket"
  :description "Most open (active or waiting, not parked) work items.")
(define-setting "docket.max_item_steps"
  :type :integer :min 1 :max 1000 :default 12 :group "Work docket"
  :description "Most model calls one work item may spend across all its runs before it is parked.")
(define-setting "docket.max_item_usd"
  :type :number :min 0 :max 100 :default 0.15d0 :group "Work docket"
  :description "Most one work item may spend across all its runs before it is parked.")
(define-setting "docket.revisit_seconds"
  :type :integer :min 60 :max 604800 :default 1800 :group "Work docket"
  :description "How long after a run before an item may be worked on again, unless the run says otherwise.")

(define-event-type "docket.opened"
  :schema (("work_id" :string) ("title" :string) ("purpose" :string) ("operator_benefit" :string)
           ("next_step" :string) ("priority" :string) ("source_event_id" :integer :optional)))
(define-event-type "docket.transitioned"
  :schema (("work_id" :string) ("state" :string) ("note" :string) ("next_step" :string)
           ("next_eligible_at" :string)))
(define-event-type "docket.parked"
  :schema (("work_id" :string) ("code" :string)))
(define-event-type "docket.reopened"
  :schema (("work_id" :string)))
(define-event-type "docket.attended"
  :schema (("work_id" :string) ("thought_id" :integer) ("next_eligible_at" :string)))

(defparameter +docket-states+ '("active" "waiting" "completed" "cancelled"))

(defun %docket-thought (db thought-id)
  "The work item a thought (or its root) is a run of, or NIL."
  (let ((root (or (sql-value db "SELECT root_id FROM thoughts WHERE id=?1 LIMIT 1" thought-id) thought-id)))
    (sql-value db "SELECT work_id FROM docket_thoughts WHERE thought_id=?1 LIMIT 1" root)))

(defun %reduce-docket (db e)
  (let ((p (event-payload e)) (type (event-type e)) (at (event-at e)))
    (flet ((f (k) (obj-get p k)))
      (cond
        ((string= type "docket.opened")
         (sql-exec db "INSERT INTO docket_items(work_id,title,purpose,operator_benefit,next_step,priority,state,note,next_eligible_at,parked,parked_code,parked_at,steps,microusd,revision,opened_at,last_progress_at,source_event_id) VALUES(?1,?2,?3,?4,?5,?6,'active',NULL,?7,0,NULL,NULL,0,0,1,?7,?7,?8)"
                   (f "work_id") (f "title") (f "purpose") (f "operator_benefit") (f "next_step") (f "priority") at
                   (f "source_event_id")))
        ((string= type "docket.transitioned")
         (sql-exec db "UPDATE docket_items SET state=?2,note=?3,next_step=?4,next_eligible_at=?5,last_progress_at=?6,revision=revision+1 WHERE work_id=?1"
                   (f "work_id") (f "state") (f "note") (f "next_step") (f "next_eligible_at") at))
        ((string= type "docket.parked")
         (sql-exec db "UPDATE docket_items SET parked=1,parked_code=?2,parked_at=?3,state='waiting',revision=revision+1 WHERE work_id=?1"
                   (f "work_id") (f "code") at))
        ((string= type "docket.reopened")
         ;; Reopening starts the item's allowance again, as in v2.
         (sql-exec db "UPDATE docket_items SET parked=0,parked_code=NULL,parked_at=NULL,state='waiting',steps=0,microusd=0,next_eligible_at=?2,revision=revision+1 WHERE work_id=?1"
                   (f "work_id") at))
        ((string= type "docket.attended")
         (sql-exec db "INSERT OR IGNORE INTO docket_thoughts(thought_id,work_id) VALUES(?1,?2)" (f "thought_id") (f "work_id"))
         (sql-exec db "UPDATE docket_items SET next_eligible_at=?2,last_progress_at=?3 WHERE work_id=?1"
                   (f "work_id") (f "next_eligible_at") at))
        ((and (string= type "thought.step") (equal "model" (f "kind")))
         (let ((item (%docket-thought db (f "thought_id"))))
           (when item (sql-exec db "UPDATE docket_items SET steps=steps+1 WHERE work_id=?1" item))))
        ((string= type "provider.call.completed")
         ;; The call's work row names the thought that made it.
         (let* ((thought (and (f "work_id")
                              (sql-value db "SELECT entity FROM v3_work WHERE id=?1 AND handler=?2 LIMIT 1" (f "work_id") "thought")))
                (item (and thought (ignore-errors (%docket-thought db (parse-integer thought))))))
           (when item
             (sql-exec db "UPDATE docket_items SET microusd=microusd+?2 WHERE work_id=?1" item (or (f "microusd") 0)))))))))

(define-projection "docket"
  :kind :inline
  :consumes ("docket.opened" "docket.transitioned" "docket.parked" "docket.reopened" "docket.attended"
             "thought.step" "provider.call.completed")
  :tables ("CREATE TABLE IF NOT EXISTS docket_items (work_id TEXT PRIMARY KEY, title TEXT NOT NULL, purpose TEXT NOT NULL, operator_benefit TEXT NOT NULL, next_step TEXT NOT NULL, priority TEXT NOT NULL, state TEXT NOT NULL, note TEXT, next_eligible_at TEXT NOT NULL, parked INTEGER NOT NULL, parked_code TEXT, parked_at TEXT, steps INTEGER NOT NULL, microusd INTEGER NOT NULL, revision INTEGER NOT NULL, opened_at TEXT NOT NULL, last_progress_at TEXT NOT NULL, source_event_id INTEGER)"
           "CREATE INDEX IF NOT EXISTS docket_items_state ON docket_items(state,parked)"
           "CREATE TABLE IF NOT EXISTS docket_thoughts (thought_id INTEGER PRIMARY KEY, work_id TEXT NOT NULL)")
  :entity (lambda (e) (or (obj-get (event-payload e) "work_id") (princ-to-string (event-id e))))
  :reduce #'%reduce-docket)

(defparameter +docket-columns+
  "work_id,title,purpose,operator_benefit,next_step,priority,state,note,next_eligible_at,parked,parked_code,parked_at,steps,microusd,revision,opened_at,last_progress_at")

(defun docket-items (kernel &key (states '("active" "waiting")))
  "Items in STATES as objects, oldest first."
  (mapcar (lambda (r)
            (destructuring-bind (id title purpose benefit next priority state note eligible parked code parked-at steps micro revision opened last) r
              (obj "work_id" id "title" title "purpose" purpose "operator_benefit" benefit "next_step" next
                   "priority" priority "state" state "note" note "next_eligible_at" eligible
                   "parked" (if (eql parked 1) t :false) "parked_code" code "parked_at" parked-at
                   "display_state" (if (eql parked 1) "parked" state)
                   "steps" steps "microusd" micro "revision" revision "opened_at" opened "last_progress_at" last)))
          (apply #'sql-rows (kernel-db kernel)
                 (format nil "SELECT ~a FROM docket_items WHERE state IN (~a) ORDER BY opened_at LIMIT 64"
                         +docket-columns+ (%in-list 1 (length states)))
                 states)))

(defun %docket-item (kernel id)
  (first (remove-if-not (lambda (i) (equal id (obj-get i "work_id")))
                        (docket-items kernel :states +docket-states+))))

(defun %live-items (kernel)
  (remove-if (lambda (i) (eq t (obj-get i "parked"))) (docket-items kernel)))

(defun open-work (kernel &key title purpose operator-benefit next-step (priority "normal") source-event-id)
  "Open a work item. Returns (values id nil) or (values nil refusal)."
  (flet ((text-ok (s n) (and (stringp s) (< 0 (length (string-trim " " s)) (1+ n)))))
    (cond ((not (and (text-ok title 240) (text-ok purpose 1200) (text-ok operator-benefit 1200) (text-ok next-step 1200)))
           (values nil "give a title, a purpose, how it benefits the operator, and the next useful step"))
          ((not (member priority '("normal" "high") :test #'equal)) (values nil "priority is normal or high"))
          ((>= (length (%live-items kernel)) (setting kernel "docket.max_open"))
           (values nil (format nil "already ~d open work items, the most allowed; complete, cancel or let one wait first"
                               (setting kernel "docket.max_open"))))
          (t (let ((id (format nil "w-~a" (base64url-encode (ironclad:random-data 6)))))
               (append! kernel "docket.opened"
                        (obj "work_id" id "title" title "purpose" purpose "operator_benefit" operator-benefit
                             "next_step" next-step "priority" priority "source_event_id" source-event-id))
               (values id nil))))))

(defun transition-work (kernel id state note next-step &key (revisit-seconds (setting kernel "docket.revisit_seconds")))
  (append! kernel "docket.transitioned"
           (obj "work_id" id "state" state "note" note "next_step" next-step
                "next_eligible_at" (iso8601 (+ (now) (max 0 revisit-seconds))))))

(defun reopen-work (kernel id)
  "Operator only: make a parked item eligible again, under the cap."
  (let ((item (%docket-item kernel id)))
    (cond ((null item) (format nil "No work item ~a." id))
          ((not (eq t (obj-get item "parked"))) (format nil "~a is not parked." id))
          ((>= (length (%live-items kernel)) (setting kernel "docket.max_open"))
           (format nil "Not reopened: already ~d open work items." (length (%live-items kernel))))
          (t (append! kernel "docket.reopened" (obj "work_id" id))
             (format nil "Reopened ~a: ~a" id (obj-get item "title"))))))

(defun %park-spent-work (kernel)
  "Park live items whose steps or cost are spent, and tell the operator."
  (let ((max-steps (setting kernel "docket.max_item_steps"))
        (max-micro (round (* 1000000 (setting kernel "docket.max_item_usd")))))
    (dolist (item (%live-items kernel))
      (let ((code (cond ((>= (obj-get item "steps") max-steps) "steps-cap")
                        ((and (plusp max-micro) (>= (obj-get item "microusd") max-micro)) "cost-cap"))))
        (when code
          (append! kernel "docket.parked" (obj "work_id" (obj-get item "work_id") "code" code))
          (post-notice kernel :channel "web" :audience "operator"
                              :text (format nil "I parked work item \"~a\" (~a). Reopen it with /docket-reopen ~a"
                                            (obj-get item "title") code (obj-get item "work_id"))))))))

(defun %start-work (kernel)
  "A run of the eligible work item that comes first, or NIL."
  (let* ((now (now-iso8601))
         (eligible (remove-if-not (lambda (i) (string<= (obj-get i "next_eligible_at") now)) (%live-items kernel)))
         (pick (first (sort (copy-list eligible)
                            (lambda (a b)
                              (let ((ha (equal "high" (obj-get a "priority"))) (hb (equal "high" (obj-get b "priority"))))
                                (cond ((and ha (not hb)) t) ((and hb (not ha)) nil)
                                      ((string< (obj-get a "next_eligible_at") (obj-get b "next_eligible_at")) t)
                                      ((string< (obj-get b "next_eligible_at") (obj-get a "next_eligible_at")) nil)
                                      (t (string< (obj-get a "last_progress_at") (obj-get b "last_progress_at"))))))))))
    (when pick
      (let* ((id (obj-get pick "work_id"))
             (thought (request-thought
                       kernel "docket"
                       (format nil "Quiet time. You are working on one of your maintained work items (~a, ~a priority):~%~%~a~%~%~
Purpose: ~a~%How it helps the operator: ~a~%Next useful step: ~a~@[~%Where you got to: ~a~]~%~%~
Spent so far: ~d of ~d model calls. Take the next useful step now, then use the work_docket tool to record it: ~
update (progress and the new next step), wait (if it must wait, and for how long), complete, or cancel. If this work makes a document or card, use document to save an artifact, read earlier revisions to resume, and deliberately keep, deliver or abandon it. A description of what you would make is not the deliverable."
                               id (obj-get pick "priority") (obj-get pick "title") (obj-get pick "purpose")
                               (obj-get pick "operator_benefit") (obj-get pick "next_step") (obj-get pick "note")
                               (obj-get pick "steps") (setting kernel "docket.max_item_steps")))))
        (append! kernel "docket.attended"
                 (obj "work_id" id "thought_id" thought
                      "next_eligible_at" (iso8601 (+ (now) (setting kernel "docket.revisit_seconds")))))
        thought))))

;;; The tool

(define-tool "work_docket"
  :description "Keep work that should outlive this conversation: real goals that help the operator, worked on a step at a time
in quiet time. action: open (title, purpose, operator_benefit, next_step, optionally priority normal|high), list,
update (work_id, note: what was done, next_step), wait (work_id, note, next_step, revisit_after_seconds), complete
(work_id, note), cancel (work_id, note). Open items are capped, and each item has a step and cost allowance; an item
that spends it is parked until the operator reopens it. A parked item can only be completed or cancelled."
  :summary "Keep work that should outlive this conversation, and record progress on it."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "open" "list" "update" "wait" "complete" "cancel"))
                                     "work_id" (obj "type" "string") "title" (obj "type" "string")
                                     "purpose" (obj "type" "string") "operator_benefit" (obj "type" "string")
                                     "next_step" (obj "type" "string") "priority" (obj "type" "string")
                                     "note" (obj "type" "string") "revisit_after_seconds" (obj "type" "integer"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (let* ((action (obj-get args "action")) (id (obj-get args "work_id"))
                (item (and (stringp id) (%docket-item kernel id))))
           (cond
             ((equal action "open")
              (multiple-value-bind (new refusal)
                  (open-work kernel :title (obj-get args "title") :purpose (obj-get args "purpose")
                                    :operator-benefit (obj-get args "operator_benefit") :next-step (obj-get args "next_step")
                                    :priority (or (obj-get args "priority") "normal")
                                    :source-event-id thought-id)
                (if new (format nil "Opened ~a." new) (format nil "Not opened: ~a." refusal))))
             ((equal action "list")
              (let ((items (docket-items kernel)))
                (if (null items) "No open work items."
                    (format nil "~{~a~^~%~}"
                            (mapcar (lambda (i) (format nil "~a [~a, ~a, ~d calls] ~a~%  next: ~a"
                                                        (obj-get i "work_id") (obj-get i "display_state") (obj-get i "priority")
                                                        (obj-get i "steps") (obj-get i "title") (obj-get i "next_step")))
                                    items)))))
             ((null item) "Give the work_id of an item on the docket.")
             ((member (obj-get item "state") '("completed" "cancelled") :test #'equal) "That item is already closed.")
             ((and (eq t (obj-get item "parked")) (member action '("update" "wait") :test #'equal))
              "That item is parked; only the operator can reopen it. You may complete or cancel it.")
             ((member action '("update" "wait" "complete" "cancel") :test #'equal)
              (let ((note (or (obj-get args "note") "")) (next (or (obj-get args "next_step") (obj-get item "next_step"))))
                (if (zerop (length note))
                    "Give a note saying what changed."
                    (progn (transition-work kernel id
                                            (cond ((equal action "update") "active") ((equal action "wait") "waiting")
                                                  ((equal action "complete") "completed") (t "cancelled"))
                                            note next
                                            :revisit-seconds (or (obj-get args "revisit_after_seconds")
                                                                 (setting kernel "docket.revisit_seconds")))
                           (format nil "Recorded: ~a is now ~a." id
                                   (cond ((equal action "update") "active") ((equal action "wait") "waiting")
                                         ((equal action "complete") "completed") (t "cancelled")))))))
             (t "action must be open, list, update, wait, complete or cancel")))))

(define-slot-source :docket (kernel request max)
  (when (equal "operator" (getf request :audience))
    (mapcar (lambda (i) (format nil "- ~a (~a~:[~;, parked~]): next, ~a" (obj-get i "title") (obj-get i "priority")
                                (eq t (obj-get i "parked")) (obj-get i "next_step")))
            (docket-items kernel))))

(define-open-items "docket" (kernel terms audience)
  (when (equal audience "operator")
    (loop for item in (docket-items kernel)
          when (or (mentions-any-p (obj-get item "title") terms) (mentions-any-p (obj-get item "next_step") terms))
            collect (format nil "Work I am keeping: ~a (next: ~a)" (obj-get item "title") (obj-get item "next_step")))))
