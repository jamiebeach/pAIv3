(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; The work docket (bead v3-clp.18), with v2's rules: capped open items,
;;; per-item step and cost caps that park, operator-only reopening, and one
;;; eligible item per quiet run, high priority first.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defmacro with-docket ((k) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (advance 43200)
       (change-setting ,k "quiet.enabled" t :origin *operator*)
       (change-setting ,k "quiet.review" :false :origin *operator*)
       (change-setting ,k "quiet.free_time_minutes" 0 :origin *operator*)
       ,@body)))

(defun open-one (k title &key (priority "normal"))
  (open-work k :title title :purpose "keep it going" :operator-benefit "Sam saves time"
               :next-step "draft the first part" :priority priority))

(defun docket-tool (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "work_docket" *tools*)) k 1 (apply #'obj args)))

(deftest open-items-are-capped-and-need-a-purpose
  (with-docket (k)
    (is (open-one k "One")) (is (open-one k "Two")) (is (open-one k "Three"))
    (is (search "most allowed" (nth-value 1 (open-one k "Four"))))
    (is (search "give a title" (nth-value 1 (open-work k :title "No purpose"))))))

(deftest quiet-time-works-on-the-item-that-comes-first
  (with-docket (k)
    (open-one k "Ordinary")
    (let ((urgent (open-one k "Urgent" :priority "high")))
      (let ((id (quiet-tick k)))
        (is (equal "docket" (getf (thought-row k id) :origin)))
        (is (search "Urgent" (getf (thought-row k id) :reason)) "high priority first")
        (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id))
      (let ((id (quiet-tick k)))
        (is (search "Ordinary" (getf (thought-row k id) :reason)) "the worked item waits its revisit time")
        (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" id))
      (is (equal "nothing open" (nth-value 1 (quiet-tick k))) "both wait now")
      (is (search "is now waiting"
                  (docket-tool k "action" "wait" "work_id" urgent "note" "asked Sam" "next_step" "hear back"
                                 "revisit_after_seconds" 7200))))))

(deftest a-spent-item-is-parked-told-and-reopened-only-by-the-operator
  (with-docket (k)
    (change-setting k "docket.max_item_steps" 3 :origin *operator*)
    (let ((id (open-one k "Big job")))
      (sql-exec (kernel-db k) "UPDATE docket_items SET steps=3 WHERE work_id=?1" id)
      (quiet-tick k)
      (let ((item (first (docket-items k))))
        (is (equal "parked" (obj-get item "display_state")))
        (is (equal "steps-cap" (obj-get item "parked_code"))))
      (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice") "the operator is told")
      (is (search "only the operator can reopen" (docket-tool k "action" "update" "work_id" id "note" "more" "next_step" "x")))
      (is (search "Reopened" (reopen-work k id)))
      (let ((item (first (docket-items k))))
        (is (equal "waiting" (obj-get item "display_state")))
        (is (= 0 (obj-get item "steps")) "its allowance starts again")))))

(deftest a-run-counts-its-steps-against-its-item
  (with-docket (k)
    (let ((id (open-one k "Counted")))
      (let ((thought (quiet-tick k)))
        (append! k "thought.step" (obj "thought_id" thought "kind" "model" "part" 0 "text" "working"))
        (append! k "thought.step" (obj "thought_id" thought "kind" "model" "part" 0 "text" "still working"))
        (is (= 2 (obj-get (first (docket-items k)) "steps")) id)))))

(deftest the-tool-records-progress-and-closes
  (with-docket (k)
    (is (search "Opened" (docket-tool k "action" "open" "title" "Plan the course" "purpose" "a course on agents"
                                      "operator_benefit" "Sam teaches it in the fall" "next_step" "outline module 1")))
    (let ((id (obj-get (first (docket-items k)) "work_id")))
      (is (search "Plan the course" (docket-tool k "action" "list")))
      (is (search "Give a note" (docket-tool k "action" "update" "work_id" id)))
      (is (search "is now active" (docket-tool k "action" "update" "work_id" id "note" "outlined module 1"
                                                 "next_step" "outline module 2")))
      (is (equal "outline module 2" (obj-get (first (docket-items k)) "next_step")))
      (is (search "is now completed" (docket-tool k "action" "complete" "work_id" id "note" "done")))
      (is (null (docket-items k)) "a closed item is no longer open"))))

(deftest a-run-pays-from-its-item-and-the-cost-cap-parks-it
  (with-docket (k)
    (open-one k "Costly")
    (let* ((thought (quiet-tick k))
           (work (sql-value (kernel-db k) "SELECT id FROM v3_work WHERE handler=?1 AND entity=?2 LIMIT 1"
                            "thought" (princ-to-string thought))))
      (is work "the run has a work row")
      (dotimes (i 2)
        (append! k "provider.call.completed"
                 (obj "profile" "p" "model" "m" "estimated_prompt_tokens" 1 "prompt_tokens" 1 "completion_tokens" 1
                      "prompt_chars" 1 "microusd" 80000 "milliseconds" 1 "work_id" work)))
      (is (= 160000 (obj-get (first (docket-items k)) "microusd")) "each call's cost lands on its item")
      (sql-exec (kernel-db k) "UPDATE thoughts SET status='ended' WHERE id=?1" thought)
      (quiet-tick k)
      (is (equal "cost-cap" (obj-get (first (docket-items k)) "parked_code")) "over 0.15 USD: parked"))))

(run-suite "mind-docket")
