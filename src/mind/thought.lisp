(in-package #:pai-v3.mind)

;;; Thoughts (agent doc §6; spec §16.9). The mind is one recursive function:
;;;
;;;   (think mind messages)
;;;     base case: the model answers           -> answer
;;;     base case: the allowance is spent      -> rest
;;;     recursive case: the model wants tools  -> run them, recur with the
;;;                                               enriched message list
;;;
;;; It runs trampolined. Everything CONSULT and ACT produce is recorded as a
;;; step the moment it exists, so the enriched argument is already in the
;;; ledger; RECUR only re-queues the thought, and the scheduler bounces back
;;; into THINK with the argument rebuilt from thought_steps. Between bounces
;;; the operator can preempt, the budget is checked, and a crash loses
;;; nothing. New capability arrives as tools, base cases or sub-calls, never
;;; as states: the status column is a view of the fold, not a machine.

(defparameter +origins+
  '("responding" "noticing" "private" "concern" "introspection" "work" "unbidden" "docket" "reflection"))

(defparameter *thought-policy*
  ;; origin        priority     model calls  microusd  depth
  ;; responding and work match v2's 30 model calls per run of the mind.
  '(("responding"    :operator   30           300000    2)
    ("work"          :work       30           400000    3)
    ("noticing"      :concern    6            60000     1)
    ;; A concern's run matches v2's 30; its lifetime is capped across runs.
    ("concern"       :concern    30           300000    2)
    ;; One run of a work item; v2 capped a run at 12 steps.
    ("docket"        :private    12           150000    1)
    ("introspection" :private    8            80000     2)
    ;; Thinking to oneself with all one knows; it cannot publish (docs/reflect-then-speak.md).
    ("reflection"    :private    8            80000     1)
    ("private"       :private    4            40000     1)
    ("unbidden"      :private    2            20000     0))
  "The one allowance table for all thinking: per root thought, shared by its
sub-calls. Change budgets here, not per kind.")

(define-setting "thought.max_tool_calls_per_step"
  :type :integer :min 1 :max 64 :default 12 :group "Thinking"
  :description "Most tool calls one model response may run. The rest are answered as not run, so the model asks again with fewer (a model once asked for 347 at once).")

(define-setting "thought.policy"
  :type :array :group "Thinking"
  :default (loop for (origin priority calls microusd depth) in *thought-policy*
                 collect (obj "origin" origin "priority" (string-downcase (symbol-name priority))
                              "model_calls" calls "microusd" microusd "depth" (or depth 1)))
  :validate (lambda (rows)
              (loop for r in rows
                    unless (and (hash-table-p r) (member (obj-get r "origin") +origins+ :test #'equal)
                                (member (obj-get r "priority") '("operator" "work" "concern" "private" "maintenance")
                                        :test #'equal)
                                (integerp (obj-get r "model_calls")) (integerp (obj-get r "microusd")))
                      return "each row needs a known origin and priority, and whole numbers for model_calls and microusd"))
  :description "Allowance per kind of thought: priority, model calls and spend per root thought, and how deep sub-questions may go.")

(defun %policy (kernel origin)
  (let ((row (or (find origin (setting kernel "thought.policy") :key (lambda (r) (obj-get r "origin")) :test #'equal)
                 ;; Only the newly introduced reflection origin receives a default for older saved policies.
                 (let ((d (and (equal origin "reflection") (assoc origin *thought-policy* :test #'equal))))
                   (and d (obj "origin" origin "priority" (string-downcase (symbol-name (second d)))
                               "model_calls" (third d) "microusd" (fourth d) "depth" (or (fifth d) 1)))))))
    (unless row (error "No thought policy for origin ~s" origin))
    (list :origin origin
          :priority (intern (string-upcase (obj-get row "priority")) :keyword)
          :max-calls (obj-get row "model_calls") :max-microusd (obj-get row "microusd")
          :max-depth (obj-get row "depth" 1))))

(defun %purpose (thought)
  "The model route a thought uses."
  (let ((origin (getf thought :origin)))
    (cond ((string= origin "responding") "chat")
          ((string= origin "work") "work")
          (t "thinking"))))

(defparameter *step-text-limit* 12000
  "Characters per recorded step part; longer text is split, never cut (I9).")

(define-event-type "thought.requested"
  :schema (("origin" :string) ("reason" :string) ("audience" :string)
           ("channel" :string :optional)       ; where a reply would go
           ("source_event_id" :integer :optional)
           ("concern_id" :string :optional)
           ("parent_id" :integer :optional)    ; sub-calls (ponder)
           ("parent_call_id" :string :optional)
           ("root_id" :integer :optional)
           ("depth" :integer :optional)))

(define-event-type "thought.step"
  :schema (("thought_id" :integer) ("kind" :string)   ; model | tool_call | tool_result
           ("part" :integer) ("text" :string)
           ("tool" :string :optional) ("call_id" :string :optional)
           ("arguments" :string :optional)
           ("failed" :boolean :optional) ("private_context" :boolean :optional)))        ; a tool result that is a failure

(define-event-type "thought.ended"
  :schema (("thought_id" :integer) ("outcome" :string) ("summary" :string)))

(define-event-type "thought.failed"
  :schema (("thought_id" :integer) ("class" :string) ("reason" :string)))

(define-event-type "thought.parked"
  :schema (("thought_id" :integer) ("reason" :string)))

(defun %reduce-thoughts (db event)
  (let ((p (event-payload event)) (at (event-at event)))
    (flet ((f (k) (obj-get p k)))
      (let ((type (event-type event)))
        (cond
          ((string= type "thought.requested")
           (sql-exec db "INSERT INTO thoughts(id,origin,reason,audience,channel,source_event_id,parent_id,parent_call_id,root_id,depth,status,model_calls,steps,created_at,updated_at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,'queued',0,0,?11,?11)"
                     (event-id event) (f "origin") (f "reason") (f "audience") (f "channel")
                     (f "source_event_id") (f "parent_id") (f "parent_call_id")
                     (or (f "root_id") (event-id event)) (or (f "depth") 0) at))
          ((string= type "thought.step")
           (sql-exec db "INSERT INTO thought_steps(event_id,thought_id,kind,part,text,tool,call_id,arguments) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                     (event-id event) (f "thought_id") (f "kind") (f "part") (f "text")
                     (f "tool") (f "call_id") (f "arguments"))
           (sql-exec db "UPDATE thoughts SET status='running',steps=steps+1,model_calls=model_calls+?2,updated_at=?3 WHERE id=?1 AND status NOT IN ('ended','failed','parked')"
                     (f "thought_id")
                     (if (and (string= (f "kind") "model") (= 0 (f "part"))) 1 0) at))
          (t
           (sql-exec db "UPDATE thoughts SET status=?2,outcome=?3,updated_at=?4 WHERE id=?1 AND status NOT IN ('ended','failed','parked')"
                     (f "thought_id")
                     (cond ((string= type "thought.ended") "ended")
                           ((string= type "thought.failed") "failed")
                           (t "parked"))
                     (or (f "outcome") (f "reason")) at)))))))

(define-projection "thoughts"
  :kind :inline
  :consumes ("thought.requested" "thought.step" "thought.ended" "thought.failed" "thought.parked")
  :tables ("CREATE TABLE IF NOT EXISTS thoughts (id INTEGER PRIMARY KEY, origin TEXT NOT NULL, reason TEXT NOT NULL, audience TEXT NOT NULL, channel TEXT, source_event_id INTEGER, parent_id INTEGER, parent_call_id TEXT, root_id INTEGER NOT NULL, depth INTEGER NOT NULL, status TEXT NOT NULL, model_calls INTEGER NOT NULL, steps INTEGER NOT NULL, outcome TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS thoughts_status_idx ON thoughts(status,updated_at)"
           "CREATE INDEX IF NOT EXISTS thoughts_audience_idx ON thoughts(audience,id)"
           "CREATE INDEX IF NOT EXISTS thoughts_audience_time_idx ON thoughts(audience,created_at)"
           "CREATE INDEX IF NOT EXISTS thoughts_root_idx ON thoughts(root_id)"
           "CREATE TABLE IF NOT EXISTS thought_steps (event_id INTEGER PRIMARY KEY, thought_id INTEGER NOT NULL, kind TEXT NOT NULL, part INTEGER NOT NULL, text TEXT NOT NULL, tool TEXT, call_id TEXT, arguments TEXT)"
           "CREATE INDEX IF NOT EXISTS thought_steps_thought_idx ON thought_steps(thought_id,event_id)")
  :entity (lambda (e) (or (obj-get (event-payload e) "thought_id") (event-id e)))
  :reduce #'%reduce-thoughts)

;;; Private reflection needs its own recent activity across conversation
;;; audiences. This is an indexed projection, never an in-memory history fold.
(define-projection "self_activity"
  :kind :inline
  :consumes ("thought.requested" "thought.ended" "thought.failed" "thought.parked")
  :tables ("CREATE TABLE IF NOT EXISTS self_activity (thought_id INTEGER PRIMARY KEY, agent_id TEXT NOT NULL, reason TEXT NOT NULL, status TEXT NOT NULL, conclusion TEXT, updated_at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS self_activity_agent_idx ON self_activity(agent_id,thought_id)")
  :entity (lambda (e) (or (obj-get (event-payload e) "thought_id") (event-id e)))
  :reduce
  (lambda (db e)
    (let ((p (event-payload e)) (type (event-type e)))
      (if (equal type "thought.requested")
          (sql-exec db "INSERT INTO self_activity(thought_id,agent_id,reason,status,updated_at) VALUES(?1,?2,?3,'queued',?4)"
                    (event-id e) (event-agent-id e) (obj-get p "reason") (event-at e))
          (sql-exec db "UPDATE self_activity SET status=?2,conclusion=?3,updated_at=?4 WHERE thought_id=?1"
                    (obj-get p "thought_id")
                    (cond ((equal type "thought.ended") "ended") ((equal type "thought.failed") "failed") (t "parked"))
                    (or (obj-get p "summary") (obj-get p "reason") (obj-get p "outcome")) (event-at e))))))

;;; The frame: who I am and what is going on, assembled fresh on every
;;; bounce (the context assembler installs itself here). TOOLS-TEXT is the
;;; index of tools not in hand, or NIL.

(defvar *assemble-thought-context*
  (lambda (kernel thought tools-text)
    (let ((dialogue (recent-dialogue kernel (getf thought :audience) :token-budget 4000)))
      (list (obj "role" "system"
                 "content" (format nil "Reason for this thought: ~a~@[~%~%~a~]" (getf thought :reason) tools-text))
            (obj "role" "user"
                 "content" (format nil "~{~a~^~%~}" (mapcar #'dialogue-row-text dialogue))))))
  "(lambda (kernel thought-plist tools-text)) -> list of message objects.")


;;; Streaming and interruption (NFR EXT-2, EXT-3). Responding thoughts stream
;;; reply fragments to *reply-delta-hook*; an interrupt stops the reply where
;;; it is, and what was already said is kept and delivered marked.

(defvar *reply-delta-hook* nil "(lambda (thought-id text)) or NIL.")

(defvar *activity-hook* nil
  "(lambda (thought kind detail)) or NIL: what a thought is doing, for the
operator's live view (v2's activity and stream-progress notes). KIND is
model-request, progress, tool-start, tool-result, reply-ready or
model-failed; DETAIL is an object of content-free counts and labels, plus a
short preview of tool input. Never recorded; live only.")

(defun %activity-detail (thought kind detail)
  (when *activity-hook* (ignore-errors (funcall *activity-hook* thought kind detail))))

(defun %activity (thought kind &rest detail)
  (when *activity-hook*
    (ignore-errors (funcall *activity-hook* thought kind (apply #'obj detail)))))
(defvar *interrupts* (make-hash-table))
(defvar *interrupt-lock* (bt:make-lock "thought interrupts"))

(define-condition thought-interrupted (condition) ()
  (:documentation "Not an ERROR, so provider error handling never absorbs it."))

(defun interrupt-thought (id)
  (bt:with-lock-held (*interrupt-lock*) (setf (gethash id *interrupts*) t)))

(defun %interrupted-p (id)
  (bt:with-lock-held (*interrupt-lock*) (remhash id *interrupts*)))

;;; Rows

(defun request-thought (kernel origin reason &key (audience "operator") channel source-event-id
                                                   parent-id parent-call-id concern-id)
  "Start a thought. With PARENT-ID it is a sub-call of that thought: it takes
the parent's origin, audience and root allowance. Returns its id."
  (assert (member origin +origins+ :test #'string=))
  (let* ((parent (and parent-id (thought-row kernel parent-id)))
         (origin (if parent (getf parent :origin) origin))
         ;; Every introspection thinks for itself, including ones started by
         ;; quiet-time callers that formerly supplied a fleet audience.
         (audience (if (reflecting-p (list :origin origin)) "operator"
                       (if parent (getf parent :audience) audience)))
         (thought (append! kernel "thought.requested"
                           (obj "origin" origin "reason" reason
                                "audience" audience
                                "channel" (and (not parent) channel)
                                "source_event_id" source-event-id
                                "concern_id" (and (not parent) concern-id)
                                "parent_id" parent-id "parent_call_id" parent-call-id
                                "root_id" (and parent (getf parent :root-id))
                                "depth" (if parent (1+ (getf parent :depth)) 0))
                           :caused-by (or parent-id source-event-id)))
         (id (event-id thought)))
    (request-work kernel "thought" id (obj) :priority (getf (%policy kernel origin) :priority) :caused-by id)
    id))

(defun thought-row (kernel id)
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT origin,reason,audience,channel,status,model_calls,steps,outcome,parent_id,parent_call_id,root_id,depth,source_event_id FROM thoughts WHERE id=?1 LIMIT 1"
                      id)))
    (and row (destructuring-bind (origin reason audience channel status calls steps outcome
                                  parent-id parent-call-id root-id depth source) row
               (list :id id :origin origin :reason reason :audience audience :channel channel
                     :status status :model-calls calls :steps steps :outcome outcome
                     :parent-id parent-id :parent-call-id parent-call-id
                     :root-id root-id :depth depth :source-event-id source
                     :communicating (funcall *communication-root-p* kernel root-id))))))

(defun thought-steps (kernel id &key (limit -1))
  "Recorded steps of thought ID in order, as (kind part text tool call-id arguments).
Every step by default: a replay that keeps tool calls but drops their results
waits forever (bead v3-fzr)."
  (sql-rows (kernel-db kernel)
            "SELECT kind,part,text,tool,call_id,arguments FROM thought_steps WHERE thought_id=?1 ORDER BY event_id LIMIT ?2"
            id limit))

(defun %record-text (kernel id kind text &rest more)
  "Record TEXT as one or more step parts; nothing is cut."
  ;; Reviewing a proposed communication is also private. Only transport
  ;; events record what was actually shared with the recipient.
  (loop for start from 0 below (max 1 (length text)) by *step-text-limit*
        for part from 0
        do (append! kernel "thought.step"
                    (apply #'obj "thought_id" id "kind" kind "part" part
                           "text" (subseq text start (min (length text) (+ start *step-text-limit*)))
                           "private_context" t more))))

(defun %replay-messages (kernel id)
  "The argument of thought ID: the messages it has produced so far, rebuilt
from its recorded steps. A model reply and the tool calls it made are one
assistant message; a long tool result shows its first part and how to page."
  (let ((messages '()) (text nil) (calls '()) (results (make-hash-table :test 'equal)))
    (flet ((flush ()
             (when (or text calls)
               (push (if calls
                         (obj "role" "assistant" "content" (if (plusp (length text)) text :null)
                              "tool_calls" (reverse calls))
                         (obj "role" "assistant" "content" text))
                     messages)
               (setf text nil calls '()))))
      (loop for (kind part step-text tool call-id arguments) in (thought-steps kernel id)
            do (cond ((string= kind "model")
                      (when (and (zerop part) (or calls text)) (flush))
                      (setf text (if (zerop part) step-text (concatenate 'string text step-text))))
                     ((string= kind "tool_call")
                      (push (obj "id" call-id "type" "function"
                                 "function" (obj "name" tool "arguments" arguments))
                            calls))
                     ((string= kind "tool_result")
                      (flush)
                      (if (zerop part)
                          (push (setf (gethash call-id results)
                                      (obj "role" "tool" "tool_call_id" call-id "content" step-text))
                                messages)
                          (let ((shown (gethash call-id results)))
                            (when (and shown (= part 1))
                              (setf (gethash "content" shown)
                                    (format nil "~a~%[The result continues. Call read_result with call_id ~a and part 1 or later to read more.]"
                                            (obj-get shown "content") call-id))))))))
      (flush))
    (nreverse messages)))

;;; The mind

(defstruct mind kernel thought work)

(defun think (mind messages)
  "The mind. MESSAGES is the argument folded through the recursion."
  (cond ((awaiting-p messages) (wait mind))
        ((spent-p mind) (rest-here mind messages))
        (t (let ((reply (consult mind (%with-low-allowance-note mind messages))))
             (if (obj-get reply "tool_calls")
                 (recur mind (append messages (list reply) (act mind reply)))
                 (answer mind reply))))))

(defun think-step (kernel work)
  "The trampoline: one bounce of THINK for the thought WORK names."
  (let* ((id (parse-integer (getf work :entity)))
         (thought (thought-row kernel id)))
    (unless (member (getf thought :status) '("ended" "failed" "parked") :test #'string=)
      (catch 'thought-done
        (think (make-mind :kernel kernel :thought thought :work work)
               (%replay-messages kernel id))))
    (obj "thought_id" id)))

(define-handler "thought" :run #'think-step :paid t :estimate-microusd 5000
  :on-dead (lambda (kernel entity class reason)
             (let ((thought (thought-row kernel (parse-integer entity))))
               (conclude kernel thought "thought.failed"
                         (obj "class" class "reason" (format nil "~a: ~a" class reason))
                         (format nil "(This sub-thought failed: ~a)" reason)))))

;;; Base and recursive cases

(defun awaiting-p (messages)
  "True while a tool call has no result yet (a sub-call still thinking)."
  (let ((asked '()) (answered '()))
    (dolist (m messages)
      (dolist (call (obj-get m "tool_calls")) (push (obj-get call "id") asked))
      (when (equal "tool" (obj-get m "role")) (push (obj-get m "tool_call_id") answered)))
    (set-difference asked answered :test #'equal)))

(defun wait (mind)
  "Nothing to do until a sub-call answers; its answer re-queues this thought."
  (declare (ignore mind))
  nil)

(defun %root-model-calls (kernel thought)
  (sql-value (kernel-db kernel) "SELECT SUM(model_calls) FROM thoughts WHERE root_id=?1 LIMIT 1"
             (getf thought :root-id)))

(defun spent-p (mind)
  (let ((thought (mind-thought mind)))
    (>= (%root-model-calls (mind-kernel mind) thought)
        (getf (%policy (mind-kernel mind) (getf thought :origin)) :max-calls))))

(defparameter +wrap-up+
  "You have used this thought's allowance of model calls and can call no more tools. Answer now with what you have: what you found, what is unfinished, and what you would do next.")

(defparameter *checkpoint-every* 8 "Model calls between checkpoints in a long thought.")

(defun %guidance-note (mind)
  "A note for this step of a long thought, or NIL. Steering, not new
authority: a plan at the start, a checkpoint every few calls, and a warning
when the allowance is nearly spent (a thought once spent 53 calls reading
source and never answered)."
  (let* ((thought (mind-thought mind))
         (max (getf (%policy (mind-kernel mind) (getf thought :origin)) :max-calls))
         (used (%root-model-calls (mind-kernel mind) thought))
         (left (- max used)))
    (cond ((<= left (max 1 (min 3 (floor max 5))))
           (format nil "~d model call~:p left in this thought. Stop exploring and finish with what you have." left))
          ((< max 10) nil)
          ((and (zerop used) (member (getf thought :origin) '("work" "concern") :test #'string=))
           (format nil "Before you start: if this needs several steps, write a short plan first, saying what you will check and about how many of your ~d model calls it should take." max))
          ((and (plusp used) (zerop (mod used *checkpoint-every*)))
           (format nil "Checkpoint, ~d of ~d calls used. If more is needed, say in two lines what you have established and what is still open, together with your next tool call. If you are done, skip the checkpoint and write your answer itself: a message without a tool call is delivered as it is and ends this thought, so it must be the answer, not a note about it." used max)))))

(defun %with-low-allowance-note (mind messages)
  "MESSAGES with this step's guidance note, if it has one."
  (let ((note (%guidance-note mind)))
    (if note (append messages (list (obj "role" "user" "content" note))) messages)))

(defun %canonical-arguments (text)
  "Tool arguments with their keys in order, so the same call compares equal."
  (let ((parsed (ignore-errors (json-parse (or text "")))))
    (if (hash-table-p parsed)
        (format nil "~{~s~^,~}" (sort (loop for k being the hash-keys of parsed using (hash-value v)
                                            collect (format nil "~a=~a" k (json-encode v)))
                                      #'string<))
        (or text ""))))

(defvar *step-notes* '()
  "Functions (lambda (kernel thought-id tool-name arguments prior result)) run
after each tool call; each returns a note for that call's result, or NIL.
PRIOR is the thought's earlier (tool arguments) calls; RESULT the call's
result text. The mind's extension point
for what should come to mind as the agent acts (design note
interaction-recall §11); notes are shown before the result, in order.")

(defmacro define-step-note (name lambda-list &body body)
  "Register step note NAME (a symbol): LAMBDA-LIST is (kernel thought-id
tool-name arguments prior result). Redefining NAME replaces it in place."
  `(progn
     (defun ,name ,lambda-list (declare (ignorable ,@lambda-list)) ,@body)
     (pushnew ',name *step-notes*)
     ',name))

(defun %step-notes (kernel thought-id name arguments prior result)
  (loop for note in (reverse *step-notes*)
        for text = (ignore-errors (funcall note kernel thought-id name arguments prior result))
        when (and (stringp text) (plusp (length text))) collect text))

(defun %repeat-note (name arguments prior)
  "A note when this call repeats one the thought already made, or re-reads
source it has read; PRIOR is the thought's earlier (tool arguments) calls."
  (let ((here (%canonical-arguments arguments)))
    (cond ((find-if (lambda (p) (and (equal name (first p)) (string= here (%canonical-arguments (second p))))) prior)
           "(You already made this exact call earlier in this thought; the result is the same. What are you looking for that it did not give you?)")
          ((equal name "source")
           (let ((a (ignore-errors (json-parse arguments))))
             (when (and (hash-table-p a) (equal "read" (obj-get a "action")))
               (let ((times (count-if (lambda (p)
                                        (let ((b (ignore-errors (json-parse (second p)))))
                                          (and (equal "source" (first p)) (hash-table-p b)
                                               (equal "read" (obj-get b "action"))
                                               (equal (obj-get a "path") (obj-get b "path")))))
                                      prior)))
                 (when (>= times 2)
                   (format nil "(This is read number ~d of ~a in this thought. Use outline, or search for the name you need, instead of reading it again.)"
                           (1+ times) (obj-get a "path"))))))))))

(define-step-note repeated-call-note (kernel thought-id name arguments prior result)
  (%repeat-note name arguments prior))

(defvar *wrap-up-hook* nil
  "(lambda (kernel thought text)) or NIL: delivers a wrap-up answer that has
no conversation to go to (the coding agent's, on its bead).")

(defun rest-here (mind messages)
  "The allowance is spent. The thought still answers, from one last call with
no tools; if even that fails, it parks."
  (let* ((kernel (mind-kernel mind)) (thought (mind-thought mind))
         (max (getf (%policy kernel (getf thought :origin)) :max-calls)))
    (%activity thought "allowance-spent" "model_calls" max)
    (let ((reply (handler-case (consult mind (append messages (list (obj "role" "user" "content" +wrap-up+)))
                                        :tools nil)
                   (error () nil))))
      (when (and reply (plusp (length (obj-get reply "content" ""))))
        (when (and *wrap-up-hook* (null (getf thought :parent-id)))
          (ignore-errors (funcall *wrap-up-hook* kernel thought (obj-get reply "content"))))
        (answer mind reply)
        (return-from rest-here)))
    (conclude kernel thought "thought.parked"
              (obj "reason" (format nil "reached its allowance of ~d model calls" max))
              "(This sub-thought ran out of allowance before it reached an answer.)")))

(defun recur (mind messages)
  "Recur with the enriched MESSAGES. They are already recorded, so the
trampoline rebuilds them on the next bounce; recurring is re-queuing."
  (declare (ignore messages))
  (let ((thought (mind-thought mind)))
    (request-work (mind-kernel mind) "thought" (getf thought :id) (obj)
                  :priority (getf (%policy (mind-kernel mind) (getf thought :origin)) :priority)
                  :caused-by (getf thought :id))))

(defun direct-reply-p (thought)
  "Only the private owner receives an unfiltered reply; other recipients use review."
  (and (equal "responding" (getf thought :origin)) (getf thought :channel)
       (equal "operator" (getf thought :audience))))

(defun answer (mind reply)
  (let* ((kernel (mind-kernel mind)) (thought (mind-thought mind))
         (text (let ((text (obj-get reply "content" "")))
                 ;; A reply the output limit cut off says so, rather than passing as whole.
                 (if (equal "length" (obj-get reply "finish_reason"))
                     (format nil "~a … [cut off: I reached my output limit before finishing]" text)
                     text))))
    (%activity thought "reply-ready")
    (when (getf thought :communicating)
      (funcall *communication-answer* kernel thought text))
    (when (direct-reply-p thought)
      (deliver-reply kernel :channel (getf thought :channel) :audience (getf thought :audience)
                            :text text :in-reply-to (getf thought :id)))
    (conclude kernel thought "thought.ended"
              (obj "outcome" (if (direct-reply-p thought) "replied" "concluded")
                   "summary" (subseq text 0 (min 500 (length text))))
              text)))

(defvar *concluded-hook* nil
  "(lambda (kernel thought)) or NIL: called when a top-level thought ends or
parks, for work it left unfinished (artifacts.lisp). Its errors are ignored.")

(defun conclude (kernel thought type payload result)
  "End THOUGHT with event TYPE. A sub-call returns RESULT to its parent as
the parent's tool result, which re-queues the parent."
  (setf (gethash "thought_id" payload) (getf thought :id))
  (append! kernel type payload)
  (when (and *concluded-hook* (null (getf thought :parent-id)))
    (ignore-errors (funcall *concluded-hook* kernel thought)))
  (let ((parent (getf thought :parent-id)))
    (when parent
      (%record-text kernel parent "tool_result" result
                    "tool" "ponder" "call_id" (getf thought :parent-call-id))
      (request-work kernel "thought" parent (obj)
                    :priority (getf (%policy kernel (getf thought :origin)) :priority)
                    :caused-by parent))))

;;; Consulting the model

(defun %picked-up (messages)
  "Tool names this thought has described or used: they stay in hand."
  (loop for m in messages
        append (loop for call in (obj-get m "tool_calls")
                     for fn = (obj-get call "function")
                     collect (obj-get fn "name")
                     when (equal "describe_tool" (obj-get fn "name"))
                       collect (ignore-errors (obj-get (json-parse (obj-get fn "arguments")) "name")))))

(defun consult (mind messages &key (tools t))
  "Ask the model, recording what it says as it says it. Returns the reply.
TOOLS NIL asks for an answer with no tools offered."
  (let* ((kernel (mind-kernel mind)) (thought (mind-thought mind))
         (id (getf thought :id))
         (tools (and tools (tools-for thought)))
         (images (thought-images kernel thought))
         (profile (let ((p (profile-for kernel (%purpose thought) (funcall *route-signals* kernel thought))))
                    ;; A turn with images asks for a model that can see, if one is routed.
                    (if (and images (not (provider-profile-vision p)))
                        (let ((v (ignore-errors (profile-for kernel "vision" (funcall *route-signals* kernel thought)))))
                          (if (and v (provider-profile-vision v)) v p))
                        p)))
         ;; Some models will not pick a tool up with describe_tool before using
         ;; it; a profile marked all_tools is given every tool it may use.
         (hand (and tools (if (provider-profile-all-tools profile)
                              tools
                              (tools-in-hand thought tools (%picked-up messages)))))
         (frame (funcall *assemble-thought-context* kernel thought (tool-index-text tools hand)))
         (spoken (make-string-output-stream))
         (streaming (direct-reply-p thought))
         (all (let ((all (append frame messages)))
                (if images (with-images kernel all images profile) all)))
         (result
           (handler-case
               (let ((*progress-hook* (lambda (detail) (%activity-detail thought "progress" detail))))
                 (%activity thought "model-request" "message_count" (length all)
                            "message_characters" (%messages-chars all))
                 (complete kernel profile
                         all
                         :tools (and hand (tool-specs hand))
                         :work-id (getf (mind-work mind) :id)
                         :lane (cond ((eq :operator (getf (%policy kernel (getf thought :origin)) :priority))
                                      "operator")
                                     ;; Quiet thinking has its own paced daily budget.
                                     ((member (getf thought :origin) '("concern" "introspection" "docket" "reflection") :test #'string=)
                                      "quiet")
                                     (t "background"))
                         ;; Every thought streams, so its progress can be watched;
                         ;; only a reply is spoken as it arrives.
                         :on-delta (if streaming
                                       (lambda (text)
                                         (when (%interrupted-p id) (error 'thought-interrupted))
                                         (write-string text spoken)
                                         (when *reply-delta-hook* (funcall *reply-delta-hook* id text)))
                                       (lambda (text) (declare (ignore text))))))
             (thought-interrupted ()
               (let ((said (get-output-stream-string spoken)))
                 (%record-text kernel id "model" said)
                 (deliver-reply kernel :channel (getf thought :channel) :audience (getf thought :audience)
                                       :text said :in-reply-to id :interrupted t)
                 (conclude kernel thought "thought.ended"
                           (obj "outcome" "interrupted" "summary" said) said)
                 (throw 'thought-done nil)))
             (provider-failure (f)
               (%activity thought "model-failed" "reason" (let ((r (princ-to-string f))) (subseq r 0 (min 300 (length r)))))
               ;; Keep whatever arrived, then let the scheduler retry by class.
               (when (plusp (length (provider-failure-partial f)))
                 (%record-text kernel id "model" (provider-failure-partial f)))
               (error f))))
         (text (obj-get result "content" ""))
         (most (setting kernel "thought.max_tool_calls_per_step"))
         (asked 0)
         ;; A call with no name can never run, and every provider rejects a
         ;; history that holds one, so it is dropped here (bead v3-o1z).
         ;; Calls past the per-step cap are dropped too, before anything is
         ;; recorded, and the model is told (bead v3-fzr): a reply once asked
         ;; for 1,179 at once.
         (calls (loop for call in (obj-get result "tool_calls")
                      for fn = (obj-get call "function")
                      when (and (stringp (obj-get fn "name")) (plusp (length (obj-get fn "name")))
                                (<= (incf asked) most))
                      collect (obj "id" (or (obj-get call "id") (format nil "call-~d-~d" id (random 1000000)))
                                   "type" "function"
                                   "function" (obj "name" (obj-get fn "name")
                                                   "arguments" (or (obj-get fn "arguments") "{}"))))))
    (%record-text kernel id "model" text)
    ;; Words alongside tool calls are said now, as Claude Code does between
    ;; steps: a plan before starting, status along the way.
    (when (and calls streaming (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
      (deliver-reply kernel :channel (getf thought :channel) :audience (getf thought :audience)
                            :text text :in-reply-to id :interim t))
    (if calls
        (let ((reply (obj "role" "assistant" "content" (if (plusp (length text)) text :null) "tool_calls" calls)))
          (when (> asked most) (setf (gethash "asked_calls" reply) asked))
          reply)
        ;; The finish reason travels to ANSWER, which marks a reply the limit cut off.
        (obj "role" "assistant" "content" text "finish_reason" (obj-get result "finish_reason")))))

;;; Acting

(defvar *call-id* nil "The id of the tool call being run.")

(defun act (mind reply)
  "Run the tool calls in REPLY. Returns the tool messages that are ready; a
sub-call's result arrives later."
  (let* ((kernel (mind-kernel mind)) (thought (mind-thought mind)) (id (getf thought :id))
         (calls (obj-get reply "tool_calls"))
         (asked (prog1 (obj-get reply "asked_calls") (remhash "asked_calls" reply)))
         (prior (sql-rows (kernel-db kernel)
                          "SELECT tool,arguments FROM thought_steps WHERE thought_id=?1 AND kind=?2 ORDER BY event_id LIMIT 400"
                          id "tool_call")))
    (dolist (call calls)
      (let ((fn (obj-get call "function")))
        (%record-text kernel id "tool_call" "" "tool" (obj-get fn "name")
                      "call_id" (obj-get call "id") "arguments" (obj-get fn "arguments"))))
    (loop with most = (setting kernel "thought.max_tool_calls_per_step")
          for call in calls
          for n from 1
          for fn = (obj-get call "function")
          for name = (obj-get fn "name")
          for tool = (gethash name *tools*)
          for failed = nil
          for result = (let ((*call-id* (obj-get call "id")))
                         (%activity thought "tool-start" "tool_name" name
                                    "input" (let ((a (or (obj-get fn "arguments") ""))) (subseq a 0 (min 200 (length a)))))
                         (let ((result (cond ((> n most)
                                (setf failed t)
                                (format nil "Not run: this response asked for ~d tool calls and at most ~d run at once. Ask again for the ones you still need, a few at a time."
                                        (length calls) most))
                               ((not (and tool (tool-available-p tool thought)))
                                (setf failed t)
                                (format nil "No tool named ~a." name))
                               (t (let* ((*publication-recipient* nil)
                                         (result (handler-case
                                                    (progn
                                                      (ignore-errors (funcall *tool-action-hook* kernel thought name (obj-get fn "arguments")))
                                                      (funcall (tool-run tool) kernel id
                                                             (let ((args (json-parse (obj-get fn "arguments"))))
                                                               (when (tool-publishes tool)
                                                                 (multiple-value-bind (allowed restricted-recipient)
                                                                     (funcall *communication-egress-guard* kernel thought name args)
                                                                   (unless allowed (error "Publication refused: no matching recipient-bound disclosure draft"))
                                                                   (setf *publication-recipient* restricted-recipient)))
                                                               args)))
                                                  (error (e) (setf failed t) (format nil "Tool error: ~a" e))))
                                        (notes (and (stringp result) (%step-notes kernel id name (obj-get fn "arguments") prior result))))
                                    (if (and notes (stringp result)) (format nil "~{~a~%~%~}~a" notes result) result))))))
                         (when (and asked (= n (length calls)) (stringp result))
                           (setf result (format nil "~a~%~%[Only the first ~d of the ~d tool calls in that response ran; the rest were dropped. Ask again for the ones you still need, a few at a time.]"
                                                result most asked)))
                         result))
          unless (eq result :pending)
            do (%activity thought "tool-result" "tool_name" name
                          "result_characters" (if (stringp result) (length result) 0))
               (when (and (stringp result) (>= (length result) 6) (string-equal "Error:" result :end2 6))
                 (setf failed t))
               (apply #'%record-text kernel id "tool_result" result "tool" name "call_id" (obj-get call "id")
                      (and failed (list "failed" t)))
            and collect (obj "role" "tool" "tool_call_id" (obj-get call "id") "content" result))))

;;; Tools of thinking itself

(define-tool "ponder"
  :description "Think a sub-question through on its own; its answer comes back as this tool's result. Use it to take a large question in parts."
  :parameters (obj "type" "object"
                   "properties" (obj "question" (obj "type" "string"))
                   "required" (list "question"))
  :run (lambda (kernel id args)
         (let ((thought (thought-row kernel id)))
           (if (>= (getf thought :depth) (getf (%policy kernel (getf thought :origin)) :max-depth))
               "You are already as deep as this kind of thought may go. Answer from here."
               (progn (request-thought kernel (getf thought :origin) (obj-get args "question" "")
                                       :parent-id id :parent-call-id *call-id*)
                      :pending)))))

(defun stimulus-text (kernel thought-id)
  "What started THOUGHT-ID's root thought, read from the ledger: the event,
its time, and the principal it was recorded with (bead v3-clp.17)."
  (let* ((thought (thought-row kernel thought-id))
         (root (if (eql (getf thought :root-id) thought-id) thought (thought-row kernel (getf thought :root-id))))
         (source (getf root :source-event-id))
         (event (and source (event-by-id kernel source))))
    (format nil "This thought: ~a, origin ~a, audience ~a~@[, a sub-thought of ~a~].~%~a"
            thought-id (getf thought :origin) (getf thought :audience)
            (and (not (eql (getf thought :root-id) thought-id)) (getf thought :root-id))
            (if (null event)
                (format nil "No recorded event started it; the reason it was given: ~a" (getf root :reason))
                (let ((origin (obj-get (event-payload event) "origin")))
                  (format nil "Started by event ~a, ~a, at ~a.~%Recorded principal: ~a"
                          source (event-type event) (event-at event)
                          (if (hash-table-p origin) (json-encode origin) "none recorded")))))))

(define-tool "stimulus"
  :description "Read, from the ledger, what started this thought: the event, when, and the principal it was recorded with. Use it to check who a message is really from rather than trusting what the prompt says."
  :run (lambda (kernel id args) (declare (ignore args)) (stimulus-text kernel id)))

(define-tool "read_result"
  :description "Read a further part of a long tool result."
  :parameters (obj "type" "object"
                   "properties" (obj "call_id" (obj "type" "string") "part" (obj "type" "integer"))
                   "required" (list "call_id" "part"))
  :run (lambda (kernel id args)
         (or (sql-value (kernel-db kernel)
                        "SELECT text FROM thought_steps WHERE thought_id=?1 AND call_id=?2 AND kind='tool_result' AND part=?3 LIMIT 1"
                        id (obj-get args "call_id" "") (obj-get args "part" 0))
             "There is no such part.")))
