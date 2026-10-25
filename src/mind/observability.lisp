(in-package #:pai-v3.mind)

;;; Observability (docs/pwa-compatibility.md; bead v3-clp.5). What the v2
;;; observability page shows, from bounded state only:
;;;
;;;   observed_activity  hourly counts per series, use per tool and model,
;;;                      and the newest events in the page's own names;
;;;   model captures     the exact request and response of the latest model
;;;                      calls, for "View exact context".
;;;
;;; Captures are diagnostics, not history: they are written beside the call,
;;; never derived from the ledger, and a rebuild empties them.

(defparameter *observed-events-kept* 300 "Newest observed events kept.")
(defparameter *observed-hours-kept* (* 24 31) "Hours of activity counts kept.")
(defparameter *captures-kept* 200 "Model calls whose request and response are kept.")

(defun %universal (iso)
  (local-time:timestamp-to-universal (local-time:parse-timestring iso)))

(defun %hour (iso) (* 3600 (floor (%universal iso) 3600)))

(defun %clip (text n)
  (if (and (stringp text) (> (length text) n)) (format nil "~a…" (subseq text 0 n)) text))

(defun %observe (db event hour &key series name-kind name type payload)
  (when series
    (sql-exec db "INSERT INTO activity_hours(hour,series,count) VALUES(?1,?2,1) ON CONFLICT(hour,series) DO UPDATE SET count=count+1"
              hour series))
  (when name
    (sql-exec db "INSERT INTO activity_names(hour,kind,name,count) VALUES(?1,?2,?3,1) ON CONFLICT(hour,kind,name) DO UPDATE SET count=count+1"
              hour name-kind name))
  (when type
    (sql-exec db "INSERT INTO observed_events(event_id,hour,at,type,payload) VALUES(?1,?2,?3,?4,?5)"
              (event-id event) hour (event-at event) type (json-encode payload))
    (sql-exec db "DELETE FROM observed_events WHERE seq<=?1"
              (- (sql-last-insert-rowid db) *observed-events-kept*))))

(defun %reduce-observed (db e)
  (let* ((p (event-payload e)) (hour (%hour (event-at e))))
    (flet ((f (k) (obj-get p k)))
      (sql-exec db "DELETE FROM activity_hours WHERE hour<?1" (- hour (* 3600 *observed-hours-kept*)))
      (sql-exec db "DELETE FROM activity_names WHERE hour<?1" (- hour (* 3600 *observed-hours-kept*)))
      (let ((type (event-type e)))
        (cond
          ((string= type "provider.call.completed")
           (%observe db e hour :series "model_requests" :name-kind "model" :name (f "model")
                               :type "model-request"
                               :payload (obj "model" (f "model") "model_call_id" (event-id e)
                                             "prompt_characters" (f "prompt_chars")))
           (%observe db e hour :type "model-response"
                               :payload (obj "status" "ok" "model" (f "model") "milliseconds" (f "milliseconds")
                                             "usage" (obj "prompt_tokens" (f "prompt_tokens")
                                                          "completion_tokens" (f "completion_tokens")
                                                          "microusd" (f "microusd")))))
          ((string= type "provider.call.failed")
           (%observe db e hour :type "model-response"
                               :payload (obj "status" (format nil "failed (~a)" (or (f "class") "error"))
                                             "model" (f "model") "error" (%clip (f "reason") 300))))
          ((and (string= type "thought.step") (equal "tool_call" (f "kind")))
           (%observe db e hour :series "tool_calls" :name-kind "tool" :name (f "tool")
                               :type "recursive-tool-execution"
                               :payload (obj "tool_name" (f "tool") "thought_id" (f "thought_id")
                                             "input" (%clip (f "arguments") 300))))
          ((and (string= type "thought.step") (equal "tool_result" (f "kind")))
           (%observe db e hour :type "recursive-tool-result"
                               :payload (obj "tool_name" (f "tool") "thought_id" (f "thought_id")
                                             "status" (if (eq t (f "failed")) "failed" "result recorded")
                                             "characters" (length (or (f "text") "")))))
          ((string= type "thought.requested")
           (%observe db e hour :series (unless (equal "responding" (f "origin")) "private_cognition")
                               :type "thought-opened"
                               :payload (obj "origin" (f "origin") "audience" (f "audience")
                                             "thought_id" (event-id e) "reason" (%clip (f "reason") 300))))
          ((member type '("thought.ended" "thought.parked" "thought.failed") :test #'string=)
           (%observe db e hour :type (subseq type 8)
                               :payload (obj "thought_id" (f "thought_id")
                                             "outcome" (%clip (or (f "outcome") (f "reason")) 300))))
          ((and (string= type "turn.received") (equal "operator" (f "audience")))
           (%observe db e hour :series "operator_messages"))
          ((string= type "reply.delivered")
           (%observe db e hour :series "agent_messages"))
          ((string= type "memory.formed")
           (%observe db e hour :series "memory_writes" :type "memory-formed"
                               :payload (obj "kind" (f "kind") "content" (%clip (f "content") 200))))
          ((string= type "coding_agent.message")
           (%observe db e hour :type "coding-agent-message"
                               :payload (obj "from" (obj-get (f "origin") "name") "bead" (f "bead")
                                             "text" (%clip (f "text") 300)))))))))

(define-projection "observed_activity"
  :kind :inline
  :consumes ("provider.call.completed" "provider.call.failed" "thought.step" "thought.requested"
             "thought.ended" "thought.parked" "thought.failed" "turn.received" "reply.delivered"
             "memory.formed" "coding_agent.message")
  :tables ("CREATE TABLE IF NOT EXISTS activity_hours (hour INTEGER NOT NULL, series TEXT NOT NULL, count INTEGER NOT NULL, PRIMARY KEY(hour,series))"
           "CREATE TABLE IF NOT EXISTS activity_names (hour INTEGER NOT NULL, kind TEXT NOT NULL, name TEXT NOT NULL, count INTEGER NOT NULL, PRIMARY KEY(hour,kind,name))"
           "CREATE TABLE IF NOT EXISTS observed_events (seq INTEGER PRIMARY KEY AUTOINCREMENT, event_id INTEGER NOT NULL, hour INTEGER NOT NULL, at TEXT NOT NULL, type TEXT NOT NULL, payload TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS observed_events_hour ON observed_events(hour,seq)")
  :reduce #'%reduce-observed)

;;; Cost: one row per paid model call for *cost-days-kept* days, so spending
;;; can be read by day, model and the kind of thought that paid for it.

(defparameter *cost-days-kept* 30)

(define-projection "cost_calls"
  :kind :inline
  :consumes ("provider.call.completed")
  :tables ("CREATE TABLE IF NOT EXISTS cost_calls (event_id INTEGER PRIMARY KEY, day TEXT NOT NULL, profile TEXT NOT NULL, model TEXT NOT NULL, work_id INTEGER, prompt_tokens INTEGER NOT NULL, completion_tokens INTEGER NOT NULL, microusd INTEGER NOT NULL)"
           "CREATE INDEX IF NOT EXISTS cost_calls_day_idx ON cost_calls(day)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (day (subseq (event-at e) 0 10)))
              (sql-exec db "INSERT OR IGNORE INTO cost_calls VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                        (event-id e) day (obj-get p "profile") (obj-get p "model")
                        (let ((w (obj-get p "work_id"))) (if (integerp w) w nil))
                        (obj-get p "prompt_tokens") (obj-get p "completion_tokens") (obj-get p "microusd"))
              (sql-exec db "DELETE FROM cost_calls WHERE day<?1"
                        (subseq (iso8601 (- (%universal (event-at e)) (* 86400 *cost-days-kept*))) 0 10)))))

(defun spend-report (kernel &key (days 7))
  "Spending over the last DAYS days: by day and lane, by profile and model,
by the kind of thought that paid, and the costliest thoughts."
  (let* ((db (kernel-db kernel))
         (since (subseq (iso8601 (- (now) (* 86400 days))) 0 10))
         (usd (lambda (m) (/ (or m 0) 1000000d0))))
    (flet ((rows (sql &rest args) (apply #'sql-rows db sql args)))
      (obj "since" since
           "by_day_lane" (mapcar (lambda (r) (obj "day" (first r) "lane" (second r) "usd" (funcall usd (third r))))
                                 (rows "SELECT day,lane,microusd FROM v3_spend WHERE day>=?1 ORDER BY day,lane LIMIT 400" since))
           "by_model" (mapcar (lambda (r) (obj "profile" (first r) "model" (second r) "calls" (third r)
                                               "prompt_tokens" (fourth r) "completion_tokens" (fifth r) "usd" (funcall usd (sixth r))))
                              (rows "SELECT profile,model,count(*),sum(prompt_tokens),sum(completion_tokens),sum(microusd) FROM cost_calls WHERE day>=?1 GROUP BY profile,model ORDER BY sum(microusd) DESC LIMIT 40" since))
           "by_origin" (mapcar (lambda (r) (obj "origin" (or (first r) "not a thought") "calls" (second r) "thoughts" (third r) "usd" (funcall usd (fourth r))))
                               (rows "SELECT t.origin,count(*),count(DISTINCT t.root_id),sum(c.microusd) FROM cost_calls c LEFT JOIN v3_work w ON w.id=c.work_id AND w.handler=?2 LEFT JOIN thoughts t ON t.id=CAST(w.entity AS INTEGER) WHERE c.day>=?1 GROUP BY t.origin ORDER BY sum(c.microusd) DESC LIMIT 40" since "thought"))
           "top_thoughts" (mapcar (lambda (r) (obj "thought_id" (first r) "origin" (second r) "calls" (third r) "usd" (funcall usd (fourth r))
                                                   "reason" (%clip (fifth r) 160)))
                                  (rows "SELECT t.root_id,t.origin,count(*),sum(c.microusd),r.reason FROM cost_calls c JOIN v3_work w ON w.id=c.work_id AND w.handler=?2 JOIN thoughts t ON t.id=CAST(w.entity AS INTEGER) JOIN thoughts r ON r.id=t.root_id WHERE c.day>=?1 GROUP BY t.root_id ORDER BY sum(c.microusd) DESC LIMIT 15" since "thought"))))))

(define-projection "model_captures"
  :kind :inline
  :consumes ()
  :tables ("CREATE TABLE IF NOT EXISTS model_captures (seq INTEGER PRIMARY KEY AUTOINCREMENT, call_event_id INTEGER NOT NULL UNIQUE, at TEXT NOT NULL, request TEXT NOT NULL, response TEXT NOT NULL)")
  :reduce (lambda (db e) (declare (ignore db e))))

(defun capture-model-call (kernel call-event request response)
  "Keep the exact REQUEST and RESPONSE (JSON text) of the model call recorded
as CALL-EVENT, dropping the oldest beyond *captures-kept*."
  (ignore-errors
   (let ((db (kernel-db kernel)))
     (sql-exec db "INSERT INTO model_captures(call_event_id,at,request,response) VALUES(?1,?2,?3,?4)"
               (event-id call-event) (event-at call-event) request response)
     (sql-exec db "DELETE FROM model_captures WHERE seq<=?1" (- (sql-last-insert-rowid db) *captures-kept*)))))

(setf *capture-hook* #'capture-model-call)

;;; Reading

(defun %hours-window (hours now)
  (let ((last (* 3600 (floor now 3600))))
    (loop for i from (1- hours) downto 0 collect (- last (* 3600 i)))))

(defun observed-history (kernel hours)
  "The page's history: hourly buckets for the last HOURS, use per tool and
model over them, and the newest events within them."
  (let* ((hours (max 1 (min 336 hours)))
         (now (get-universal-time))
         (window (%hours-window hours now))
         (in (%in-list 1 (length window)))   ; memory.lisp
         (db (kernel-db kernel))
         (counts (apply #'sql-rows db (format nil "SELECT hour,series,count FROM activity_hours WHERE hour IN (~a) LIMIT ~d"
                                              in (* 8 (length window)))
                        window))
         (names (apply #'sql-rows db (format nil "SELECT kind,name,count FROM activity_names WHERE hour IN (~a) LIMIT 5000" in)
                       window))
         (events (apply #'sql-rows db (format nil "SELECT event_id,at,type,payload FROM observed_events WHERE hour IN (~a) ORDER BY seq DESC LIMIT ~d"
                                              in *observed-events-kept*)
                        window))
         (tools (obj)) (models (obj)))
    (loop for (kind name count) in names
          do (let ((table (if (string= kind "tool") tools models)))
               (setf (gethash name table) (+ count (gethash name table 0)))))
    (obj "as_of" now
         "activity" (obj "buckets" (loop for hour in window
                                         collect (let ((b (obj "start" hour)))
                                                   (loop for (h series count) in counts
                                                         when (= h hour) do (setf (gethash series b) count))
                                                   b))
                         "tool_usage" tools "model_usage" models "truncated" :false)
         "events" (loop for (id at type payload) in (reverse events)
                        collect (obj "id" id "timestamp" at "type" type "payload" (json-parse payload))))))

(defun model-capture (kernel call-event-id)
  "The page's exact-context record for one model call."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT at,request,response FROM model_captures WHERE call_event_id=?1 LIMIT 1"
                      call-event-id)))
    (if (null row)
        (obj "status" "not-found" "reason" "Only the latest model calls are kept, and this one is no longer among them.")
        (destructuring-bind (at request response) row
          (let ((req (json-parse request)) (event (event-by-id kernel call-event-id)))
            (obj "status" "captured" "purpose" (obj-get (event-payload event) "profile")
                 "message_count" (length (obj-get req "messages"))
                 "message_characters" (obj-get (event-payload event) "prompt_chars")
                 "request_metadata" (obj "model_call_id" call-event-id "captured_at" at
                                         "milliseconds" (obj-get (event-payload event) "milliseconds"))
                 "request" req "response" (json-parse response)))))))

(defun %age (iso now) (max 0 (- now (%universal iso))))

(defun observed-live (kernel)
  "The page's live snapshot: what is being attended to, the work docket,
runtime parameters, and messages from coding agents."
  (let* ((now (get-universal-time))
         (db (kernel-db kernel))
         (open (sql-rows db "SELECT id,origin,reason,audience,status,steps,created_at,updated_at FROM thoughts WHERE status IN (?1,?2) ORDER BY id DESC LIMIT 50"
                         "queued" "running"))
         (public (find "responding" open :key #'second :test #'string=))
         (private (remove "responding" open :key #'second :test #'string=))
         (focus (find "running" private :key #'fifth :test #'string=))
         (inbox (sql-rows db "SELECT id,source_event_id,status,created_at FROM thoughts WHERE audience=?1 ORDER BY id DESC LIMIT 30"
                          "coding-agent"))
         (parameters (obj)))
    (dolist (s (settings-list kernel))
      (let ((group (string-downcase (substitute #\_ #\Space (obj-get s "group")))))
        (setf (gethash (obj-get s "name") (or (gethash group parameters)
                                              (setf (gethash group parameters) (obj))))
              (obj-get s "value"))))
    (multiple-value-bind (modulators drives) (affect-now kernel)
      (setf (gethash "affect" parameters) modulators
            (gethash "drives" parameters) drives))
    (obj "as_of" now
         "attention"
         (obj "state" (cond (public "responding") (focus "thinking") (open "queued") (t "idle"))
              "not_ready" (not-ready-warning kernel)
              "description" (cond (public (format nil "Answering the operator (~a steps so far)" (sixth public)))
                                  (focus (%clip (third focus) 160))
                                  (t "Nothing in hand"))
              "public_turn" (if public
                                (obj "active" t "stage" (fifth public) "stage_age_seconds" (%age (eighth public) now))
                                (obj "active" :false))
              "private_attention"
              ;; The attention register is the agent's open concerns (v3-clp.7).
              (let* ((concerns (open-concerns kernel))
                     (rows (mapcar (lambda (r)
                                     (destructuring-bind (id kind question rationale salience steps attended last note next) r
                                       (obj "motive_id" id "status" (if (find id private :key (lambda (p) (search id (third p))))
                                                                        "attending" "open")
                                            "phase" kind "question" question "rationale" rationale
                                            "salience" salience "steps" steps "attended" attended
                                            "last_attended_at" last "progress" note "next_look_at" next)))
                                   concerns))
                     (o (obj "open_motive_count" (length concerns) "curiosity_count" (length concerns))))
                (setf (gethash "curiosities" o) rows)
                o)
              "current_focus" (and focus (obj "question" (%clip (third focus) 200) "status" (fifth focus)
                                              "age_seconds" (%age (seventh focus) now))))
         "work_docket" (let ((o (obj)))
                         (setf (gethash "items" o) (docket-items kernel :states '("active" "waiting" "completed" "cancelled")))
                         o)
         "runtime_parameters" parameters
         "peer_inbox"
         (let ((counts (obj)))
           (dolist (r inbox) (incf (gethash (third r) counts 0)))
           (let ((o (obj "pending_count" (count-if (lambda (r) (member (third r) '("queued" "running") :test #'string=)) inbox)
                         "total" (length inbox) "counts" counts)))
             (setf (gethash "items" o)
                   (mapcar (lambda (r)
                             (destructuring-bind (id source status created) r
                               (declare (ignore created))
                               (let ((p (and source (ignore-errors (event-payload (event-by-id kernel source))))))
                                 (obj "receipt_event_id" source "sender_name" (and p (obj-get (obj-get p "origin") "name"))
                                      "state" status "thread_id" (and p (obj-get p "bead"))
                                      "message_id" (format nil "thought ~d" id)))))
                           (reverse inbox)))
             o)))))
