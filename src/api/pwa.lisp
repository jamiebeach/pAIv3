(in-package #:pai-v3.api)

;;; The operator PWA carried over from v2 (docs/pwa-compatibility.md, bead
;;; v3-clp.5). Its pages are served unchanged where they can be; the routes
;;; they call are answered here in the shapes they expect, over the same
;;; functions as /v1 and from projections only. The login page is the one
;;; page rewritten: v3 signs in with passkeys.
;;;
;;; The assets are read when the system loads, so a standalone binary
;;; carries them and serves nothing from disk.

(defparameter +asset-types+
  '(("html" . "text/html; charset=utf-8") ("js" . "text/javascript; charset=utf-8")
    ("svg" . "image/svg+xml") ("webmanifest" . "application/manifest+json; charset=utf-8")))

(defun read-web-assets ()
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file (uiop:directory-files (asdf:system-relative-pathname "pai-v3" "web/")) table)
      (setf (gethash (file-namestring file) table) (uiop:read-file-string file :external-format :utf-8)))))

(defparameter *web-assets* (read-web-assets)
  "File name -> contents of every PWA asset.")

;;; Live reload, for a development agent run from source (bead v3-jm9): the
;;; changed files are loaded into the running image, so a change needs no
;;; build and no restart. Off unless dev.live_reload is set, and answered
;;; only from the agent's own machine.

(define-setting "dev.live_reload"
  :type :boolean :default :false :group "Development"
  :description "Allow the agent's own machine to reload changed source into the running agent. For a development agent run from source only.")

(defvar *reload-lock* (bt:make-lock "live reload"))

(defparameter *coding-agent-hourly-limit* 20 "Messages per coding agent per hour.")
(defvar *coding-agent-sent* '() "(name . time) of recent coding-agent messages.")

(defun %coding-agent-name (kernel)
  "The coding agent this request's bearer token belongs to, or NIL."
  (let ((auth (or (hunchentoot:header-in* :authorization) "")))
    (and (> (length auth) 7) (string-equal "Bearer " auth :end2 7)
         (pai-v3.mind:coding-agent-for-token kernel (subseq auth 7)))))

(defun coding-agent-route (kernel)
  "POST /v1/coding-agent/message {text, bead?}: the coding agent's message
becomes a stimulus (pai-v3.mind:coding-agent-message)."
  (let ((now (get-universal-time))
        (name (%coding-agent-name kernel)))
    (cond ((null (setting kernel "coding_agent.tokens"))
           (reply 404 (obj "error" "no such route")))
          ((null name)
           (reply 401 (obj "error" "not authorised")))
          ((>= (count-if (lambda (sent) (and (string= name (car sent)) (> (cdr sent) (- now 3600))))
                         *coding-agent-sent*)
               *coding-agent-hourly-limit*)
           (reply 429 (obj "error" (format nil "at most ~d messages an hour" *coding-agent-hourly-limit*))))
          (t (let* ((body (ignore-errors (json-parse (hunchentoot:raw-post-data :force-text t))))
                    (text (and body (obj-get body "text")))
                    (bead (and body (obj-get body "bead"))))
               (cond ((not (and (stringp text) (plusp (length text))
                                (<= (length text) pai-v3.mind::*coding-agent-max-chars*)))
                      (reply 400 (obj "error" (format nil "text is required, at most ~d characters"
                                                      pai-v3.mind::*coding-agent-max-chars*))))
                     ((and bead (not (and (stringp bead) (<= (length bead) 40))))
                      (reply 400 (obj "error" "bead must be a bead id")))
                     (t (push (cons name now) *coding-agent-sent*)
                        (setf *coding-agent-sent* (remove-if (lambda (sent) (< (cdr sent) (- now 3600)))
                                                             *coding-agent-sent*))
                        (reply 200 (obj "ok" t "from" name "thought_id"
                                        (pai-v3.mind:coding-agent-message kernel name text :bead bead))))))))))

(defun dev-peek (kernel what limit)
  "Recent state for a coding agent on a dev agent: dialogue, model calls
(their raw responses), thoughts or concerns. Read-only and bounded."
  (let ((db (kernel-db kernel)) (limit (if (equal what "request") limit (max 1 (min 50 limit)))) (o (obj "what" what)))
    (flet ((rows (sql &rest args) (apply #'sql-rows db sql args))
           (clip (s n) (if (and (stringp s) (> (length s) n)) (subseq s 0 n) s)))
      (setf (gethash "items" o)
            (cond ((equal what "dialogue")
                   (mapcar (lambda (r) (obj "event_id" (first r) "kind" (second r) "text" (clip (third r) 3000)))
                           (reverse (rows "SELECT event_id,kind,rendered FROM working_dialogue WHERE audience=?1 ORDER BY event_id DESC LIMIT ?2" "operator" limit))))
                  ((equal what "calls")
                   (mapcar (lambda (r) (obj "call_event_id" (first r) "at" (second r)
                                            "response" (or (ignore-errors (json-parse (third r))) (clip (third r) 6000))))
                           (loop for id in (latest-event-ids kernel "provider.call.completed" limit)
                                 for r = (sql-row db "SELECT call_event_id,at,response FROM model_captures WHERE call_event_id=?1 LIMIT 1" id)
                                 when r collect r)))
                  ((equal what "request")
                   ;; The exact messages one call was sent: limit is its call event id.
                   (let ((r (sql-row db "SELECT request FROM model_captures WHERE call_event_id=?1 LIMIT 1" limit)))
                     (if r
                         (let ((parsed (ignore-errors (json-parse (first r)))))
                           (mapcar (lambda (m) (obj "role" (obj-get m "role")
                                                    "content" (let ((c (obj-get m "content"))) (if (stringp c) c (json-encode c)))))
                                   (obj-get parsed "messages")))
                         (list "no capture for that call"))))
                  ((equal what "formation")
                   (mapcar (lambda (id) (let ((e (event-by-id kernel id))) (obj "event_id" id "at" (event-at e) "run" (event-payload e))))
                           (latest-event-ids kernel "graph.formation.done" limit)))
                  ((equal what "failures")
                   (loop for id in (latest-event-ids kernel "provider.call.failed" limit)
                         for r = (sql-row db "SELECT call_event_id,at,response FROM model_captures WHERE call_event_id=?1 LIMIT 1" id)
                         collect (obj "call_event_id" id "at" (second r)
                                      "response" (and r (or (ignore-errors (json-parse (third r))) (clip (third r) 6000))))))
                  ((equal what "thoughts")
                   (mapcar (lambda (r) (obj "id" (first r) "origin" (second r) "audience" (third r) "status" (fourth r)
                                            "steps" (fifth r) "outcome" (clip (sixth r) 500) "reason" (clip (seventh r) 500)))
                           (rows "SELECT id,origin,audience,status,steps,outcome,reason FROM thoughts WHERE status IN (?1,?2,?3,?4,?5,?6) ORDER BY id DESC LIMIT ?7"
                                 "queued" "requested" "running" "ended" "failed" "interrupted" limit)))
                  (t (list "what is dialogue, calls, failures or thoughts"))))
      o)))

(defun live-reload ()
  "Load whatever source changed since the last load, then re-read the PWA
assets. Returns the compiler's output."
  (bt:with-lock-held (*reload-lock*)
    (let ((output (make-string-output-stream)))
      (let ((*standard-output* output) (*error-output* output))
        (asdf:load-system "pai-v3"))
      (setf *web-assets* (read-web-assets))
      (get-output-stream-string output))))

(defparameter +pages+
  '(("/" . "terminal.html") ("/terminal" . "terminal.html") ("/dashboard" . "observability.html")
    ("/graph" . "graph-explorer.html") ("/g" . "graph-explorer.html") ("/affect" . "affect.html")
    ("/notifications" . "notifications.html") ("/settings" . "settings.html"))
  "Pages that need a signed-in operator. /login is open.")

(defun %asset (name)
  (let ((body (gethash name *web-assets*)))
    (when body
      (setf (hunchentoot:content-type*)
            (or (cdr (assoc (pathname-type name) +asset-types+ :test #'string=)) "application/octet-stream"))
      (when (string= name "service-worker.js")
        (setf (hunchentoot:header-out "Service-Worker-Allowed") "/"))
      (setf (hunchentoot:header-out "Cache-Control") "no-cache")
      body)))

(defun %signed-in (agent)
  "The operator origin of this request's session, rotating it when due."
  (multiple-value-bind (origin new) (session-origin (agent-kernel agent) (session-token))
    (when new (%issue-session agent new))
    origin))

(defun loopback-request-p ()
  (member (hunchentoot:remote-addr*) '("127.0.0.1" "::1") :test #'string=))

;;; Terminal history: dialogue rows in the v2 presentation shape.

(defun %terminal-event (kernel row)
  "ROW is (event-id kind at). Returns the v2 terminal event."
  (destructuring-bind (id kind at) row
    (let* ((payload (event-payload (event-by-id kernel id)))
           (text (obj-get payload "text" ""))
           (ts (parse-iso8601 at)))
      (cond ((string= kind "turn")
             (let ((data (obj "text" text)))
               (when (obj-get payload "images")
                 (setf (gethash "images" data)
                       (mapcar (lambda (i) (format nil "/api/v2/images/~a" i)) (obj-get payload "images"))))
               (obj "id" id "ts" ts "type" "user" "data" data)))
            ((string= kind "notice")
             (obj "id" id "ts" ts "type" "notice"
                  "data" (obj "event_id" id "text" text "at" ts
                              "topic" (or (obj-get payload "importance") "general"))))
            (t (obj "id" id "ts" ts "type" "final"
                    "data" (if (eq t (obj-get payload "interrupted")) (format nil "~a *[interrupted]*" text) text)))))))

(defun terminal-history (kernel &key before (limit 200))
  "Operator dialogue before event id BEFORE, oldest first, and whether more exists."
  (let* ((rows (sql-rows (kernel-db kernel)
                         "SELECT event_id,kind,at FROM working_dialogue WHERE audience=?1 AND event_id<?2 ORDER BY event_id DESC LIMIT ?3"
                         "operator" (or before most-positive-fixnum) (1+ limit)))
         (more (> (length rows) limit)))
    (values (mapcar (lambda (r) (%terminal-event kernel r)) (reverse (subseq rows 0 (min limit (length rows)))))
            more)))

(defun %operator-thought-in-flight-p (kernel)
  (and (sql-value (kernel-db kernel)
                  "SELECT 1 FROM thoughts WHERE audience=?1 AND status IN (?2,?3) LIMIT 1"
                  "operator" "requested" "running")
       t))

;;; Live activity (v2's grey notes): what each operator thought is doing.
;;; Ported from v2's conscious-conversation presentation: the same wording,
;;; progress throttled to one note per call every two seconds, replies in
;;; the transcript and private thinking on the ambient shelf.

(defvar *progress-last* (make-hash-table) "generation id -> internal time of the last note")
(defvar *progress-lock* (bt:make-lock "terminal progress"))

(defun %progress-due-p (detail)
  (let ((generation (obj-get detail "generation_id"))
        (now (get-internal-real-time)))
    (bt:with-lock-held (*progress-lock*)
      (let ((last (gethash generation *progress-last*)))
        (when (or (null last) (obj-get detail "usage")
                  (>= (- now last) (* 2 internal-time-units-per-second)))
          (setf (gethash generation *progress-last*) now)
          (when (> (hash-table-count *progress-last*) 256) (clrhash *progress-last*))
          t)))))

(defun progress-display (detail private-p)
  "v2's progress wording."
  (let* ((usage (obj-get detail "usage"))
         (exact (and (hash-table-p usage) (obj-get usage "completion_tokens")))
         (reasoning-exact (and (hash-table-p usage)
                               (or (obj-get (obj-get usage "completion_tokens_details") "reasoning_tokens")
                                   (obj-get usage "reasoning_tokens"))))
         (estimate (obj-get detail "estimated_output_tokens" 0))
         (activity (if (plusp (obj-get detail "reasoning_characters" 0)) "reasoning" "responding"))
         (prefix (if private-p "private cognition" "pAI")))
    (cond (exact (format nil "~a model response complete · ~:d completion tokens~@[ · ~:d reasoning tokens~]"
                         prefix exact (and (integerp reasoning-exact) (plusp reasoning-exact) reasoning-exact)))
          ((plusp estimate) (format nil "~a is ~a · ~~ ~:d tokens received" prefix activity estimate))
          ((eq t (obj-get detail "heartbeat")) (format nil "~a provider is still processing" prefix))
          (t (format nil "~a is ~a" prefix activity)))))

(defun activity-text (kind detail)
  "v2's activity wording."
  (cond ((string= kind "model-request")
         (format nil "model step · ~d messages · ~:d characters"
                 (obj-get detail "message_count" 0) (obj-get detail "message_characters" 0)))
        ((string= kind "tool-start")
         (format nil "~a · ~a" (obj-get detail "tool_name" "tool") (obj-get detail "input" "")))
        ((string= kind "tool-result")
         (format nil "~a completed · ~:d characters" (obj-get detail "tool_name" "tool")
                 (obj-get detail "result_characters" 0)))
        ((string= kind "reply-ready") "final reply ready")
        ((string= kind "allowance-spent")
         (format nil "allowance of ~d model calls reached · answering with what I have"
                 (obj-get detail "model_calls" 0)))
        ((string= kind "model-failed")
         (format nil "model boundary failed · ~a" (obj-get detail "reason" "unknown provider failure")))
        (t kind)))

(defparameter +toasted-audiences+ '("operator" "coding-agent")
  "Thoughts whose activity the operator's terminal shows; guests' are theirs.")

(defun install-activity (agent)
  "Send thoughts' activity to connected terminals: a reply's in its
transcript, all other cognition as toasts that name the kind of thought."
  (setf *activity-hook*
        (lambda (thought kind detail)
          (when (and (member (getf thought :audience) +toasted-audiences+ :test #'equal)
                     (or (string/= kind "progress") (%progress-due-p detail)))
            (broadcast agent "activity"
                       (obj "thought_id" (getf thought :id) "kind" kind
                            "private" (if (string= "responding" (getf thought :origin)) :false t)
                            "origin" (if (equal "coding-agent" (getf thought :audience))
                                         "coding agent" (getf thought :origin))
                            ;; The first step says what the thought is about.
                            "about" (and (string= kind "model-request") (eql 0 (getf thought :steps))
                                         (let ((r (or (getf thought :reason) "")))
                                           (subseq r 0 (min 90 (or (position #\Newline r) (length r))))))
                            "detail" detail))))))

;;; Terminal stream: new dialogue as it lands, reply progress as it streams,
;;; and failed operator thoughts, as unnamed SSE messages with ids for resume.

(defun %sse (out object &optional id)
  (write-sequence (babel:string-to-octets
                   (format nil "~@[id: ~a~%~]data: ~a~%~%" id (json-encode object)) :encoding :utf-8)
                  out)
  (finish-output out))

(defun terminal-stream (agent since)
  (setf (hunchentoot:content-type*) "text/event-stream"
        (hunchentoot:header-out "Cache-Control") "no-cache")
  (let* ((k (agent-kernel agent))
         (box (make-mailbox))
         (out (hunchentoot:send-headers))
         (last since)
         (last-failed (or (sql-value (kernel-db k) "SELECT max(id) FROM thoughts WHERE audience=?1 LIMIT 1" "operator") 0)))
    (bt:with-lock-held ((agent-lock agent)) (push box (agent-subscribers agent)))
    (write-sequence (babel:string-to-octets (format nil ": connected~%~%")) out)
    (finish-output out)
    (unwind-protect
         (handler-case
             (loop with quiet = 0
                   while (agent-running agent)
                   do (dolist (item (take-lines box 0.5))
                        (destructuring-bind (event . data) item
                          (when (string= event "command")
                            (%sse out (obj "id" 0 "type" "user" "data" (obj "text" (obj-get data "text"))))
                            (%sse out (obj "id" 0 "type" "command" "data" (obj-get data "reply"))))
                          (when (string= event "activity")
                            (let ((kind (obj-get data "kind"))
                                  (private (eq t (obj-get data "private")))
                                  (detail (obj-get data "detail")))
                              (if (string= kind "progress")
                                  (%sse out (obj "id" 0 "type" "stream-progress"
                                                 "data" (obj "generation_id" (obj-get detail "generation_id")
                                                             "display" (progress-display detail private)
                                                             "private" (if private t :false)
                                                             "estimated_output_tokens" (obj-get detail "estimated_output_tokens" 0)
                                                             "heartbeat" (obj-get detail "heartbeat")
                                                             "usage" (obj-get detail "usage"))))
                                  (%sse out (obj "id" 0 "type" (if private "ambient" "tool")
                                                 "data" (if private
                                                            (format nil "~a · ~a~@[~%~a~]" (obj-get data "origin" "thinking")
                                                                    (activity-text kind detail) (obj-get data "about"))
                                                            (activity-text kind detail)))))))))
                      (dolist (row (sql-rows (kernel-db k)
                                             "SELECT event_id,kind,at FROM working_dialogue WHERE audience=?1 AND event_id>?2 ORDER BY event_id LIMIT 50"
                                             "operator" last))
                        (let ((ev (%terminal-event k row)))
                          (%sse out ev (first row))
                          (setf last (first row) quiet 0)))
                      (dolist (row (sql-rows (kernel-db k)
                                             "SELECT id,outcome,status FROM thoughts WHERE audience=?1 AND status IN (?2,?3) AND id>?4 ORDER BY id LIMIT 10"
                                             "operator" "failed" "parked" last-failed))
                        (setf last-failed (first row))
                        (%sse out (obj "id" 0 "type" "error"
                                       "data" (obj "status" (third row)
                                                   "operator_message" (format nil (if (equal "parked" (third row))
                                                                                      "That turn stopped without a reply: ~a"
                                                                                      "That thought failed: ~a")
                                                                              (or (second row) "no reason recorded"))))))
                      (when (> (incf quiet) 30)
                        (setf quiet 0)
                        (write-sequence (babel:string-to-octets (format nil ": keepalive~%~%")) out)
                        (finish-output out)))
           (error (e) (format *error-output* "~&terminal stream ended: ~a~%" e)))
      (bt:with-lock-held ((agent-lock agent))
        (setf (agent-subscribers agent) (remove box (agent-subscribers agent)))))
    nil))

;;; Slash commands: a small v3 set over the same functions as the tools.

(defun terminal-command (kernel text &optional token)
  (let* ((words (remove "" (uiop:split-string (string-trim " " text) :separator " ") :test #'string=))
         (name (string-downcase (subseq (first words) 1))))
    (cond ((string= name "status")
           (format nil "~a · ledger head ~d · ~:[idle~;a thought is running~] · spent today ~,2f USD~@[~%~a~]"
                   (kernel-agent-id kernel) (ledger-head kernel) (%operator-thought-in-flight-p kernel)
                   (/ (spent-today kernel) 1000000) (not-ready-warning kernel)))
          ((string= name "affect") (render-affect kernel))
          ((string= name "inspect")
           (funcall (pai-v3.mind::tool-run (gethash "inspect" *tools*)) kernel 0
                    (obj "what" (or (second words) "overview") "name" (third words))))
          ((string= name "settings")
           (format nil "~{~a~^~%~}"
                   (mapcar (lambda (s) (format nil "~a = ~a" (obj-get s "name") (json-encode (obj-get s "value"))))
                           (remove-if-not (lambda (s) (or (null (second words)) (search (second words) (obj-get s "name"))))
                                          (settings-list kernel)))))
          ((string= name "docket")
           (let ((items (docket-items kernel)))
             (if items
                 (format nil "~{~a~^~%~}" (mapcar (lambda (i) (format nil "~a [~a] ~a — next: ~a" (obj-get i "work_id")
                                                                      (obj-get i "display_state") (obj-get i "title")
                                                                      (obj-get i "next_step")))
                                                  items))
                 "No open work items.")))
          ((string= name "docket-reopen")
           (if (second words) (reopen-work kernel (second words)) "Give the work_id to reopen: /docket-reopen w-..."))
          ((string= name "pair")
           (handler-case
               (format nil "Pairing code: ~a~%On the other device, open this agent's address, choose \"Use a recovery code\", ~
enter this code and create a passkey there. It works once, for ten minutes."
                       (issue-pairing-code kernel token))
             (error (e) (format nil "Cannot pair: ~a" e))))
          ((member name '("fleet" "fleet-request" "fleet-approve" "board") :test #'string=)
           (fleet-command kernel name words))
          (t "Commands: /status, /affect, /inspect [what] [name], /settings [part of a name], /docket, /docket-reopen <id>, /fleet [pending], /fleet-request <address>, /fleet-approve <id> <code>, /board [read <thread>], /pair."))))

;;; Routes

(defun pwa-route (agent method path)
  "Answer PATH if it belongs to the PWA. Returns (values body handled-p)."
  (handler-case (%pwa-route agent method path)
    (error (e)
      (values (reply 500 (obj "error" (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m))))))
              t))))

(defun %canonical-redirect (agent path)
  "When a page is opened at another name for this agent (127.0.0.1 for
localhost), the URL to send it to; passkeys only work at the agent's origin."
  (let ((host (hunchentoot:host)) (origin (agent-origin agent)))
    (when (and host origin (search "//localhost" origin)
               (not (search (concatenate 'string "//" host) origin))
               (or (search "127.0.0.1" host) (search "[::1]" host)))
      (format nil "~a~a~@[?~a~]" origin path (hunchentoot:query-string*)))))

(defun %pwa-route (agent method path)
  (let ((k (agent-kernel agent)))
    (cond
      ;; Peers sign their requests; an operator session never stands in.
      ((fleet-path-p path) (values (fleet-route agent method path) t))

      ((or (uiop:string-prefix-p "/oauth/google/" path) (string= path "/api/google/disconnect"))
       (values (let ((origin (%signed-in agent)))
                 (cond ((null origin) (reply 401 (obj "error" "sign in first")))
                       ((and (eq method :post) (not (same-origin-p agent))) (reply 403 (obj "error" (%refusal agent))))
                       (t (google-route agent method path origin))))
               t))

      ((uiop:string-prefix-p "/api/notifications/" path)
       (values (let ((origin (%signed-in agent)))
                 (cond ((null origin) (reply 401 (obj "error" "not signed in")))
                       ((and (eq method :post) (not (same-origin-p agent))) (reply 403 (obj "error" (%refusal agent))))
                       (t (notify-route agent method path origin))))
               t))

      ((and (eq method :get) (or (string= path "/login") (assoc path +pages+ :test #'string=))
            (%canonical-redirect agent path))
       (values (hunchentoot:redirect (%canonical-redirect agent path)) t))

      ((and (eq method :get) (uiop:string-prefix-p "/api/dashboard/" path))
       (values (if (not (%signed-in agent))
                   (reply 401 (obj "error" "sign in first"))
                   (let ((param (lambda (name) (hunchentoot:get-parameter name))))
                     (cond ((string= path "/api/dashboard/observability/live")
                            (reply 200 (observed-live k)))
                           ((string= path "/api/dashboard/observability/history")
                            (reply 200 (observed-history
                                        k (or (ignore-errors (parse-integer (funcall param "hours"))) 24))))
                           ((string= path "/api/dashboard/model-context")
                            (let ((id (ignore-errors (parse-integer (funcall param "model_call_id")))))
                              (reply 200 (if id (model-capture k id)
                                             (obj "status" "not-found" "reason" "No model call named.")))))
                           (t (reply 404 (obj "error" "no such route"))))))
               t))

      ((and (eq method :post) (string= path "/api/v2/graph/search"))
       ;; The graph explorer page: the operator's, so signed in only.
       (values (if (not (%signed-in agent))
                   (reply 401 (obj "error" "sign in first"))
                   (let ((body (or (ignore-errors (json-parse (hunchentoot:raw-post-data :force-text t))) (obj))))
                     (flet ((text (k) (let ((v (obj-get body k))) (and (stringp v) (plusp (length v)) v))))
                       (reply 200 (graph-search k :query (text "query") :start-id (text "starting_node_id")
                                                  :depth (let ((d (obj-get body "maximum_depth"))) (if (integerp d) d 2)))))))
               t))

      ((and (eq method :post) (string= path "/v1/coding-agent/message"))
       (values (coding-agent-route k) t))

      ((and (eq method :post) (string= path "/v1/dev/reload"))
       (values (cond ((not (eq t (setting k "dev.live_reload")))
                      (reply 404 (obj "error" "no such route")))
                     ((not (loopback-request-p))
                      (reply 403 (obj "error" "live reload is answered only from this machine")))
                     (t (handler-case (reply 200 (obj "ok" t "output" (live-reload) "not_ready" (not-ready-warning k)))
                          (error (e) (reply 500 (obj "ok" :false "error" (princ-to-string e)))))))
               t))

      ((and (eq method :get) (string= path "/v1/dev/peek"))
       ;; For the coding agent on a dev agent: read recent state while it runs.
       ;; Same gate as reload: the dev setting, and only from this machine.
       (values (cond ((not (eq t (setting k "dev.live_reload")))
                      (reply 404 (obj "error" "no such route")))
                     ((not (loopback-request-p))
                      (reply 403 (obj "error" "peek is answered only from this machine")))
                     (t (reply 200 (dev-peek k (hunchentoot:get-parameter "what")
                                             (or (ignore-errors (parse-integer (hunchentoot:get-parameter "limit"))) 6)))))
               t))

      ((and (eq method :get) (string= path "/v1/dev/health"))
       ;; For the deploy step: a projection left needs-rebuild must stop it.
       (values (cond ((not (eq t (setting k "dev.live_reload")))
                      (reply 404 (obj "error" "no such route")))
                     ((not (loopback-request-p))
                      (reply 403 (obj "error" "health is answered only from this machine")))
                     (t (reply 200 (obj "ok" (if (not-ready-warning k) :false t)
                                        "not_ready" (not-ready-warning k)))))
               t))

      ((and (eq method :get) (uiop:string-prefix-p "/api/v2/images/" path))
       (values (let ((row (and (%signed-in agent) (pai-v3.mind::image-row k (subseq path 15)))))
                 (if row
                     (progn (setf (hunchentoot:content-type*) (first row)
                                  (hunchentoot:header-out "Cache-Control") "private, max-age=86400")
                            (second row))
                     (reply 404 (obj "error" "no such image"))))
               t))

      ((and (eq method :get) (string= path "/v1/auth/state"))
       (values (reply 200 (obj "agent" (kernel-agent-id k)
                               "devices" (length (active-devices k))
                               ;; So the browser offers only this agent's passkeys, not every
                               ;; passkey it holds for the same host name. IDs are not secrets.
                               "credentials" (mapcar #'first (active-devices k))
                               "bootstrap" (if (and (null (active-devices k)) (loopback-request-p)) t :false)
                               "signed_in" (if (%signed-in agent) t :false)))
               t))

      ((and (eq method :get) (string= path "/logout"))
       ;; A page to open, so a browser can sign out without a button.
       (when (session-token) (end-session k (session-token) "signed out"))
       (hunchentoot:set-cookie "pai_session" :value "" :path "/" :max-age 0 :http-only t :same-site "Strict")
       (values (see-other "/login") t))

      ((and (eq method :get) (string= path "/login"))
       (values (%asset "login.html") t))

      ((and (eq method :get) (assoc path +pages+ :test #'string=))
       (if (%signed-in agent)
           (values (%asset (cdr (assoc path +pages+ :test #'string=))) t)
           (values (see-other (format nil "/login?next=~a" (hunchentoot:url-encode path))) t)))

      ((and (eq method :get) (gethash (subseq path 1) *web-assets*)
            (not (search ".html" path)))
       (values (%asset (subseq path 1)) t))

      ((and (eq method :get) (string= path "/api/v2/history"))
       (values (if (%signed-in agent)
                   (multiple-value-bind (events more)
                       (terminal-history k :before (ignore-errors (parse-integer (hunchentoot:get-parameter "before")))
                                           :limit (max 1 (min 500 (or (ignore-errors (parse-integer (hunchentoot:get-parameter "limit"))) 200))))
                     (let ((page (obj "has_more" (if more t :false)
                                      "turn_in_flight" (if (%operator-thought-in-flight-p k) t :false)
                                      ;; Shown on the operational shelf until it is fixed.
                                      "operational" (not-ready-warning k))))
                       ;; Set directly: OBJ drops empty lists, and the page needs "events": [].
                       (setf (gethash "events" page) events)
                       (reply 200 page)))
                   (reply 401 (obj "error" "not signed in")))
               t))

      ((and (eq method :get) (string= path "/api/v2/stream"))
       (values (if (%signed-in agent)
                   (terminal-stream agent (or (ignore-errors (parse-integer (hunchentoot:get-parameter "since"))) 0))
                   (reply 401 (obj "error" "not signed in")))
               t))

      ((and (eq method :post) (string= path "/api/v2/send"))
       (values
        (cond ((not (same-origin-p agent)) (reply 403 (obj "error" (%refusal agent))))
              (t (let ((origin (%signed-in agent)))
                   (if (null origin)
                       (reply 401 (obj "error" "not signed in"))
                       (let* ((b (body-json))
                              (text (string-trim '(#\Space #\Newline #\Tab) (obj-get b "message" ""))))
                         (cond ((> (length (obj-get b "images")) pai-v3.mind::*images-per-turn*)
                                (reply 400 (obj "error" (format nil "At most ~d images at a time." pai-v3.mind::*images-per-turn*))))
                               ((and (zerop (length text)) (null (obj-get b "images"))) (reply 400 (obj "error" "Nothing to send.")))
                               ((char= #\/ (char text 0))
                                (broadcast agent "command" (obj "text" text "reply" (terminal-command k text (session-token))))
                                (reply 200 (obj "ok" t "command" text)))
                               (t
                                (let* ((images (handler-case
                                                   (mapcar (lambda (url) (pai-v3.mind::store-image k url)) (obj-get b "images"))
                                                 (error (e) (return-from %pwa-route
                                                              (values (reply 400 (obj "error" (princ-to-string e))) t)))))
                                       (turn (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                                                             :text (if (zerop (length text)) "(an image)" text) :images images
                                                             :origin (progn (setf (gethash "channel" origin) "web") origin)))
                                       (thought (request-thought k "responding" "The operator spoke."
                                                                 :channel "web" :source-event-id (event-id turn))))
                                  (reply 200 (obj "ok" t "receipt" (obj "turn_id" (event-id turn) "thought_id" thought)))))))))))
        t))

      (t (values nil nil)))))
