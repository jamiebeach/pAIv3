(load "/v3/tests/support-webauthn.lisp")
(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; The v2 PWA served by v3 (bead v3-clp.5): pages behind sign-in, passkey
;;; login state, terminal history, send, slash commands and the stream.

(defvar *port* (+ 43000 (random 2000)))
(defun url (path) (format nil "http://127.0.0.1:~d~a" *port* path))

(defun raw-get (path &key token)
  "Status, body text and Location of a GET, without following redirects."
  (handler-case
      (multiple-value-bind (body status headers)
          (dex:get (url path) :max-redirects 0
                              :headers (and token (list (cons "Authorization" (format nil "Bearer ~a" token)))))
        (values status (if (stringp body) body (babel:octets-to-string body)) (gethash "location" headers)))
    (dex:http-request-failed (e)
      (values (dex:response-status e) (dex:response-body e) (gethash "location" (dex:response-headers e))))))

(defun pwa-post (path body &key token (header "X-PAI-Request"))
  (handler-case
      (multiple-value-bind (out status)
          (dex:post (url path) :content (json-encode body)
                               :headers (append (list (cons "Content-Type" "application/json"))
                                                (and header (list (cons header "same-origin")))
                                                (and token (list (cons "Authorization" (format nil "Bearer ~a" token))))))
        (values status (json-parse out)))
    (dex:http-request-failed (e)
      (values (dex:response-status e) (ignore-errors (json-parse (dex:response-body e)))))
    (error (e) (format t "~&  pwa-post ~a: ~a~%" path e) (values 0 nil))))

(defun b64 (o) (base64url-encode o))

(defun pwa-register (a)
  (let* ((challenge (obj-get (nth-value 1 (pwa-post "/v1/auth/challenge" (obj "purpose" "register"))) "challenge")))
    (pwa-post "/v1/auth/register"
              (obj "name" "phone" "challenge" challenge
                   "client_data_json" (b64 (client-data "webauthn.create" challenge))
                   "attestation_object" (b64 (cbor-map (cbor-text "fmt") (cbor-text "none")
                                                        (cbor-text "attStmt") (cbor-map)
                                                        (cbor-text "authData") (cbor-bytes (auth-data a :attested t))))))))

(defun pwa-login (a)
  (let* ((challenge (obj-get (nth-value 1 (pwa-post "/v1/auth/challenge" (obj "purpose" "login"))) "challenge"))
         (cd (client-data "webauthn.get" challenge))
         (ad (auth-data a))
         (sig (ironclad:sign-message (authn-private a) (sha (cat ad (sha cd))))))
    (obj-get (nth-value 1 (pwa-post "/v1/auth/login"
                                    (obj "credential_id" (b64 (authn-cred-id a)) "challenge" challenge
                                         "client_data_json" (b64 cd) "authenticator_data" (b64 ad)
                                         "signature" (b64 (raw-to-der sig)))))
             "token")))

(defmacro with-pwa ((k agent) &body body)
  ;; A fresh port each time: a stream left open by one test must not reach the next.
  `(with-temp-kernel (,k)
     (incf *port*)
     (let ((,agent (start-agent ,k :port *port* :address "127.0.0.1" :rp-id *rp* :origin *origin*)))
       (unwind-protect (progn ,@body) (stop-agent ,agent)))))

(deftest pages-need-a-session-and-assets-do-not
  (with-pwa (k agent)
    (multiple-value-bind (status body location) (raw-get "/terminal")
      (declare (ignore body))
      (is (= 302 status))
      (is (uiop:string-prefix-p "/login?next=" location)
          "a path, not an absolute URL: behind an HTTPS proxy the browser keeps https"))
    (is (search "passkey" (nth-value 1 (raw-get "/login"))))
    (is (= 404 (raw-get "/terminal.html")) "html is only served as a page")
    (is (= 200 (raw-get "/app-shell.js")))
    (is (= 200 (raw-get "/manifest.webmanifest")))
    (let ((state (json-parse (nth-value 1 (raw-get "/v1/auth/state")))))
      (is (= 0 (obj-get state "devices")))
      (is (eq t (obj-get state "bootstrap")) "from this machine the first passkey may be made"))))

(defun wait-for-reply (k)
  (loop repeat 200
        thereis (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "reply")
        do (sleep 0.05)))

(deftest the-terminal-over-http
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider*
        (make-provider-profile :name "test" :base-url "http://fake" :model "m"
                               :transport (lambda (url headers body &key stream-fn timeout)
                                            (declare (ignore url headers body timeout))
                                            (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "content" "Hello there."))))))
                                            (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj) "finish_reason" "stop"))
                                                                                 "usage" (obj "prompt_tokens" 5 "completion_tokens" 2))))
                                            (values 200 nil ""))))
  (with-pwa (k agent)
    (let ((a (make-software-authenticator)))
      (is (= 200 (pwa-register a)))
      (let ((token (pwa-login a)))
        (is (= 200 (raw-get "/terminal" :token token)) "signed in, the page is served")
        (is (= 403 (pwa-post "/api/v2/send" (obj "message" "hi") :token token :header nil)) "no client header: refused")
        (is (= 200 (pwa-post "/api/v2/send" (obj "message" "Good morning") :token token)))
        (is (wait-for-reply k))
        (let ((history (let ((text (nth-value 1 (raw-get "/api/v2/history?limit=10" :token token))))
                         (json-parse text))))
          (is (equal '("user" "final") (mapcar (lambda (e) (obj-get e "type")) (obj-get history "events"))))
          (is (equal "Good morning" (obj-get (obj-get (first (obj-get history "events")) "data") "text")))
          (is (equal "Hello there." (obj-get (second (obj-get history "events")) "data")))
          (let ((first-id (obj-get (second (obj-get history "events")) "id")))
            (is (eq :false (obj-get (json-parse (nth-value 1 (raw-get (format nil "/api/v2/history?before=~d&limit=1" first-id) :token token)))
                               "has_more"))
                "paging back from the reply finds the turn and nothing earlier")))
        (is (= 200 (pwa-post "/api/v2/send" (obj "message" "/status") :token token)) "slash commands are answered")
        (is (search "ledger head" (terminal-command k "/status")))
        (is (not (search "Not ready" (terminal-command k "/status"))))
        (pai-v3.kernel::sql-exec (kernel-db k) "UPDATE v3_projection_state SET status='needs-rebuild' WHERE name=?1" "graph")
        (is (search "Not ready, so answering from stale or empty state: graph" (terminal-command k "/status"))
            "a projection that is not ready is on the status line")
        (pai-v3.kernel::sql-exec (kernel-db k) "UPDATE v3_projection_state SET status='ready' WHERE name=?1" "graph")
        (is (search "Commands:" (terminal-command k "/nonsense")))
        (multiple-value-bind (status body location) (raw-get "/logout" :token token)
          (declare (ignore body))
          (is (= 302 status))
          (is (search "/login" location)))
        (is (= 302 (raw-get "/terminal" :token token)) "after /logout the session is gone")))))


(defun wait-for-line (predicate &optional (seconds 10))
  (loop repeat (* 20 seconds) thereis (funcall predicate) do (sleep 0.05)))

(deftest the-terminal-stream-shows-turns-and-failures
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider* nil)                 ; no model set up, as on a fresh agent
  (with-pwa (k agent)
    (let ((a (make-software-authenticator)) (lines '()))
      (pwa-register a)
      (let ((token (pwa-login a)))
        (let ((curl (uiop:launch-program (list "curl" "-sN" "--max-time" "25"
                                               "-H" (format nil "Authorization: Bearer ~a" token)
                                               (url "/api/v2/stream?since=0"))
                                         :output :stream)))
          (bt:make-thread
           (lambda ()
             (loop for line = (read-line (uiop:process-info-output curl) nil) while line
                   do (push line lines)))))
        (sleep 1)
        (is (search "\"events\":[]" (nth-value 1 (raw-get "/api/v2/history?limit=200" :token token)))
            "an empty history still has its events list, or the page never opens the stream")
        (is (= 200 (pwa-post "/api/v2/send" (obj "message" "Hello") :token token)))
        (is (wait-for-line (lambda () (some (lambda (l) (search "\"type\":\"user\"" l)) lines)))
            (format nil "the turn appears on the stream; got ~s" (reverse lines)))
        (is (wait-for-line (lambda () (some (lambda (l) (search "\"type\":\"error\"" l)) lines)) 20)
            (format nil "a thought that cannot run shows as an error; got ~s~%thoughts ~s~%work ~s" (reverse lines)
                    (sql-rows (kernel-db k) "SELECT id,status,outcome FROM thoughts WHERE audience=?1 LIMIT 5" "operator")
                    (sql-rows (kernel-db k) "SELECT id,handler,status,attempts,last_class,last_reason,next_at FROM v3_work WHERE handler=?1 LIMIT 5" "thought")))))))



(defun scripted-stream (turns)
  "A provider that streams each of TURNS in order: a list of chunk objects."
  (let ((left turns))
    (make-provider-profile
     :name "test" :base-url "http://fake" :model "m"
     :transport (lambda (url headers body &key stream-fn timeout)
                  (declare (ignore url headers body timeout))
                  (funcall stream-fn :heartbeat)
                  (dolist (chunk (pop left)) (funcall stream-fn (json-encode chunk)))
                  (values 200 nil "")))))

(defun delta (&rest fields) (obj "choices" (list (obj "delta" (apply #'obj fields)))))

(deftest the-terminal-shows-v2-activity-notes
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider*
        (scripted-stream
         (list (list (delta "reasoning" "Let me look.")
                     (delta "tool_calls" (list (obj "index" 0 "id" "c1" "type" "function"
                                                    "function" (obj "name" "inspect" "arguments" ""))))
                     (delta "tool_calls" (list (obj "index" 0 "function" (obj "arguments" "{\"what\":\"overview\"}"))))
                     (obj "choices" (list (obj "delta" (obj) "finish_reason" "tool_calls"))
                          "usage" (obj "prompt_tokens" 10 "completion_tokens" 7)))
               (list (delta "content" "All is well.")
                     (obj "choices" (list (obj "delta" (obj) "finish_reason" "stop"))
                          "usage" (obj "prompt_tokens" 20 "completion_tokens" 4))))))
  (with-pwa (k agent)
    (let ((a (make-software-authenticator)) (lines '()))
      (pwa-register a)
      (let* ((token (pwa-login a))
             (curl (uiop:launch-program (list "curl" "-sN" "--max-time" "20"
                                              "-H" (format nil "Authorization: Bearer ~a" token)
                                              (url "/api/v2/stream?since=0"))
                                        :output :stream)))
        (bt:make-thread (lambda ()
                          (loop for line = (read-line (uiop:process-info-output curl) nil) while line
                                do (push line lines))))
        (sleep 1)
        (pwa-post "/api/v2/send" (obj "message" "How are you?") :token token)
        (flet ((seen (text) (wait-for-line (lambda () (some (lambda (l) (search text l)) lines)) 15)))
          (is (seen "model step · ") "each model call is noted")
          (is (seen "\"type\":\"stream-progress\"") "streaming progress is shown")
          (is (seen "model response complete · 7 completion tokens") "and its exact size at the end")
          (is (seen "inspect · {\\\"what\\\":\\\"overview\\\"}") "tool calls show their input")
          (is (seen "inspect completed · ") "and their result size")
          (is (seen "final reply ready"))
          (is (seen "\"type\":\"final\"") (format nil "the reply lands; got ~s" (reverse lines))))))))

(deftest live-reload-is-off-unless-set
  (with-pwa (k agent)
    (multiple-value-bind (status body) (pwa-post "/v1/dev/reload" (obj)) (is (= 404 status) (format nil "a production agent has no such route; got ~a ~a" status (and body (json-encode body)))))
    (change-setting k "dev.live_reload" t :origin (obj "principal_type" "operator" "auth_method" "passkey"))
    (multiple-value-bind (status body) (pwa-post "/v1/dev/reload" (obj))
      (is (= 200 status) "from this machine, the running agent reloads")
      (is (eq t (obj-get body "ok"))))))

(defun bearer-post (path body token)
  (handler-case
      (multiple-value-bind (out status)
          (dex:post (url path) :content (json-encode body)
                               :headers (list (cons "Content-Type" "application/json")
                                              (cons "Authorization" (format nil "Bearer ~a" token))))
        (values status (json-parse out)))
    (dex:http-request-failed (e) (values (dex:response-status e) nil))))

(deftest the-coding-agent-messages-as-a-stimulus-not-the-operator
  (setf *thought-provider* nil)
  (with-pwa (k agent)
    (is (= 404 (bearer-post "/v1/coding-agent/message" (obj "text" "hi") "tok")) "off until a token is set")
    (change-setting k "coding_agent.tokens"
                    (list (obj "name" "Claude Code" "token_sha256" (pai-v3.kernel::sha256-hex "the-token"))
                          (obj "name" "Codex" "token_sha256" (pai-v3.kernel::sha256-hex "other-token")))
                    :origin (obj "principal_type" "operator" "auth_method" "passkey"))
    (signals setting-refused
      (change-setting k "coding_agent.tokens" (list (obj "name" "X" "token_sha256" "abc"))
                      :origin (obj "principal_type" "operator" "auth_method" "passkey")))
    (is (= 401 (bearer-post "/v1/coding-agent/message" (obj "text" "hi") "wrong")))
    (is (= 400 (bearer-post "/v1/coding-agent/message" (obj "bead" "v3-x") "the-token")) "text is required")
    (multiple-value-bind (status body)
        (bearer-post "/v1/coding-agent/message" (obj "text" "Fixed in abc123; please check." "bead" "v3-o1z") "the-token")
      (is (= 200 status))
      (is (equal "Claude Code" (obj-get body "from")) "the token names its coding agent")
      (let ((thought (thought-row k (obj-get body "thought_id"))))
        (is (equal "coding-agent" (getf thought :audience)))
        (is (search "carries no operator authority" (getf thought :reason)))
        (let ((names (mapcar #'tool-name (tools-for thought))))
          (is (member "beads" names :test #'equal) "it answers on the bead")
          (is (not (member "lisp_eval" names :test #'equal)) "and gains no operator trust")
          (is (not (member "change_setting" names :test #'equal))))
        (let ((said (pai-v3.mind::stimulus-text k (getf thought :id))))
          (is (search "coding_agent.message" said) "the thought can read what started it")
          (is (search "\"principal_type\":\"coding-agent\"" said) (format nil "and who; got ~a" said))
          (is (search "\"name\":\"Claude Code\"" said) "and which coding agent"))))
    (multiple-value-bind (status body) (bearer-post "/v1/coding-agent/message" (obj "text" "Hello from me.") "other-token")
      (is (= 200 status))
      (is (equal "Codex" (obj-get body "from")) "another coding agent is told apart")
      (is (search "Codex, a coding agent" (getf (thought-row k (obj-get body "thought_id")) :reason))))
    (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice")
        "the operator is told")))

(deftest a-stopped-agent-answers-no-one
  ;; A client's kept-alive connection outlives the listener (bead v3-uy8): the
  ;; stopped agent must refuse it, not serve it from a closed store.
  (incf *port*)
  (let (agent)
    (with-temp-kernel (k)
      (setf agent (start-agent k :port *port* :address "127.0.0.1" :rp-id *rp* :origin *origin*))
      (is (= 200 (nth-value 1 (dex:get (url "/v1/auth/state") :keep-alive t :use-connection-pool t))))
      (stop-agent agent))
    (let ((status (handler-case (nth-value 1 (dex:get (url "/v1/auth/state") :keep-alive t :use-connection-pool t))
                    (dex:http-request-failed (e) (dex:response-status e))
                    (error () :refused))))
      (is (member status '(503 :refused)) (format nil "got ~s" status)))))

(deftest the-observability-page-sees-a-turn
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider*
        (scripted-stream
         (list (list (delta "tool_calls" (list (obj "index" 0 "id" "c1" "type" "function"
                                                    "function" (obj "name" "feel" "arguments" "{}"))))
                     (obj "choices" (list (obj "delta" (obj) "finish_reason" "tool_calls"))
                          "usage" (obj "prompt_tokens" 10 "completion_tokens" 3)))
               (list (delta "content" "Steady.")
                     (obj "choices" (list (obj "delta" (obj) "finish_reason" "stop"))
                          "usage" (obj "prompt_tokens" 12 "completion_tokens" 2))))))
  (with-pwa (k agent)
    (let ((a (make-software-authenticator)))
      (pwa-register a)
      (let ((token (pwa-login a)))
        (is (= 401 (raw-get "/api/dashboard/observability/live")) "signed out: refused")
        (pwa-post "/api/v2/send" (obj "message" "How do you feel?") :token token)
        (is (wait-for-reply k))
        (let ((history (json-parse (nth-value 1 (raw-get "/api/dashboard/observability/history?hours=2" :token token)))))
          (let ((buckets (obj-get (obj-get history "activity") "buckets")))
            (is (= 2 (length buckets)))
            (is (= 2 (obj-get (car (last buckets)) "model_requests")) "both model calls counted this hour")
            (is (= 1 (obj-get (car (last buckets)) "tool_calls")))
            (is (= 1 (obj-get (car (last buckets)) "operator_messages"))))
          (is (= 1 (obj-get (obj-get (obj-get history "activity") "tool_usage") "feel")))
          (let* ((types (mapcar (lambda (e) (obj-get e "type")) (obj-get history "events")))
                 (request (find "model-request" (obj-get history "events")
                                :key (lambda (e) (obj-get e "type")) :test #'equal)))
            (is (member "recursive-tool-execution" types :test #'equal))
            (is (member "ended" types :test #'equal))
            (let ((capture (json-parse (nth-value 1 (raw-get (format nil "/api/dashboard/model-context?model_call_id=~d"
                                                                    (obj-get (obj-get request "payload") "model_call_id"))
                                                            :token token)))))
              (is (equal "captured" (obj-get capture "status")))
              (is (obj-get (obj-get capture "request") "messages") "the exact messages sent")
              (is (obj-get capture "response")))))
        (let ((live (json-parse (nth-value 1 (raw-get "/api/dashboard/observability/live" :token token)))))
          (is (equal "idle" (obj-get (obj-get live "attention") "state")))
          (is (obj-get (obj-get live "runtime_parameters") "thinking") "settings by group")
          (is (obj-get (obj-get live "runtime_parameters") "affect")))
        (is (equal "not-found" (obj-get (json-parse (nth-value 1 (raw-get "/api/dashboard/model-context?model_call_id=999999" :token token)))
                                       "status")))))))

(defun site-post (path body origin &key token)
  "POST as a browser at ORIGIN would: with its Origin header."
  (handler-case
      (multiple-value-bind (out status)
          (dex:post (url path) :content (json-encode body)
                               :headers (append (list (cons "Content-Type" "application/json")
                                                      (cons "X-PAI-Request" "same-origin")
                                                      (cons "Origin" origin))
                                                (and token (list (cons "Authorization" (format nil "Bearer ~a" token))))))
        (values status (json-parse out)))
    (dex:http-request-failed (e) (values (dex:response-status e) (ignore-errors (json-parse (dex:response-body e)))))))

(defun site-enrol (a origin &key grant)
  (let ((challenge (obj-get (nth-value 1 (site-post "/v1/auth/challenge" (obj "purpose" "register") origin)) "challenge")))
    (site-post "/v1/auth/register"
               (obj "name" "device" "challenge" challenge "recovery_grant" grant
                    "client_data_json" (b64 (client-data "webauthn.create" challenge origin))
                    "attestation_object" (b64 (cbor-map (cbor-text "fmt") (cbor-text "none")
                                                        (cbor-text "attStmt") (cbor-map)
                                                        (cbor-text "authData") (cbor-bytes (auth-data a :attested t)))))
               origin)))

(defun site-login (a origin)
  (let* ((challenge (obj-get (nth-value 1 (site-post "/v1/auth/challenge" (obj "purpose" "login") origin)) "challenge"))
         (cd (client-data "webauthn.get" challenge origin))
         (ad (auth-data a))
         (sig (ironclad:sign-message (authn-private a) (sha (cat ad (sha cd))))))
    (multiple-value-bind (status body)
        (site-post "/v1/auth/login" (obj "credential_id" (b64 (authn-cred-id a)) "challenge" challenge
                                         "client_data_json" (b64 cd) "authenticator_data" (b64 ad)
                                         "signature" (b64 (raw-to-der sig)))
                   origin)
      (values (and (= 200 status) (obj-get body "token")) status))))

(deftest a-phone-pairs-at-a-second-name
  (incf *port*)
  (with-temp-kernel (k)
    (let ((agent (start-agent k :port *port* :address "127.0.0.1" :rp-id *rp* :origin *origin*
                                :sites (list (cons "phone.example" "https://phone.example")))))
      (unwind-protect
           (let ((pc (make-software-authenticator)) (phone (make-software-authenticator)))
             (is (= 200 (site-enrol pc *origin*)) "the PC's passkey, at the agent's own name")
             (let* ((token (site-login pc *origin*))
                    (said (terminal-command k "/pair" token))
                    (code (subseq said (+ 14 (search "Pairing code: " said)) (+ 14 14 (search "Pairing code: " said)))))
               (is token)
               (is (search "Pairing code" said) said)
               (let ((*rp* "phone.example"))
                 (let ((grant (obj-get (nth-value 1 (site-post "/v1/auth/recover" (obj "code" code) "https://phone.example"))
                                       "recovery_grant")))
                   (is grant "the code buys one enrolment")
                   (is (= 200 (site-enrol phone "https://phone.example" :grant grant)) "the phone's own passkey, at its name")
                   (is (site-login phone "https://phone.example") "the phone signs in at its name")
                   (is (not (eql 200 (site-post "/v1/auth/recover" (obj "code" code) "https://phone.example")))
                       "a pairing code works once")))
               (is (site-login pc *origin*) "the PC still signs in as before")
               (is (= 0 (pai-v3.trust:recovery-codes-left k)) "recovery codes are untouched")
               (is (search "Cannot pair" (terminal-command k "/pair" "not-a-session")))))
        (stop-agent agent)))))

(deftest private-cognition-shows-as-toasts
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider* (scripted-stream (list (list (delta "content" "Settled.")
                                                        (obj "choices" (list (obj "delta" (obj) "finish_reason" "stop"))
                                                             "usage" (obj "prompt_tokens" 5 "completion_tokens" 1))))))
  (with-pwa (k agent)
    (let ((a (make-software-authenticator)) (lines '()))
      (pwa-register a)
      (let* ((token (pwa-login a))
             (curl (uiop:launch-program (list "curl" "-sN" "--max-time" "15"
                                              "-H" (format nil "Authorization: Bearer ~a" token)
                                              (url "/api/v2/stream?since=0"))
                                        :output :stream)))
        (bt:make-thread (lambda () (loop for line = (read-line (uiop:process-info-output curl) nil) while line
                                         do (push line lines))))
        (sleep 1)
        (request-thought k "concern" "Is the summary drifting?")
        (is (wait-for-line (lambda () (some (lambda (l) (and (search "\"type\":\"ambient\"" l)
                                                             (search "concern · model step" l)
                                                             (search "Is the summary drifting?" l)))
                                            lines))
                           10)
            (format nil "a toast names the kind of thought and what it is about; got ~s" (reverse lines)))))))

(run-suite "api-pwa")
