(load "/v3/tests/support-webauthn.lisp")
(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; End-to-end over HTTP: passkey sign-in, a turn, streamed reply deltas over
;;; SSE, the reply in the dialogue, and refusals. Real scheduler thread; the
;;; model is a scripted provider that streams its words.

(defvar *port* (+ 41000 (random 2000)))
(defun url (path) (format nil "http://127.0.0.1:~d~a" *port* path))

(defun call (method path &key body token (client-header t) origin)
  (let ((headers (append (list (cons "Content-Type" "application/json"))
                         (and client-header (list (cons "X-PAI-Client" "test")))
                         (and token (list (cons "Authorization" (format nil "Bearer ~a" token))))
                         (and origin (list (cons "Origin" origin))))))
    (handler-case
        (multiple-value-bind (out status)
            (if (eq method :get)
                (dex:get (url path) :headers headers)
                (dex:post (url path) :headers headers :content (json-encode (or body (obj)))))
          (values status (json-parse out)))
      (dex:http-request-failed (e)
        (values (dex:response-status e) (ignore-errors (json-parse (dex:response-body e))))))))

(defun b64 (o) (base64url-encode o))

(defun api-register (a)
  (let* ((challenge (obj-get (nth-value 1 (call :post "/v1/auth/challenge" :body (obj "purpose" "register"))) "challenge"))
         (ad (auth-data a :attested t)))
    (call :post "/v1/auth/register"
          :body (obj "name" "phone" "challenge" challenge
                     "client_data_json" (b64 (client-data "webauthn.create" challenge))
                     "attestation_object" (b64 (cbor-map (cbor-text "fmt") (cbor-text "none")
                                                          (cbor-text "attStmt") (cbor-map)
                                                          (cbor-text "authData") (cbor-bytes ad)))))))

(defun api-login (a)
  (let* ((challenge (obj-get (nth-value 1 (call :post "/v1/auth/challenge" :body (obj "purpose" "login"))) "challenge"))
         (cd (client-data "webauthn.get" challenge))
         (ad (auth-data a))
         (sig (ironclad:sign-message (authn-private a) (sha (cat ad (sha cd))))))
    (obj-get (nth-value 1 (call :post "/v1/auth/login"
                                :body (obj "credential_id" (b64 (authn-cred-id a)) "challenge" challenge
                                           "client_data_json" (b64 cd) "authenticator_data" (b64 ad)
                                           "signature" (b64 (raw-to-der sig)))))
             "token")))

(defun streaming-provider (text)
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers body timeout))
                (dolist (w (list (subseq text 0 5) (subseq text 5)))
                  (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "content" w)))))))
                (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj) "finish_reason" "stop"))
                                                     "usage" (obj "prompt_tokens" 5 "completion_tokens" 2))))
                (values 200 nil ""))))

(defun wait-for (predicate &optional (seconds 10))
  (loop repeat (* 20 seconds) thereis (funcall predicate) do (sleep 0.05)))

(deftest the-full-path-over-http
  (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
        *thought-provider* (streaming-provider "Good morning, Sam."))
  (with-temp-kernel (k)
    (let ((agent (start-agent k :port *port* :address "127.0.0.1"
                                :rp-id *rp* :origin *origin*))
          (a (make-software-authenticator))
          (sse-lines '()))
      (unwind-protect
           (progn
             (is (= 401 (call :post "/v1/turns" :body (obj "text" "hi"))) "signed out: refused")
             (is (= 200 (api-register a)) "bootstrap enrollment")
             (let ((token (api-login a)))
               (is (stringp token))
               (is (= 200 (call :get "/v1/me" :token token)))
               ;; Listen to the event stream.
               (bt:make-thread
                (lambda ()
                  (handler-case
                      (let ((s (dex:get (url "/v1/stream") :want-stream t :use-connection-pool nil :keep-alive nil :read-timeout 60
                                                            :headers (list (cons "Authorization" (format nil "Bearer ~a" token))))))
                        (loop for line = (read-line s nil) while line
                              do (push line sse-lines)))
                    (error (e) (push (format nil "client error: ~a" e) sse-lines)))))
               (wait-for (lambda () (pai-v3.api::agent-subscribers agent)))
               (multiple-value-bind (status body) (call :post "/v1/turns" :token token
                                                                         :body (obj "text" "Hi Alpha" "channel" "web"))
                 (is (= 202 status))
                 (is (integerp (obj-get body "thought_id"))))
               (is (wait-for (lambda ()
                               (some (lambda (turn) (search "Good morning, Sam." (obj-get turn "text")))
                                     (obj-get (nth-value 1 (call :get "/v1/dialogue" :token token)) "turns"))))
                   "the reply arrives in the dialogue")
               (is (wait-for (lambda () (some (lambda (l) (search "\"text\":\"Good \"" l)) sse-lines)))
                   (format nil "reply deltas streamed over SSE; got ~s" (reverse sse-lines)))
               (let ((origin-row (sql-value (kernel-db k) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                                            (kernel-agent-id k) "turn.received")))
                 (is (search "\"auth_method\":\"passkey\"" origin-row) "the turn carries a verified origin"))
               (let ((secrets (db-open (format nil "/tmp/pai-v3-api-secrets-~d.db" (random 100000000))
                                       :key *test-key*)))
                 (pai-v3.trust:open-secrets secrets)
                 (is (= 200 (call :post "/v1/secrets" :token token
                                                     :body (obj "name" "brave" "value" "BSAverySecretValue123"
                                                                "hosts" (list "api.search.brave.com"))))
                     "a freshly verified session may set a secret")
                 (multiple-value-bind (status body) (call :get "/v1/secrets" :token token)
                   (is (= 200 status))
                   (is (equal "brave" (obj-get (first (obj-get body "secrets")) "name")))
                   (is (null (search "BSAvery" (json-encode body))) "values are never returned"))
                 (is (null (search "BSAvery" (sql-value (kernel-db k) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                                                        (kernel-agent-id k) "secret.set")))
                     "the audit event names the secret, never its value")
                 (pai-v3.trust:close-secrets)
                 (db-close secrets))
               (is (= 403 (call :post "/v1/turns" :token token :origin "https://evil.example"
                                                  :body (obj "text" "x")))
                   "foreign origin refused")
               (is (= 403 (call :post "/v1/turns" :token token :client-header nil :body (obj "text" "x")))
                   "missing client header refused")
               (is (= 401 (call :post "/v1/turns" :token "forged" :body (obj "text" "x"))))
               (call :post "/v1/auth/logout" :token token)
               (is (= 401 (call :get "/v1/me" :token token)) "signed out")))
        (stop-agent agent)))))

(run-suite "api-server")
