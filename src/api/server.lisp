(defpackage #:pai-v3.api
  (:use #:cl #:pai-v3.kernel #:pai-v3.mind #:pai-v3.trust)
  (:export #:start-agent #:stop-agent #:agent-port #:*api-version* #:terminal-command #:terminal-history))
(in-package #:pai-v3.api)

;;; The client API (NFR EXT-1..5). One versioned HTTP API for every client;
;;; the web PWA has no other way in. Commands are JSON over HTTPS; events
;;; stream over Server-Sent Events. Authentication is a passkey session
;;; (cookie for browsers, bearer token for native apps); every operator turn
;;; carries the origin attestation derived from that session, never from
;;; the message. The process also runs the scheduler loop.

(defparameter *api-version* "v1")

(defstruct agent kernel acceptor scheduler (running t) rp-id origin
  (sites '())                           ; more (rp-id . origin) it also answers at
  (subscribers '()) (lock (bt:make-lock "sse subscribers")))

;;; An agent can be reached at more than one name: localhost on its own
;;; machine, and a Tailscale name from a phone. Passkeys are bound to a name,
;;; so each request is served for the site it came to, and each device's
;;; passkey works at the name it was made at.

(defun agent-all-sites (agent)
  (cons (cons (agent-rp-id agent) (agent-origin agent)) (agent-sites agent)))

(defun %request-authority ()
  (or (hunchentoot:header-in* :x-forwarded-host) (hunchentoot:host)))

(defun request-site (agent)
  "The (rp-id . origin) this request came to: by its Origin header, else its
host, else the agent's first site."
  (let ((sites (agent-all-sites agent))
        (origin (hunchentoot:header-in* :origin))
        (authority (ignore-errors (%request-authority))))
    (or (and origin (find origin sites :key #'cdr :test #'string=))
        (and authority
             (find-if (lambda (site) (let ((o (cdr site)))
                                       (and o (string= authority (subseq o (+ 3 (search "://" o)))))))
                      sites))
        (first sites))))

(defun request-origin (agent) (cdr (request-site agent)))
(defun request-rp-id (agent) (car (request-site agent)))

(defvar *agent* nil)

;;; Server-Sent Events: one mailbox per connected client.

(defstruct mailbox (items '()) (lock (bt:make-lock)) (cv (bt:make-condition-variable)))

(defun broadcast (agent event data)
  "Queue (EVENT . DATA) for every connected client; each stream formats it."
  (let ((item (cons event data)))
    (bt:with-lock-held ((agent-lock agent))
      (dolist (box (agent-subscribers agent))
        (bt:with-lock-held ((mailbox-lock box))
          (setf (mailbox-items box) (append (mailbox-items box) (list item)))
          (bt:condition-notify (mailbox-cv box)))))))

(defun take-lines (box timeout)
  (bt:with-lock-held ((mailbox-lock box))
    (unless (mailbox-items box)
      (bt:condition-wait (mailbox-cv box) (mailbox-lock box) :timeout timeout))
    (prog1 (mailbox-items box) (setf (mailbox-items box) '()))))

;;; Request helpers

(defun body-json ()
  (let ((raw (hunchentoot:raw-post-data :force-text t)))
    (if (and raw (plusp (length raw))) (json-parse raw) (obj))))

(defun see-other (location)
  "Redirect to LOCATION as given. A path stays a path, so the browser keeps
the scheme and host it used: behind an HTTPS proxy (Tailscale serve) the
agent sees plain HTTP, and an absolute redirect built from that would send
the browser to http://."
  (setf (hunchentoot:return-code*) 302
        (hunchentoot:header-out :location) location)
  "")

(defun reply (status object)
  (setf (hunchentoot:return-code*) status
        (hunchentoot:content-type*) "application/json")
  (json-encode object))

(defvar *agent-name* nil
  "The name of the agent answering this request, for page titles: with
several agents open, each tab and home-screen app says whose it is.")

(defvar *session-cookie* "pai_session"
  "The session cookie's name for the agent answering this request. Each agent
has its own: browsers keep cookies by host, not port, so agents sharing a
host name would otherwise sign each other out.")

(defun session-cookie-name (agent)
  (format nil "pai_session_~a" (kernel-agent-id (agent-kernel agent))))

(defun session-token ()
  (or (hunchentoot:cookie-in *session-cookie*)
      (let ((auth (hunchentoot:header-in* :authorization)))
        (and auth (> (length auth) 7) (string-equal "Bearer " auth :end2 7) (subseq auth 7)))))

;;; Secrets set from the app. The app manages some names itself (a signing key,
;;; the tokens of a connected account, a peer's shared key); replacing one by
;;; hand would silently break what depends on it, so those are refused.

(defparameter +managed-secret-prefixes+ '("google-" "fleet-peer-" "web-push-"))

(defun %secret-problem (name hosts &optional (changing t))
  "Why NAME and HOSTS may not be used for a secret set from the app, or NIL."
  (flet ((host-ok (h) (and (stringp h) (< 0 (length h) 254)
                           (every (lambda (c) (or (alphanumericp c) (find c "-.*"))) h)
                           (find #\. h) (not (search ".." h))
                           (or (not (find #\* h)) (and (> (length h) 2) (string= "*." h :end2 2) (not (find #\* h :start 1)))))))
    (cond ((not (and (stringp name) (< 0 (length name) 65) (alphanumericp (char name 0))
                     (every (lambda (c) (or (and (alphanumericp c) (not (upper-case-p c))) (find c "-_"))) name)))
           "a secret's name is lower-case letters, digits, - and _ (up to 64)")
          ((find-if (lambda (p) (uiop:string-prefix-p p name)) +managed-secret-prefixes+)
           "the app manages that one itself")
          ((and changing (not (and (consp hosts) (every #'host-ok hosts))))
           "name the hosts it may be sent to, such as api.example.com or *.example.com (no https://, paths or spaces)"))))

(defun same-origin-p (agent)
  "Cross-site protection: state-changing browser requests must come from our
origin and carry the client header (a simple form cannot set it)."
  (let ((origin (hunchentoot:header-in* :origin)))
    (and (or (hunchentoot:header-in* :x-pai-client)
             (hunchentoot:header-in* :x-pai-request))       ; the v2 pages' header
         (or (null origin) (find origin (agent-all-sites agent) :key #'cdr :test #'equal)))))

(defun %refusal (agent)
  (let ((origin (hunchentoot:header-in* :origin)))
    (if (and origin (not (find origin (agent-all-sites agent) :key #'cdr :test #'equal)))
        (format nil "cross-origin request refused: this page is at ~a but the agent answers at ~{~a~^ or ~}; open it there"
                origin (mapcar #'cdr (agent-all-sites agent)))
        "cross-origin request refused: the request lacks the client header")))

(defun b64 (field body) (base64url-decode (obj-get body field "")))

(defun %issue-session (agent token)
  "Hand TOKEN to the client: an HttpOnly cookie for browsers, and a header
for native clients that send it as a bearer token."
  (hunchentoot:set-cookie *session-cookie* :value token :path "/" :http-only t
                                        :same-site "Strict"
                                        :secure (and (search "https:" (request-origin agent)) t)
                                        :max-age *session-absolute-seconds*)
  (setf (hunchentoot:header-out "X-PAI-Session-Token") token))

(defmacro with-operator ((origin agent) &body body)
  (let ((new (gensym)))
    `(multiple-value-bind (,origin ,new) (session-origin (agent-kernel ,agent) (session-token))
       (when ,new (%issue-session ,agent ,new))
       (if ,origin (progn ,@body) (reply 401 (obj "error" "not signed in"))))))

;;; Routes

(defun route (agent method path)
  (let ((k (agent-kernel agent)))
    (handler-case
        (cond
          ((and (eq method :post) (not (same-origin-p agent)))
           (reply 403 (obj "error" (%refusal agent))))

          ((and (eq method :post) (string= path "/v1/auth/challenge"))
           (let ((purpose (obj-get (body-json) "purpose")))
             (reply 200 (obj "challenge" (new-challenge (cond ((equal purpose "register") :register)
                                                              ((equal purpose "step-up") :step-up)
                                                              (t :login)))
                             "rp_id" (request-rp-id agent)))))

          ((and (eq method :post) (string= path "/v1/auth/register"))
           (let ((b (body-json)))
             (reply 200 (obj "credential_id"
                             (enroll-device k :name (obj-get b "name" "device")
                                              :client-data-json (b64 "client_data_json" b)
                                              :attestation-object (b64 "attestation_object" b)
                                              :challenge (obj-get b "challenge")
                                              :origin (request-origin agent) :rp-id (request-rp-id agent)
                                              :authorized-by (cond ((obj-get b "recovery_grant")
                                                                    (list :recovery (obj-get b "recovery_grant")))
                                                                   ((active-devices k) (session-token))
                                                                   ;; The first passkey only from the machine itself.
                                                                   ((loopback-request-p) :bootstrap)
                                                                   (t (error 'webauthn-error :reason "the first passkey must be created on the machine running the agent"))))))))

          ((and (eq method :post) (string= path "/v1/auth/recover"))
           ;; No session: a recovery code buys one passkey enrolment, nothing else.
           (reply 200 (obj "recovery_grant" (redeem-recovery-code k (obj-get (body-json) "code" "")))))

          ((and (eq method :post) (string= path "/v1/auth/recovery-codes"))
           (with-operator (origin agent)
             (reply 200 (obj "codes" (issue-recovery-codes k (session-token))))))

          ((and (eq method :post) (member path '("/v1/auth/login" "/v1/auth/step-up") :test #'string=))
           (let* ((b (body-json))
                  (assertion (list :credential-id (obj-get b "credential_id")
                                   :client-data-json (b64 "client_data_json" b)
                                   :authenticator-data (b64 "authenticator_data" b)
                                   :signature (b64 "signature" b)
                                   :challenge (obj-get b "challenge")
                                   :origin (request-origin agent) :rp-id (request-rp-id agent))))
             (if (string= path "/v1/auth/login")
                 (let ((token (apply #'login k assertion)))
                   (%issue-session agent token)
                   (reply 200 (obj "ok" t "token" token)))
                 (progn (apply #'step-up k (session-token) assertion)
                        (reply 200 (obj "ok" t))))))

          ((and (eq method :post) (string= path "/v1/auth/logout"))
           (when (session-token) (end-session k (session-token) "signed out"))
           (reply 200 (obj "ok" t)))

          ((and (eq method :get) (string= path "/v1/me"))
           (with-operator (origin agent)
             (reply 200 (obj "principal" (obj-get origin "principal_type")
                             "devices" (mapcar (lambda (d) (obj "credential_id" (first d) "name" (second d)))
                                               (active-devices k))))))

          ((and (eq method :post) (string= path "/v1/turns"))
           (with-operator (origin agent)
             (let* ((b (body-json))
                    (channel (obj-get b "channel" "web"))
                    (turn (receive-turn k :channel channel :audience "operator" :speaker (setting k "operator.name")
                                          :text (obj-get b "text" "")
                                          :origin (progn (setf (gethash "channel" origin) channel) origin)))
                    (thought (request-thought k "responding" "The operator spoke."
                                              :channel channel :source-event-id (event-id turn))))
               (reply 202 (obj "turn_id" (event-id turn) "thought_id" thought)))))

          ((and (eq method :post) (search "/v1/thoughts/" path) (search "/interrupt" path))
           (with-operator (origin agent)
             (interrupt-thought (parse-integer path :start (length "/v1/thoughts/") :junk-allowed t))
             (reply 202 (obj "ok" t))))

          ((and (eq method :get) (string= path "/v1/dialogue"))
           (with-operator (origin agent)
             (reply 200 (obj "turns" (mapcar (lambda (r) (obj "id" (dialogue-row-event-id r)
                                                              "channel" (dialogue-row-channel r)
                                                              "text" (dialogue-row-text r)))
                                             (recent-dialogue k "operator" :token-budget 8000))))))

          ((and (eq method :get) (string= path "/v1/settings"))
           (with-operator (origin agent)
             (reply 200 (obj "settings" (settings-list k)))))

          ((and (eq method :post) (string= path "/v1/settings"))
           (with-operator (origin agent)
             (if (not (stepped-up-p k (session-token)))
                 (reply 403 (obj "error" "step-up required" "step_up" t))
                 (let ((b (body-json)))
                   (reply 200 (obj "name" (obj-get b "name")
                                   "value" (change-setting k (obj-get b "name" "") (obj-get b "value")
                                                           :origin origin
                                                           :reason (obj-get b "reason" "changed from the app"))))))))

          ((and (eq method :get) (string= path "/v1/folders"))
           (with-operator (origin agent)
             (let ((result (host-folder-request k (obj "action" "folders"))))
               (reply (if (obj-get result "error") 503 200) result))))

          ((and (eq method :post) (string= path "/v1/folders"))
           (with-operator (origin agent)
             (if (not (stepped-up-p k (session-token)))
                 (reply 403 (obj "error" "step-up required" "step_up" t))
                 (let* ((body (body-json)) (folders (obj-get body "folders")) (request (obj)))
                   (if (not (and (hash-table-p body) (nth-value 1 (gethash "folders" body))
                                 (listp folders) (<= (length folders) 16)))
                       (reply 400 (obj "error" "Supply a folders array, at most 16 entries."))
                       (progn
                         (setf (gethash "folders" request) folders)
                         (let ((result (host-folder-request k request :admin t)))
                           (unless (obj-get result "error")
                             (append! k "host.folders.changed" (obj "folders" folders "origin" origin)))
                           (reply (if (obj-get result "error") 400 200) result))))))))

          ((and (eq method :get) (string= path "/v1/secrets"))
           (with-operator (origin agent)
             (reply 200 (obj "secrets" (mapcar (lambda (s) (obj "name" (first s) "hosts" (second s)
                                                                "updated_at" (third s)))
                                               (secret-names))))))

          ((and (eq method :post) (member path '("/v1/secrets" "/v1/secrets/remove") :test #'string=))
           (with-operator (origin agent)
             (if (not (stepped-up-p k (session-token)))
                 (reply 403 (obj "error" "step-up required" "step_up" t))
                 (let ((b (body-json)))
                   (if (string= path "/v1/secrets")
                       (let ((problem (%secret-problem (obj-get b "name") (obj-get b "hosts"))))
                         (when problem (error 'keyring-error :message problem))
                         (set-secret (obj-get b "name" "") (obj-get b "value" "") (obj-get b "hosts"))
                         (append! k "secret.set" (obj "name" (obj-get b "name") "hosts" (obj-get b "hosts")
                                                      "origin" origin))
                         (reply 200 (obj "name" (obj-get b "name") "hosts" (obj-get b "hosts"))))
                       (let ((problem (%secret-problem (obj-get b "name") nil nil)))
                         (when problem (error 'keyring-error :message problem))
                         (remove-secret (obj-get b "name" ""))
                         (append! k "secret.removed" (obj "name" (obj-get b "name" "") "origin" origin))
                         (reply 200 (obj "name" (obj-get b "name")))))))))

          ((and (eq method :get) (string= path "/v1/stream"))
           (with-operator (origin agent) (stream-events agent)))

          (t (reply 404 (obj "error" "no such route"))))
      (webauthn-error (e) (reply 401 (obj "error" (princ-to-string e))))
      (keyring-error (e) (reply 400 (obj "error" (princ-to-string e))))
      (setting-refused (e) (reply 400 (obj "error" (princ-to-string e))))
      (error (e) (reply 500 (obj "error" (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m))))))))))

(defun stream-events (agent)
  "Hold the connection open and write SSE lines until the client leaves."
  (setf (hunchentoot:content-type*) "text/event-stream"
        (hunchentoot:header-out "Cache-Control") "no-cache")
  (let ((box (make-mailbox))
        (out (hunchentoot:send-headers)))
    (bt:with-lock-held ((agent-lock agent)) (push box (agent-subscribers agent)))
    ;; Flush the headers and a first comment now, so the client knows the
    ;; stream is open before any event arrives.
    (write-sequence (babel:string-to-octets (format nil ": connected~%~%")) out)
    (finish-output out)
    (unwind-protect
         (handler-case
             (loop while (agent-running agent)
                   do (let ((lines (take-lines box 15)))
                        (if lines
                            (dolist (l lines)
                              (write-sequence (babel:string-to-octets
                                               (format nil "event: ~a~%data: ~a~%~%" (car l) (json-encode (cdr l)))
                                               :encoding :utf-8)
                                              out))
                            (write-sequence (babel:string-to-octets (format nil ": keepalive~%~%")) out))
                        (finish-output out)))
           (error () nil))
      (bt:with-lock-held ((agent-lock agent))
        (setf (agent-subscribers agent) (remove box (agent-subscribers agent)))))
    nil))

(defclass api-acceptor (hunchentoot:easy-acceptor) ((agent :initarg :agent :reader acceptor-agent)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-acceptor) request)
  (let* ((agent (acceptor-agent acceptor))
         (method (hunchentoot:request-method request))
         (path (hunchentoot:script-name request))
         (*session-cookie* (session-cookie-name agent))
         (*agent-name* (ignore-errors (fleet-own-name (agent-kernel agent)))))
    (if (not (agent-running agent))
        ;; A kept-alive connection outlives the listener; once stopped, the
        ;; agent's store may be closed, so refuse and drop it (bead v3-uy8).
        (progn (setf (hunchentoot:header-out :connection) "close"
                     hunchentoot::*close-hunchentoot-stream* t)
               (reply 503 (obj "error" "this agent has stopped")))
        (multiple-value-bind (body handled) (pwa-route agent method path)
          (if handled body (route agent method path))))))

;;; Lifecycle

(defun start-agent (kernel &key (port 8080) (address "127.0.0.1") rp-id origin sites (idle-sleep 0.25)
                                on-idle (idle-every 300))
  "Serve the API and run the scheduler loop for KERNEL. ON-IDLE, if given, is
called when the scheduler is idle, at most every IDLE-EVERY seconds (daily
backups and other housekeeping); its errors never stop the loop. The API
listens on localhost by default (AUTH-10)."
  (pai-v3.self:restore-self kernel)
  ;; RANDOM otherwise repeats the same sequence in every process (bead v3-clp.2.1).
  (setf *random-state* (make-random-state t))
  (let* ((agent (make-agent :kernel kernel :rp-id rp-id :origin origin :sites sites))
         (acceptor (make-instance 'api-acceptor :agent agent :port port :address address
                                                :access-log-destination nil :message-log-destination nil)))
    (setf (agent-acceptor agent) acceptor
          *reply-delta-hook* (lambda (id text) (broadcast agent "delta" (obj "thought_id" id "text" text))))
    (install-activity agent)
    (hunchentoot:start acceptor)
    (setf (agent-scheduler agent)
          (bt:make-thread
           (lambda ()
             (loop with last-idle-run = 0
                   with last-due-check = 0
                   while (agent-running agent)
                   ;; Reminders fire on time even while the scheduler is busy.
                   do (when (> (get-universal-time) (+ last-due-check 15))
                        (setf last-due-check (get-universal-time))
                        (ignore-errors (pai-v3.mind:run-periodic kernel)))
                      (let ((outcome (handler-case (run-scheduler-step kernel)
                                       (error () :idle))))
                        (when (and (consp outcome) (eq :ran (first outcome)))
                          (broadcast agent "step" (obj "work_id" (second outcome)
                                                       "outcome" (string-downcase (symbol-name (third outcome))))))
                        (when (eq outcome :idle)
                          (when (and on-idle (> (get-universal-time) (+ last-idle-run idle-every)))
                            (setf last-idle-run (get-universal-time))
                            (ignore-errors (funcall on-idle)))
                          (sleep idle-sleep)))))
           :name "pai-v3 scheduler"))
    (when (fboundp 'start-watchdog) (funcall 'start-watchdog agent port))
    (setf *agent* agent)))

(defun stop-agent (agent)
  (setf (agent-running agent) nil)
  (hunchentoot:stop (agent-acceptor agent))
  (bt:join-thread (agent-scheduler agent))
  (setf *reply-delta-hook* nil))

(defun agent-port (agent) (hunchentoot:acceptor-port (agent-acceptor agent)))
