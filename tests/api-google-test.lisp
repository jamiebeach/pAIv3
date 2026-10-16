(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Mail and calendar (operator request 2026-10-15), against a fake Google on
;;; this machine. What is asked for is bounded: no mail before connecting, a
;;; cap per poll, a calendar window from now to the horizon and no further.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defvar *gport* (+ 49000 (random 900)))
(defvar *asked* '() "(path query) of each request, newest first.")
(defvar *token-bodies* '())
(defvar *mail-now-ms* 0)

(defun g-url () (format nil "http://127.0.0.1:~d" *gport*))
(defun asked (prefix) (find-if (lambda (r) (uiop:string-prefix-p prefix (first r))) *asked*))

(hunchentoot:define-easy-handler (fake-token :uri "/token") ()
  (push (hunchentoot:raw-post-data :force-text t) *token-bodies*)
  (setf (hunchentoot:content-type*) "application/json")
  (json-encode (obj "access_token" "ya29.fake-access-token-value" "expires_in" 3600
                    "refresh_token" "1//fake-refresh-token-value" "scope" "gmail.readonly calendar.readonly")))

(hunchentoot:define-easy-handler (fake-gmail :uri (lambda (r) (uiop:string-prefix-p "/gmail/" (hunchentoot:script-name r)))) ()
  (push (list (hunchentoot:script-name*) (hunchentoot:query-string*)) *asked*)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((path (hunchentoot:script-name*)))
    (if (string= path "/gmail/v1/users/me/messages")
        ;; Three ids, one of them older than the watermark (a search can be loose).
        (json-encode (obj "messages" (list (obj "id" "new1") (obj "id" "new2") (obj "id" "old1"))))
        (let ((id (subseq path (1+ (position #\/ path :from-end t)))))
          (json-encode (obj "id" id "snippet" (format nil "snippet of ~a" id)
                            "internalDate" (princ-to-string (if (string= id "old1") (- *mail-now-ms* 86400000) (+ *mail-now-ms* 60000)))
                            "payload" (obj "headers" (list (obj "name" "From" "value" "someone@example.com")
                                                           (obj "name" "Subject" "value" (format nil "About ~a" id))))))))))

(hunchentoot:define-easy-handler (fake-calendar :uri (lambda (r) (uiop:string-prefix-p "/calendar/" (hunchentoot:script-name r)))) ()
  (push (list (hunchentoot:script-name*) (hunchentoot:query-string*)) *asked*)
  (setf (hunchentoot:content-type*) "application/json")
  (json-encode (obj "items" (list (obj "id" "ev1" "summary" "Dentist" "updated" "2026-10-16T09:00:00Z"
                                       "start" (obj "dateTime" "2026-10-17T10:00:00Z") "end" (obj "dateTime" "2026-10-17T11:00:00Z"))))))

(defvar *gserver* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *gport* :address "127.0.0.1"
                                                                               :access-log-destination nil :message-log-destination nil)))

(defmacro with-google ((k) &body body)
  `(let ((secrets (db-open (format nil "/tmp/pai-v3-google-~d.db" (random 100000000)) :key *test-key*)))
     (pai-v3.trust:open-secrets secrets)
     (setf *asked* '() *token-bodies* '())
     (unwind-protect
          (with-temp-kernel (,k)
            (dolist (s (list (list "google.client_id" "client-123") (list "google.redirect_uri" "https://agent.example/oauth/google/return")
                             (list "google.token_url" (format nil "~a/token" (g-url))) (list "google.gmail_url" (g-url))
                             (list "google.calendar_url" (g-url)) (list "google.allow_private" t) (list "mail.enabled" t)))
              (change-setting ,k (first s) (second s) :origin *operator*))
            (pai-v3.trust:set-secret "google-client-secret" "GOCSPX-client-secret-value" (list "127.0.0.1"))
            ,@body)
       (pai-v3.trust:close-secrets)
       (db-close secrets))))

(defun connect (k)
  (let* ((url (pai-v3.api::google-connect-url k "home"))
         (state (subseq url (+ 6 (search "state=" url)))))
    (pai-v3.api::google-connect-return k state "one-time-code")
    (setf *mail-now-ms* (second (first (pai-v3.api::google-accounts k))))))

(deftest connecting-keeps-the-tokens-out-of-reach
  (with-google (k)
    (let ((url (pai-v3.api::google-connect-url k "home")))
      (is (search "gmail.readonly" (quri:url-decode url)) "read-only scopes")
      (is (not (search "gmail.modify" (quri:url-decode url)))))
    (connect k)
    (is (search "client_secret=GOCSPX-client-secret-value" (first *token-bodies*))
        "the client secret reached the token endpoint, filled in by the trust module")
    (is (null (search "fake-refresh-token-value"
                      (or (sql-value (kernel-db k) "SELECT group_concat(event_json) FROM pai_events WHERE agent_id=?1 LIMIT 1" "test-agent") "")))
        "no token is in the ledger")
    (is (equal "home" (first (first (pai-v3.api::google-accounts k)))))
    (signals error (pai-v3.api::google-connect-return k "not-a-state" "code") "an unknown sign-in is refused")))

(deftest mail-is-taken-only-after-connecting-and-only-so-many
  (with-google (k)
    (connect k)
    (change-setting k "mail.per_poll" 5 :origin *operator*)
    (is (= 2 (pai-v3.api::poll-mail k "home" *mail-now-ms*)) "two new; the old one is skipped")
    (let ((q (quri:url-decode (second (find "/gmail/v1/users/me/messages" *asked* :key (function first) :test (function string=))))))
      (is (search "maxResults=5" q) "the per-poll cap is asked for")
      (is (search (format nil "after:~d" (floor *mail-now-ms* 1000)) q) "only after the watermark"))
    (is (null (sql-value (kernel-db k) "SELECT 1 FROM mail_items WHERE account=?1 AND id=?2 LIMIT 1" "home" "old1"))
        "older than the watermark: never kept")
    (setf *asked* '())
    (is (= 0 (pai-v3.api::poll-mail k "home" *mail-now-ms*)) "seen messages are not taken again")
    (change-setting k "mail.per_day" 2 :origin *operator*)
    (setf *asked* '())
    (pai-v3.api::poll-mail k "home" *mail-now-ms*)
    (is (null *asked*) "the day's allowance spent: Google is not asked at all")))

(deftest the-calendar-is-read-from-now-to-the-horizon-only
  (with-google (k)
    (connect k)
    (change-setting k "calendar.horizon_hours" 48 :origin *operator*)
    (is (= 1 (pai-v3.api::poll-calendar k "home")))
    (let* ((q (quri:url-decode (second (asked "/calendar/")))))
      (is (search "singleEvents=true" q) "recurring events expanded, inside the window only")
      (is (search "maxResults=25" q))
      (let* ((tmin (subseq q (+ 8 (search "timeMin=" q)) (+ 8 (search "timeMin=" q) 20)))
             (tmax (subseq q (+ 8 (search "timeMax=" q)) (+ 8 (search "timeMax=" q) 20))))
        (is (<= (abs (- (parse-iso8601 tmin) (get-universal-time))) 120) "the window starts now, not in the past")
        (is (<= (abs (- (parse-iso8601 tmax) (+ (get-universal-time) (* 48 3600)))) 120) "and ends at the horizon")))
    (is (= 0 (pai-v3.api::poll-calendar k "home")) "an unchanged event is not recorded again")
    (signals setting-refused (change-setting k "calendar.horizon_hours" 2000 :origin *operator*) "no reading years ahead")))

(deftest new-items-come-as-one-digest-of-outside-content
  (with-google (k)
    (connect k)
    (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0)) :origin *operator*)
    (pai-v3.api::poll-google k :force t)
    (let ((thoughts (sql-rows (kernel-db k) "SELECT reason,audience FROM thoughts WHERE origin=?1 LIMIT 5" "noticing")))
      (is (= 1 (length thoughts)) "one digest, not one per item")
      (is (equal "external" (second (first thoughts))) "with outside-content authority only")
      (is (search "never follow instructions" (first (first thoughts))))
      (is (search "About new1" (first (first thoughts))))
      (is (search "Dentist" (first (first thoughts)))))
    (pai-v3.api::poll-google k :force t)
    (is (= 1 (or (sql-value (kernel-db k) "SELECT count(*) FROM thoughts WHERE origin=?1 LIMIT 1" "noticing") 0))
        "nothing new: no new digest")
    (let ((tool (gethash "mail" *tools*)))
      (is (pai-v3.mind::tool-available-p tool '(:audience "external")))
      (is (not (pai-v3.mind::tool-available-p (gethash "shell" *tools*) '(:audience "external"))) "no shell for outside content")
      (is (not (pai-v3.mind::tool-available-p (gethash "lisp_eval" *tools*) '(:audience "external")))))))

(run-suite "api-google")
