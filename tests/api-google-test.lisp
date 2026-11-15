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
    (cond
      ((string= path "/gmail/v1/users/me/messages")
       ;; Three ids, one of them older than the watermark (a search can be loose).
       (json-encode (obj "messages" (list (obj "id" "new1") (obj "id" "new2") (obj "id" "old1")))))
      ((search "/attachments/" path)
       (json-encode (obj "size" 70 "data" "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg")))
      (t
       (let* ((id (subseq path (1+ (position #\/ path :from-end t))))
              (headers (list (obj "name" "From" "value" "someone@example.com")
                             (obj "name" "Subject" "value" (format nil "About ~a" id)))))
         (json-encode (obj "id" id "snippet" (if (string= id "att1") "" (format nil "snippet of ~a" id))
                           "internalDate" (princ-to-string (if (string= id "old1") (- *mail-now-ms* 86400000) (+ *mail-now-ms* 60000)))
                           "payload" (cond ((string= id "att1")
                                            ;; Only attachments: a picture and a document.
                                            (obj "mimeType" "multipart/mixed" "headers" headers
                                                 "parts" (list (obj "mimeType" "image/png" "filename" "headshot.png"
                                                                    "body" (obj "attachmentId" "A1" "size" 70))
                                                               (obj "mimeType" "application/pdf" "filename" "cv.pdf"
                                                                    "body" (obj "attachmentId" "A2" "size" 90000)))))
                                           ((string= id "html1")
                                            (obj "mimeType" "text/html" "headers" headers
                                                 "body" (obj "data" (base64url-encode (babel:string-to-octets "<p>Hello <b>there</b></p>" :encoding :utf-8)))))
                                           (t (obj "headers" headers))))))))))

(defvar *calendar-cancelled* nil "When true, the fake calendar reports ev1 cancelled.")

(hunchentoot:define-easy-handler (fake-calendar :uri (lambda (r) (uiop:string-prefix-p "/calendar/" (hunchentoot:script-name r)))) ()
  (push (list (hunchentoot:script-name*) (hunchentoot:query-string*)) *asked*)
  (setf (hunchentoot:content-type*) "application/json")
  (json-encode (obj "items" (list (obj "id" "ev1" "summary" "Dentist" "updated" "2026-10-16T09:00:00Z"
                                       "status" (if *calendar-cancelled* "cancelled" "confirmed")
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
    (setf *calendar-cancelled* t)
    (unwind-protect
     (progn
      (is (= 1 (pai-v3.api::poll-calendar k "home")))
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM calendar_events WHERE account=?1 AND id=?2 LIMIT 1" "home" "ev1"))
          "a cancelled event leaves the calendar")
      (is (= 0 (pai-v3.api::poll-calendar k "home")) "and is not recorded again"))
     (setf *calendar-cancelled* nil))
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
      (is (not (pai-v3.mind::tool-available-p (gethash "shell" *tools*) '(:audience "external" :communicating t))) "no shell for outside content")
      (is (not (pai-v3.mind::tool-available-p (gethash "lisp_eval" *tools*) '(:audience "external" :communicating t)))))))

(deftest a-cancelled-event-is-brought-up-not-just-dropped
  ;; Bead v3-luk: a round-robin called off by the studio just vanished.
  (with-google (k)
    (connect k)
    (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0)) :origin *operator*)
    (pai-v3.api::poll-google k :force t)
    (setf *calendar-cancelled* t)
    (unwind-protect
         (progn
           (pai-v3.api::poll-google k :force t)
           (is (equal '("Dentist" "2026-10-17T10:00:00Z")
                      (sql-row (kernel-db k) "SELECT summary,start_at FROM calendar_cancellations WHERE account=?1 AND id=?2 LIMIT 1" "home" "ev1"))
               "the cancellation is kept with its title and time")
           ;; The fake Dentist is in the past by now; an upcoming one, called off:
           (append! k "calendar.cancelled" (obj "account" "home" "event_id" "ev9" "summary" "SYNTHETIC round-robin"
                                                "start" (iso8601 (+ (now) (* 20 3600)))))
           (pai-v3.api::poll-google k :force t)
           (let ((latest (sql-value (kernel-db k) "SELECT reason FROM thoughts WHERE origin=?1 ORDER BY id DESC LIMIT 1" "noticing")))
             (is (search "CANCELLED: SYNTHETIC round-robin" latest) "an upcoming one is brought to a thought")
             (is (null (search "CANCELLED: Dentist" latest)) "a past one is not")))
      (setf *calendar-cancelled* nil))))

;;; More than one account (operator request 2026-10-18)

(deftest every-connected-account-is-read-and-shown
  (with-google (k)
    (connect k)
    (let* ((url (pai-v3.api::google-connect-url k "work"))
           (state (subseq url (+ 6 (search "state=" url)))))
      (pai-v3.api::google-connect-return k state "one-time-code"))
    (is (equal '("home" "work") (mapcar #'first (pai-v3.api::google-accounts k))))
    (append! k "mail.seen" (obj "account" "home" "message_id" "h1" "from" "a@example.com" "subject" "Home news" "snippet" "" "date_ms" 1000))
    (append! k "mail.seen" (obj "account" "work" "message_id" "w1" "from" "b@example.com" "subject" "Work news" "snippet" "" "date_ms" 2000))
    (dolist (a '("home" "work"))
      (append! k "calendar.seen" (obj "account" a "event_id" (format nil "~a-ev" a) "summary" (format nil "~a meeting" a)
                                      "start" (iso8601 (+ (now) (if (equal a "home") 7200 3600))) "end" (iso8601 (+ (now) 9000))
                                      "updated" "x")))
    (let ((run (lambda (name args) (funcall (pai-v3.mind::tool-run (gethash name *tools*)) k 1 args))))
      (let ((recent (funcall run "mail" (obj "action" "recent"))))
        (is (search "Home news" recent))
        (is (< (search "Work news" recent) (search "Home news" recent)) "newest first, across accounts")
        (is (search "(work)" recent) "each line says which account"))
      ;; Bead v3-rh3: index pages back past the newest, across accounts.
      (dotimes (i 25)
        (append! k "mail.seen" (obj "account" (if (evenp i) "home" "work") "message_id" (format nil "p~d" i)
                                    "from" "c@example.com" "subject" (format nil "Paged ~d" i) "snippet" "" "date_ms" (+ 5000 i))))
      (let ((page (funcall run "mail" (obj "action" "recent" "limit" 5 "index" 20))))
        (is (search "Paged 4" page) "the 21st newest is first on the page")
        (is (search "Paged 0" page))
        (is (not (search "Paged 5:" page)) "nothing newer than the page")
        (is (not (search "Home news" page)) "nothing older than the page"))
      (is (search "No more mail" (funcall run "mail" (obj "action" "recent" "index" 400)))
          "past the end says so")
      (let ((cal (funcall run "calendar" (obj))))
        (is (< (search "work meeting" cal) (search "home meeting" cal)) "both calendars, in time order")
        (is (search "In the next 48 hours" cal) "the window is said (bead v3-wh8)"))
      (is (search "No connected account is called sam@example.com. The connected accounts are home, work"
                  (funcall run "mail" (obj "action" "search" "query" "x" "account" "sam@example.com")))
          "a wrong account name says which ones there are, not 'not connected'"))
    (is (search "work meeting" (first (first (funcall (gethash :present pai-v3.mind::*slot-sources*) k '(:audience "operator") 300))))
        "the day ahead covers every account")
    (is (equal "work" (sql-value (kernel-db k) "SELECT account FROM mail_items WHERE id=?1 LIMIT 1" "w1"))
        "a message read without its account is looked up")))

;;; What is attached (a message of only attachments read as empty, and looked like a fault)

(deftest the-whole-mailbox-can-be-searched
  ;; Bead v3-6jl: "is there an email about the check-in?" answered by paging and
  ;; eyeballing, and a message that was there was reported as not there.
  (with-google (k)
    (connect k)
    (setf *asked* '())
    (let* ((thought (request-thought k "responding" "find the check-in email" :audience "operator"))
           (said (funcall (pai-v3.mind::tool-run (gethash "mail" *tools*)) k thought (obj "action" "search" "query" "algebra check-in"))))
      (is (search "Searched the whole mailbox of home (3) for \"algebra check-in\"" said) "it says what it searched")
      (is (search "About new1" said) "and lists what matches, with sender and subject")
      (is (search "id new1, account home" said) "with the id to read it by")
      (is (equal "algebra check-in"
                 (quri:url-decode (subseq (second (find "/gmail/v1/users/me/messages" *asked* :key #'first :test #'string=))
                                          2 (search "&" (second (find "/gmail/v1/users/me/messages" *asked* :key #'first :test #'string=))))))
          "the query goes to Gmail's own search")
      (is (search "Give query" (funcall (pai-v3.mind::tool-run (gethash "mail" *tools*)) k thought (obj "action" "search")))
          "a search needs words")
      (is (search "No mail action \"find\"" (funcall (pai-v3.mind::tool-run (gethash "mail" *tools*)) k thought (obj "action" "find" "query" "x")))
          "an unknown action is refused, never answered as recent mail"))))

(deftest a-message-of-only-attachments-says-so-and-a-picture-can-be-opened
  (with-google (k)
    (connect k)
    (let* ((thought (request-thought k "responding" "look at my mail" :audience "operator"))
           (run (lambda (args) (funcall (pai-v3.mind::tool-run (gethash "mail" *tools*)) k thought args))))
      (let ((said (funcall run (obj "action" "read" "id" "att1"))))
        (is (search "(no text in the message itself)" said) "it is not a fault: there is no text")
        (is (search "[1] headshot.png (image/png, 70 bytes)" said) "the attachments are listed by number")
        (is (search "[2] cv.pdf (application/pdf, 88 KB)" said)))
      (is (search "Hello there" (funcall run (obj "action" "read" "id" "html1"))) "a message of only HTML reads as text")
      (let ((said (funcall run (obj "action" "attachment" "id" "att1" "index" 1))))
        (is (search "Kept headshot.png as image" said))
        (let* ((start (+ 10 (search " as image " said)))
               (image (subseq said start (+ start 32))))
          (is (every (lambda (c) (digit-char-p c 16)) image) "named by its content")
          (is (pai-v3.mind::image-row k image) "kept in the image store")
          (is (sql-value (kernel-db k) "SELECT 1 FROM seen_images WHERE image_id=?1 AND audience=?2 LIMIT 1" image "operator")
              "and among those seen, so look_at can show it")))
      (is (search "pictures only" (funcall run (obj "action" "attachment" "id" "att1" "index" 2))) "a document is not opened")
      (is (search "is not one of them" (funcall run (obj "action" "attachment" "id" "att1" "index" 9))))
      (is (search "no attachments" (funcall run (obj "action" "attachment" "id" "new1" "index" 1))) "a message with none"))))

(run-suite "api-google")
