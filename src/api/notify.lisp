(in-package #:pai-v3.api)

;;; Notifying the operator (bead v3-clp.10), ported from v2's one delivery
;;; boundary. The agent judges importance (1 low, 2 normal, 3 high) and writes
;;; the summary; this boundary never judges. It enforces only what the operator
;;; set, in a fixed order: master switch, topic, expiry, dedupe, quiet hours,
;;; hourly and daily caps, then delivery.
;;;
;;; - Caps are tiered so the lowest importance is pruned first: importance 3 may
;;;   use the whole allowance, 2 and 1 a share of it. Over the cap an item is
;;;   dropped, never queued. The runtime's own alerts have a separate allowance.
;;; - In quiet hours a reminder that can wait is held and released after;
;;;   anything else is suppressed, unless importance 3 may break through.
;;; - Web Push carries no payload: it wakes the phone's service worker, which
;;;   fetches the summary from this agent over the authenticated app. The push
;;;   services see only that a push happened. The VAPID signing key is in the
;;;   secret store and never leaves the trust module.
;;; - The policy is a setting only the operator changes, so every change is in
;;;   the ledger. The agent's message is always posted in the terminal; the
;;;   push only points to it.

(defparameter +notify-topics+
  '(("system-alert" . 3) ("reminder" . 2) ("calendar" . 2) ("important-mail" . 2) ("update" . 2))
  "Topics and their default importance.")
(defparameter +push-hosts+
  '("fcm.googleapis.com" "push.services.mozilla.com" "push.apple.com" "notify.windows.com" "push.api.windows.com")
  "Push services a subscription may name; the agent never posts to another address.")
(defparameter +vapid-secret+ "web-push-vapid")
(defparameter *notify-summary-max* 140)
(defparameter *notify-messages-per-day* 20 "Most agent messages posted to the terminal in a day.")

(defun %default-policy ()
  (obj "enabled" t
       "topics" (let ((o (obj))) (dolist (e +notify-topics+ o) (setf (gethash (car e) o) (obj "enabled" t))))
       "channels" (obj "web-terminal" (obj "enabled" t) "web-push" (obj "enabled" t) "telegram" (obj "enabled" :false))
       "quiet_hours" (obj "enabled" t "start_minute" 1380 "end_minute" 360)
       "quiet_breakthrough" :false
       "limits" (obj "per_hour" 4 "per_day" 12 "alert_per_hour" 3 "alert_per_day" 6
                     "tier_low_percent" 50 "tier_normal_percent" 75)
       "web_push_subject" ""
       "dedupe_window_seconds" 3600))

(defun %merge (base change)
  "BASE with CHANGE laid over it, object by object."
  (let ((out (obj)))
    (maphash (lambda (k v) (setf (gethash k out) v)) base)
    (when (hash-table-p change)
      (maphash (lambda (k v)
                 (setf (gethash k out)
                       (if (and (hash-table-p v) (hash-table-p (gethash k out))) (%merge (gethash k out) v) v)))
               change))
    out))

(defun %policy-problem (policy)
  "Why POLICY is not valid, or NIL."
  (let ((limits (obj-get policy "limits")) (quiet (obj-get policy "quiet_hours"))
        (subject (obj-get policy "web_push_subject" "")))
    (cond ((not (hash-table-p limits)) "limits must be an object")
          ((notevery (lambda (k) (let ((v (obj-get limits k))) (and (integerp v) (<= 0 v 1000))))
                     '("per_hour" "per_day" "alert_per_hour" "alert_per_day"))
           "each limit is a whole number from 0 to 1000")
          ((notevery (lambda (k) (let ((v (obj-get limits k))) (and (integerp v) (<= 0 v 100))))
                     '("tier_low_percent" "tier_normal_percent"))
           "tier percentages are from 0 to 100")
          ((not (and (hash-table-p quiet)
                     (every (lambda (k) (let ((v (obj-get quiet k))) (and (integerp v) (<= 0 v 1439))))
                            '("start_minute" "end_minute"))))
           "quiet hours need start_minute and end_minute from 0 to 1439")
          ((not (stringp subject)) "web_push_subject must be text")
          ((and (plusp (length subject))
                (not (or (uiop:string-prefix-p "mailto:" subject) (uiop:string-prefix-p "https://" subject))))
           "web_push_subject must start with mailto: or https://"))))

(define-setting "notifications.policy"
  :type :object :default (obj) :group "Notifications"
  :validate (lambda (v) (%policy-problem (%merge (%default-policy) v)))
  :description "How and when I may notify the operator: switches, topics, quiet hours, caps and the push contact. Only the operator changes it.")

(defun notify-policy (kernel)
  (%merge (%default-policy) (setting kernel "notifications.policy")))

;;; The record of decisions

(define-event-type "notification.decided"
  :schema (("topic" :string) ("importance" :integer) ("decision" :string) ("class" :string)
           ("origin" :string) ("dedupe_sha256" :string :optional) ("channels" :array :optional)
           ("message_event_id" :integer :optional) ("held" :object :optional)))
(define-event-type "notification.released" :schema (("decision_event_id" :integer)))
(define-event-type "notification.subscribed" :schema (("subscription_id" :string) ("endpoint" :string)))
(define-event-type "notification.unsubscribed" :schema (("subscription_id" :string) ("reason" :string)))

(define-projection "notifications"
  :kind :inline
  :consumes ("notification.decided" "notification.released" "notification.subscribed" "notification.unsubscribed")
  :tables ("CREATE TABLE IF NOT EXISTS notifications (event_id INTEGER PRIMARY KEY, topic TEXT NOT NULL, importance INTEGER NOT NULL, decision TEXT NOT NULL, class TEXT NOT NULL, origin TEXT NOT NULL DEFAULT 'agent', dedupe TEXT, at_unix INTEGER NOT NULL, held TEXT, released INTEGER NOT NULL DEFAULT 0)"
           "CREATE INDEX IF NOT EXISTS notifications_class_idx ON notifications(decision,class,at_unix)"
           "CREATE INDEX IF NOT EXISTS notifications_dedupe_idx ON notifications(dedupe,at_unix)"
           "CREATE INDEX IF NOT EXISTS notifications_origin_idx ON notifications(origin,at_unix)"
           "CREATE INDEX IF NOT EXISTS notifications_held_idx ON notifications(decision,released,event_id)"
           "CREATE TABLE IF NOT EXISTS push_subscriptions (id TEXT PRIMARY KEY, kind TEXT NOT NULL DEFAULT 'push', endpoint TEXT NOT NULL, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS push_subscriptions_kind_idx ON push_subscriptions(kind,at)")
  :reduce
  (lambda (db e)
    (let ((p (event-payload e)) (type (event-type e)))
      (cond ((string= type "notification.decided")
             (sql-exec db "INSERT INTO notifications(event_id,topic,importance,decision,class,dedupe,at_unix,held,origin) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
                       (event-id e) (obj-get p "topic") (obj-get p "importance") (obj-get p "decision") (obj-get p "class")
                       (%given (obj-get p "dedupe_sha256")) (- (parse-iso8601 (event-at e)) 2208988800)
                       (and (obj-get p "held") (json-encode (obj-get p "held"))) (obj-get p "origin")))
            ((string= type "notification.released")
             (sql-exec db "UPDATE notifications SET released=1 WHERE event_id=?1" (obj-get p "decision_event_id")))
            ((string= type "notification.subscribed")
             (sql-exec db "INSERT OR REPLACE INTO push_subscriptions(id,endpoint,at) VALUES(?1,?2,?3)"
                       (obj-get p "subscription_id") (obj-get p "endpoint") (event-at e)))
            (t (sql-exec db "DELETE FROM push_subscriptions WHERE id=?1" (obj-get p "subscription_id")))))))

(defun %count-sent (kernel class seconds)
  (or (sql-value (kernel-db kernel) "SELECT count(*) FROM notifications WHERE decision=?1 AND class=?2 AND at_unix>?3 LIMIT 1"
                 "sent" class (- (unix-now) seconds))
      0))

(defun %over-cap-p (kernel policy class importance)
  (let* ((limits (obj-get policy "limits"))
         (alert (string= class "alert"))
         (share (cond (alert 100) ((= importance 3) 100) ((= importance 2) (obj-get limits "tier_normal_percent"))
                      (t (obj-get limits "tier_low_percent"))))
         (hour (ceiling (* share (obj-get limits (if alert "alert_per_hour" "per_hour"))) 100))
         (day (ceiling (* share (obj-get limits (if alert "alert_per_day" "per_day"))) 100)))
    (or (>= (%count-sent kernel class 3600) hour) (>= (%count-sent kernel class 86400) day))))

(defun %quiet-now-p (kernel policy)
  (let ((q (obj-get policy "quiet_hours")))
    (and (eq t (obj-get q "enabled"))
         (let* ((zone (or (ignore-errors (local-time:find-timezone-by-location-name (setting kernel "time.zone")))
                          local-time:+utc-zone+))
                (ts (local-time:universal-to-timestamp (now)))
                (minute (+ (* 60 (local-time:timestamp-hour ts :timezone zone)) (local-time:timestamp-minute ts :timezone zone)))
                (start (obj-get q "start_minute")) (end (obj-get q "end_minute")))
           (cond ((= start end) nil)
                 ((< start end) (and (<= start minute) (< minute end)))
                 (t (or (<= start minute) (< minute end))))))))

(defun %sanitize (text)
  "Plain text for a lock screen: one line, no links or markup, bounded, redacted."
  (let* ((s (redact (substitute #\Space #\Newline (substitute #\Space #\Return (or text "")))))
         (s (with-output-to-string (out)
              (loop for word in (uiop:split-string s :separator " ")
                    unless (or (zerop (length word)) (search "://" word) (uiop:string-prefix-p "www." word))
                      do (format out "~a " (remove-if (lambda (c) (find c "<>[]`*_#")) word)))))
         (s (string-trim " " s)))
    (if (> (length s) *notify-summary-max*) (concatenate 'string (subseq s 0 (1- *notify-summary-max*)) "…") s)))

;;; Web Push

(defvar *push-pending* (make-hash-table :test 'equal) "agent id -> newest-first (id summary deep-link at)")
(defvar *push-last* (make-hash-table :test 'equal) "agent id -> content-free result of the last push")
(defvar *notify-lock* (bt:make-lock "notify"))

(defun %b64url (octets) (string-right-trim "=" (base64url-encode octets)))

(defun push-public-key (kernel)
  "The applicationServerKey, made on first use."
  (declare (ignore kernel))
  (%b64url (or (secret-es256-public +vapid-secret+) (ensure-es256-secret +vapid-secret+ +push-hosts+))))

(defun %push-host (endpoint)
  "ENDPOINT's host when it is an https push service on port 443, else signals."
  (let* ((uri (quri:uri endpoint)) (host (string-downcase (or (quri:uri-host uri) ""))))
    (unless (and (equal "https" (quri:uri-scheme uri)) (member (quri:uri-port uri) '(nil 443))
                 (null (quri:uri-userinfo uri))
                 (some (lambda (suffix) (or (string= host suffix)
                                            (and (> (length host) (length suffix))
                                                 (string= (concatenate 'string "." suffix) host
                                                          :start2 (- (length host) (length suffix) 1)))))
                       +push-hosts+))
      (error "That is not a known push service address."))
    host))

(defun push-subscriptions (kernel)
  (sql-rows (kernel-db kernel) "SELECT id,endpoint,at FROM push_subscriptions WHERE kind=?1 ORDER BY at LIMIT 8" "push"))

(defun push-subscribe (kernel endpoint)
  (unless (and (stringp endpoint) (<= (length endpoint) 1024)) (error "Give the subscription endpoint."))
  (let ((host (%push-host endpoint))
        (existing (find endpoint (push-subscriptions kernel) :key #'second :test #'equal)))
    (if existing
        (obj "id" (first existing) "host" host)
        (let ((id (subseq (pai-v3.kernel::sha256-hex endpoint) 0 16)))
          (when (>= (length (push-subscriptions kernel)) 8) (error "Eight devices are subscribed already; remove one first."))
          (push-public-key kernel)
          (append! kernel "notification.subscribed" (obj "subscription_id" id "endpoint" endpoint))
          (obj "id" id "host" host)))))

(defun push-unsubscribe (kernel id &optional (reason "removed by the operator"))
  (when (find id (push-subscriptions kernel) :key #'first :test #'equal)
    (append! kernel "notification.unsubscribed" (obj "subscription_id" id "reason" reason))))

(defun push-pending (kernel &optional after)
  "Summaries waiting for a device, oldest first, newer than AFTER."
  (bt:with-lock-held (*notify-lock*)
    (let* ((now (unix-now))
           (items (remove-if (lambda (i) (> (- now (fourth i)) 86400)) (gethash (kernel-agent-id kernel) *push-pending*)))
           (newer (if after (loop for i in items until (equal (first i) after) collect i) items)))
      (setf (gethash (kernel-agent-id kernel) *push-pending*) items)
      (mapcar (lambda (i) (obj "id" (first i) "summary" (second i) "deep_link" (third i))) (reverse newer)))))

(defun %vapid-jwt (endpoint subject)
  (let* ((uri (quri:uri endpoint))
         (header (%b64url (babel:string-to-octets (json-encode (obj "typ" "JWT" "alg" "ES256")) :encoding :utf-8)))
         (claims (%b64url (babel:string-to-octets
                           (json-encode (obj "aud" (format nil "https://~a" (quri:uri-host uri))
                                             "exp" (+ (unix-now) 43200) "sub" subject))
                           :encoding :utf-8)))
         (input (format nil "~a.~a" header claims)))
    (format nil "~a.~a" input (%b64url (secret-es256-sign +vapid-secret+ input)))))

(defun %push-all (kernel policy id summary deep-link importance)
  "Remember the summary for the device, then wake every subscription. :sent
when at least one push service accepted it."
  (let ((subject (obj-get policy "web_push_subject" "")) (subs (push-subscriptions kernel)))
    (flet ((result (reason &optional (accepted 0) classes)
             (setf (gethash (kernel-agent-id kernel) *push-last*)
                   (obj "at" (unix-now) "reason" reason "accepted" accepted "status_classes" (or classes '())))))
      (cond ((zerop (length subject)) (result "no-contact") :failed)
            ((null subs) (result "no-subscriptions") :failed)
            (t
             (bt:with-lock-held (*notify-lock*)
               (push (list id summary deep-link (unix-now)) (gethash (kernel-agent-id kernel) *push-pending*))
               (let ((l (gethash (kernel-agent-id kernel) *push-pending*)))
                 (when (> (length l) 8) (setf (gethash (kernel-agent-id kernel) *push-pending*) (subseq l 0 8)))))
             (let ((accepted 0) (classes '()))
               (dolist (s subs)
                 (let ((status (handler-case
                                   (nth-value 1 (http-request :post (second s)
                                                              :headers (list (cons "Authorization" (format nil "vapid t=~a, k=~a" (%vapid-jwt (second s) subject) (push-public-key kernel)))
                                                                             (cons "TTL" "3600")
                                                                             (cons "Urgency" (case importance (3 "high") (2 "normal") (t "low"))))
                                                              :body "" :connect-timeout 5 :read-timeout 10 :max-redirects 0))
                                 (error () nil))))
                   (pushnew (cond ((null status) "error") ((<= 200 status 299) "2xx") ((member status '(404 410)) "gone")
                                  ((<= 400 status 499) "4xx") (t "5xx"))
                            classes :test #'equal)
                   (cond ((and status (<= 200 status 299)) (incf accepted))
                         ((member status '(404 410)) (push-unsubscribe kernel (first s) "the push service says it is gone")))))
               (result (if (plusp accepted) "accepted" "rejected") accepted (reverse classes))
               (if (plusp accepted) :sent :failed)))))))

;;; The boundary

(defun notify (kernel &key topic importance summary (deep-link "/terminal") (origin "agent") dedupe-key
                           message-event-id release-of)
  "Decide, in the fixed order, whether to tell the operator SUMMARY now, and
deliver it if so. Returns the decision."
  (let* ((policy (notify-policy kernel))
         (importance (or importance (cdr (assoc topic +notify-topics+ :test #'string=)) 2))
         (summary (%sanitize summary))
         (test (string= origin "test"))
         (quiet (%quiet-now-p kernel policy))
         (breakthrough (and quiet (eq t (obj-get policy "quiet_breakthrough")) (= importance 3)))
         (class (cond (test "test") ((and (string= origin "runtime") (or (string= topic "system-alert") breakthrough)) "alert")
                      (t "normal")))
         (dedupe (and dedupe-key (pai-v3.kernel::sha256-hex dedupe-key))))
    (labels ((decide (decision &key channels held)
               (append! kernel "notification.decided"
                        (obj "topic" topic "importance" importance "decision" decision "class" class "origin" origin
                             "dedupe_sha256" dedupe "channels" channels "message_event_id" message-event-id "held" held))
               (when release-of (append! kernel "notification.released" (obj "decision_event_id" release-of)))
               decision))
      (cond
        ((not (eq t (obj-get policy "enabled"))) (decide "disabled"))
        ((not (assoc topic +notify-topics+ :test #'string=)) (decide "unknown-topic"))
        ((and (not test) (not (eq t (obj-get (obj-get (obj-get policy "topics") topic) "enabled")))) (decide "suppressed-topic"))
        ((and dedupe (sql-value (kernel-db kernel) "SELECT 1 FROM notifications WHERE dedupe=?1 AND at_unix>?2 AND decision IN (?3,?4) LIMIT 1"
                                dedupe (- (unix-now) (obj-get policy "dedupe_window_seconds" 3600)) "sent" "held"))
         (decide "deduped"))
        ((and quiet (not breakthrough) (not test))
         (if (and (string= topic "reminder") (not release-of)
                  (< (or (sql-value (kernel-db kernel) "SELECT count(*) FROM notifications WHERE decision=?1 AND released=?2 LIMIT 1" "held" 0) 0) 8))
             (decide "held" :held (obj "summary" summary "deep_link" deep-link "origin" origin "until" (+ (unix-now) 43200)))
             (decide "suppressed-quiet")))
        ((and (not test) (%over-cap-p kernel policy class importance)) (decide "pruned"))
        ((not (eq t (obj-get (obj-get (obj-get policy "channels") "web-push") "enabled"))) (decide "no-channel"))
        (t (let ((id (format nil "n~d" (+ (ledger-head kernel) 1))))
             (if (eq :sent (%push-all kernel policy id summary deep-link importance))
                 (decide "sent" :channels (list "web-push"))
                 (decide "failed" :channels (list "web-push")))))))))

(defun release-held-notifications (kernel)
  "When quiet hours end, send what was held, unless it has gone stale."
  (unless (%quiet-now-p kernel (notify-policy kernel))
    (dolist (row (sql-rows (kernel-db kernel) "SELECT event_id,topic,importance,held FROM notifications WHERE decision=?1 AND released=?2 ORDER BY event_id LIMIT 8"
                           "held" 0))
      (let ((held (json-parse (fourth row))))
        (if (> (unix-now) (obj-get held "until" 0))
            (append! kernel "notification.released" (obj "decision_event_id" (first row)))
            (notify kernel :topic (second row) :importance (third row) :summary (obj-get held "summary")
                           :deep-link (obj-get held "deep_link") :origin (obj-get held "origin") :release-of (first row)))))))

(defun notify-reminders (kernel)
  "A reminder that went off in the terminal also reaches the operator's phone."
  (let* ((mark (or (sql-value (kernel-db kernel) "SELECT body FROM fleet_ops WHERE kind=?1 AND operation_id=?2 ORDER BY event_id DESC LIMIT 1"
                              "notify-mark" "reminders")
                   "{}"))
         (seen (or (obj-get (ignore-errors (json-parse mark)) "through") 0))
         (rows (sql-rows (kernel-db kernel) "SELECT event_id,rendered FROM working_dialogue WHERE audience=?1 AND channel=?2 AND event_id>?3 ORDER BY event_id LIMIT 20"
                         "operator" "reminder" seen)))
    (when rows
      (dolist (r rows)
        (let ((text (second r)))
          (notify kernel :topic "reminder" :origin "runtime" :message-event-id (first r)
                         :summary (format nil "Reminder: ~a" (subseq text (min (length text) (1+ (or (search ": " text) -2)))))
                         :deep-link (format nil "/terminal#m=~d" (first r))
                         :dedupe-key (format nil "reminder-~d" (first r)))))
      (append! kernel "fleet.op" (obj "kind" "notify-mark" "operation_id" "reminders" "peer_id" "self"
                                      "body" (obj "through" (first (car (last rows)))))))))

(defun notify-periodic (kernel)
  (ignore-errors (notify-reminders kernel))
  (ignore-errors (release-held-notifications kernel)))

(pushnew 'notify-periodic *periodic-hooks*)

;;; Shadow mode (docs/charges-and-attention.md §5): outreach the agent starts
;;; on its own is decided and written, then kept for the operator to review
;;; instead of being sent. The operator rates each (useful, noise, wrong) on
;;; the notifications page; that is how the agent's judgment is checked
;;; before it may interrupt. Answers within a conversation the operator
;;; started are never shadowed.

(define-setting "outreach.shadow"
  :type :boolean :default :false :group "Notifications"
  :description "Keep messages I start on my own for the operator's review instead of sending them.")

(define-event-type "outreach.withheld"
  :schema (("thought_id" :integer) ("origin" :string) ("topic" :string) ("importance" :integer)
           ("summary" :string) ("message" :string :optional)))
(define-event-type "outreach.rated" :schema (("withheld_event_id" :integer) ("rating" :string)))

(define-projection "outreach"
  :kind :inline
  :consumes ("outreach.withheld" "outreach.rated")
  :tables ("CREATE TABLE IF NOT EXISTS outreach_shadow (event_id INTEGER PRIMARY KEY, kind TEXT NOT NULL DEFAULT 'withheld', thought_id INTEGER NOT NULL, origin TEXT NOT NULL, topic TEXT NOT NULL, importance INTEGER NOT NULL, summary TEXT NOT NULL, message TEXT, at TEXT NOT NULL, rating TEXT)"
           "CREATE INDEX IF NOT EXISTS outreach_shadow_kind_idx ON outreach_shadow(kind,event_id)")
  :reduce
  (lambda (db e)
    (let ((p (event-payload e)))
      (if (string= (event-type e) "outreach.withheld")
          (sql-exec db "INSERT INTO outreach_shadow(event_id,thought_id,origin,topic,importance,summary,message,at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                    (event-id e) (obj-get p "thought_id") (obj-get p "origin") (obj-get p "topic") (obj-get p "importance")
                    (obj-get p "summary") (%given (obj-get p "message")) (event-at e))
          (sql-exec db "UPDATE outreach_shadow SET rating=?2 WHERE event_id=?1"
                    (obj-get p "withheld_event_id") (obj-get p "rating"))))))

(defparameter +outreach-ratings+ '("useful" "noise" "wrong"))

(defun %root-origin (kernel thought-id)
  "The origin of the root of THOUGHT-ID: what started this line of thinking."
  (let ((root (sql-value (kernel-db kernel) "SELECT root_id FROM thoughts WHERE id=?1 LIMIT 1" thought-id)))
    (and root (sql-value (kernel-db kernel) "SELECT origin FROM thoughts WHERE id=?1 LIMIT 1" root))))

(defun shadow-outreach (kernel limit)
  "The newest withheld messages, as objects for the page."
  (mapcar (lambda (r)
            (destructuring-bind (id origin topic importance summary message at rating) r
              (obj "id" id "origin" origin "topic" topic "importance" importance "summary" summary
                   "message" (or message :null) "at" at "rating" (or rating :null))))
          (sql-rows (kernel-db kernel) "SELECT event_id,origin,topic,importance,summary,message,at,rating FROM outreach_shadow WHERE kind=?1 ORDER BY event_id DESC LIMIT ?2"
                    "withheld" limit)))

(defun rate-outreach (kernel id rating)
  (unless (member rating +outreach-ratings+ :test #'equal) (error "rating is useful, noise or wrong"))
  (unless (sql-value (kernel-db kernel) "SELECT 1 FROM outreach_shadow WHERE event_id=?1 LIMIT 1" id) (error "no such message"))
  (append! kernel "outreach.rated" (obj "withheld_event_id" id "rating" rating)))

;;; The agent's tool

(define-tool "notify_operator"
  :description "Tell the operator something they would want to know now. Your message is posted in the terminal; the notification policy alone decides whether their phone is also woken (caps, quiet hours, topics), and most requests rightly are not. Use it sparingly. summary is one short plain sentence (at most 140 characters, no links); message is the optional longer text for the terminal. topic: reminder for something you set yourself to remember, system-alert only for your own operational problems, update for anything else worth their attention now, calendar or important-mail for those. importance: 1 low, 2 normal, 3 high; your judgment."
  :summary "Notify the operator."
  :parameters (obj "type" "object"
                   "properties" (obj "topic" (obj "type" "string" "enum" (mapcar #'car +notify-topics+))
                                     "importance" (obj "type" "integer")
                                     "summary" (obj "type" "string")
                                     "message" (obj "type" "string"))
                   "required" (list "topic" "importance" "summary"))
  ;; The operator's own thoughts, and digests of their mail and calendar.
  :available '("operator" "external")
  :run (lambda (kernel id args)
         (let ((topic (obj-get args "topic")) (importance (obj-get args "importance"))
               (summary (obj-get args "summary")) (message (%given (obj-get args "message")))
               (origin (%root-origin kernel id)))
           (cond ((not (assoc topic +notify-topics+ :test #'equal)) "Choose a topic from the list.")
                 ((not (typep importance '(integer 1 3))) "importance is 1, 2 or 3.")
                 ((not (and (stringp summary) (plusp (length summary)))) "Give a short summary.")
                 ((>= (or (sql-value (kernel-db kernel) "SELECT count(*) FROM notifications WHERE origin=?1 AND at_unix>?2 LIMIT 1"
                                     "agent" (- (unix-now) 86400))
                          0)
                      *notify-messages-per-day*)
                  "I have sent enough messages to the operator today; it can wait for the next conversation.")
                 ((and (eq t (setting kernel "outreach.shadow")) (not (equal origin "responding")))
                  (append! kernel "outreach.withheld"
                           (obj "thought_id" id "origin" (or origin "unknown") "topic" topic "importance" importance
                                "summary" (subseq summary 0 (min 300 (length summary)))
                                "message" (and message (subseq message 0 (min 1500 (length message))))))
                  "Kept for the operator's review, not sent: I am in shadow mode, so messages I start on my own are shown to the operator later to check my judgment. Nothing more to do.")
                 (t (let* ((event (post-notice kernel :channel "notify" :audience "operator"
                                                      :importance (if (= importance 3) "high" "normal")
                                                      :text (if message (subseq message 0 (min 1500 (length message))) summary)))
                           (decision (notify kernel :topic topic :importance importance :summary summary
                                                    :deep-link (format nil "/terminal#m=~d" (event-id event))
                                                    :message-event-id (event-id event))))
                      (format nil "Posted in the terminal. Phone: ~a."
                              (cond ((string= decision "sent") "woken")
                                    ((string= decision "held") "held until quiet hours end")
                                    (t (format nil "not woken (~a)" decision))))))))))

;;; Routes for the notifications page

(defun %notify-state (kernel)
  (let* ((policy (notify-policy kernel))
         (subs (length (push-subscriptions kernel)))
         (push (obj-get (obj-get policy "channels") "web-push"))
         (subject (obj-get policy "web_push_subject" ""))
         (problems '()))
    (unless (eq t (obj-get policy "enabled")) (push (obj "code" "policy-off" "text" "Notifications are switched off in the policy.") problems))
    (when (and (plusp subs) (not (eq t (obj-get push "enabled"))))
      (push (obj "code" "push-disabled-with-subscriptions" "text" "A device is subscribed to push, but the web-push channel is disabled in the policy.") problems))
    (when (and (eq t (obj-get push "enabled")) (zerop subs))
      (push (obj "code" "push-no-subscriptions" "text" "The web-push channel is enabled but no device is subscribed.") problems))
    (when (and (eq t (obj-get push "enabled")) (zerop (length subject)))
      (push (obj "code" "push-no-contact" "text" "The push contact (web_push_subject) is not set in the policy.") problems))
    (let ((o (obj "policy_enabled" (if (eq t (obj-get policy "enabled")) t :false)
                  "policy_revision" (or (sql-value (kernel-db kernel) "SELECT event_id FROM settings WHERE name=?1 LIMIT 1" "notifications.policy") 0)
                  "quiet_now" (if (%quiet-now-p kernel policy) t :false)
                  "sent_last_hour" (%count-sent kernel "normal" 3600)
                  "sent_last_day" (%count-sent kernel "normal" 86400)
                  "limits" (obj-get policy "limits")
                  "held" (or (sql-value (kernel-db kernel) "SELECT count(*) FROM notifications WHERE decision=?1 AND released=?2 LIMIT 1" "held" 0) 0)
                  "channels" (obj "web-terminal" (obj "enabled" t "registered" t)
                                  "web-push" (obj "enabled" (if (eq t (obj-get push "enabled")) t :false) "registered" t
                                                  "subscriptions" subs
                                                  "vapid_key_present" (if (secret-es256-public +vapid-secret+) t :false)
                                                  "contact_set" (if (plusp (length subject)) t :false)
                                                  "contact_kind" (cond ((zerop (length subject)) "none")
                                                                       ((uiop:string-prefix-p "mailto:" subject) "mailto") (t "https"))
                                                  "last_result" (or (gethash (kernel-agent-id kernel) *push-last*) :null))
                                  "telegram" (obj "enabled" :false "registered" :false "token_stored" :false "paired" :false)))))
      (setf (gethash "problems" o) (nreverse problems))
      o)))

(defun notify-route (agent method path origin)
  "The notifications page's API, for a signed-in operator (ORIGIN). Returns the body."
  (let ((k (agent-kernel agent)))
    (handler-case
        (cond
          ((string= path "/api/notifications/state") (reply 200 (%notify-state k)))
          ((string= path "/api/notifications/shadow")
           (when (eq method :post)
             (let ((b (body-json)))
               (rate-outreach k (obj-get b "id") (obj-get b "rating"))))
           (reply 200 (obj "shadow" (if (eq t (setting k "outreach.shadow")) t :false)
                           "messages" (shadow-outreach k 40))))
          ((string= path "/api/notifications/policy")
           (when (eq method :post)
             (let ((merged (%merge (setting k "notifications.policy") (body-json))))
               (change-setting k "notifications.policy" merged :origin origin :reason "notifications page")))
           (reply 200 (notify-policy k)))
          ((string= path "/api/notifications/push/key")
           (reply 200 (obj "public_key" (push-public-key k)
                           "subject_set" (if (plusp (length (obj-get (notify-policy k) "web_push_subject" ""))) t :false)
                           "subscriptions" (length (push-subscriptions k)))))
          ((string= path "/api/notifications/push/subscriptions")
           (if (eq method :post)
               (let ((b (body-json)))
                 (if (obj-get b "unsubscribe")
                     (progn (push-unsubscribe k (obj-get b "unsubscribe")) (reply 200 (obj "ok" t)))
                     (reply 200 (push-subscribe k (obj-get b "endpoint")))))
               (let ((o (obj)))
                 (setf (gethash "subscriptions" o)
                       (mapcar (lambda (s) (obj "id" (first s) "host" (ignore-errors (%push-host (second s))) "at" (third s)))
                               (push-subscriptions k)))
                 (reply 200 o))))
          ((string= path "/api/notifications/pending")
           (let ((o (obj)))
             (setf (gethash "items" o) (push-pending k (let ((a (hunchentoot:get-parameter "after"))) (and (plusp (length a)) a))))
             (reply 200 o)))
          ((and (eq method :post) (string= path "/api/notifications/test"))
           (reply 200 (obj "decision" (notify k :topic "update" :importance 2 :origin "test"
                                                :summary "Test notification: this is a test")
                           "last_result" (or (gethash (kernel-agent-id k) *push-last*) :null))))
          ((string= path "/api/notifications/telegram")
           (reply 200 (obj "available" :false "reason" "Telegram is not part of v3 yet.")))
          (t (reply 404 (obj "error" "no such route"))))
      (setting-refused (e) (reply 400 (obj "error" (princ-to-string e))))
      (error (e) (reply 400 (obj "error" (princ-to-string e)))))))
