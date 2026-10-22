(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Notifying the operator (bead v3-clp.10): one boundary with v2's order of
;;; checks, content-free Web Push signed with a key in the secret store, and a
;;; policy only the operator changes.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defvar *pushed* '())

(defmacro with-notify ((k) &body body)
  "A kernel with secrets open, the clock at midnight UTC, and pushes recorded
instead of sent."
  `(let ((secrets (db-open (format nil "/tmp/pai-v3-notify-secrets-~d.db" (random 100000000)) :key *test-key*))
         (real (fdefinition 'pai-v3.api::%push-all)))
     (pai-v3.trust:open-secrets secrets)
     (setf *pushed* '())
     (unwind-protect
          (with-clock
            (with-temp-kernel (,k)
              (sb-ext:without-package-locks
                (setf (fdefinition 'pai-v3.api::%push-all)
                      (lambda (kernel policy id summary link importance)
                        (declare (ignore kernel policy link importance))
                        (push (list id summary) *pushed*) :sent)))
              (change-setting ,k "notifications.policy"
                              (obj "web_push_subject" "mailto:ops@example.com"
                                   "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0))
                              :origin *operator*)
              ,@body))
       (sb-ext:without-package-locks (setf (fdefinition 'pai-v3.api::%push-all) real))
       (pai-v3.trust:close-secrets)
       (db-close secrets))))

(defun policy (k change)
  (change-setting k "notifications.policy" (pai-v3.api::%merge (setting k "notifications.policy") change) :origin *operator*))

(deftest a-notification-is-sent-and-a-repeat-is-deduped
  (with-notify (k)
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 2 :summary "The build finished." :dedupe-key "build-1")))
    (is (equal "The build finished." (second (first *pushed*))))
    (is (equal "deduped" (pai-v3.api::notify k :topic "update" :importance 2 :summary "The build finished." :dedupe-key "build-1")))
    (is (= 1 (length *pushed*)))))

(deftest switches-and-topics-are-the-operators
  (with-notify (k)
    (policy k (obj "topics" (obj "update" (obj "enabled" :false))))
    (is (equal "suppressed-topic" (pai-v3.api::notify k :topic "update" :importance 3 :summary "x")))
    (policy k (obj "enabled" :false))
    (is (equal "disabled" (pai-v3.api::notify k :topic "reminder" :importance 3 :summary "x")))
    (is (null *pushed*))
    (signals setting-refused
      (change-setting k "notifications.policy" (obj "web_push_subject" "ops@example.com") :origin (obj "principal_type" "self")))
    (signals setting-refused
      (change-setting k "notifications.policy" (obj "web_push_subject" "not a uri") :origin *operator*))))

(deftest quiet-hours-hold-a-reminder-and-release-it-after
  (with-notify (k)
    (policy k (obj "quiet_hours" (obj "enabled" t "start_minute" 1380 "end_minute" 120)))   ; 23:00-02:00, now is midnight UTC
    (is (equal "held" (pai-v3.api::notify k :topic "reminder" :importance 2 :summary "Call the dentist")))
    (is (equal "suppressed-quiet" (pai-v3.api::notify k :topic "update" :importance 2 :summary "Something else")))
    (is (null *pushed*))
    (pai-v3.api::release-held-notifications k)
    (is (null *pushed*) "still quiet")
    (advance 7200)
    (pai-v3.api::release-held-notifications k)
    (is (equal "Call the dentist" (second (first *pushed*))) "sent once quiet hours end")
    (pai-v3.api::release-held-notifications k)
    (is (= 1 (length *pushed*)) "released once")))

(deftest the-lowest-importance-is-pruned-first
  (with-notify (k)
    ;; per_hour 4: importance 1 may use half (2), 2 three quarters (3), 3 all of it.
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 1 :summary "a")))
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 1 :summary "b")))
    (is (equal "pruned" (pai-v3.api::notify k :topic "update" :importance 1 :summary "c")))
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 2 :summary "d")))
    (is (equal "pruned" (pai-v3.api::notify k :topic "update" :importance 2 :summary "e")))
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 3 :summary "f")))
    (is (equal "pruned" (pai-v3.api::notify k :topic "update" :importance 3 :summary "g")))
    (is (equal "sent" (pai-v3.api::notify k :topic "update" :importance 1 :summary "test" :origin "test")) "a test ignores the caps")))

(deftest summaries-are-plain-and-bounded
  (with-notify (k)
    (pai-v3.api::notify k :topic "update" :importance 2
              :summary (format nil "Read **this** at https://evil.example/x now~%please ~a" (make-string 300 :initial-element #\a)))
    (let ((s (second (first *pushed*))))
      (is (not (search "https://" s)) "links removed")
      (is (not (search "**" s)) "markup removed")
      (is (<= (length s) 140)))))

(deftest the-tool-posts-in-the-terminal-and-says-what-the-phone-got
  (with-notify (k)
    (let ((said (funcall (pai-v3.mind::tool-run (gethash "notify_operator" *tools*)) k 1
                         (obj "topic" "update" "importance" 2 "summary" "The report is ready."
                              "message" "The quarterly report is ready for you to read."))))
      (is (search "Phone: woken" said))
      (is (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1"
                     "notice" "%quarterly report%")
          "the longer message is in the terminal"))
    (is (not (pai-v3.mind::tool-available-p (gethash "notify_operator" *tools*) '(:audience "peer:x" :communicating t)))
        "a peer cannot make me notify the operator")))

(deftest the-same-thing-is-told-once-however-it-is-worded
  (with-notify (k)
    (let ((run (pai-v3.mind::tool-run (gethash "notify_operator" *tools*))))
      (is (search "Phone: woken" (funcall run k 1 (obj "topic" "reminder" "importance" 2 "about" "c-synthetic"
                                                       "summary" "SYNTHETIC-FIRST review the plan."))))
      (is (search "already told about c-synthetic"
                  (funcall run k 1 (obj "topic" "update" "importance" 2 "about" "c-synthetic"
                                        "summary" "SYNTHETIC-SECOND the plan needs a review."))))
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1"
                           "notice" "%SYNTHETIC-SECOND%"))
          "nothing reaches the terminal either")
      (is (= 1 (length *pushed*)))
      (is (search "Phone:" (funcall run k 1 (obj "topic" "update" "importance" 2 "about" "c-other"
                                                 "summary" "SYNTHETIC-THIRD something else.")))
          "another thing is told"))))

;;; Shadow mode (docs/charges-and-attention.md §5)

(deftest shadow-mode-keeps-what-i-start-and-sends-what-i-am-asked
  (with-notify (k)
    (change-setting k "outreach.shadow" t :origin *operator*)
    (let* ((run (pai-v3.mind::tool-run (gethash "notify_operator" *tools*)))
           (noticing (request-thought k "noticing" "new mail" :audience "external"))
           (said (funcall run k noticing (obj "topic" "important-mail" "importance" 2 "summary" "The landlord wrote about the lease."))))
      (is (search "Kept for the operator's review" said))
      (is (null *pushed*) "nothing reached the phone")
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 AND rendered LIKE ?2 LIMIT 1" "notice" "%landlord%"))
          "nor the terminal")
      (let ((kept (pai-v3.api::shadow-outreach k 10)))
        (is (= 1 (length kept)))
        (is (equal "noticing" (obj-get (first kept) "origin")))
        (pai-v3.api::rate-outreach k (obj-get (first kept) "id") "useful")
        (is (equal "useful" (obj-get (first (pai-v3.api::shadow-outreach k 10)) "rating")) "the operator's rating is kept")
        (signals error (pai-v3.api::rate-outreach k (obj-get (first kept) "id") "meh") "only the three ratings"))
      (let ((responding (request-thought k "responding" "the operator asked")))
        (is (search "Phone: woken" (funcall run k responding (obj "topic" "update" "importance" 2 "summary" "Here is the answer you asked for.")))
            "an answer in the operator's own conversation is never shadowed"))
      (change-setting k "outreach.shadow_exempt" (list "noticing") :origin *operator*)
      (is (search "Phone:" (funcall run k noticing (obj "topic" "important-mail" "importance" 2 "summary" "Mail and calendar are trusted now.")))
          "an exempt kind of thought is sent")
      (let ((quiet (request-thought k "introspection" "quiet time")))
        (is (search "Kept for the operator's review" (funcall run k quiet (obj "topic" "update" "importance" 2 "summary" "A thought from quiet time.")))
            "the rest stay in shadow"))
      (signals setting-refused (change-setting k "outreach.shadow_exempt" (list "anything") :origin *operator*))
      (change-setting k "outreach.shadow" :false :origin *operator*)
      (is (search "Phone:" (funcall run k noticing (obj "topic" "update" "importance" 2 "summary" "Shadow mode off: this one goes.")))))))

(deftest reminders-reach-the-phone
  (with-notify (k)
    (post-notice k :channel "reminder" :audience "operator" :text "Take the bins out")
    (pai-v3.api::notify-reminders k)
    (is (search "Take the bins out" (second (first *pushed*))))
    (pai-v3.api::notify-reminders k)
    (is (= 1 (length *pushed*)) "each reminder once")))

(deftest subscriptions-name-only-push-services-and-the-token-verifies
  (with-notify (k)
    (signals error (pai-v3.api::push-subscribe k "https://evil.example/push/1"))
    (signals error (pai-v3.api::push-subscribe k "http://fcm.googleapis.com/fcm/send/abc"))
    (is (equal "fcm.googleapis.com" (obj-get (pai-v3.api::push-subscribe k "https://fcm.googleapis.com/fcm/send/abc") "host")))
    (is (= 1 (length (pai-v3.api::push-subscriptions k))))
    (let* ((public (pai-v3.trust:secret-es256-public "web-push-vapid"))
           (jwt (pai-v3.api::%vapid-jwt "https://fcm.googleapis.com/fcm/send/abc" "mailto:ops@example.com"))
           (dot (position #\. jwt :from-end t))
           (signature (base64url-decode (subseq jwt (1+ dot)))))
      (is (= 65 (length public)) "an uncompressed P-256 point")
      (is (ironclad:verify-signature (ironclad:make-public-key :secp256r1 :y public)
                                     (ironclad:digest-sequence :sha256 (babel:string-to-octets (subseq jwt 0 dot)))
                                     signature)
          "the VAPID token verifies against the public key"))))

(deftest a-lonely-agent-reaches-out-when-enabled-and-within-bounds
  (with-notify (k)
    (let ((op (obj "principal_type" "operator")))
      (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0)) :origin op)
      (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "SYNTHETIC hello")
      (sql-exec (kernel-db k) "UPDATE working_dialogue SET at=?1" (iso8601 (- (now) (* 5 3600))))
      (is (null (pai-v3.api::reach-out-periodic k)) "off unless enabled")
      (change-setting k "outreach.drive.enabled" t :origin op)
      (change-setting k "outreach.drive.threshold" 0 :origin op)
      (let ((id (pai-v3.api::reach-out-periodic k)))
        (is id "a thought starts")
        (is (search "topic reach-out" (getf (thought-row k id) :reason)))
        (is (equal "operator" (getf (thought-row k id) :audience))))
      (is (null (pai-v3.api::reach-out-periodic k)) "not again until the silence has passed once more")
      (change-setting k "outreach.drive.threshold" 1 :origin op)
      (sql-exec (kernel-db k) "UPDATE drive_outreach SET at_unix=at_unix-20000")
      (is (null (pai-v3.api::reach-out-periodic k)) "not while the drive is below the threshold")
      (change-setting k "outreach.drive.threshold" 0 :origin op)
      (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "SYNTHETIC back")
      (is (null (pai-v3.api::reach-out-periodic k)) "not soon after the operator spoke")
      (is (assoc "reach-out" pai-v3.api::+notify-topics+ :test #'equal) "a topic of its own"))))

(run-suite "api-notify")
