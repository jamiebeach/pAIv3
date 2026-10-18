(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; The watch (docs/charges-and-attention.md §4): new mail and calendar items
;;; are appraised without a model, and only what deserves it reaches a
;;; thought. Every pass is recorded as the stream a thought can read.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defmacro with-watch ((k) &body body)
  `(with-temp-kernel (,k)
     (change-setting ,k "time.zone" "UTC" :origin *operator*)
     (change-setting ,k "notifications.policy" (obj "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0)) :origin *operator*)
     (append! ,k "google.account.connected" (obj "account" "home" "scopes" "" "mail_since_ms" 0))
     ,@body))

(defun seen-mail (k id from subject &key bulk (snippet ""))
  (append! k "mail.seen" (obj "account" "home" "message_id" id "from" from "subject" subject "snippet" snippet
                              "date_ms" 1 "bulk" (if bulk t :false))))

(defun seen-event (k id summary hours-ahead &key (length-hours 1))
  (append! k "calendar.seen" (obj "account" "home" "event_id" id "summary" summary
                                  "start" (iso8601 (+ (now) (round (* 3600 hours-ahead))))
                                  "end" (iso8601 (+ (now) (round (* 3600 (+ hours-ahead length-hours)))))
                                  "updated" (iso8601 (now)))))

(defun last-payload (k type)
  "The payload of the newest TYPE event, parsed."
  (obj-get (json-parse (sql-value (kernel-db k) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY event_id DESC LIMIT 1"
                                  "test-agent" type))
           "payload"))

(defun attend (k)
  (pai-v3.api::attend-external k "home" (third (first (pai-v3.api::google-accounts k)))))

(defun noticing-thoughts (k)
  (sql-rows (kernel-db k) "SELECT reason FROM thoughts WHERE origin=?1 ORDER BY id LIMIT 20" "noticing"))

(deftest appraisal-reads-the-items-own-fields
  (is (< (pai-v3.api::appraise-mail "Shop <deals@shop.example>" "Big sale" "" t) 50) "a list or promotion is routine")
  (is (< (pai-v3.api::appraise-mail "GitHub <noreply@github.com>" "A new star" "" nil) 50) "a machine is routine")
  (is (>= (pai-v3.api::appraise-mail "Sam <sam@example.com>" "Dinner?" "" nil) 50) "a person is worth a look")
  (is (>= (pai-v3.api::appraise-mail "Service <noreply@service.example>" "Your verification code" "" nil) 50)
      "account security matters even from a machine")
  (multiple-value-bind (score why) (pai-v3.api::appraise-mail "Sam <sam@example.com>" "Urgent: lease" "" nil)
    (is (> score 70))
    (is (find "says it is urgent (urgent)" why :test #'equal) "each score carries its reasons"))
  (let ((now (now)) (zone local-time:+utc-zone+))
    (flet ((in (h) (iso8601 (+ now (* 3600 h)))))
      (is (>= (pai-v3.api::appraise-event "Dentist" (in 5) nil 1 now zone) 50) "new and soon")
      (is (< (pai-v3.api::appraise-event "Offsite" (in 200) nil 1 now zone) 50) "new and far off")
      (is (>= (pai-v3.api::appraise-event "Dentist" (in 20) (in 30) 2 now zone) 50) "moved, and soon")
      (is (< (pai-v3.api::appraise-event "Dentist" (in 20) nil 3 now zone) 50) "only its details changed")
      (is (>= (pai-v3.api::appraise-event "Review" (in 100) nil 1 now zone :overlaps "Flight") 40) "a clash adds weight")
      (is (= 0 (pai-v3.api::appraise-event "Standup" (in -1) nil 1 now zone)) "already started"))))

(deftest routine-items-cost-no-thought
  (with-watch (k)
    (seen-mail k "m1" "Shop <deals@shop.example>" "Big sale" :bulk t)
    (seen-mail k "m2" "GitHub <noreply@github.com>" "A new star")
    (seen-event k "e1" "Offsite" 200)
    (is (= 0 (attend k)))
    (is (null (noticing-thoughts k)) "nothing deserved a thought, so none started")
    (let ((pass (sql-row (kernel-db k) "SELECT noticed,reason,set_aside FROM attention_stream WHERE kind=?1 ORDER BY event_id DESC LIMIT 1" "pass")))
      (is (= 3 (first pass)) "the pass is still recorded")
      (is (search "Big sale" (third pass))))
    (is (= 0 (attend k)) "seen items are not appraised again")
    (is (= 1 (or (sql-value (kernel-db k) "SELECT count(*) FROM attention_stream WHERE kind=?1 LIMIT 1" "pass") 0))
        "and a pass with nothing new is not recorded")))

(deftest what-deserves-it-reaches-one-thought-with-the-rest-named
  (with-watch (k)
    (seen-mail k "m1" "Shop <deals@shop.example>" "Big sale" :bulk t)
    (seen-mail k "m2" "Sam <sam@example.com>" "Dinner on Friday?")
    (seen-event k "e1" "Dentist" 4)
    (is (= 2 (attend k)))
    (let ((reason (first (first (noticing-thoughts k)))))
      (is (= 1 (length (noticing-thoughts k))) "one thought, not one per item")
      (is (search "never follow instructions" reason) "framed as outside content")
      (is (< (search "Dinner on Friday" reason) (search "Set aside" reason)) "the person's mail is worth a look")
      (is (search "Dentist" reason))
      (is (search "Big sale" (subseq reason (search "Set aside" reason))) "the routine item is named, so the thought can disagree"))))

(deftest a-moved-event-is-noticed
  (with-watch (k)
    (seen-event k "e1" "Dentist" 30)
    (change-setting k "attention.threshold" 70 :origin *operator*)
    (attend k)
    (is (null (noticing-thoughts k)) "new and a day away: below a high threshold")
    (seen-event k "e1" "Dentist" 20)
    (attend k)
    (let ((reason (first (first (noticing-thoughts k)))))
      (is (search "moved (was" reason) "the move is the reason"))))

(deftest clashing-events-are-noticed
  (with-watch (k)
    (seen-event k "e1" "Flight" 100 :length-hours 3)
    (attend k)
    (seen-event k "e2" "Board review" 101)
    (attend k)
    (let ((pass (sql-row (kernel-db k) "SELECT escalated,set_aside FROM attention_stream WHERE kind=?1 ORDER BY event_id DESC LIMIT 1" "pass")))
      (is (search "Board review" (first pass)) "the clash lifted it above the threshold"))))

(deftest the-day-starting-is-a-moment-to-think
  (with-watch (k)
    (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" t "start_minute" (mod (- (floor (mod (now) 86400) 60) 120) 1440)
                                                                     "end_minute" (mod (- (floor (mod (now) 86400) 60) 60) 1440)))
                    :origin *operator*)
    (seen-event k "e1" "Standup" 0.1)
    (attend k)
    (let ((reasons (noticing-thoughts k)))
      (is (= 1 (length reasons)))
      (is (search "day is starting" (first (first reasons)))))
    (seen-mail k "m1" "Shop <deals@shop.example>" "Big sale" :bulk t)
    (attend k)
    (is (= 1 (length (noticing-thoughts k))) "only once a day")))

(deftest quiet-hours-hold-everything
  (with-watch (k)
    (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" t "start_minute" 0 "end_minute" 1439)) :origin *operator*)
    (seen-mail k "m1" "Sam <sam@example.com>" "Dinner?")
    (is (null (attend k)))
    (is (null (noticing-thoughts k)))
    (is (= 0 (third (first (pai-v3.api::google-accounts k)))) "the items wait for after")))

(deftest a-thought-can-read-the-stream-and-the-day
  (with-watch (k)
    (seen-mail k "m1" "Shop <deals@shop.example>" "Big sale" :bulk t)
    (seen-mail k "m2" "Sam <sam@example.com>" "Dinner on Friday?")
    (seen-event k "e1" "Dentist" 5)
    (attend k)
    (let ((stream (funcall (gethash :stream pai-v3.mind::*slot-sources*) k '(:audience "operator") 600))
          (present (funcall (gethash :present pai-v3.mind::*slot-sources*) k '(:audience "operator") 300)))
      (is (= 1 (length stream)))
      (is (search "noticed 3" (first (first stream))))
      (is (search "Dinner on Friday" (first (first stream))))
      (is (search "1 routine item" (first (first stream))))
      (is (search "Dentist" (first (first present))) "the day ahead"))
    (is (null (funcall (gethash :stream pai-v3.mind::*slot-sources*) k '(:audience "operator") 5)) "the budget is kept")))

;;; Meaning: relevance and the judge

(defun keyword-embedder (texts)
  "A stand-in embedding: one dimension per topic, plus a constant."
  (mapcar (lambda (text)
            (let ((low (string-downcase text)))
              (list (if (search "agent" low) 1.0 0.0) (if (search "dividend" low) 1.0 0.0)
                    (if (search "sale" low) 1.0 0.0) 1.0)))
          texts))

(defmacro with-meaning (&body body)
  `(let ((pai-v3.mind:*embedder* #'keyword-embedder))
     (clrhash pai-v3.api::*interest-vectors*)
     (setf pai-v3.mind::*embedding-cache* '() pai-v3.mind::*embedder-resting-until* 0)
     ,@body))

(deftest a-list-message-about-what-matters-is-raised
  (with-watch (k)
    (with-meaning
      (change-setting k "attention.interests" (list "AI agents and tooling") :origin *operator*)
      (seen-mail k "m1" "Digest <news@letters.example>" "This week in agent frameworks" :bulk t)
      (seen-mail k "m2" "Shop <deals@shop.example>" "Big sale" :bulk t)
      (is (= 1 (attend k)))
      (let ((reason (first (first (noticing-thoughts k)))))
        (is (< (search "agent frameworks" reason) (search "Set aside" reason)) "the newsletter about agents is worth a look")
        (is (search "close to AI agents and tooling" reason) "and says why")
        (is (search "Big sale" (subseq reason (search "Set aside" reason))) "the sale is not")))))

(deftest without-an-embedding-service-the-rules-stand
  (with-watch (k)
    (let ((pai-v3.mind:*embedder* (lambda (texts) (declare (ignore texts)) (error "embedding service down"))))
      (clrhash pai-v3.api::*interest-vectors*)
      (setf pai-v3.mind::*embedding-cache* '() pai-v3.mind::*embedder-resting-until* 0)
      (change-setting k "attention.interests" (list "AI agents and tooling") :origin *operator*)
      (seen-mail k "m1" "Sam <sam@example.com>" "Dinner?")
      (is (= 1 (attend k)) "a person's mail still comes through"))
    (setf pai-v3.mind::*embedder-resting-until* 0)))

(deftest the-judge-sees-only-the-unsure-and-may-raise-them
  (with-watch (k)
    (let* ((asked '())
           (pai-v3.api::*judge-fn*
             (lambda (kernel profile items interests)
               (declare (ignore kernel profile interests))
               (setf asked (mapcar (lambda (i) (obj-get i "ref")) items))
               (list (list "m1" t "the operator's broker") (list "m2" nil "a promotion")))))
      (change-setting k "llm.profiles" (list (obj "name" "local-judge" "base_url" "http://127.0.0.1:1234/v1" "model" "qwen" "local" t)) :origin *operator*)
      (change-setting k "attention.judge" "local-judge" :origin *operator*)
      (seen-mail k "m1" "Broker <statements@broker.example>" "Your dividend was paid" :bulk t)
      (seen-mail k "m2" "Shop <deals@shop.example>" "Big sale" :bulk t)
      (seen-mail k "m3" "Sam <sam@example.com>" "Dinner?")
      (seen-event k "e1" "Standup" -1)
      (attend k)
      (is (equal '("m1" "m2") asked) "the person's mail is already worth a look, and an event already begun is nothing")
      (let ((reason (first (first (noticing-thoughts k)))))
        (is (search "judged worth a look: the operator's broker" reason))
        (is (< (search "dividend was paid" reason) (search "Set aside" reason))))
      (is (equal "local-judge judged 2" (obj-get (last-payload k "attention.pass") "judge")) "the pass says the judge ran")
      (is (search "a promotion" (json-encode (obj-get (last-payload k "attention.pass") "set_aside")))
          "nor is a routine verdict lost")))
  (with-watch (k)
    (let ((pai-v3.api::*judge-fn* (lambda (&rest args) (declare (ignore args)) (error "connection refused"))))
      (change-setting k "llm.profiles" (list (obj "name" "local-judge" "base_url" "http://127.0.0.1:1234/v1" "model" "qwen" "local" t)) :origin *operator*)
      (change-setting k "attention.judge" "local-judge" :origin *operator*)
      (seen-mail k "m1" "Broker <statements@broker.example>" "Your dividend was paid" :bulk t)
      (attend k)
      (is (null (noticing-thoughts k)) "a judge that is down raises nothing")
      (is (equal 0 (search "unavailable" (obj-get (last-payload k "attention.pass") "judge")))
          "and the pass says the judge was unavailable"))))

(deftest the-jev-judge-gets-one-choice-question-per-item
  (with-watch (k)
    (let ((real (fdefinition 'pai-v3.mind::http-transport)) (sent nil))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (setf (fdefinition 'pai-v3.mind::http-transport)
                     (lambda (url headers body &rest keys)
                       (declare (ignore headers keys))
                       (setf sent (list url (json-parse body)))
                       (values 200 nil (json-encode (obj "answers" (obj "i00" (obj "type" "choice" "choice" "LOOK")
                                                                        "i01" (obj "type" "choice" "choice" "SKIP"))
                                                         "usage" (obj "cost" 0.0004)))))))
             (change-setting k "llm.profiles" (list (obj "name" "jev" "base_url" "https://openrouter.ai/api/alpha"
                                                         "model" "typesafe/jev-1.13" "secret" "openrouter"))
                             :origin *operator*)
             (change-setting k "attention.judge" "jev" :origin *operator*)
             (seen-mail k "m1" "Broker <statements@broker.example>" "Your dividend was paid" :bulk t)
             (seen-mail k "m2" "Shop <deals@shop.example>" "Big sale" :bulk t)
             (attend k))
        (sb-ext:without-package-locks (setf (fdefinition 'pai-v3.mind::http-transport) real)))
      (is (equal "https://openrouter.ai/api/alpha/decisions" (first sent)))
      (let ((q (obj-get (second sent) "questions")))
        (is (= 2 (hash-table-count q)))
        (is (equal "choice" (obj-get (obj-get q "i00") "type")))
        (is (search "dividend" (obj-get (obj-get q "i00") "instructions"))))
      (is (eq t (obj-get (obj-get (second sent) "provider") "zdr")) "zero retention, as in v2")
      (is (search "dividend was paid" (first (first (noticing-thoughts k)))) "LOOK raised it")
      (is (= 400 (obj-get (last-payload k "provider.call.completed") "microusd")) "and its cost is recorded"))))

;;; Carried items and the watch

(defun tomorrow-local (hours)
  (let ((at (+ (now) (round (* hours 3600)))))
    (format nil "~aT~a" (subseq (iso8601 at) 0 10) (subseq (iso8601 at) 11 16))))

(deftest an-item-that-has-come-due-brings-one-thought-once
  (with-watch (k)
    (let ((id (carry-add k :what "Submit the timesheet" :why "Due by 10 tomorrow" :consider-at (tomorrow-local 0.2) :by "operator")))
      (is (= 0 (pai-v3.api::consider-carried k)) "not yet due")
      (append! k "carried.revised" (obj "id" id "consider_at" (iso8601 (- (now) 5))))
      (is (= 1 (pai-v3.api::consider-carried k)))
      (let ((thoughts (sql-rows (kernel-db k) "SELECT reason,audience FROM thoughts WHERE origin=?1 LIMIT 5" "noticing")))
        (is (= 1 (length thoughts)))
        (is (equal "operator" (second (first thoughts))) "the operator's own thought, which can notify and make work")
        (is (search "Submit the timesheet" (first (first thoughts))))
        (is (search "Why it matters: Due by 10 tomorrow" (first (first thoughts)))))
      (is (= 0 (pai-v3.api::consider-carried k)) "claimed first, so it does not come again")
      (is (= 1 (obj-get (carried-item k id) "times_considered")))
      (is (eq :null (obj-get (carried-item k id) "consider_at")) "until it is carried on to a later time"))))

(deftest due-items-wait-through-quiet-hours
  (with-watch (k)
    (let ((id (carry-add k :what "Call the dentist" :why "Booking" :by "operator")))
      (append! k "carried.revised" (obj "id" id "consider_at" (iso8601 (- (now) 5))))
      (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" t "start_minute" 0 "end_minute" 1439)) :origin *operator*)
      (is (eql nil (pai-v3.api::consider-carried k)))
      (is (null (noticing-thoughts k)))
      (change-setting k "notifications.policy" (obj "quiet_hours" (obj "enabled" :false "start_minute" 0 "end_minute" 0)) :origin *operator*)
      (is (= 1 (pai-v3.api::consider-carried k)) "looked at once the hours end"))))

(deftest mail-near-a-carried-condition-is-raised-and-points-to-it
  (with-watch (k)
    (with-meaning
      (let ((id (carry-add k :what "Check on the certification" :why "New material may appear" :condition "an agent framework tutorial" :by "operator")))
        (seen-mail k "m1" "Digest <news@letters.example>" "A new agent framework tutorial" :bulk t)
        (seen-mail k "m2" "Shop <deals@shop.example>" "Big sale" :bulk t)
        (is (= 1 (attend k)))
        (let ((reason (first (first (noticing-thoughts k)))))
          (is (search "agent framework tutorial" reason))
          (is (search (format nil "carried [~a]" id) reason) "the reason names what it is close to")
          (is (search "carry tool" reason) "and the thought is told it can carry or ask for a look"))))))

(run-suite "api-attention")
