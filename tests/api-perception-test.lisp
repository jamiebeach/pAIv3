(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; System-1 senses, synthetic (bead v3-byw.2; docs/system1-sensory-perception.md
;;; §10). No camera: observations are what a perception service would send.

(defparameter *operator* (obj "principal_type" "operator"))
(defparameter *webcam-key* "synthetic-webcam-key")

(defmacro with-senses ((k &key (owner t)) &body body)
  "An agent subscribed to one synthetic webcam, at 14:00 UTC on a weekday."
  `(with-clock
     (with-temp-kernel (,k)
       (at-hour 14)
       (change-setting ,k "perception.sources"
                       (list (obj "source_id" "webcam-a" "name" "Office webcam" "notify_owner" ,(if owner t :false)
                                  "token_sha256" (pai-v3.kernel::sha256-hex *webcam-key*)))
                       :origin *operator*)
       ,@body)))

(defun seen (k seq kind track &key (epoch (first (pai-v3.api::sense-source k "webcam-a"))) (recognition "unknown")
                                   candidate score (ttl 30))
  (pai-v3.api::admit-percept
   k "webcam-a"
   (obj "source_id" "webcam-a" "epoch" epoch "sequence" seq "kind" kind "track_id" track
        "recognition" recognition "candidate" candidate "score" score
        "observed_at" (iso8601 (now)) "expires_at" (iso8601 (+ (now) ttl)))))

(defun present (k) (format nil "~{~a~^~%~}" (pai-v3.api::sense-present-lines k)))

(defun sense-thoughts (k)
  (sql-rows (kernel-db k) "SELECT id,reason FROM thoughts WHERE reason LIKE ?1 ORDER BY id LIMIT 20" "Senses (%"))

(deftest a-source-is-off-until-the-operator-starts-it
  (with-senses (k)
    (is (search "not started" (present k)))
    (is (equal "refused" (seen k 1 "person.entered" "t1" :epoch 0)) "nothing is admitted before a start")
    (is (= 1 (pai-v3.api::set-sense-state k "webcam-a" "live" "started")))
    (is (search "watching; no one present" (present k)))))

(deftest enter-stay-leave-gives-one-thought-and-a-current-present
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (is (equal "admitted" (seen k 1 "person.entered" "t1")))
    (dotimes (i 5) (advance 10) (seen k (+ 2 i) "person.present" "t1"))
    (is (search "1 present" (present k)))
    (is (search "not known" (present k)))
    (is (= 1 (length (sense-thoughts k))) "staying wakes nobody")
    (is (search "someone has appeared at Office webcam" (second (first (sense-thoughts k)))))
    (advance 10) (seen k 7 "person.left" "t1")
    (is (search "no one present" (present k)))))

(deftest repeats-old-sequences-and-old-epochs-are-dropped
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 5 "person.entered" "t1")
    (is (equal "dropped" (seen k 5 "person.present" "t1")) "a repeat")
    (is (equal "dropped" (seen k 3 "person.left" "t1")) "out of order")
    (is (search "1 present" (present k)) "the late 'left' did not remove them")
    (is (equal "refused" (seen k 6 "person.present" "t1" :epoch 0)) "an old epoch")
    (is (equal "refused" (pai-v3.api::admit-percept k "webcam-a" (obj "source_id" "webcam-b" "epoch" 1 "sequence" 9)))
        "a key speaks only for its own source")))

(deftest presence-expires-into-not-knowing
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 1 "person.entered" "t1" :ttl 3600)
    (is (search "1 present" (present k)))
    (advance 61)
    (is (search "no one present" (present k)) "the receiver caps validity at 60 seconds, whatever was sent")))

(deftest stopping-fences-work-in-flight-and-a-restart-resurrects-nothing
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (let ((old (first (pai-v3.api::sense-source k "webcam-a"))))
      (seen k 1 "person.entered" "t1")
      (pai-v3.api::stop-all-senses k "stopped by the operator")
      (is (search "off (stopped by the operator)" (present k)))
      (is (equal "refused" (seen k 2 "person.present" "t1" :epoch old)) "late work from before the stop")
      (pai-v3.api::set-sense-state k "webcam-a" "live" "started again")
      (is (> (first (pai-v3.api::sense-source k "webcam-a")) (1+ old)) "every start and stop moves the epoch on")
      (is (search "no one present" (present k)) "the old sighting does not come back")
      (is (equal "refused" (seen k 3 "person.present" "t1" :epoch old))))))

(deftest a-candidate-stays-a-candidate-until-corrected
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 1 "person.entered" "t1" :recognition "candidate" :candidate "Sam" :score 0.62)
    (is (search "possibly Sam (similarity 0.62)" (present k)))
    (is (search "evidence, not identity, and it grants nothing" (second (first (sense-thoughts k)))))
    (append! k "percept.corrected" (obj "observation_id" "x" "source_id" "webcam-a" "track_id" "t1" "correction" "Alex" "by" "operator"))
    (is (search "corrected to Alex" (present k)))
    (is (null (search "possibly Sam" (present k))) "one current identity, not two")))

(deftest arrivals-past-the-hourly-allowance-are-present-but-wake-no-one
  (with-senses (k)
    (change-setting k "perception.wakes_per_hour" 2 :origin *operator*)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (loop for i from 1 to 4 do (seen k i "person.entered" (format nil "t~d" i)))
    (is (= 2 (length (sense-thoughts k))))
    (is (search "4 present" (present k)))))

(deftest only-the-notification-owner-may-tell-the-operator
  (with-senses (k :owner nil)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 1 "person.entered" "t1")
    (is (search "Another agent tells the operator about this source" (second (first (sense-thoughts k)))))))

(deftest a-lost-source-is-one-change-and-says-it-cannot-know
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 1 "person.entered" "t1")
    (pai-v3.api::set-sense-state k "webcam-a" "lost" "camera unplugged")
    (pai-v3.api::set-sense-state k "webcam-a" "lost" "camera unplugged")
    (is (search "lost since" (present k)))
    (is (search "Nobody can be known to be there or not" (present k)) "no one present is never claimed")))

(deftest the-present-slot-shows-senses-only-to-the-operators-own-thinking
  (with-senses (k)
    (pai-v3.api::set-sense-state k "webcam-a" "live" "started")
    (seen k 1 "person.entered" "t1")
    (flet ((slot (audience) (mapcar #'first (funcall (gethash :present pai-v3.mind::*slot-sources*) k (list :audience audience) 300))))
      (is (some (lambda (l) (search "Office webcam" l)) (slot "operator")))
      (is (notany (lambda (l) (search "Office webcam" l)) (slot "external")) "outside content never sees the senses"))))

(run-suite "api-perception")
