(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defun seed-persona (k)
  (append! k "persona.instructions.set" (obj "revision" 1 "text" "I do not fabricate."))
  (append! k "persona.profile.set" (obj "persona_id" "test" "revision" 3
                                        "identity" "I am Test Agent." "voice" "I speak plainly."))
  (append! k "persona.tone.set" (obj "text" "Warm and brief."))
  (append! k "self.trait.adopted" (obj "trait_id" "t1" "text" "I follow up." "reason" "operator asked"
                                       "proposed_by" "operator")))

(defun last-assembly (k)
  (event-payload (event-by-id k (sql-value (kernel-db k) "SELECT event_id FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                                           (kernel-agent-id k) "context.assembled"))))

(deftest identity-renders-in-declared-order-with-fingerprint
  (with-temp-kernel (k)
    (seed-persona k)
    (multiple-value-bind (text fp) (render-identity k)
      (is (< (search "I do not fabricate" text) (search "I am Test Agent" text)
             (search "I speak plainly" text) (search "I follow up" text) (search "Warm" text)))
      (is (= 64 (length fp)))
      (is (null (search "Warm" (render-identity k :operator-facing nil))) "tone only for the operator")
      (append! k "self.trait.retired" (obj "trait_id" "t1" "reason" "outgrown"))
      (multiple-value-bind (text2 fp2) (render-identity k)
        (is (null (search "I follow up" text2)))
        (is (string/= fp fp2) "fingerprint changes with identity")))))

(deftest assembly-puts-identity-first-and-records-a-manifest
  (with-temp-kernel (k)
    (seed-persona k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "Hi"
                    :origin (obj "principal_type" "operator"))
    (let* ((kept (gethash :recollection pai-v3.mind::*slot-sources*))
           (messages (progn (remhash :recollection pai-v3.mind::*slot-sources*)
                            (unwind-protect (assemble-context k "operator-turn" (list :audience "operator" :task "Reply."))
                              (setf (gethash :recollection pai-v3.mind::*slot-sources*) kept)))))
      (is (search "I am Test Agent" (obj-get (first messages) "content")) "stable prefix first")
      (is (search "Operator: Hi" (obj-get (second messages) "content")))
      (let* ((a (last-assembly k))
             (slots (obj-get a "slots")))
        (is (= 64 (length (obj-get a "persona_fingerprint"))))
        (is (string= "ok" (obj-get (find "dialogue" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status")))
        (is (search "unavailable" (obj-get (find "recollection" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status"))
            "a missing source is reported, not fatal")))))

(deftest failing-source-degrades-only-its-slot
  (with-temp-kernel (k)
    (let ((kept (gethash :recollection pai-v3.mind::*slot-sources*)))
      (define-slot-source :recollection (kernel request max) (error "index missing"))
      (unwind-protect
           (let ((messages (assemble-context k "operator-turn" (list :audience "operator" :task "x"))))
             (is (= 3 (length messages)))
             (is (search "unavailable: index missing"
                         (obj-get (find "recollection" (obj-get (last-assembly k) "slots")
                                        :key (lambda (s) (obj-get s "slot")) :test #'equal)
                                  "status"))))
        (setf (gethash :recollection pai-v3.mind::*slot-sources*) kept)))))

(deftest other-audiences-never-see-operator-dialogue
  (with-temp-kernel (k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                    :text "My salary is private" :origin (obj "principal_type" "operator"))
    (let ((messages (assemble-context k "private-cognition" (list :audience "person:guest" :task "x"))))
      (is (notany (lambda (m) (search "salary" (or (obj-get m "content") ""))) messages)))))

(deftest slot-budgets-are-respected
  (with-temp-kernel (k)
    (dotimes (i 400)
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                      :text (format nil "message ~d with a fair number of words in it to take space" i)
                      :origin (obj "principal_type" "operator")))
    (assemble-context k "operator-turn" (list :audience "operator" :task "x"))
    (let ((d (find "dialogue" (obj-get (last-assembly k) "slots") :key (lambda (s) (obj-get s "slot")) :test #'equal)))
      (is (<= (obj-get d "tokens") 4000)))))

(deftest no-slot-of-either-profile-fails-to-fill
  (with-temp-kernel (k)
    (pai-v3.mind::raise-concern k "Is anything broken?")
    (dolist (profile '("operator-turn" "private-cognition"))
      (pai-v3.mind::assemble-context k profile (list :audience "operator" :task "hello"))
      (let ((slots (obj-get (event-payload (event-by-id k (ledger-head k))) "slots")))
        (dolist (slot (remove-if (lambda (s) (member (obj-get s "slot") '("recall" "restricted") :test #'equal)) slots))
          ;; Recall needs the embedding service, which tests do not run.
          (is (not (search "unavailable" (obj-get slot "status")))
              (format nil "~a in ~a: ~a" (obj-get slot "slot") profile (obj-get slot "status"))))
        (is (find "concerns" slots :key (lambda (s) (obj-get s "slot")) :test #'equal))
        (is (equal "ok" (obj-get (find "concerns" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status"))
            "an open concern reaches the context")))))

(deftest a-reply-is-asked-with-the-operators-words-and-a-transcript-in-order
  (with-temp-kernel (k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "first message"
                    :origin (obj "principal_type" "operator"))
    (deliver-reply k :channel "web" :audience "operator" :text "first answer")
    (let* ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "Do you see my kids in your graph?"
                                 :origin (obj "principal_type" "operator")))
           (id (request-thought k "responding" "The operator spoke." :channel "web" :source-event-id (event-id turn)))
           (messages (funcall pai-v3.mind::*assemble-thought-context* k (thought-row k id) nil))
           (last (car (last messages)))
           (now (obj-get (second messages) "content")))
      (is (equal "user" (obj-get last "role")))
      (is (search "Do you see my kids in your graph?" (obj-get last "content")) "the task is the operator's words")
      (is (< (search "first message" now) (search "first answer" now) (search "Do you see my kids" now))
          "the conversation reads oldest to newest"))))

(deftest private-slots-are-blocked-before-their-producers-run
  (with-temp-kernel (k)
    (let ((saved (gethash :present pai-v3.mind::*slot-sources*)) (calls 0))
      (unwind-protect
           (progn
             (define-slot-source :present (kernel request max)
               (incf calls) (list "SYNTHETIC_OPERATOR_CALENDAR"))
             (dolist (audience '("peer:beta" "coding-agent" "person:guest"))
               (is (null (funcall (gethash :present pai-v3.mind::*slot-sources*) k (list :audience audience) 300)))
               (multiple-value-bind (rows used status)
                   (pai-v3.mind::%fill k :present (list :audience audience) 300)
                 (is (null rows)) (is (= 0 used)) (is (equal "withheld" status))))
             (is (= 0 calls))
             (dolist (audience '("operator" "external"))
               (is (funcall (gethash :present pai-v3.mind::*slot-sources*) k (list :audience audience) 300)))
             (is (= 2 calls)))
        (setf (gethash :present pai-v3.mind::*slot-sources*) saved)))))

(deftest both-context-profiles-protect-private-state-and-preserve-operator-reflection
  (with-temp-kernel (k)
    (let ((closed (raise-concern k "SYNTHETIC_CLOSED_CONCERN")))
      (close-concern k closed "satisfied" "SYNTHETIC_PRIVATE_CONCLUSION"))
    (raise-concern k "SYNTHETIC_OPEN_CONCERN" :rationale "Synthetic fixture")
    (append! k "drive.triggered" (obj "drive" "curiosity" "amount" 0.2 "target" "SYNTHETIC_PRIVATE_TARGET"))
    (append! k "google.account.connected" (obj "account" "test" "scopes" "" "mail_since_ms" 0))
    (sql-exec (kernel-db k) "INSERT INTO calendar_events VALUES('test','c1','SYNTHETIC_CALENDAR',?1,?2,NULL,?1,1)"
              (iso8601 (+ (now) 3600)) (iso8601 (+ (now) 7200)))
    (sql-exec (kernel-db k) "INSERT INTO attention_stream(event_id,account,at,noticed,reason,escalated,set_aside) VALUES(1,'test',?1,1,'new-items',?2,'[]')"
              (now-iso8601) (json-encode (list (obj "text" "SYNTHETIC_MAIL"))))
    (dolist (profile '("operator-turn" "private-cognition"))
      (dolist (audience '("peer:beta" "coding-agent" "person:guest"))
        (let ((text (json-encode (assemble-context k profile (list :audience audience :task "A public topic")))))
          (dolist (marker '("SYNTHETIC_CLOSED_CONCERN" "SYNTHETIC_PRIVATE_CONCLUSION" "SYNTHETIC_OPEN_CONCERN"
                            "SYNTHETIC_PRIVATE_TARGET" "SYNTHETIC_CALENDAR" "SYNTHETIC_MAIL"))
            (is (not (search marker text)) marker))))
      (let ((text (json-encode (assemble-context k profile (list :audience "operator" :reflecting t :task "Review")))))
        (dolist (marker '("SYNTHETIC_PRIVATE_CONCLUSION" "SYNTHETIC_OPEN_CONCERN" "SYNTHETIC_PRIVATE_TARGET" "SYNTHETIC_CALENDAR" "SYNTHETIC_MAIL"))
          (is (search marker text) marker))))
    (let ((text (json-encode (assemble-context k "private-cognition" (list :audience "external" :task "Appraise")))))
      (is (search "SYNTHETIC_CALENDAR" text))
      (is (search "SYNTHETIC_MAIL" text))
      (is (not (search "SYNTHETIC_OPEN_CONCERN" text)))
      (is (not (search "SYNTHETIC_PRIVATE_TARGET" text))))))

(deftest new-slot-sources-default-to-operator-only
  (with-temp-kernel (k)
    (unwind-protect
         (progn
           (define-slot-source :synthetic-unreviewed (kernel request max) (list "PRIVATE"))
           (is (funcall (gethash :synthetic-unreviewed pai-v3.mind::*slot-sources*) k (list :audience "operator") 10))
           (is (null (funcall (gethash :synthetic-unreviewed pai-v3.mind::*slot-sources*) k (list :audience "peer:beta") 10))))
      (remhash :synthetic-unreviewed pai-v3.mind::*slot-sources*))))

(run-suite "mind-context")
