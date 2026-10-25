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

(deftest recent-activity-is-one-compact-line-per-thought
  ;; Bead v3-okh.16: each row used to quote the thought's whole prompt.
  (let ((review (pai-v3.mind::%activity-line
                 (format nil "Check ONE proposed post for the crew forum. This is a light crew review...~%~a" (make-string 2000 :initial-element #\x))
                 "sent" "ended" "2026-10-25T20:37:00Z"))
        (long (pai-v3.mind::%activity-line
               (format nil "Quiet time. You are looking into one of your concerns (c-1, curiosity): why the brief is late.~%~a" (make-string 3000 :initial-element #\y))
               (make-string 3000 :initial-element #\z) "ended" "2026-10-25T20:37:00Z")))
    (is (search "Reviewed a draft before it was sent -> ended: sent" review))
    (is (< (length review) 120))
    (is (< (length long) 700) "a long prompt and outcome become one bounded line")
    (is (search "Quiet time. You are looking into one of your concerns" long))))

(deftest older-long-tool-results-are-shortened-in-the-replay
  (with-temp-kernel (k)
    (let ((id (request-thought k "introspection" "SYNTHETIC reading")))
      (dotimes (i 6)
        (pai-v3.mind::%record-text k id "tool_call" "" "tool" "source" "call_id" (format nil "c~d" i) "arguments" "{}")
        (pai-v3.mind::%record-text k id "tool_result" (make-string 5000 :initial-element (code-char (+ 97 i))) "tool" "source" "call_id" (format nil "c~d" i)))
      (let* ((results (remove-if-not (lambda (m) (equal "tool" (obj-get m "role"))) (pai-v3.mind::%replay-messages k id))))
        (is (= 6 (length results)))
        (is (search "shortened here to save space" (obj-get (first results) "content")))
        (is (search "call_id c0 and part 0" (obj-get (first results) "content")))
        (is (< (length (obj-get (second results) "content")) 1500))
        (is (= 5000 (length (obj-get (third results) "content"))) "the newest four stay whole")
        (is (= 5000 (length (obj-get (sixth results) "content"))))
        (is (= 5000 (length (funcall (pai-v3.mind::tool-run (gethash "read_result" pai-v3.mind::*tools*)) k id (obj "call_id" "c0" "part" 0))))
            "read_result still gives the whole of it")))))

(deftest a-thought-thinks-from-the-same-frame-on-every-call
  ;; Bead v3-okh.16: the same opening messages each call, so a provider can
  ;; serve the repeated prefix from its cache.
  (with-temp-kernel (k)
    (seed-persona k)
    (let* ((thought (thought-row k (request-thought k "introspection" "SYNTHETIC look into something")))
           (first-frame (pai-v3.mind::thought-frame k thought "TOOLS"))
           (assembled (lambda () (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1"
                                            (kernel-agent-id k) "context.assembled"))))
      (append! k "turn.received" (obj "channel" "web" "audience" "operator" "speaker" "Sam" "text" "SYNTHETIC something new"
                                      "origin" (obj "principal_type" "operator")))
      (is (eq first-frame (pai-v3.mind::thought-frame k thought "TOOLS")) "reused as it was, whatever happened since")
      (is (= 1 (funcall assembled)) "assembled once")
      (pai-v3.mind::thought-frame k thought "OTHER TOOLS")
      (is (= 2 (funcall assembled)) "a changed tool set builds it again")
      (let ((system (obj-get (first first-frame) "content")))
        (is (< (search "I do not fabricate" system) (search "TOOLS" system) (search "Private thought" system))
            "identity, then the tools, then what is particular to this thought"))
      (let ((now-text (obj-get (second first-frame) "content")))
        (is (< (search "## About me" now-text :end2 (length now-text)) (or (search "## Now" now-text) most-positive-fixnum))
            "the clock comes last")))))

(run-suite "mind-context")
