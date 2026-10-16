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
    (let ((messages (assemble-context k "operator-turn" (list :audience "operator" :task "Reply."))))
      (is (search "I am Test Agent" (obj-get (first messages) "content")) "stable prefix first")
      (is (search "Operator: Hi" (obj-get (second messages) "content")))
      (let* ((a (last-assembly k))
             (slots (obj-get a "slots")))
        (is (= 64 (length (obj-get a "persona_fingerprint"))))
        (is (string= "ok" (obj-get (find "dialogue" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status")))
        (is (search "unavailable" (obj-get (find "recall" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status"))
            "a missing source is reported, not fatal")))))

(deftest failing-source-degrades-only-its-slot
  (with-temp-kernel (k)
    (define-slot-source :recall (kernel request max) (error "index missing"))
    (unwind-protect
         (let ((messages (assemble-context k "operator-turn" (list :audience "operator" :task "x"))))
           (is (= 3 (length messages)))
           (is (search "unavailable: index missing"
                       (obj-get (find "recall" (obj-get (last-assembly k) "slots")
                                      :key (lambda (s) (obj-get s "slot")) :test #'equal)
                                "status"))))
      (remhash :recall pai-v3.mind::*slot-sources*))))

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

(run-suite "mind-context")
