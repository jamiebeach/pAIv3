(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; A scripted provider: each call pops the next response (or failure).
(defvar *script* '())
(defvar *seen-messages* '())

(defun scripted-transport ()
  (lambda (url headers body &key stream-fn timeout)
    (declare (ignore url headers timeout))
    (push (obj-get (json-parse body) "messages") *seen-messages*)
    (let ((next (pop *script*))
          (usage (obj "prompt_tokens" 50 "completion_tokens" 10 "cost" 0.001)))
      (etypecase next
        (integer (values next nil "failure"))
        (hash-table
         (if stream-fn
             (progn
               ;; Stream the content word by word, as a provider would.
               (loop for word in (split-words (obj-get next "content" ""))
                     do (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "content" word)))))))
               (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "tool_calls" (obj-get next "tool_calls"))
                                                                         "finish_reason" "stop"))
                                                    "usage" usage)))
               (values 200 nil ""))
             (values 200 nil (json-encode (obj "choices" (list (obj "message" next "finish_reason" "stop"))
                                               "usage" usage)))))))))

(defun split-words (text)
  "TEXT as fragments that concatenate back to TEXT."
  (loop with start = 0
        for space = (position #\Space text :start start)
        collect (subseq text start (if space (1+ space) (length text)))
        while space
        do (setf start (1+ space))))

(defmacro with-thinking ((k) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (let ((*thought-provider* (make-provider-profile :name "test" :base-url "http://fake"
                                                        :model "m" :transport (scripted-transport)))
             (*seen-messages* '())
             (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
         (advance 43200)
         ,@body))))

(defun run-until-idle (k &optional (limit 50))
  (loop repeat limit until (eq :idle (run-scheduler-step k))))

(define-tool "clock" :description "Current time"
  :run (lambda (k id args) (declare (ignore k id args)) "It is noon."))

(deftest responding-thought-replies-in-conversation
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "Good morning, Sam!")))
           (turn (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                                 :text "Hi Alpha" :origin (obj "principal_type" "operator")))
           (id (request-thought k "responding" "operator spoke" :channel "web"
                                                                :source-event-id (event-id turn))))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (is (search "Me: Good morning, Sam!"
                  (dialogue-row-text (car (last (recent-dialogue k "operator")))))))))

(deftest tool-calls-run-and-the-thought-continues
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls"
                                (list (obj "id" "c1" "type" "function"
                                           "function" (obj "name" "clock" "arguments" "{}"))))
                           (obj "content" "It's noon.")))
           (id (request-thought k "private" "check the time")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (is (= 2 (getf (thought-row k id) :model-calls)))
      (is (find "It is noon." (thought-steps k id) :key #'third :test #'equal) "tool result recorded")
      (is (find "tool" (first *seen-messages*) :key (lambda (m) (obj-get m "role")) :test #'equal)
          "second call saw the tool result"))))

(deftest allowance-parks-a-thought-and-keeps-its-work
  (with-thinking (k)
    (let* ((*thought-policy* '(("private" :private 2 1000000)))
           (loop-call (obj "content" "thinking" "tool_calls"
                           (list (obj "id" "c" "type" "function"
                                      "function" (obj "name" "clock" "arguments" "{}")))))
           ;; The wrap-up call fails too, so the thought parks.
           (*script* (list loop-call loop-call 400))
           (id (request-thought k "private" "ruminate")))
      (run-until-idle k)
      (is (equal "parked" (getf (thought-row k id) :status)))
      (is (search "allowance" (getf (thought-row k id) :outcome)))
      (is (= 2 (count "model" (thought-steps k id) :key #'first :test #'equal)) "both steps kept"))))

(defun call (id name &optional (arguments "{}"))
  (obj "id" id "type" "function" "function" (obj "name" name "arguments" arguments)))

(defun operator-turn (k text)
  (let ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                              :text text :origin (obj "principal_type" "operator"))))
    (request-thought k "responding" "operator spoke" :channel "web" :source-event-id (event-id turn))))

(defun replies (k)
  (mapcar #'dialogue-row-text (remove "Operator" (recent-dialogue k "operator")
                                      :key #'dialogue-row-speaker :test #'equal)))

(deftest a-reply-turn-out-of-allowance-still-answers
  (with-thinking (k)
    (let* ((*thought-policy* '(("responding" :operator 2 1000000 2)))
           (loop-call (obj "content" "" "tool_calls" (list (call "c" "clock"))))
           (*script* (list loop-call loop-call (obj "content" "Out of steps: it is noon, and I stopped there.")))
           (id (operator-turn k "Dig into the clock")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)) "it answered instead of parking")
      (is (search "it is noon" (car (last (replies k)))))
      (is (search "allowance" (obj-get (car (last (first *seen-messages*))) "content"))
          "the last call is told why it must answer now"))))

(deftest a-reply-turn-whose-wrap-up-fails-parks
  (with-thinking (k)
    (let* ((*thought-policy* '(("responding" :operator 1 1000000 2)))
           (*script* (list (obj "content" "" "tool_calls" (list (call "c" "clock"))) 400))
           (id (operator-turn k "Dig")))
      (run-until-idle k)
      (is (equal "parked" (getf (thought-row k id) :status))))))

(deftest words-between-steps-reach-the-operator-at-once
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "Checking the clock first." "tool_calls" (list (call "c" "clock")))
                           (obj "content" "It is noon.")))
           (id (operator-turn k "What time is it?")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (let ((said (replies k)))
        (is (= 2 (length said)) (format nil "a status line and the answer; got ~s" said))
        (is (search "Checking the clock first." (first said)))
        (is (search "It is noon." (second said)))))))

(deftest private-thoughts-say-nothing-between-steps
  (with-thinking (k)
    (let ((*script* (list (obj "content" "Let me look." "tool_calls" (list (call "c" "clock")))
                          (obj "content" "Noon."))))
      (request-thought k "private" "check")
      (run-until-idle k)
      (is (null (replies k))))))

(deftest a-tool-call-with-no-name-is-never-sent-back
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls"
                                (list (call "a" "clock")
                                      (obj "id" "b" "type" "function" "function" (obj "arguments" "{}"))))
                           (obj "content" "Noon.")))
           (id (request-thought k "private" "check")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (let ((assistant (find "assistant" (first *seen-messages*) :key (lambda (m) (obj-get m "role")) :test #'equal)))
        (is (equal '("clock") (mapcar (lambda (c) (obj-get (obj-get c "function") "name"))
                                      (obj-get assistant "tool_calls"))))))))

(deftest any-thought-out-of-allowance-answers-from-a-wrap-up
  (with-thinking (k)
    (let* ((*thought-policy* '(("private" :private 2 1000000)))
           (loop-call (obj "content" "" "tool_calls" (list (call "c" "clock"))))
           (*script* (list loop-call loop-call (obj "content" "It is noon; I stopped there.")))
           (id (request-thought k "private" "ruminate")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (is (search "allowance" (obj-get (car (last (first *seen-messages*))) "content"))))))

(deftest a-thought-near-its-allowance-is-told-to-finish
  (with-thinking (k)
    (let* ((*thought-policy* '(("work" :work 10 1000000 1)))
           (loop-call (obj "content" "" "tool_calls" (list (call "c" "clock"))))
           (*script* (append (loop repeat 8 collect loop-call) (list (obj "content" "Done."))))
           (id (request-thought k "work" "explore")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (let ((notes (loop for messages in *seen-messages*
                         count (search "left in this thought" (or (obj-get (car (last messages)) "content") "")))))
        (is (= 1 notes) (format nil "told once two calls remain, then it finished; got ~d" notes))))))

(defun last-note (messages) (or (obj-get (car (last messages)) "content") ""))

(deftest long-work-starts-with-a-plan-and-checks-itself
  (with-thinking (k)
    (let* ((*thought-policy* '(("work" :work 30 1000000 1)))
           (loop-call (obj "content" "" "tool_calls" (list (call "c" "clock"))))
           (*script* (append (loop repeat 9 collect loop-call) (list (obj "content" "Done."))))
           (id (request-thought k "work" "explore")))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (let ((notes (mapcar #'last-note (reverse *seen-messages*))))
        (is (search "write a short plan first" (first notes)) "the first call is asked for a plan")
        (is (search "Checkpoint, 8 of 30" (nth 8 notes)) "the ninth call follows a checkpoint")
        (is (search "delivered as it is and ends this thought" (nth 8 notes))
            "the checkpoint says that a message without a tool call is the answer (bead v3-35t)")
        (is (= 1 (count-if (lambda (n) (search "Checkpoint" n)) notes)))))))

(deftest a-repeated-call-is-pointed-out
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (list (call "a" "clock" "{\"x\":1,\"y\":2}")))
                           (obj "content" "" "tool_calls" (list (call "b" "clock" "{\"y\":2,\"x\":1}")))
                           (obj "content" "Noon.")))
           (id (request-thought k "work" "check the time")))
      (run-until-idle k)
      (let ((results (remove "tool_result" (thought-steps k id) :key #'first :test-not #'equal)))
        (is (not (search "already made" (third (first results)))) "the first call is new")
        (is (search "already made this exact call" (third (second results))) "the same call, keys reordered, is noticed")))))

(deftest a-flood-of-tool-calls-runs-only-the-first-few
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (loop for i below 20 collect (call (format nil "c~d" i) "clock")))
                           (obj "content" "Done.")))
           (id (request-thought k "work" "look around")))
      (run-until-idle k)
      (let ((results (remove "tool_result" (thought-steps k id) :key #'first :test-not #'equal)))
        (is (= 20 (length results)) "every call is answered, so the history stays valid")
        (is (= 12 (count-if (lambda (r) (not (search "Not run" (third r)))) results)) "the first twelve run")
        (is (search "asked for 20 tool calls" (third (car (last results)))) "the rest say why"))
      (is (equal "ended" (getf (thought-row k id) :status))))))

(deftest provider-outage-retries-without-losing-the-thought
  (with-thinking (k)
    (let* ((*script* (list 503 (obj "content" "Back now.")))
           (id (request-thought k "concern" "follow up")))
      (run-scheduler-step k)
      (is (equal "queued" (getf (thought-row k id) :status)) "thought still alive after a 503")
      (advance 60)
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status))))))

(deftest operator-thoughts-preempt-background-thinking
  (with-thinking (k)
    (let* ((tool-call (obj "content" "" "tool_calls"
                           (list (obj "id" "c" "type" "function"
                                      "function" (obj "name" "clock" "arguments" "{}")))))
           (*script* (list tool-call (obj "content" "Hello!") (obj "content" "done")))
           (bg (request-thought k "private" "background musing")))
      (run-scheduler-step k)                                ; background takes one step
      (let ((op (request-thought k "responding" "operator spoke" :channel "web")))
        (run-scheduler-step k)                              ; operator goes next
        (is (equal "ended" (getf (thought-row k op) :status)))
        (is (not (equal "ended" (getf (thought-row k bg) :status))))
        (run-until-idle k)
        (is (equal "ended" (getf (thought-row k bg) :status)))))))

(deftest rejected-request-fails-the-thought
  (with-thinking (k)
    (let* ((*script* (list 400))
           (id (request-thought k "private" "doomed")))
      (run-until-idle k)
      (is (equal "failed" (getf (thought-row k id) :status)))
      (is (search "400" (getf (thought-row k id) :outcome))))))

(deftest exhausted-model-failures-mark-the-thought-failed
  (with-thinking (k)
    (let* ((empty (obj "content" ""))
           (*script* (list empty empty empty empty))
           (id (request-thought k "private" "keeps producing nothing")))
      (loop repeat 6 do (run-until-idle k) (advance 7200))
      (is (equal "failed" (getf (thought-row k id) :status)) "dead-lettered work marks the thought")
      (is (search "model" (getf (thought-row k id) :outcome))))))

(deftest interrupting-a-reply-keeps-what-was-said
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "One two three four five six seven")))
           (id (request-thought k "responding" "operator spoke" :channel "voice"))
           (*reply-delta-hook* (lambda (tid text)
                                 (when (search "two" text) (interrupt-thought tid)))))
      (run-until-idle k)
      (is (equal "ended" (getf (thought-row k id) :status)))
      (is (equal "interrupted" (getf (thought-row k id) :outcome)))
      (let ((said (dialogue-row-text (car (last (recent-dialogue k "operator"))))))
        (is (search "One two" said) "what was already said is kept")
        (is (null (search "seven" said)) "nothing after the interruption")
        (is (search "[interrupted]" said))))))

(deftest several-tool-calls-replay-as-one-assistant-message
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "Let me check." "tool_calls" (list (call "a" "clock") (call "b" "clock")))
                           (obj "content" "Noon twice.")))
           (id (request-thought k "private" "check twice")))
      (run-until-idle k)
      (let ((second-call (remove "system" (first *seen-messages*)
                                 :key (lambda (m) (obj-get m "role")) :test #'equal)))
        (is (equal '("user" "assistant" "tool" "tool") (mapcar (lambda (m) (obj-get m "role")) second-call))
            "the argument is rebuilt the way the provider expects it")
        (is (equal "Let me check." (obj-get (second second-call) "content")))
        (is (= 2 (length (obj-get (second second-call) "tool_calls")))))
      (is (equal "ended" (getf (thought-row k id) :status))))))

(deftest ponder-recurses-into-a-sub-thought-and-returns
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (list (call "p1" "ponder" "{\"question\":\"what time is it\"}")))
                           (obj "content" "It is noon.")              ; the sub-thought answers
                           (obj "content" "So: noon.")))             ; the parent continues
           (id (request-thought k "work" "plan the day")))
      (run-until-idle k)
      (let ((child (sql-value (kernel-db k) "SELECT id FROM thoughts WHERE parent_id=?1 LIMIT 1" id)))
        (is child "a sub-thought was started")
        (is (equal "ended" (getf (thought-row k child) :status)))
        (is (= 1 (getf (thought-row k child) :depth)))
        (is (= id (getf (thought-row k child) :root-id))))
      (is (find "It is noon." (thought-steps k id) :key #'third :test #'equal)
          "the sub-thought's answer is the parent's tool result")
      (is (equal "ended" (getf (thought-row k id) :status)))
      (is (equal "So: noon." (third (car (last (thought-steps k id)))))
          "the parent answered after its sub-question returned"))))

(deftest sub-thoughts-share-the-root-allowance
  (with-thinking (k)
    (let* ((*thought-policy* '(("work" :work 2 1000000 3)))
           (*script* (list (obj "content" "" "tool_calls" (list (call "p1" "ponder" "{\"question\":\"q\"}")))
                           (obj "content" "" "tool_calls" (list (call "c1" "clock")))
                           400))                                     ; the wrap-ups fail
           (id (request-thought k "work" "big job")))
      (run-until-idle k)
      (let ((child (sql-value (kernel-db k) "SELECT id FROM thoughts WHERE parent_id=?1 LIMIT 1" id)))
        (is (equal "parked" (getf (thought-row k child) :status)) "the child hit the root's allowance")
        (is (equal "parked" (getf (thought-row k id) :status)) "and so does the parent")
        (is (search "allowance" (third (find "p1" (thought-steps k id) :key #'fifth :test #'equal
                                                                     :from-end t)))
            "the parent is told why its sub-question has no answer")))))

(deftest ponder-stops-at-the-depth-limit
  (with-thinking (k)
    (let* ((*thought-policy* '(("private" :private 6 1000000 0)))
           (*script* (list (obj "content" "" "tool_calls" (list (call "p1" "ponder" "{\"question\":\"q\"}")))
                           (obj "content" "fine")))
           (id (request-thought k "private" "shallow")))
      (run-until-idle k)
      (is (null (sql-value (kernel-db k) "SELECT id FROM thoughts WHERE parent_id=?1 LIMIT 1" id)))
      (is (find-if (lambda (s) (search "as deep as" (third s))) (thought-steps k id))))))

(define-tool "secret_lever" :description "Pull a lever only the operator may pull." :available :operator-trust
  :run (lambda (k id args) (declare (ignore k id args)) "pulled"))

(deftest tools-are-listed-by-availability-and-picked-up-on-describe
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (list (call "d" "describe_tool" "{\"name\":\"clock\"}")))
                           (obj "content" "ok")))
           (id (request-thought k "private" "look around")))
      (run-until-idle k)
      (let ((index (obj-get (first (second *seen-messages*)) "content")))
        (is (search "- clock: Current time" index) "available tools appear as one line")
        (is (search "secret_lever" index) "the operator's own thoughts see operator-trust tools"))
      (is (search "Current time" (third (find "tool_result" (thought-steps k id) :key #'first :test #'equal))))
      (let ((thought (thought-row k id)))
        (is (find "clock" (tools-in-hand thought (tools-for thought) '("clock")) :key #'tool-name :test #'equal)
            "a described tool is in hand"))))
  (let ((guest (list :origin "responding" :audience "person:sam")))
    (is (null (find "secret_lever" (tools-for guest) :key #'tool-name :test #'equal))
        "a guest's thought is never told about operator-trust tools")))

(deftest a-guest-cannot-call-an-operator-tool-by-name
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (list (call "x" "secret_lever")))
                           (obj "content" "ok")))
           (id (request-thought k "responding" "guest spoke" :audience "person:sam")))
      (run-until-idle k)
      (is (find "No tool named secret_lever." (thought-steps k id) :key #'third :test #'equal)))))

(define-tool "long_read" :description "Return a long text."
  :run (lambda (k id args) (declare (ignore k id args)) (make-string 30000 :initial-element #\a)))

(deftest long-results-show-their-head-and-page
  (with-thinking (k)
    (let* ((*script* (list (obj "content" "" "tool_calls" (list (call "l" "long_read")))
                           (obj "content" "" "tool_calls" (list (call "r" "read_result" "{\"call_id\":\"l\",\"part\":2}")))
                           (obj "content" "done")))
           (id (request-thought k "work" "read it")))
      (run-until-idle k)
      (let ((tool-messages (remove "tool" (first *seen-messages*) :key (lambda (m) (obj-get m "role"))
                                                                  :test-not #'equal)))
        (is (search "The result continues" (obj-get (first tool-messages) "content")))
        (is (< (length (obj-get (first tool-messages) "content")) 13000) "only the head is shown")
        (is (= 6000 (length (obj-get (second tool-messages) "content"))) "the third part, on request"))
      (is (equal "ended" (getf (thought-row k id) :status))))))

(deftest a-reply-cut-off-by-the-output-limit-says-so
  (with-clock
    (with-temp-kernel (k)
      (let* ((sent nil)
             (*thought-provider*
               (make-provider-profile
                :name "test" :base-url "http://fake" :model "m" :reasoning (obj "max_tokens" 2000)
                :transport (lambda (url headers body &key stream-fn timeout)
                             (declare (ignore url headers timeout))
                             (setf sent (json-parse body))
                             (let ((usage (obj "prompt_tokens" 5 "completion_tokens" 4096)))
                               (if stream-fn
                                   (progn (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "content" "Good timing. Two habits before I")
                                                                                                    "finish_reason" "length"))
                                                                               "usage" usage)))
                                          (values 200 nil ""))
                                   (values 200 nil (json-encode (obj "choices" (list (obj "message" (obj "role" "assistant" "content" "Good timing. Two habits before I")
                                                                                          "finish_reason" "length"))
                                                                     "usage" usage))))))))
             (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
        (advance 43200)
        (let ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "Hello"
                                    :origin (obj "principal_type" "operator"))))
          (request-thought k "responding" "Hello" :channel "web" :source-event-id (event-id turn)))
        (run-until-idle k)
        (is (equal 2000 (obj-get (obj-get sent "reasoning") "max_tokens")) "the thinking budget is sent")
        (is (search "cut off" (sql-value (kernel-db k) "SELECT rendered FROM working_dialogue WHERE kind=?1 ORDER BY event_id DESC LIMIT 1" "reply"))
            "the operator sees it was cut off")))))

(deftest a-profile-marked-all-tools-is-given-every-tool
  (with-clock
    (with-temp-kernel (k)
      (let* ((counts '())
             (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
        (dolist (all '(nil t))
          (let ((*thought-provider*
                  (make-provider-profile
                   :name "test" :base-url "http://fake" :model "m" :all-tools all
                   :transport (lambda (url headers body &key stream-fn timeout)
                                (declare (ignore url headers stream-fn timeout))
                                (push (length (obj-get (json-parse body) "tools")) counts)
                                (values 200 nil (json-encode (obj "choices" (list (obj "message" (obj "role" "assistant" "content" "done")
                                                                                       "finish_reason" "stop"))
                                                                  "usage" (obj "prompt_tokens" 5 "completion_tokens" 1))))))))
            (advance 60)
            (request-thought k "work" "do a thing")
            (run-until-idle k)))
        (let ((core (car (last counts))) (with-all (first counts)))
          (is (< core with-all) "the core set is smaller")
          (is (= with-all (length (pai-v3.mind::tools-for (list :audience "operator" :origin "work"))))
              "all_tools sends every tool the thought may use"))))))

(run-suite "mind-thought")
