(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Provider tests use a fake transport: no network, no spend.

(defvar *behaviour* nil)
(define-handler "test.step" :run (lambda (k w) (funcall *behaviour* k w)))

(defun fake (status body &key headers chunks)
  "A transport returning STATUS and BODY, or streaming CHUNKS (strings)."
  (lambda (url headers-in body-in &key stream-fn timeout)
    (declare (ignore url headers-in body-in timeout))
    (when (and stream-fn chunks) (mapc stream-fn chunks))
    (values status headers (if stream-fn "" body))))

(defun profile (transport)
  (make-provider-profile :name "test" :base-url "http://fake" :model "m1"
                         :transport transport))

(defun user (text) (list (obj "role" "user" "content" text)))

(defun ok-body (text &key (prompt 10) (completion 5) cost)
  (json-encode (obj "choices" (list (obj "message" (obj "content" text) "finish_reason" "stop"))
                    "usage" (obj "prompt_tokens" prompt "completion_tokens" completion "cost" cost))))

(deftest completion-records-usage-and-spend
  (with-temp-kernel (k)
    (let ((r (complete k (profile (fake 200 (ok-body "hello" :cost 0.0123))) (user "hi"))))
      (is (string= "hello" (obj-get r "content")))
      (is (= 12300 (spent-today k "background")) "reported cost becomes spend")
      (is (string= "ready" (projection-status k "provider_calibration"))))))

(defun thinking-only (completion reasoning)
  (json-encode (obj "choices" (list (obj "message" (obj "content" "" "reasoning" "planning the chapter at length") "finish_reason" "length"))
                    "usage" (obj "prompt_tokens" 100 "completion_tokens" completion "cost" 0.002
                                 "completion_tokens_details" (obj "reasoning_tokens" reasoning)))))

(deftest a-model-that-thinks-until-it-runs-out-is-asked-again-with-room
  (with-temp-kernel (k)
    (let* ((asked '())
           (script (list (thinking-only 8192 8191) (ok-body "Chapter one begins." :cost 0.004)))
           (p (make-provider-profile :name "voice" :base-url "http://fake" :model "m1" :max-tokens 8192
                                     :transport (lambda (url headers body &key stream-fn timeout allow-private)
                                                  (declare (ignore url headers stream-fn timeout allow-private))
                                                  (push (obj-get (json-parse body) "max_tokens") asked)
                                                  (values 200 nil (pop script)))))
           (r (complete k p (user "write a chapter"))))
      (is (equal "Chapter one begins." (obj-get r "content")) "the second ask answered")
      (is (equal '(8192 32768) (reverse asked)) "with four times the room")
      (is (= 1 (or (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1" "test-agent" "provider.call.failed") 0))
          "the first attempt is still recorded as a failure")
      (is (= 6000 (spent-today k "background")) "and both attempts are billed: 2000 + 4000 microusd"))))

(deftest asking-again-happens-once-and-only-for-thinking
  (with-temp-kernel (k)
    (let* ((calls 0)
           (p (make-provider-profile :name "voice" :base-url "http://fake" :model "m1" :max-tokens 8192
                                     :transport (lambda (url headers body &key stream-fn timeout allow-private)
                                                  (declare (ignore url headers body stream-fn timeout allow-private))
                                                  (incf calls)
                                                  (values 200 nil (thinking-only 32768 32767))))))
      (signals provider-failure (complete k p (user "write a chapter")))
      (is (= 2 calls) "one more ask, not a loop"))
    (let* ((calls 0)
           (p (make-provider-profile :name "voice" :base-url "http://fake" :model "m1" :max-tokens 8192
                                     :transport (lambda (url headers body &key stream-fn timeout allow-private)
                                                  (declare (ignore url headers body stream-fn timeout allow-private))
                                                  (incf calls)
                                                  (values 200 nil (thinking-only 50 2))))))
      (signals provider-failure (complete k p (user "hi")))
      (is (= 1 calls) "an empty answer that was not thinking is not asked again"))))

(deftest a-local-server-without-a-key-is-sent-none
  (let ((local (pai-v3.mind::%profile (obj "name" "judge" "base_url" "http://127.0.0.1:1234/v1" "model" "q" "local" t)))
        (remote (pai-v3.mind::%profile (obj "name" "main" "base_url" "https://example.com/v1" "model" "m"))))
    (is (null (pai-v3.mind::provider-profile-secret local)))
    (is (equal "provider" (pai-v3.mind::provider-profile-secret remote)) "a remote profile still names its key")
    (with-temp-kernel (k)
      (let ((sent nil))
        (setf (pai-v3.mind::provider-profile-transport local)
              (lambda (url headers body &key stream-fn timeout allow-private)
                (declare (ignore url body stream-fn timeout allow-private))
                (setf sent headers)
                (values 200 nil (ok-body "ok"))))
        (complete k local (user "hi"))
        (is (null (assoc "Authorization" sent :test #'string=)) "no Authorization header")))))

(deftest streaming-delivers-deltas-and-keeps-the-whole
  (with-temp-kernel (k)
    (let* ((seen '())
           (chunks (list (json-encode (obj "choices" (list (obj "delta" (obj "content" "Good ")))))
                         (json-encode (obj "choices" (list (obj "delta" (obj "content" "morning")
                                                                "finish_reason" "stop"))
                                           "usage" (obj "prompt_tokens" 3 "completion_tokens" 2)))))
           (r (complete k (profile (fake 200 nil :chunks chunks)) (user "hi")
                        :on-delta (lambda (d) (push d seen)))))
      (is (equal '("Good " "morning") (reverse seen)))
      (is (string= "Good morning" (obj-get r "content"))))))

(deftest streamed-tool-calls-are-reassembled
  ;; As OpenRouter streams them: the name once, then argument fragments by index.
  (let* ((chunks (list (obj "index" 0 "id" "call_a" "type" "function" "function" (obj "name" "recall" "arguments" ""))
                       (obj "index" 0 "function" (obj "arguments" "{\"about\":"))
                       (obj "index" 1 "id" "call_b" "type" "function" "function" (obj "name" "clock" "arguments" ""))
                       (obj "index" 0 "function" (obj "arguments" "\"tea\"}"))
                       (obj "index" 1 "function" (obj "name" "clock" "arguments" "{}"))))
         (calls (reduce (lambda (acc d) (pai-v3.mind::merge-tool-call-deltas acc (list d))) chunks :initial-value '())))
    (is (= 2 (length calls)))
    (is (equal '("recall" "clock") (mapcar (lambda (c) (obj-get (obj-get c "function") "name")) calls)))
    (is (equal "{\"about\":\"tea\"}" (obj-get (obj-get (first calls) "function") "arguments")))
    (is (equal "{}" (obj-get (obj-get (second calls) "function") "arguments")))
    (is (equal "call_b" (obj-get (second calls) "id")))))

(deftest failures-are-classified
  (with-temp-kernel (k)
    (flet ((class-of-call (transport &optional stream)
             (handler-case
                 (progn (complete k (profile transport) (user "x")
                                  :on-delta (and stream (lambda (d) (declare (ignore d)))))
                        :no-error)
               (provider-failure (f) (failure-class f)))))
      (let ((empty (json-encode (obj "choices" (list (obj "message" (obj "content" ""))))))
            (partial (json-encode (obj "choices" (list (obj "delta" (obj "content" "partial"))))))
            (died (json-encode (obj "error" (obj "message" "upstream died")))))
        (is (eq :transient (class-of-call (fake 429 "slow down"))))
        (is (eq :transient (class-of-call (fake 503 "unavailable"))))
        (is (eq :source (class-of-call (fake 400 "bad request"))))
        (is (eq :source (class-of-call (fake 401 "no key"))))
        (is (eq :model (class-of-call (fake 200 "not json"))))
        (is (eq :model (class-of-call (fake 200 empty))))
        (is (eq :transient (class-of-call (lambda (&rest args)
                                            (declare (ignore args))
                                            (error "connection refused")))))
        (is (eq :transient (class-of-call (fake 200 nil :chunks (list partial died)) t))
            "mid-stream provider error")
        ;; Bead v3-efb: a host not found just now is the network, not configuration.
        (is (eq :transient (class-of-call (lambda (&rest args) (declare (ignore args))
                                            (error 'egress-refused :transient t :reason "openrouter.ai could not be found."))))
            "a failed lookup is retried")
        (is (eq :source (class-of-call (lambda (&rest args) (declare (ignore args))
                                         (error 'egress-refused :reason "no secret named openrouter"))))
            "a refusal by rule is not")))))

(deftest mid-stream-failure-keeps-what-was-received
  (with-temp-kernel (k)
    (let ((partial (json-encode (obj "choices" (list (obj "delta" (obj "content" "I was saying"))))))
          (died (json-encode (obj "error" (obj "message" "upstream died")))))
      (handler-case (complete k (profile (fake 200 nil :chunks (list partial died))) (user "x")
                              :on-delta (lambda (d) (declare (ignore d))))
        (provider-failure (f)
          (is (string= "I was saying" (provider-failure-partial f))))))))

(deftest retry-after-reaches-the-scheduler
  (progn
    (with-clock
      (with-temp-kernel (k)
        (let ((*behaviour* (lambda (k w) (declare (ignore w))
                             (complete k (profile (fake 429 "later" :headers (list (cons "retry-after" "600"))))
                                       (user "x")))))
          (let ((id (request-work k "test.step" "r" nil)))
            (run-scheduler-step k)
            (advance 599)
            (is (eq :idle (run-scheduler-step k)) "waits as long as the provider asked")
            (advance 1)
            (is (eq :ran (first (run-scheduler-step k))))
            (is (= 2 (getf (work-status k id) :attempts)))))))))

(deftest calibration-measures-chars-per-token
  (with-temp-kernel (k)
    (dotimes (i 5)
      (complete k (profile (fake 200 (ok-body "ok" :prompt 10))) (user (make-string 40 :initial-element #\a))))
    (is (= 4.0 (calibrated-chars-per-token k "m1")))))

(deftest an-empty-response-says-why
  (with-temp-kernel (k)
    (let ((profile (make-provider-profile
                    :name "test" :base-url "http://fake" :model "m"
                    :transport (lambda (url headers body &key stream-fn timeout)
                                 (declare (ignore url headers body timeout))
                                 (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "reasoning" "thinking hard"))))))
                                 (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj) "finish_reason" "length"))
                                                                      "usage" (obj "prompt_tokens" 5 "completion_tokens" 4096
                                                                                   "completion_tokens_details" (obj "reasoning_tokens" 4096)))))
                                 (values 200 nil "")))))
      (let ((reason (handler-case (progn (complete k profile (list (obj "role" "user" "content" "hi"))
                                                   :on-delta (lambda (x) (declare (ignore x))))
                                         nil)
                      (provider-failure (f) (princ-to-string f)))))
        (is (search "empty response (finish: length · 4096 completion tokens, 4096 reasoning" reason) reason)))))

(deftest a-stream-read-as-latin-1-is-decoded-as-utf-8
  (let ((garbled (babel:octets-to-string (babel:string-to-octets "Sam — café ✓" :encoding :utf-8) :encoding :latin-1)))
    (is (equal "Sam — café ✓" (pai-v3.mind::%utf-8-line garbled)) "re-decoded")
    (is (equal "plain ascii" (pai-v3.mind::%utf-8-line "plain ascii")))
    (is (equal "already — right" (pai-v3.mind::%utf-8-line "already — right")) "a correctly read line is left alone")
    (is (equal "café" (pai-v3.mind::%utf-8-line "café")) "Latin-1 text that is not UTF-8 is left alone")))

(run-suite "mind-provider")
