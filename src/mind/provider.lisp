(in-package #:pai-v3.mind)

;;; Provider module (NFR BUD-9, EXT-2; spec §14). One client for
;;; OpenAI-compatible chat completion APIs (OpenRouter, Nous Portal, local
;;; servers). Every call is bounded in time, classified on failure, recorded
;;; with its spend and reported token usage, and can stream.
;;;
;;; The HTTP transport is a function so tests run without a network:
;;;   (funcall transport url headers body-string &key stream-fn timeout)
;;;     -> (values status response-headers body-string)
;;; With STREAM-FN the transport calls it once per SSE data payload string.

(defstruct provider-profile
  name base-url model
  (secret "provider")                     ; name in the secret store (ENC-4)
  (local nil)                             ; base-url is a configured local server
  (connect-timeout 10) (read-timeout 120)
  (max-tokens 4096) (temperature nil)
  (vision nil)                            ; the model can see images
  (all-tools nil)                         ; give every tool, not the core set and describe_tool
  (reasoning nil)                         ; passed through, e.g. {"max_tokens": 4000}: bounds thinking so a reply has room
  (microusd-per-1k-prompt 0) (microusd-per-1k-completion 0)  ; when no reported cost
  (transport #'http-transport))

(define-event-type "provider.call.completed"
  :schema (("profile" :string) ("model" :string)
           ("estimated_prompt_tokens" :integer)
           ("prompt_tokens" :integer) ("completion_tokens" :integer)
           ("prompt_chars" :integer)
           ("microusd" :integer) ("milliseconds" :integer)
           ("work_id" :integer :optional)))

(define-event-type "provider.call.failed"
  :schema (("profile" :string) ("model" :string) ("class" :string)
           ("status" :integer) ("reason" :string) ("milliseconds" :integer)
           ("work_id" :integer :optional)))

;;; Calibration (spec §14): rolling totals of characters sent and tokens the
;;; provider reported, from which the chars-per-token estimate is derived.

(define-projection "provider_calibration"
  :kind :inline :consumes ("provider.call.completed")
  :tables ("CREATE TABLE IF NOT EXISTS provider_calibration (model TEXT PRIMARY KEY, chars INTEGER NOT NULL, tokens INTEGER NOT NULL, calls INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO provider_calibration(model,chars,tokens,calls) VALUES(?1,?2,?3,1) ON CONFLICT(model) DO UPDATE SET chars=chars+excluded.chars,tokens=tokens+excluded.tokens,calls=calls+1"
                        (obj-get p "model") (obj-get p "prompt_chars") (obj-get p "prompt_tokens")))))

(defun calibrated-chars-per-token (kernel model)
  "Measured characters per token for MODEL, or NIL before enough calls."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT chars,tokens,calls FROM provider_calibration WHERE model=?1 LIMIT 1" model)))
    (and row (>= (third row) 5) (plusp (second row))
         (/ (first row) (second row) 1.0))))

;;; Transport

(defun %utf-8-line (line)
  "LINE as UTF-8. An event stream with no charset is read as Latin-1, which
turns every multi-byte character into two or three wrong ones; re-decode
such a line, and leave one that was read correctly alone."
  (if (every (lambda (c) (< (char-code c) 256)) line)
      (handler-case (babel:octets-to-string (babel:string-to-octets line :encoding :latin-1) :encoding :utf-8)
        (error () line))
      line))

(defun http-transport (url headers body &key stream-fn (timeout 120) allow-private)
  "Post through the kernel's one outbound transport (spec §17)."
  (if stream-fn
      (multiple-value-bind (stream status response-headers)
          (http-request :post url :headers headers :body body :stream t
                                  :read-timeout timeout :allow-private allow-private)
        (unwind-protect
             (if (eql status 200)
                 (progn
                   (loop for raw = (read-line stream nil)
                         while raw
                         for line = (%utf-8-line raw)
                         when (and (> (length line) 6) (string= "data: " line :end2 6))
                           do (let ((data (string-trim '(#\Return) (subseq line 6))))
                                (unless (string= data "[DONE]")
                                  (funcall stream-fn data)))
                         ;; ": OPENROUTER PROCESSING" and the like: the call is alive.
                         when (and (plusp (length line)) (char= #\: (char line 0)))
                           do (funcall stream-fn :heartbeat))
                   (values status response-headers ""))
                 (values status response-headers
                         (if (streamp stream)
                             (with-output-to-string (s)
                               (loop for line = (read-line stream nil) while line
                                     do (write-line line s)))
                             (or stream ""))))
          (when (streamp stream) (close stream))))
      (multiple-value-bind (body-out status response-headers)
          (http-request :post url :headers headers :body body
                                  :read-timeout timeout :allow-private allow-private)
        (values status response-headers body-out))))

;;; Classification

(defun %header (headers name)
  (cond ((hash-table-p headers) (gethash name headers))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))))

(defun classify-status (status)
  "Failure class for an HTTP STATUS (spec §14)."
  (cond ((member status '(408 409 425 429)) :transient)
        ((and (integerp status) (>= status 500)) :transient)
        ((eql status 0) :transient)          ; no response: network
        (t :source)))                         ; 400, 401, 403, 404: request or config

(define-condition provider-failure (handler-failure)
  ((status :initarg :status :initform 0 :reader provider-failure-status)
   ;; Text already received before the failure. Never discarded (§13.3):
   ;; the caller keeps it as part of the thought.
   (partial :initarg :partial :initform "" :reader provider-failure-partial)))

;;; Calls

(defun %messages-chars (messages)
  (loop for m in messages
        for c = (obj-get m "content")
        sum (cond ((stringp c) (length c))
                  ;; Parts: count the text, not an image's encoding.
                  ((listp c) (loop for part in c sum (length (obj-get part "text" ""))))
                  (t 0))))

(defun %microusd (profile usage prompt completion)
  (let ((cost (obj-get usage "cost")))
    (if (realp cost)
        (round (* cost 1000000))
        (round (+ (* prompt (provider-profile-microusd-per-1k-prompt profile))
                  (* completion (provider-profile-microusd-per-1k-completion profile)))
               1000))))

(defun merge-tool-call-deltas (calls deltas)
  "Fold streamed tool-call DELTAS into CALLS. A streamed call arrives in
pieces sharing an index: the first names the function, the rest add to its
arguments. A whole call (no index, as in a non-streamed reply) is kept as is."
  (dolist (d deltas calls)
    (let* ((index (obj-get d "index"))
           (call (and index (find index calls :key (lambda (c) (obj-get c "index")) :test #'eql))))
      (if (null call)
          (let ((fn (obj-get d "function")))
            (setf calls (append calls
                                (list (obj "index" index "id" (obj-get d "id") "type" (or (obj-get d "type") "function")
                                           "function" (obj "name" (obj-get fn "name")
                                                           "arguments" (or (obj-get fn "arguments") "")))))))
          (let ((fn (obj-get d "function")) (have (obj-get call "function")))
            (when (obj-get d "id") (setf (gethash "id" call) (obj-get d "id")))
            (when (and (obj-get fn "name") (zerop (length (or (obj-get have "name") ""))))
              (setf (gethash "name" have) (obj-get fn "name")))
            (when (obj-get fn "arguments")
              (setf (gethash "arguments" have)
                    (concatenate 'string (or (obj-get have "arguments") "") (obj-get fn "arguments")))))))))

(defvar *progress-hook* nil
  "(lambda (detail)) or NIL: called as a streamed call progresses, with an
object of generation_id, output_characters, reasoning_characters,
tool_argument_characters, estimated_output_tokens, heartbeat, and usage
once the call completes. Content-free: counts only.")

(defvar *progress-generation* 0)

(defvar *capture-hook* nil
  "(lambda (kernel call-event request-json response-json)) or NIL: keeps the
exact exchange of a completed call for the observability page.")

(defparameter *room-to-answer-factor* 4
  "When a model spends its whole output allowance thinking, ask again once with this many times the allowance.")
(defparameter *room-to-answer-ceiling* 65536 "...but never more than this many output tokens.")

(defun complete (kernel profile messages &key tools on-delta work-id (lane "background") grown)
  "Call PROFILE with MESSAGES (list of objects with role and content).
Returns an object with \"content\", \"tool_calls\" and \"finish_reason\".
With ON-DELTA, streams and calls it with each text fragment as it arrives.
Records provider.call.completed and its spend, or provider.call.failed, and
signals PROVIDER-FAILURE with a class on failure. A reasoning model that
spends its whole output allowance thinking and says nothing is asked again
once with more room (GROWN marks that second ask)."
  (let* ((start (get-internal-real-time))
         (chars (%messages-chars messages))
         (estimate (estimate-tokens (make-string chars :initial-element #\x)))
         (body (json-encode (obj "model" (provider-profile-model profile)
                                 "messages" messages
                                 "tools" tools
                                 "max_tokens" (provider-profile-max-tokens profile)
                                 "temperature" (provider-profile-temperature profile)
                                 "reasoning" (provider-profile-reasoning profile)
                                 "stream" (if on-delta t :false)
                                 "usage" (obj "include" t))))
         (headers (cons (cons "Content-Type" "application/json")
                        (and (provider-profile-secret profile)
                             (list (cons "Authorization" (list :secret (provider-profile-secret profile) "Bearer ~a"))))))
         (content (make-string-output-stream))
         (tool-calls '()) (finish nil) (usage (obj)) (stream-error nil)
         (raw '()) (raw-count 0)          ; the last chunks as sent, kept for a failure
         (generation (incf *progress-generation*))
         (output-chars 0) (reasoning-chars 0) (argument-chars 0))
    (labels ((ms () (round (* 1000 (- (get-internal-real-time) start))
                           internal-time-units-per-second))
             (fail (class status reason &optional retry-after)
               (let ((event (append! kernel "provider.call.failed"
                                     (obj "profile" (provider-profile-name profile)
                                          "model" (provider-profile-model profile)
                                          "class" (string-downcase (symbol-name class))
                                          "status" (or status 0)
                                          "reason" (subseq reason 0 (min 300 (length reason)))
                                          "milliseconds" (ms) "work_id" work-id))))
                 ;; What the provider actually sent, so a failure can be read, not guessed.
                 (when *capture-hook*
                   (ignore-errors
                    (funcall *capture-hook* kernel event body
                             (json-encode (obj "failed" reason "finish_reason" (or finish :null) "usage" usage
                                               "raw_stream" (reverse raw)))))))
               (error 'provider-failure :class class :reason reason
                                        :status (or status 0) :retry-after retry-after
                                        :partial (get-output-stream-string content)))
             (progress (&key heartbeat final)
               (when *progress-hook*
                 (ignore-errors
                  (funcall *progress-hook*
                           (obj "generation_id" generation "output_characters" output-chars
                                "reasoning_characters" reasoning-chars
                                "tool_argument_characters" argument-chars
                                "estimated_output_tokens" (ceiling (+ output-chars reasoning-chars argument-chars) 4)
                                "heartbeat" (if heartbeat t :false)
                                "usage" (and final (plusp (hash-table-count usage)) usage))))))
             (take (chunk)
               (let ((err (obj-get chunk "error")))
                 (when err (setf stream-error err)))
               (let ((u (obj-get chunk "usage"))) (when (hash-table-p u) (setf usage u)))
               (let* ((choice (first (obj-get chunk "choices")))
                      (delta (or (obj-get choice "delta") (obj-get choice "message"))))
                 (when choice
                   (let ((text (obj-get delta "content")))
                     (when (and (stringp text) (plusp (length text)))
                       (write-string text content)
                       (incf output-chars (length text))
                       (when on-delta (funcall on-delta text))))
                   ;; Reasoning is counted, never kept or shown (the terminal shows sizes only).
                   (let ((thinking (or (obj-get delta "reasoning") (obj-get delta "reasoning_content"))))
                     (when (stringp thinking) (incf reasoning-chars (length thinking))))
                   (let ((calls (obj-get delta "tool_calls")))
                     (when (consp calls)
                       (dolist (c calls)
                         (let ((args (obj-get (obj-get c "function") "arguments")))
                           (when (stringp args) (incf argument-chars (length args)))))
                       (setf tool-calls (merge-tool-call-deltas tool-calls calls))))
                   (when on-delta (progress))
                   (let ((reason (obj-get choice "finish_reason")))
                     (when (stringp reason) (setf finish reason)))))))
      (multiple-value-bind (status response-headers response-body)
          (handler-case
              (apply (provider-profile-transport profile)
                     (format nil "~a/chat/completions" (provider-profile-base-url profile))
                     headers body
                     :timeout (provider-profile-read-timeout profile)
                     :stream-fn (and on-delta
                                     (lambda (data)
                                       (when (stringp data)
                                         (push data raw)
                                         (when (> (incf raw-count) 60) (setf raw (subseq raw 0 60))))
                                       (if (eq data :heartbeat)
                                           (progress :heartbeat t)
                                           (handler-case (take (json-parse data))
                                             (error () (setf stream-error (obj "message" "malformed stream chunk")))))))
                     (and (provider-profile-local profile) (list :allow-private t)))
            ;; A refused request (no key, wrong host) is configuration: it
            ;; fails visibly rather than retrying.
            (egress-refused (e) (fail :source 0 (egress-reason e)))
            (error (e) (fail :transient 0 (format nil "transport: ~a" e))))
        (unless (eql status 200)
          (let ((retry (%header response-headers "retry-after")))
            (fail (classify-status status) status
                  (format nil "HTTP ~a: ~a" status (or response-body ""))
                  (and retry (ignore-errors (parse-integer (princ-to-string retry)))))))
        (unless on-delta
          (handler-case (take (json-parse response-body))
            (error () (fail :model status "response is not valid JSON"))))
        (when stream-error
          (fail :transient status (format nil "provider error mid-stream: ~a"
                                          (or (obj-get stream-error "message") "unknown"))))
        (let ((text (get-output-stream-string content)))
          (when (and (zerop (length text)) (null tool-calls))
            ;; The provider billed for what it generated, though nothing came of it.
            (let ((microusd (%microusd profile usage (or (obj-get usage "prompt_tokens") 0) (or (obj-get usage "completion_tokens") 0))))
              (when (plusp microusd) (record-spend kernel microusd :work-id work-id :lane lane)))
            ;; Say why it was empty, so a cause can be told from the note:
            ;; out of output tokens on reasoning, or a turn ended with nothing.
            (let* ((completion (obj-get usage "completion_tokens"))
                   (reasoned (obj-get (obj-get usage "completion_tokens_details") "reasoning_tokens"))
                   (why (format nil "empty response (finish: ~a~@[ · ~d completion tokens~]~@[, ~d reasoning~] · ~:d reasoning characters streamed)"
                                (or finish "none") completion reasoned reasoning-chars)))
              (if (and (not grown) (equal finish "length") (integerp completion) (integerp reasoned) (plusp completion)
                       (>= reasoned (* 0.9 completion))
                       (< (provider-profile-max-tokens profile) *room-to-answer-ceiling*))
                  ;; It thought until it ran out of room, with no answer: the allowance was too small for this task.
                  (progn
                    (handler-case (fail :model status why) (provider-failure () nil))
                    (let ((roomier (copy-provider-profile profile)))
                      (setf (provider-profile-max-tokens roomier)
                            (min *room-to-answer-ceiling* (* *room-to-answer-factor* (provider-profile-max-tokens profile))))
                      (return-from complete
                        (complete kernel roomier messages :tools tools :on-delta on-delta :work-id work-id :lane lane :grown t))))
                  (fail :model status why))))
          (let* ((prompt (or (obj-get usage "prompt_tokens") 0))
                 (completion (or (obj-get usage "completion_tokens") 0))
                 (microusd (%microusd profile usage prompt completion)))
            (let ((call (append! kernel "provider.call.completed"
                                 (obj "profile" (provider-profile-name profile)
                                      "model" (provider-profile-model profile)
                                      "estimated_prompt_tokens" estimate
                                      "prompt_tokens" prompt "completion_tokens" completion
                                      "prompt_chars" chars "microusd" microusd
                                      "milliseconds" (ms) "work_id" work-id))))
              (when *capture-hook*
                (funcall *capture-hook* kernel call body
                         (json-encode (obj "content" text "tool_calls" tool-calls
                                           "finish_reason" (or finish "stop") "usage" usage)))))
            (when (plusp microusd)
              (record-spend kernel microusd :work-id work-id :lane lane))
            (when on-delta (progress :final t))
            (obj "content" text "tool_calls" tool-calls "finish_reason" (or finish "stop"))))))))
