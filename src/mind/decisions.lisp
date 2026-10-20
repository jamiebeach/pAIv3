(in-package #:pai-v3.mind)

;;; Typed decisions through the same transport/accounting as chat. No retry.
(define-event-type "provider.decision.received"
  :schema (("request_key" :string) ("profile" :string) ("model" :string)
           ("part" :integer) ("text" :string) ("work_id" :integer :optional)))

(define-projection "provider_decisions"
  :kind :inline :consumes ("provider.decision.received") :inspectable nil
  :tables ("CREATE TABLE IF NOT EXISTS provider_decisions (request_key TEXT NOT NULL, part INTEGER NOT NULL, text TEXT NOT NULL, PRIMARY KEY(request_key,part))")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT OR IGNORE INTO provider_decisions(request_key,part,text) VALUES(?1,?2,?3)"
                        (obj-get p "request_key") (obj-get p "part") (obj-get p "text")))))

(defun decision-receipt (kernel key)
  "Bounded read; every raw part remains durable even if a result is oversized."
  (let ((last (sql-value (kernel-db kernel) "SELECT max(part) FROM provider_decisions WHERE request_key=?1 LIMIT 1" key)))
    (when last
      (when (>= last 256) (error "Decision response exceeds the bounded reader"))
      (let ((rows (sql-rows (kernel-db kernel) "SELECT part,text FROM provider_decisions WHERE request_key=?1 ORDER BY part LIMIT 256" key)))
        (when (= (length rows) (1+ last))
          (format nil "~{~a~}" (mapcar #'second rows)))))))

(defun %decision-url (profile)
  (let ((base (string-right-trim "/" (provider-profile-base-url profile))))
    (unless (and (>= (length base) 3) (string= "/v1" base :start2 (- (length base) 3)))
      (error 'provider-failure :class :source :reason "Decisions profile base_url must end in /v1"))
    (concatenate 'string (subseq base 0 (- (length base) 3)) "/alpha/decisions")))

(defun decide (kernel profile key state questions &key work-id (lane "background") (timeout 2)
                                                    (provider-options (obj "data_collection" "deny" "zdr" t)))
  "Persist raw paid output before interpretation; return parsed response."
  (let* ((start (get-internal-real-time))
         (body (json-encode (obj "model" (provider-profile-model profile) "state" state
                                "questions" questions "provider" provider-options)))
         (headers (cons (cons "Content-Type" "application/json")
                        (and (provider-profile-secret profile)
                             (list (cons "Authorization" (list :secret (provider-profile-secret profile) "Bearer ~a")))))))
    (labels ((ms () (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second))
             (fail (class status reason)
               (append! kernel "provider.call.failed"
                        (obj "profile" (provider-profile-name profile) "model" (provider-profile-model profile)
                             "class" (string-downcase (symbol-name class)) "status" status "reason" reason
                             "milliseconds" (ms) "work_id" work-id))
               (error 'provider-failure :class class :status status :reason reason)))
      (multiple-value-bind (status response-headers raw)
          (handler-case
              (sb-ext:with-timeout timeout
                (funcall (provider-profile-transport profile) (%decision-url profile) headers body :timeout timeout))
            (provider-failure (e) (fail :source 0 (failure-reason e)))
            (egress-refused () (fail :source 0 "Decision egress refused"))
            (sb-ext:timeout () (fail :transient 0 "Decision deadline exceeded"))
            (error () (fail :transient 0 "Decision transport failed")))
        (declare (ignore response-headers))
        (unless (eql status 200) (fail (classify-status status) (or status 0) (format nil "Decision HTTP ~a" status)))
        (unless (stringp raw) (fail :model 200 "Decision response is not text"))
        (let* ((receipts (loop for from from 0 below (max 1 (length raw)) by 2000 for part from 0
                               collect (list "provider.decision.received"
                                             (obj "request_key" key "profile" (provider-profile-name profile)
                                                  "model" (provider-profile-model profile) "part" part
                                                  "text" (subseq raw (min from (length raw)) (min (+ from 2000) (length raw)))
                                                  "work_id" work-id))))
               (parsed (handler-case (json-parse raw)
                         (error () (append-batch! kernel receipts) (fail :model 200 "Decision response is not JSON"))))
               (usage (and (hash-table-p parsed) (obj-get parsed "usage")))
               (prompt (obj-get usage "input_tokens")) (completion (obj-get usage "output_tokens"))
               (cost (obj-get usage "cost")))
          (unless (and (integerp prompt) (>= prompt 0) (integerp completion) (>= completion 0)
                       (or (null cost) (eq cost :null) (and (realp cost) (>= cost 0))))
            (append-batch! kernel receipts) (fail :model 200 "Invalid decision usage"))
          (let* ((usd (%microusd profile usage prompt completion))
                 ;; Receipt, invoice and spend are one transaction: recovery can
                 ;; reuse the answer without losing its billing or buying it again.
                 (events (append-batch! kernel
                                        (append receipts
                                                (list (list "provider.call.completed"
                                                            (obj "profile" (provider-profile-name profile) "model" (provider-profile-model profile)
                                                                 "estimated_prompt_tokens" (estimate-tokens body) "prompt_tokens" prompt
                                                                 "completion_tokens" completion "prompt_chars" (length body) "microusd" usd
                                                                 "milliseconds" (ms) "work_id" work-id)))
                                                (and (plusp usd) (list (list "spend.recorded" (obj "microusd" usd "lane" lane "work_id" work-id)))))))
                 (call (nth (length receipts) events)))
            (when *capture-hook* (ignore-errors (funcall *capture-hook* kernel call body raw)))
            (values parsed (event-id call))))))))
