(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defun decision-response (&key (input 100) (output 10) (cost 0.0000042))
  (json-encode (obj "model" "typesafe/jev-1.13-test" "answers" (obj)
                    "usage" (obj "input_tokens" input "output_tokens" output "cost" cost))))

(deftest decisions-use-native-protocol-and-existing-accounting
  (with-temp-kernel (k)
    (let* ((seen nil) (headers-seen nil) (calls 0)
           (raw (decision-response :input 200 :output 30 :cost 0.012345))
           (profile (make-provider-profile :name "typed" :model "typesafe/jev-1.13"
                       :base-url "https://openrouter.ai/api/v1" :secret "openrouter"
                       :transport (lambda (url headers body &key timeout)
                                    (incf calls) (setf seen (json-parse body) headers-seen headers)
                                    (is (equal "https://openrouter.ai/api/alpha/decisions" url))
                                    (is (= timeout 2)) (values 200 nil raw)))))
      (pai-v3.mind::decide k profile "synthetic:key" (obj "message" "synthetic")
                           (obj "test" (obj "type" "score" "instructions" "Assess" "criteria" '("absent" "present"))))
      (is (= calls 1))
      (is (equal "synthetic" (obj-get (obj-get seen "state") "message")))
      (is (null (obj-get seen "messages"))) (is (null (obj-get seen "tools")))
      (is (equal '(:secret "openrouter" "Bearer ~a") (cdr (assoc "Authorization" headers-seen :test #'equal))))
      (is (= 12345 (spent-today k)))
      (is (= 200 (sql-value (kernel-db k) "SELECT tokens FROM provider_calibration WHERE model=?1 LIMIT 1" "typesafe/jev-1.13")))
      (is (equal raw (pai-v3.mind::decision-receipt k "synthetic:key"))))))

(deftest raw-paid-response-is-durable-even-when-semantic-validation-fails
  (with-temp-kernel (k)
    (let* ((raw (decision-response :cost 0.000123))
           (profile (make-provider-profile :name "typed" :model "typesafe/jev-1.13" :base-url "https://fake/api/v1"
                      :transport (lambda (&rest ignore) (declare (ignore ignore)) (values 200 nil raw)))))
      (let ((result (pai-v3.mind::decide k profile "invalid-answers" (obj) (obj))))
        (signals error (pai-v3.mind::%appraisal-judgment result)))
      (is (= 123 (spent-today k)))
      (is (equal raw (pai-v3.mind::decision-receipt k "invalid-answers"))))))

(deftest response-chunks-and-fallback-pricing-are-preserved
  (with-temp-kernel (k)
    (let* ((body (json-parse (decision-response :input 1000 :output 2000 :cost nil)))
           (ignored (setf (gethash "extra" body) (make-string 15000 :initial-element #\Newline)))
           (raw (json-encode body))
           (profile (make-provider-profile :name "typed" :model "typesafe/jev-1.13" :base-url "https://fake/api/v1"
                      :microusd-per-1k-prompt 42
                      :transport (lambda (&rest ignore) (declare (ignore ignore)) (values 200 nil raw)))))
      (declare (ignore ignored))
      (pai-v3.mind::decide k profile "large-receipt" (obj) (obj))
      (is (= 42 (spent-today k)))
      (is (equal raw (pai-v3.mind::decision-receipt k "large-receipt")))
      (is (> (sql-value (kernel-db k) "SELECT count(*) FROM provider_decisions WHERE request_key=?1 LIMIT 1" "large-receipt") 1)))))

(deftest malformed-json-is-kept-and-http-failure-is-not-retried
  (with-temp-kernel (k)
    (let* ((calls 0)
           (profile (make-provider-profile :name "typed" :model "typesafe/jev-1.13" :base-url "https://fake/api/v1"
                      :transport (lambda (&rest ignore) (declare (ignore ignore)) (incf calls) (values 200 nil "not JSON")))))
      (signals provider-failure (pai-v3.mind::decide k profile "malformed" (obj) (obj)))
      (is (equal "not JSON" (pai-v3.mind::decision-receipt k "malformed")))
      (is (= calls 1))
      (setf (pai-v3.mind::provider-profile-transport profile)
            (lambda (&rest ignore) (declare (ignore ignore)) (incf calls) (values 429 nil "rate limited")))
      (signals provider-failure (pai-v3.mind::decide k profile "limited" (obj) (obj)))
      (is (= calls 2))
      (is (= 0 (spent-today k))))))

(run-suite "mind-decisions")
