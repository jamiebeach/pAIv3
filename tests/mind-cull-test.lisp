(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Culling the optional context (bead v3-okh.16).

(defparameter *operator* (obj "principal_type" "operator"))

(defun cull-judge (&key (fail nil) (calls (list 0)))
  "A Decisions provider that finds rows mentioning UNRELATED unrelated, and the rest relevant."
  (make-provider-profile
   :name "affect-jev" :model "typesafe/jev-1.13" :base-url "https://openrouter.ai/api/v1"
   :secret "openrouter" :microusd-per-1k-prompt 42
   :transport (lambda (url headers body &key timeout)
                (declare (ignore url headers timeout))
                (incf (car calls))
                (if fail
                    (values 500 nil "{}")
                    (let ((answers (obj)) (questions (obj-get (json-parse body) "questions")))
                      (maphash (lambda (name q)
                                 (let* ((unrelated (search "UNRELATED" (obj-get q "instructions")))
                                        (level (if unrelated 0 2)) (p (obj)))
                                   (dotimes (i 3) (setf (gethash (write-to-string i) p) (if (= i level) 1d0 0d0)))
                                   (setf (gethash name answers) (obj "type" "score" "score" level "confidence" 1d0 "probabilities" p))))
                               questions)
                      (values 200 nil (json-encode (obj "model" "typesafe/jev-test" "answers" answers
                                                        "usage" (obj "input_tokens" 10 "output_tokens" 1 "cost" 0.000001)))))))))

(defun rows-of (n &key (unrelated '()))
  (loop for i from 0 below n
        collect (list (format nil "row ~d~:[~; UNRELATED~]" i (member i unrelated)) 5)))

(deftest unrelated-rows-are-left-out-once-per-thought
  (with-temp-kernel (k)
    (let* ((calls (list 0))
           (pai-v3.mind::*decision-profile* (cull-judge :calls calls))
           (thought (request-thought k "introspection" "SYNTHETIC think about the camera"))
           (request (list :thought-id thought :task "SYNTHETIC think about the camera" :audience "operator"))
           (rows (rows-of 10 :unrelated '(1 6 8))))
      (let ((kept (pai-v3.mind::cull-rows k :activity request rows)))
        (is (= 8 (length kept)) "rows 6 and 8 left out")
        (is (find "row 1 UNRELATED" kept :key #'first :test #'equal) "the newest five are never judged")
        (is (null (find "row 6 UNRELATED" kept :key #'first :test #'equal))))
      (pai-v3.mind::cull-rows k :activity request rows)
      (is (= 1 (car calls)) "judged once; every later call of the thought reuses it")
      (is (equal rows (pai-v3.mind::cull-rows k :dialogue request rows)) "a slot not listed is never culled"))))

(deftest a-failed-check-keeps-everything-and-says-so
  (with-temp-kernel (k)
    (let* ((pai-v3.mind::*decision-profile* (cull-judge :fail t))
           (thought (request-thought k "introspection" "SYNTHETIC"))
           (rows (rows-of 9 :unrelated '(7))))
      (is (= 9 (length (pai-v3.mind::cull-rows k :recollection (list :thought-id thought :task "x") rows))))
      (is (equal "kept all" (sql-value (kernel-db k) "SELECT status FROM context_culls WHERE thought_id=?1 LIMIT 1" thought))))))

(deftest culling-can-be-turned-off
  (with-temp-kernel (k)
    (change-setting k "context.cull.enabled" :false :origin *operator*)
    (let* ((calls (list 0)) (pai-v3.mind::*decision-profile* (cull-judge :calls calls))
           (thought (request-thought k "introspection" "SYNTHETIC"))
           (rows (rows-of 9 :unrelated '(7))))
      (is (= 9 (length (pai-v3.mind::cull-rows k :activity (list :thought-id thought :task "x") rows))))
      (is (= 0 (car calls))))))

(run-suite "mind-cull")
