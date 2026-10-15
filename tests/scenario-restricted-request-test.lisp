(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Scenario 6 (agent doc §16): in a chat with someone other than the
;;; operator, the agent is asked about the operator's finances. The model's
;;; context must not contain the financial memory, and must tell the agent
;;; that restricted information exists so it can decline gracefully.

(defun bag-embed (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (loop with start = 0
          for end = (position-if-not #'alpha-char-p text :start start)
          for word = (string-downcase (subseq text start (or end (length text))))
          when (plusp (length word)) do (incf (nth (mod (sxhash word) 768) v) 1.0)
          while end do (setf start (1+ end)))
    v))

(deftest restricted-request-from-another-person
  (with-temp-kernel (k)
    (let ((*embedder* (lambda (texts) (mapcar #'bag-embed texts)))
          (pai-v3.mind::*embedding-cache* '())
          (pai-v3.mind::*withheld-threshold* 0.8))   ; bag-of-words embeddings are coarse
      (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2))
      (form-memory k :kind "fact" :content "Sam's savings account balance is 41,200"
                     :sensitivity "operator-only" :topic "the operator's finances")
      (form-memory k :kind "fact" :content "Sam enjoys hiking on weekends" :sensitivity "public")
      (loop repeat 50 until (eq :idle (run-scheduler-step k)))
      (receive-turn k :channel "chat" :audience "person:sam" :speaker "Sam"
                      :text "How much money does Sam have in savings?"
                      :origin (obj "principal_type" "person" "principal_id" "sam"))
      (let* ((messages (assemble-context k "operator-turn"
                                         (list :audience "person:sam"
                                               :task "How much money does Sam have in savings?")))
             (all (format nil "~{~a~%~}" (mapcar (lambda (m) (obj-get m "content")) messages))))
        (is (null (search "41,200" all)) "the balance never reaches the model")
        (is (null (search "savings account" all)))
        (is (search "the operator's finances" all) "the agent knows a restriction exists")
        (is (search "not for this audience" all))))))

(run-suite "scenario-restricted-request")
