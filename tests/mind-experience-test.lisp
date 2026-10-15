(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defun bag (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (loop with start = 0
          for end = (position-if-not #'alpha-char-p text :start start)
          for word = (string-downcase (subseq text start (or end (length text))))
          when (plusp (length word)) do (incf (nth (mod (sxhash word) 768) v) 1.0)
          while end do (setf start (1+ end)))
    v))

(defmacro with-experience ((k) &body body)
  `(with-temp-kernel (,k)
     (let ((*embedder* (lambda (texts) (mapcar #'bag texts)))
           (pai-v3.mind::*embedding-cache* '()))
       (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2))
       ,@body)))

(defun run-tool (k thought-id name args)
  (funcall (pai-v3.mind::tool-run (gethash name *tools*)) k thought-id args))

(deftest conversations-and-memories-are-searchable
  (with-experience (k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                    :text "The plumber is coming on Thursday about the boiler"
                    :origin (obj "principal_type" "operator"))
    (deliver-reply k :channel "web" :audience "operator" :text "Noted, Thursday for the boiler.")
    (form-memory k :kind "fact" :content "The boiler was serviced last March" :sensitivity "household")
    (let ((rows (search-experience k "boiler" "operator")))
      (is (= 3 (length rows)))
      (is (find "memory" rows :key #'first :test #'equal))
      (is (search "[boiler]" (third (first rows))) "matches are marked in the snippet"))
    (is (= 1 (length (search-experience k "plumber thursday" "operator"))) "all words must match")
    (is (search-experience k "plumber holiday" "operator") "otherwise any word may")
    (is (null (search-experience k "\"quoted) OR (nonsense" "operator")) "odd input is not a syntax error")))

(deftest search-respects-the-audience
  (with-experience (k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                    :text "My salary review is next week" :origin (obj "principal_type" "operator"))
    (receive-turn k :channel "chat" :audience "person:sam" :speaker "Sam"
                    :text "Is the salary review going well?" :origin (obj "principal_type" "person"))
    (form-memory k :kind "fact" :content "Salary review outcome: a raise" :sensitivity "operator-only")
    (form-memory k :kind "fact" :content "Salary reviews happen every spring" :sensitivity "public")
    (is (= 4 (length (search-experience k "salary review" "operator"))) "the operator receives everything")
    (let ((sam (search-experience k "salary review" "person:sam")))
      (is (= 2 (length sam)))
      (is (every (lambda (row) (not (search "raise" (third row)))) sam))
      (is (every (lambda (row) (not (search "next week" (third row)))) sam)
          "Sam never sees the operator's own conversation"))
    (is (= 1 (length (search-experience k "salary review" "person:alex"))) "only public things for a stranger")))

(deftest thinking-is-searchable-in-its-own-audience
  (with-experience (k)
    (let ((private (request-thought k "private" "musing"))
          (guest (request-thought k "responding" "guest" :audience "person:sam")))
      (pai-v3.mind::%record-text k private "model" "I wonder whether the lighthouse story ends well")
      (pai-v3.mind::%record-text k guest "model" "Sam asked about the lighthouse")
      (is (= 2 (length (search-experience k "lighthouse" "operator"))))
      (is (equal '("thinking") (mapcar #'first (search-experience k "lighthouse" "person:sam"))))
      (is (search "Sam asked" (third (first (search-experience k "lighthouse" "person:sam"))))))))

(deftest remember-and-recall-round-trip
  (with-experience (k)
    (let ((id (request-thought k "private" "note it")))
      (is (search "Remembered" (run-tool k id "remember" (obj "content" "Sam prefers green tea in the afternoon"
                                                             "kind" "preference"))))
      (is (search "not a sensitivity" (run-tool k id "remember" (obj "content" "x" "sensitivity" "everyone"))))
      (loop repeat 20 until (eq :idle (run-scheduler-step k)))
      (is (search "green tea" (run-tool k id "recall" (obj "about" "what tea does Sam like"))))
      (let ((guest (request-thought k "responding" "guest" :audience "person:sam")))
        (is (equal "Nothing comes to mind." (run-tool k guest "recall" (obj "about" "what tea does Sam like")))
            "operator-only by default"))
      (let ((*embedder* (lambda (texts) (declare (ignore texts)) (error "ollama is down")))
            (pai-v3.mind::*embedding-cache* '()))
        (is (search "unavailable" (run-tool k id "recall" (obj "about" "something new"))))))))

(run-suite "mind-experience")
