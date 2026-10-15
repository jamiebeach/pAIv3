(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; The working summary (bead v3-clp.8).

(defvar *asked* '())

(defun summary-transport (reply)
  (lambda (url headers body &key stream-fn timeout)
    (declare (ignore url headers stream-fn timeout))
    (push (obj-get (json-parse body) "messages") *asked*)
    (values 200 nil (json-encode (obj "choices" (list (obj "message" (obj "role" "assistant" "content" reply)
                                                           "finish_reason" "stop"))
                                      "usage" (obj "prompt_tokens" 100 "completion_tokens" 50 "cost" 0.0001))))))

(defun chat (k n &key (audience "operator") (words 40))
  (dotimes (i n)
    (receive-turn k :channel "app" :audience audience :speaker "operator"
                    :text (format nil "Turn ~d: ~{~a~^ ~}" i (loop repeat words collect "word"))
                    :origin (obj "principal_type" "operator"))))

(defmacro with-summaries ((k reply) &body body)
  `(with-temp-kernel (,k)
     (let ((*thought-provider* (make-provider-profile :name "test" :base-url "http://fake" :model "m"
                                                      :transport (summary-transport ,reply)))
           (*asked* '())
           (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
       (change-setting ,k "summary.keep_recent_tokens" 500 :origin (obj "principal_type" "operator"))
       (change-setting ,k "summary.fold_tokens" 500 :origin (obj "principal_type" "operator"))
       ,@body)))

(defun drain (k) (loop repeat 20 until (eq :idle (run-scheduler-step k))))

(deftest nothing-to-fold-while-it-all-fits
  (with-summaries (k "S")
    (chat k 3)
    (request-summaries k)
    (drain k)
    (is (null *asked*) "no paid call when the dialogue slot holds it all")
    (is (null (working-summary k "operator")))))

(deftest old-conversation-is-folded-and-shown
  (with-summaries (k "We planned the garden; I said I would order seeds.")
    (chat k 40)
    (request-summaries k)
    (request-summaries k)
    (is (= 1 (length (remove-if-not (lambda (w) (equal "summary" (second w)))
                                    (sql-rows (kernel-db k) "SELECT id,handler FROM v3_work LIMIT 100"))))
        "one fold at a time per audience")
    (drain k)
    (is (= 1 (length *asked*)))
    (let ((row (working-summary k "operator")))
      (is (search "order seeds" (first row)))
      (is (< (third row) (ledger-head k)) "the recent turns are left to the dialogue slot"))
    (let* ((messages (assemble-context k "operator-turn" (list :audience "operator" :task "hi")))
           (now (obj-get (second messages) "content")))
      (is (search "## Earlier, in summary" now))
      (is (search "order seeds" now)))))

(deftest folds-roll-forward-from-the-last-summary
  (with-summaries (k "First summary.")
    (chat k 40)
    (request-summaries k) (drain k)
    (setf (pai-v3.mind::provider-profile-transport *thought-provider*) (summary-transport "Second summary."))
    (chat k 40)
    (request-summaries k) (drain k)
    (is (search "Summary so far:" (obj-get (second (first *asked*)) "content")) "the prior summary is folded in")
    (is (search "First summary." (obj-get (second (first *asked*)) "content")))
    (is (equal "Second summary." (first (working-summary k "operator"))))))

(deftest audiences-are-summarised-apart
  (with-summaries (k "Sam's summary.")
    (chat k 40 :audience "person:sam")
    (request-summaries k) (drain k)
    (is (working-summary k "person:sam"))
    (is (null (working-summary k "operator")) "nothing of Sam's reaches the operator's summary")))

(run-suite "mind-summary")
