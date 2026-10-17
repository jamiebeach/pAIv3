(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Sleep (design note interaction-recall §5a): once a night, what has come
;;; to mind lately is written anew; written impressions can come to mind by
;;; meaning; and a due intention, coming to mind, may interrupt.

(defparameter *op* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun impression-writer (settled)
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers body stream-fn timeout))
                (values 200 nil
                        (json-encode (obj "choices" (list (obj "message"
                                                               (obj "role" "assistant" "content" ""
                                                                    "tool_calls" (list (obj "id" "c1" "type" "function"
                                                                                            "function" (obj "name" "write_impression"
                                                                                                            "arguments" (json-encode (obj "settled" settled))))))
                                                               "finish_reason" "tool_calls"))
                                          "usage" (obj "prompt_tokens" 100 "completion_tokens" 50)))))))

(defun bag-of-words (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (loop with start = 0
          for end = (position-if-not #'alpha-char-p text :start start)
          for word = (string-downcase (subseq text start (or end (length text))))
          when (plusp (length word)) do (incf (nth (mod (sxhash word) 768) v) 1.0)
          while end do (setf start (1+ end)))
    v))

(defmacro with-sleep ((k) &body body)
  `(with-temp-kernel (,k)
     (let ((*embedder* (lambda (texts) (mapcar #'bag-of-words texts)))
           (pai-v3.mind::*embedding-cache* '())
           (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
       (setf pai-v3.mind::*embedder-resting-until* 0)
       (remhash ,k pai-v3.mind::*embedding-backfill-at*)
       ,@body)))

(defun sam-with-news (k)
  (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (list (lineage-hop (kernel-agent-id k) "formed")))))
    (dolist (text '("Sam started a pottery class" "Sam made a blue bowl" "Sam wants to sell bowls at the market"))
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text text
                      :origin (obj "principal_type" "operator")))
    (append! k "recall.recollected" (obj "hook" "stimulus" "anchors" (list sam) "cue" "Sam"))
    sam))

(deftest sleep-comes-once-a-night-in-the-operators-early-hours
  (with-sleep (k)
    (change-setting k "consolidation.enabled" t :origin *op*)
    (let ((hour (parse-integer (local-slot (now) (local-time:find-timezone-by-location-name (setting k "time.zone"))) :start 11 :end 13)))
      (change-setting k "consolidation.hour" (mod (+ hour 5) 24) :origin *op*)
      (pai-v3.mind::request-consolidation k)
      (is (not (work-pending-p k "consolidate" "impressions")) "not outside sleep hours")
      (change-setting k "consolidation.hour" hour :origin *op*)
      (pai-v3.mind::request-consolidation k)
      (is (work-pending-p k "consolidate" "impressions") "in sleep hours, each task once")
      (append! k "consolidation.ran" (obj "day" (pai-v3.mind::%sleep-day k) "task" "impressions"))
      (is (pai-v3.mind::consolidation-ran-p k (pai-v3.mind::%sleep-day k) "impressions") "recorded for the night"))))

(deftest sleep-writes-the-sense-of-someone-anew
  (with-sleep (k)
    (let ((sam (sam-with-news k)))
      (let ((*thought-provider* (impression-writer "Sam is an old friend who has taken up pottery and dreams of selling bowls.")))
        (is (pai-v3.recall::revise-in-sleep k sam (list :id nil)) "enough has happened since to write it anew"))
      (let ((imp (impression k sam "operator")))
        (is (equal '("Sam is an old friend who has taken up pottery and dreams of selling bowls.") (getf imp :settled)))
        (is (null (getf imp :fresh)) "what was folded in is no longer fresh")
        (is (= 1 (getf imp :revision))))
      (let ((*thought-provider* (impression-writer "unchanged")))
        (is (not (pai-v3.recall::revise-in-sleep k sam (list :id nil))) "nothing new: not revised again"))
      (pai-v3.mind::request-missing-embeddings k :every 0)
      (loop repeat 200 until (eq :idle (run-scheduler-step k)))
      (is (equal sam (first (first (pai-v3.recall::impressions-near k (embed "who dreams of selling pottery bowls")))))
          "a written impression can come to mind by meaning"))))

(deftest a-due-intention-interrupts-once
  (with-sleep (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (list (lineage-hop (kernel-agent-id k) "formed"))))
           (today (subseq (local-slot (now) (local-time:find-timezone-by-location-name (setting k "time.zone"))) 0 10)))
      (pai-v3.recall::form-intention k "Sam" "wish him luck at the market" :until today)
      (pai-v3.recall::form-intention k "Sam" "ask about the kiln" :until "2999-01-01")
      (pai-v3.recall::prompt-due-intentions k (list sam) "peer:someone")
      (is (null (sql-rows (kernel-db k) "SELECT id FROM thoughts WHERE origin=?1 LIMIT 5" "unbidden")) "not from a peer's thought")
      (pai-v3.recall::prompt-due-intentions k (list sam) "operator")
      (let ((unbidden (sql-rows (kernel-db k) "SELECT reason FROM thoughts WHERE origin=?1 LIMIT 5" "unbidden")))
        (is (= 1 (length unbidden)) "the due one, not the one not yet due")
        (is (search "wish him luck at the market" (first (first unbidden)))))
      (pai-v3.recall::prompt-due-intentions k (list sam) "operator")
      (is (= 1 (length (sql-rows (kernel-db k) "SELECT id FROM thoughts WHERE origin=?1 LIMIT 5" "unbidden"))) "once"))))

(run-suite "recall-sleep")
