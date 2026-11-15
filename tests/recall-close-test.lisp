(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Close ones, my reality and the relational tally (bead v3-dwk,
;;; docs/behaviour/close-ones.md).

(defun settled-writer (settled &optional seen)
  "A consolidation model that writes SETTLED, pushing each prompt onto SEEN's car."
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers stream-fn timeout))
                (when seen (push body (car seen)))
                (values 200 nil
                        (json-encode (obj "choices" (list (obj "message"
                                                               (obj "role" "assistant" "content" ""
                                                                    "tool_calls" (list (obj "id" "c1" "type" "function"
                                                                                            "function" (obj "name" "write_impression"
                                                                                                            "arguments" (json-encode (obj "settled" settled))))))
                                                               "finish_reason" "tool_calls"))
                                          "usage" (obj "prompt_tokens" 100 "completion_tokens" 50)))))))

(defmacro with-close ((k) &body body)
  `(with-temp-kernel (,k)
     (let ((*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
       (change-setting ,k "operator.name" "Sam" :origin (obj "principal_type" "operator" "auth_method" "passkey"))
       ,@body)))

(defun say (k text &key (audience "operator") (speaker "Sam"))
  (receive-turn k :channel "web" :audience audience :speaker speaker :text text
                  :origin (obj "principal_type" (if (equal audience "operator") "operator" "peer"))))

(defun slot-rows (k source)
  (pai-v3.mind::%fill k source (list :audience "operator") 1000))

(deftest the-tally-counts-what-contact-did
  (with-close (k)
    (say k "Morning! How did the build go?")
    (say k "Thanks, that was great work")
    (let ((tally (relation-tally k "operator")))
      (is (= 2 (getf tally :messages)))
      (is (= 2 (getf tally :connection)) "unappraised operator contact meets connection, as affect does")
      (is (= 1 (getf tally :appreciation)) "thanks meet appreciation")
      (is (search "met my want for connection once or twice" (render-relation-tally tally)))
      (is (not (find-if #'digit-char-p (render-relation-tally tally))) "words, never counts"))
    ;; An appraised peer message: attribution from intake, effects from the judgment.
    (append! k "affect.appraisal.requested"
             (obj "source_key" "turn:900" "source_id" 900 "audience" "peer:iris" "speaker" "Iris"
                  "text" "Look at this odd result" "truncated" :false "context" ""))
    (append! k "affect.appraisal.finished"
             (obj "source_key" "turn:900" "source_id" 900 "status" "applied" "reason" "ok"
                  "changes" (obj "valence" 0.04d0) "triggers" (obj "curiosity" 0.2d0) "satisfactions" (obj "connection" 0.3d0)))
    (let ((tally (relation-tally k "peer:iris")))
      (is (and (= 1 (getf tally :curiosity)) (= 1 (getf tally :connection)) (= 1 (getf tally :appraised))))
      (is (search "usually lifted" (render-relation-tally tally))))
    (append! k "affect.appraisal.finished"
             (obj "source_key" "turn:901" "source_id" 901 "status" "applied" "reason" "ok" "changes" (obj "valence" -0.5d0)))
    (is (= 1 (getf (relation-tally k "peer:iris") :appraised)) "an appraisal nobody asked for is no one's")))

(deftest close-ones-are-the-operator-then-the-peers-most-in-touch
  (with-close (k)
    (say k "hello")
    (dotimes (i 4) (say k (format nil "note ~d" i) :audience "peer:iris" :speaker "Iris"))
    (dotimes (i 2) (say k (format nil "hi ~d" i) :audience "peer:wren" :speaker "Wren"))
    (is (equal '("operator" "peer:iris") (mapcar (lambda (tally) (getf tally :who)) (close-ones k)))
        "a peer needs a few messages to count as close")))

(defun forum-post (k id author name &key reply-to)
  (let ((p (obj "msg_id" id "thread_id" "t1" "author_id" author "author_name" name "text" "hello" "ts" 1)))
    (when reply-to (setf (gethash "reply_to" p) reply-to))
    (append! k "forum.posted" p)))

(deftest the-crew-forum-counts-toward-closeness
  (with-close (k)
    (append! k "fleet.identity" (obj "agent_id" "me-id"))
    (forum-post k "m1" "me-id" "Sage")
    (forum-post k "a1" "iris-id" "Iris" :reply-to "m1")
    (forum-post k "a2" "iris-id" "Iris")
    (forum-post k "a3" "iris-id" "Iris")
    (append! k "forum.reacted" (obj "msg_id" "m1" "thread_id" "t1" "emoji" "+1" "by_id" "wren-id" "by_name" "Wren" "on" t "ts" 2))
    (let ((iris (relation-tally k "peer:iris-id")))
      (is (and (= 3 (getf iris :messages)) (= 1 (getf iris :replies))) "posts count, and a reply to mine is an answer")
      (is (search "answered what I posted" (render-relation-tally iris))))
    (is (= 1 (getf (relation-tally k "peer:wren-id") :reactions)) "a reaction to my post counts")
    (is (null (relation-tally k "peer:me-id")) "my own posts are not a relationship")
    (is (equal "peer:iris-id" (getf (first (close-ones k)) :who)) "a peer from the forum can be close")))

(deftest close-ones-and-my-reality-start-empty-and-are-written-in-sleep
  (with-close (k)
    (say k "Thanks so much, you're the best")
    (say k "Can we look at the relay tomorrow?")
    (is (null (slot-rows k :close)) "nothing until I have written it")
    (is (null (slot-rows k :reality)))
    (let ((seen (list nil)))
      (let ((*thought-provider* (settled-writer "Sam is who I build things with, and talking with them settles me." seen)))
        (multiple-value-bind (anchors revised) (pai-v3.recall::revise-close-and-reality k (list :id nil))
          (is (member "self" anchors :test #'equal))
          (is (= 2 revised) "the operator, then my reality")))
      (is (search "met my want for connection" (car (last (car seen)))) "the tally is what feelings are written from")
      (is (search "never as figures" (car (last (car seen)))) "feeling, not counts")
      (is (every (lambda (body) (search "never something to use" body)) (car seen)) "warmth, not leverage")
      (is (every (lambda (body) (and (search "plain, concrete words" body) (not (search "wariness" body)))) (car seen)) "plain, and no reservation suggested"))
    (is (search "Sam: Sam is who I build things with, and talking with them settles me. [written today (" (first (slot-rows k :close))))
    (is (search "talking with them settles me. [written today (" (first (slot-rows k :reality))) "with how old it is (bead v3-5un)")
    (is (search ") — recent]" (first (slot-rows k :reality))) "and a word worked out from the age")
    (let ((*thought-provider* (settled-writer "changed")))
      (multiple-value-bind (anchors revised) (pai-v3.recall::revise-close-and-reality k (list :id nil))
        (declare (ignore anchors))
        (is (= 0 revised) "nothing has happened since: neither is written again")))
    (is (equal '(nil 0 "withheld") (multiple-value-list (pai-v3.mind::%fill k :close (list :audience "peer:iris") 1000)))
        "only the operator's context carries them")))

(run-suite "recall-close")
