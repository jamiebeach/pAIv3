(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Memories from conversation (bead v3-clp.24): finished episodes of talk
;;; with the operator are sealed once into checked episode memories.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun episode-provider (arguments)
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers body stream-fn timeout))
                (values 200 nil
                        (json-encode (obj "choices" (list (obj "message"
                                                               (obj "role" "assistant" "content" ""
                                                                    "tool_calls" (list (obj "id" "c1" "type" "function"
                                                                                            "function" (obj "name" "write_episode"
                                                                                                            "arguments" (json-encode arguments)))))
                                                               "finish_reason" "tool_calls"))
                                          "usage" (obj "prompt_tokens" 100 "completion_tokens" 50)))))))

(defparameter +good+
  (obj "synopsis" "Sam told me about finishing the third draft of North Wind, and we talked about Mira."
       "subjects" (list "North Wind" "writing" "North Wind" "")
       "retrieval_cues" (list "third draft" "Mira")
       "unresolved_threads" (list "Whether the ending works")))

(defun say (k text)
  (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text text
                  :origin (obj "principal_type" "operator")))

(defun answer (k text)
  (deliver-reply k :channel "web" :audience "operator" :text text))

(defmacro with-episodes ((k) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (advance 43200)
       (change-setting ,k "memory.episodes" t :origin *operator*)
       (let ((*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
         ,@body))))

(deftest an-episode-is-cut-at-a-quiet-gap-or-its-size
  (flet ((row (id at) (list id "turn" "Sam" "hello there" (iso8601 at))))
    (let ((rows (list (row 1 1000) (row 2 1100) (row 3 9000) (row 4 9100))))
      (is (equal '(1 2) (mapcar #'first (pai-v3.mind::split-episode rows 9200 :quiet 7200 :max-turns 24 :max-characters 36000)))
          "a later turn after a quiet gap closes it")
      (is (null (pai-v3.mind::split-episode (cddr rows) 9200 :quiet 7200 :max-turns 24 :max-characters 36000))
          "the conversation still going is not finished")
      (is (equal '(3 4) (mapcar #'first (pai-v3.mind::split-episode (cddr rows) 20000 :quiet 7200 :max-turns 24 :max-characters 36000)))
          "quiet since: finished")
      (is (equal '(1) (mapcar #'first (pai-v3.mind::split-episode rows 9200 :quiet 7200 :max-turns 1 :max-characters 36000)))
          "cut at its size"))))

(deftest a-finished-episode-becomes-a-memory-once
  (with-episodes (k)
    (say k "I finished the third draft of North Wind last night.")
    (answer k "That's wonderful. How does Mira's arc feel now?")
    (pai-v3.mind::request-episode-sealing k)
    (is (not (work-pending-p k "memory.seal" "operator")) "not while the conversation may go on")
    (advance 7300)
    (pai-v3.mind::request-episode-sealing k)
    (is (work-pending-p k "memory.seal" "operator") "sealed after two quiet hours")
    (let ((*thought-provider* (episode-provider +good+)))
      (pai-v3.mind::seal-episode k (list :id nil)))
    (let ((row (sql-row (kernel-db k) "SELECT kind,content,topic,owner FROM memory_nodes WHERE kind=?1 LIMIT 1" "episode")))
      (is row "an episode memory was formed")
      (is (search "third draft of North Wind" (second row)))
      (is (search "Still open: Whether the ending works" (second row)))
      (is (equal "North Wind; writing" (third row)) "repeats and blanks are dropped")
      (is (equal "self" (fourth row))))
    (is (null (pai-v3.mind::next-episode k)) "sealed once")))

(deftest an-exchange-with-a-peer-becomes-a-memory-too
  (with-episodes (k)
    (append! k "fleet.board.posted"
             (obj "msg_id" "m-1" "thread_id" "t-1" "thread_title" "Portraits" "author_id" "peer-9"
                  "author_name" "Nova" "text" "Here is my first self-portrait." "operation_id" "op" "request_key" "rk"
                  "timestamp" 1))
    (advance 7300)
    (pai-v3.mind::request-episode-sealing k)
    (is (work-pending-p k "memory.seal" "peer:peer-9") "a finished exchange with a peer is sealed on its own")
    (is (not (work-pending-p k "memory.seal" "operator")) "apart from the conversation with the operator")
    (let ((*thought-provider* (episode-provider (obj "synopsis" "Nova shared her first self-portrait with me on my board."
                                                     "subjects" (list "Nova" "portrait") "retrieval_cues" (list "self-portrait")
                                                     "unresolved_threads" '()))))
      (pai-v3.mind::seal-episode k (list :id nil :entity "peer:peer-9")))
    (let ((row (sql-row (kernel-db k) "SELECT content,sensitivity FROM memory_nodes WHERE kind=?1 LIMIT 1" "episode")))
      (is (search "Nova shared her first self-portrait" (first row)))
      (is (search "Fleet exchange of" (first row)))
      (is (equal "operator-only" (second row)) "kept as the agent's own memory"))
    (is (null (pai-v3.mind::next-episode k "peer:peer-9")) "sealed once")))

(deftest a-poor-write-up-is-not-kept
  (with-episodes (k)
    (say k "Short one.")
    (advance 7300)
    (let ((*thought-provider* (episode-provider (obj "synopsis" "ok" "subjects" nil "retrieval_cues" nil "unresolved_threads" nil))))
      (is (search "too short" (handler-case (progn (pai-v3.mind::seal-episode k (list :id nil)) "")
                                (handler-failure (e) (princ-to-string e))))))
    (is (null (sql-row (kernel-db k) "SELECT 1 FROM memory_nodes WHERE kind=?1 LIMIT 1" "episode")))
    (is (pai-v3.mind::next-episode k) "it is tried again later")))

(deftest off-by-default
  (with-clock
    (with-temp-kernel (k)
      (say k "Hello.")
      (advance 8000)
      (pai-v3.mind::request-episode-sealing k)
      (is (not (work-pending-p k "memory.seal" "operator"))))))

(run-suite "mind-episodes")
