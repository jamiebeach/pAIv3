(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; A deterministic fake embedder: hashed bag of words in 768 dimensions.
(defun fake-embed (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (dolist (word (split-sequence-words (string-downcase text)) v)
      (incf (nth (mod (sxhash word) 768) v) 1.0))))

(defun split-sequence-words (text)
  (loop with start = 0
        for end = (position-if-not #'alpha-char-p text :start start)
        for word = (subseq text start (or end (length text)))
        when (plusp (length word)) collect word
        while end do (setf start (1+ end))))

(defmacro with-memory ((k) &body body)
  `(with-temp-kernel (,k)
     (let ((*embedder* (lambda (texts) (mapcar #'fake-embed texts)))
           (pai-v3.mind::*embedding-cache* '()))
       (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2))
       ,@body)))

(defun drain (k) (loop repeat 200 until (eq :idle (run-scheduler-step k))))

(defun lisp-cosine (a b)
  (let ((dot 0d0) (ma 0d0) (mb 0d0))
    (loop for x in a for y in b
          do (incf dot (* x y)) (incf ma (* x x)) (incf mb (* y y)))
    (- 1d0 (/ dot (sqrt (* ma mb))))))

(deftest vectors-roundtrip-in-pgvector-format
  (let* ((v (list 1.5 -2.25 0.0 3.0e-5))
         (o (encode-vector v)))
    (is (= 20 (length o)))
    (is (equal '(0 4 0 0) (coerce (subseq o 0 4) 'list)) "pgvector-send-v1 header")
    (is (equal v (decode-vector o)))))

(deftest sql-cosine-matches-exact-lisp-cosine
  (with-memory (k)
    (let* ((a (fake-embed "the garden needs water today"))
           (b (fake-embed "water the garden in the morning"))
           (d (sql-value (kernel-db k) "SELECT pai_cosine_distance(?1,?2)" (encode-vector a) (encode-vector b))))
      (is (< (abs (- d (lisp-cosine a b))) 1d-9)))))

(deftest recall-is-exact-and-ranked
  (with-memory (k)
    (let ((texts '("Sam likes strong coffee in the morning"
                   "The meeting with the bank is on Tuesday"
                   "Sam's garden has tomatoes and basil"
                   "Coffee machine is set to brew at seven"
                   "A short story about a lighthouse keeper")))
      (dolist (text texts) (form-memory k :kind "episode" :content text))
      (drain k)
      (let* ((query "when does the coffee brew")
             (rows (recall k (embed query) "operator" :k 5 :strength-weight 0))
             (expected (sort (mapcar (lambda (text) (cons text (lisp-cosine (fake-embed query) (fake-embed text)))) texts)
                             #'< :key #'cdr)))
        (is (= 5 (length rows)))
        (is (equal (mapcar #'car expected) (mapcar #'second rows)) "same order as exhaustive exact search")))))

(deftest audiences-only-recall-what-they-may-receive
  (with-memory (k)
    (form-memory k :kind "fact" :content "Sam's bank balance and salary details" :sensitivity "operator-only"
                   :topic "the operator's finances")
    (form-memory k :kind "fact" :content "The bank is open on weekdays" :sensitivity "public")
    (drain k)
    (let ((q (embed "tell me about the bank balance")))
      (is (= 2 (length (recall k q "operator"))))
      (is (equal '("The bank is open on weekdays") (mapcar #'second (recall k q "person:guest"))))
      (is (equal '("the operator's finances") (withheld-topics k q "person:guest" :threshold 0.8))
          "bag-of-words fake embeddings are coarser than real ones")
      (is (null (withheld-topics k q "operator"))))))

(deftest recall-feeds-context-and-strengthens-memories
  (with-memory (k)
    (form-memory k :kind "episode" :content "We talked about the lighthouse story" :strength 0.5)
    (drain k)
    (assemble-context k "operator-turn" (list :audience "operator" :task "the lighthouse story"))
    (let ((slots (obj-get (event-payload (event-by-id k (sql-value (kernel-db k) "SELECT event_id FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1" (kernel-agent-id k) "context.assembled"))) "slots")))
      (is (equal "ok" (obj-get (find "recollection" slots :key (lambda (s) (obj-get s "slot")) :test #'equal) "status"))))
    (is (> (sql-value (kernel-db k) "SELECT strength FROM memory_nodes WHERE kind=?1 LIMIT 1" "episode") 0.5)
        "recalled memories grow stronger")))

(deftest embedding-outage-degrades-recall-only
  (with-memory (k)
    (let* ((calls 0)
           (*embedder* (lambda (texts) (declare (ignore texts)) (incf calls) (error "ollama is down")))
           (pai-v3.mind::*embedder-resting-until* 0))
      (let ((messages (assemble-context k "operator-turn" (list :audience "operator" :task "hello"))))
        (is (= 3 (length messages)) "assembly still completes"))
      (assemble-context k "operator-turn" (list :audience "operator" :task "hello again"))
      (is (= 1 calls) "after a failure the service is not called again every turn"))))

(deftest grants-open-a-topic-to-one-person
  (with-memory (k)
    (let ((operator (obj "principal_type" "operator" "auth_method" "passkey"))
          (guest (obj "principal_type" "person")))
      (form-memory k :kind "fact" :content "Dinner plans for Saturday at the lake house"
                     :sensitivity (granted-class "weekend plans") :topic "weekend plans")
      (form-memory k :kind "fact" :content "Sam's bank balance details" :sensitivity "operator-only"
                     :topic "the operator's finances")
      (drain k)
      (let ((q (embed "what are the saturday dinner plans")))
        (is (null (recall k q "person:sam")) "nothing before a grant")
        (signals authority-refused (grant-topic k guest "person:sam" "weekend plans"))
        (grant-topic k operator "person:sam" "weekend plans")
        (is (equal '("Dinner plans for Saturday at the lake house") (mapcar #'second (recall k q "person:sam"))))
        (is (null (recall k q "person:alex")) "a grant is for one principal")
        (is (= 2 (length (recall k q "operator"))) "the operator still sees everything")
        (revoke-topic k operator "person:sam" "weekend plans")
        (is (null (recall k q "person:sam")) "revocation takes effect")))))

(deftest memories-without-embeddings-are-queued-again
  (with-clock
    (with-temp-kernel (k)
      (append! k "memory.formed" (obj "memory_id" "m-lost" "kind" "fact" "content" "Phoebe is a dog."
                                     "sensitivity" "operator-only" "owner" "self" "strength" 0.5d0))
      (is (not (work-pending-p k "memory.embed" "m-lost")) "formed with no embedding queued, as when the service was down")
      (is (= 1 (pai-v3.mind::request-missing-embeddings k)))
      (is (work-pending-p k "memory.embed" "m-lost") "queued again")
      (is (null (pai-v3.mind::request-missing-embeddings k)) "and not looked at again until it is due"))))

(deftest a-recalled-memory-says-where-it-came-from
  (with-temp-kernel (k)
    (change-setting k "operator.name" "Sam" :origin (obj "principal_type" "operator") :reason "test")
    (append! k "memory.formed" (obj "memory_id" "turn-1-2-user-0000" "kind" "observation"
                                   "content" "Nine files, 316 assertions." "sensitivity" "operator-only"
                                   "owner" "self" "strength" 0.5d0))
    (append! k "memory.formed" (obj "memory_id" "m-own" "kind" "insight" "content" "I think best at night."
                                   "sensitivity" "operator-only" "owner" "self" "strength" 0.5d0))
    (is (search "of a message from Sam" (pai-v3.mind::memory-origin k "turn-1-2-user-0000"))
        "something the operator said is remembered as theirs, never as first-hand experience")
    (is (search "a memory from" (pai-v3.mind::memory-origin k "turn-1-2-user-0000")) "with when")
    (is (search "insight I kept" (pai-v3.mind::memory-origin k "m-own")))
    (is (null (pai-v3.mind::memory-origin k "no-such-memory")))))

(run-suite "mind-memory")
