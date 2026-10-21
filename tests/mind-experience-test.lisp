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

(deftest private-thinking-is-not-indexed-as-shared-with-the-stimulating-entity
  (with-experience (k)
    (let ((private (request-thought k "private" "musing"))
          (guest (request-thought k "responding" "guest" :audience "person:sam")))
      (pai-v3.mind::%record-text k private "model" "I wonder whether the lighthouse story ends well")
      (pai-v3.mind::%record-text k guest "model" "Sam asked about the lighthouse")
      (is (= 2 (length (search-experience k "lighthouse" "operator"))))
      (is (null (search-experience k "lighthouse" "person:sam")))
      (is (search "Sam asked" (third (first (search-experience k "lighthouse" "operator"))))))))

(deftest remember-and-recall-round-trip
  (with-experience (k)
    (let ((id (request-thought k "private" "note it")))
      (is (search "Remembered" (run-tool k id "remember" (obj "content" "Sam prefers green tea in the afternoon"
                                                             "kind" "preference"))))
      (is (search "not a sensitivity" (run-tool k id "remember" (obj "content" "x" "sensitivity" "everyone"))))
      (loop repeat 20 until (eq :idle (run-scheduler-step k)))
      (is (search "green tea" (run-tool k id "recall" (obj "about" "what tea does Sam like"))))
      (let ((guest (request-thought k "responding" "guest" :audience "person:sam")))
        (is (search "green tea" (run-tool k guest "recall" (obj "about" "what tea does Sam like")))
            "private recall does not depend on the stimulus source"))
      (let ((*embedder* (lambda (texts) (declare (ignore texts)) (error "ollama is down")))
            (pai-v3.mind::*embedding-cache* '()))
        (is (search "unavailable" (run-tool k id "recall" (obj "about" "something new"))))))))

(deftest interactions-are-found-by-identifier-and-name
  (with-experience (k)
    (append! k "fleet.peer.joined" (obj "peer_id" "49463f05-c184" "name" "Nova" "address" "http://x" "how" "approved"))
    (append! k "fleet.board.posted"
             (obj "msg_id" "m-1" "thread_id" "t-77aa" "thread_title" "Portraits" "author_id" "49463f05-c184"
                  "author_name" "Nova" "text" "Here is my new portrait." "operation_id" "op" "request_key" "rk"
                  "timestamp" 1 "images" (list "abcdef0123456789abcdef0123456789")))
    (append! k "image.generated" (obj "image_id" "abcdef0123456789abcdef0123456789" "prompt" "a lighthouse at dusk"
                                      "model" "m" "width" 1 "height" 1 "cost_usd" 0.001d0))
    (is (equal '(("49463f05-c184" "Nova")) (pai-v3.mind::handles-in k "writing to 49463f05-c184 now"))
        "a peer id means its name")
    (is (search "lighthouse" (second (first (pai-v3.mind::handles-in k "image ABCDEF0123456789ABCDEF0123456789")))))
    (is (search "board thread" (second (first (pai-v3.mind::handles-in k "in t-77aa")))))
    (let ((by-id (pai-v3.mind::experience-mentioning k (list "49463f05-c184") "operator")))
      (is (= 2 (length by-id)) "the joining and the post both name the peer's id")
      (is (search "portrait" (third (first by-id))) "newest first"))
    (is (search "lighthouse" (third (first (pai-v3.mind::experience-mentioning k (list "abcdef0123456789abcdef0123456789") "operator"))))
        "the image made, and the post that carried it")
    (is (pai-v3.mind::experience-mentioning k (list "Nova") "operator") "and by name")
    (is (null (pai-v3.mind::experience-mentioning k (list "Nova") "peer:someone-else"))
        "another peer does not see what passed with this one")))

(deftest interactions-are-embedded-and-found-by-meaning
  (with-experience (k)
    (setf pai-v3.mind::*embedder-resting-until* 0)
    (remhash k pai-v3.mind::*embedding-backfill-at*)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                    :text "The garden needs water every morning" :origin (obj "principal_type" "operator"))
    (deliver-reply k :channel "web" :audience "operator" :text "Working on it" :interim t)
    (deliver-reply k :channel "web" :audience "operator" :text "The boiler service is booked for Thursday")
    (is (= 2 (pai-v3.mind::request-missing-embeddings k)) "a status line between steps is not embedded")
    (loop repeat 200 until (eq :idle (run-scheduler-step k)))
    (let ((near (pai-v3.mind::experience-near k (embed "water the garden in the morning") "operator")))
      (is (search "garden" (third (first near))) "the nearest in meaning comes first")
      (is (< (second (first near)) (second (second near)))))
    (is (null (pai-v3.mind::experience-near k (embed "garden") "peer:someone")) "scoped by audience")
    (is (zerop (pai-v3.mind::request-missing-embeddings k :every 0)) "nothing is left to embed")))

(deftest a-note-about-pictures-carries-their-ids
  (with-experience (k)
    (let* ((id (request-thought k "private" "note it"))
           (picture (pai-v3.mind::store-image k "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="))
           (said (run-tool k id "remember" (obj "content" "My portrait, rendered at dusk." "images" (list picture)))))
      (is (search "Remembered" said))
      (is (sql-value (kernel-db k) "SELECT 1 FROM memory_nodes WHERE content LIKE ?1 LIMIT 1" (format nil "%(pictures: ~a)%" picture))
          "the picture's id is in the memory, so recall and search lead back to it")
      (let ((refused (run-tool k id "remember" (obj "content" "A portrait" "images" (list "m-RJWDWLQX21YI")))))
        (is (search "not an image I hold" refused) "a memory id is not a picture's")
        (is (search "nothing was remembered" refused))
        (is (search "m-RJWDWLQX21YI" refused)))
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM memory_nodes WHERE content LIKE ?1 LIMIT 1" "%A portrait%")) "and nothing was kept"))))

(deftest asking-to-use-a-memory-id-as-a-picture-says-what-went-wrong
  (with-experience (k)
    (let ((id (request-thought k "private" "my face")))
      (let ((said (run-tool k id "set_avatar" (obj "image_id" "f-Cl8oeHWCPjBZ"))))
        (is (search "not an image I hold" said))
        (is (search "fact id (f-...)" said) "it says what the id was")
        (is (search "with no image_id to list" said) "and how to find the pictures"))
      (is (search "memory id (m-...)" (run-tool k id "look_at" (obj "images" (list "m-RJWDWLQX21YI"))))))))

(run-suite "mind-experience")
