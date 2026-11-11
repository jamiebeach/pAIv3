(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Interaction recall (design note interaction-recall): what the agent
;;; does or meets brings its impression to mind, once, when it matters.

(defun hop (k) (list (lineage-hop (kernel-agent-id k) "formed")))

(defun word-bag (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (loop with start = 0
          for end = (position-if-not #'alpha-char-p text :start start)
          for word = (string-downcase (subseq text start (or end (length text))))
          when (plusp (length word)) do (incf (nth (mod (sxhash word) 768) v) 1.0)
          while end do (setf start (1+ end)))
    v))

(defmacro with-recall ((k) &body body)
  `(with-temp-kernel (,k)
     (let ((*embedder* (lambda (texts) (mapcar #'word-bag texts)))
           (pai-v3.mind::*embedding-cache* '()))
       (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
             pai-v3.mind::*embedder-resting-until* 0)
       (remhash ,k pai-v3.mind::*embedding-backfill-at*)
       ,@body)))

(defun context-text (k audience task)
  (format nil "~{~a~%~}" (mapcar (lambda (m) (obj-get m "content"))
                                 (assemble-context k (if (equal audience "operator") "operator-turn" "private-cognition")
                                                   (list :audience audience :task task)))))

(deftest writing-to-a-peer-brings-what-passed-between-you
  ;; The case that prompted the design: an agent wrote to its sister as a
  ;; stranger, hours after they had exchanged welcomes.
  (with-recall (k)
    (form-entity k :kind "person" :name "Nova" :lineage (hop k))
    (append! k "fleet.peer.joined" (obj "peer_id" "49463f05-c184" "name" "Nova" "address" "http://x" "how" "approved"))
    (append! k "fleet.board.posted"
             (obj "msg_id" "m-1" "thread_id" "t-1" "thread_title" "Hello" "author_id" "49463f05-c184"
                  "author_name" "Nova" "text" "Welcome to the fleet, sister." "operation_id" "op" "request_key" "rk"
                  "timestamp" 1))
    (let* ((thought (request-thought k "work" "share the portrait" :audience "operator"))
           (note (pai-v3.recall::recollection-note k thought "post_fleet_message"
                                                    "{\"peer_id\":\"49463f05-c184\",\"text\":\"Here is my portrait.\"}"
                                                    nil "Posted to Nova's board.")))
      (is (search "Recalled about Nova (person):" note) "acting toward a peer by its id brings her to mind")
      (is (search "Welcome to the fleet, sister." note) "with what passed between you, and when")
      (is (null (pai-v3.recall::recollection-note k thought "post_fleet_message" "{\"peer_id\":\"49463f05-c184\"}" nil "Posted."))
          "once per thought")
      (is (equal (list (first (first (find-entities k "Nova")))) (pai-v3.recall::recalled-in k thought))
          "and the recollection is recorded"))))

(deftest a-number-is-no-cue-and-a-step-note-is-capped
  ;; A value stored as an entity ("15") came to mind beside every result that
  ;; held a 15, and two impressions ran to 10,000 characters (2026-11-11).
  (with-recall (k)
    (form-entity k :kind "attribute_value" :name "15" :lineage (hop k))
    (let ((nova (form-entity k :kind "person" :name "Nova" :lineage (hop k))))
      (dotimes (i 40)
        (admit-fact k :subject nova :predicate "likes"
                      :object (form-entity k :kind "person" :name (format nil "Friend ~r" i) :lineage (hop k))
                      :text (format nil "Nova likes Friend ~r, ~a." i (make-string 900 :initial-element #\x))
                      :evidence (list (obj "kind" "original-utterance" "quote" "x")) :lineage (hop k))))
    (let ((thought (request-thought k "work" "read a page" :audience "operator")))
      (is (null (pai-v3.recall::recollection-note k thought "web_fetch" "{}" nil "Page: 15 items, 15 more."))
          "a bare number brings nothing to mind")
      (let ((note (pai-v3.recall::recollection-note k thought "web_fetch" "{}" nil "A page that mentions Nova.")))
        (is (search "Recalled about Nova" note))
        (is (<= (length note) (+ pai-v3.recall::*step-note-chars* 200)) "capped")
        (is (search "More is recalled about Nova" note))))))

(deftest a-thought-about-someone-starts-with-what-is-known
  (with-recall (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (hop k)))
          (lake (form-entity k :kind "place" :name "the lake" :lineage (hop k))))
      (admit-fact k :subject sam :predicate "likes" :object lake :text "Sam loves swimming in the lake."
                    :evidence (list (obj "kind" "original-utterance" "quote" "I love the lake")) :lineage (hop k))
      (let ((text (context-text k "operator" "How is Sam doing?")))
        (is (search "What comes to mind" text))
        (is (search "Recalled about Sam (person):" text))
        (is (search "Sam loves swimming in the lake." text) "what the graph knows"))
      (is (null (search "swimming" (context-text k "peer:someone" "How is Sam doing?")))
          "a peer's thought gets none of the operator's knowledge"))))

(deftest meaning-brings-up-what-no-name-did
  (with-recall (k)
    (change-setting k "recall.association_distance" 0.95d0 :origin (obj "principal_type" "operator") :reason "test")
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (hop k)))
          (lee (form-entity k :kind "person" :name "Lee" :lineage (hop k))))
      (admit-fact k :subject lee :predicate "sibling_of" :object sam :text "Lee is Sam's sister, the one who paints."
                    :evidence (list (obj "kind" "original-utterance" "quote" "my sister Lee")) :lineage (hop k))
      (pai-v3.mind::request-missing-embeddings k :every 0)
      (loop repeat 200 until (eq :idle (run-scheduler-step k)))
      (let ((text (context-text k "operator" "Who paints in the family?")))
        (is (search "Recalled about Lee" text) "a fact near in meaning brings up who it is about")))))

(run-suite "recall-mechanism")
