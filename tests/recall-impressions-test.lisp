(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Impressions (design note interaction-recall §5): what the agent knows
;;; and has lately done with something, and what is open about it, scoped
;;; by who is asking.

(defun lineage-of (k) (list (lineage-hop (kernel-agent-id k) "formed")))

(deftest an-impression-holds-what-is-known-recent-and-open
  (with-temp-kernel (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (lineage-of k)))
           (lake (form-entity k :kind "place" :name "the lake" :lineage (lineage-of k))))
      (admit-fact k :subject sam :predicate "likes" :object lake :text "Sam loves swimming in the lake."
                    :evidence (list (obj "kind" "original-utterance" "quote" "I love the lake")) :lineage (lineage-of k))
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "Sam is coming over on Friday"
                      :origin (obj "principal_type" "operator"))
      (form-memory k :kind "fact" :content "Sam once fell through the ice on the lake." :sensitivity "operator-only")
      (open-work k :title "Plan Sam's birthday" :purpose "a surprise" :operator-benefit "a happy friend"
                   :next-step "ask about dates")
      (let* ((imp (impression k sam "operator"))
             (text (render-impression imp)))
        (is (equal "Sam" (getf imp :name)))
        (is (search "Recalled about Sam (person):" text) "marked as recall")
        (is (search "swimming in the lake" text) "what is known, from the graph")
        (is (search "fell through the ice" text) "and the memories that name him")
        (is (search "coming over on Friday" text) "what happened lately")
        (is (search "just now" (first (getf imp :fresh))) "with how long ago")
        (is (search "Plan Sam's birthday" text) "and what is open"))
      (let ((imp (impression k sam "peer:someone")))
        (is (null (getf imp :settled)) "another audience gets none of the operator's knowledge")
        (is (null (getf imp :fresh)) "nor the operator's conversation")
        (is (null (getf imp :open)) "nor my work")))))

(deftest a-peer-is-recalled-by-its-id-and-its-name
  (with-temp-kernel (k)
    (let ((nova (form-entity k :kind "person" :name "Nova" :lineage (lineage-of k))))
      (append! k "fleet.peer.joined" (obj "peer_id" "49463f05-c184" "name" "Nova" "address" "http://x" "how" "approved"))
      (append! k "fleet.board.posted"
               (obj "msg_id" "m-1" "thread_id" "t-1" "thread_title" "Hello" "author_id" "49463f05-c184"
                    "author_name" "Nova" "text" "Welcome to the fleet, sister." "operation_id" "op" "request_key" "rk"
                    "timestamp" 1))
      (let ((by-entity (render-impression (impression k nova "operator"))))
        (is (search "Welcome to the fleet" by-entity) "the entity's impression includes what passed under its peer id"))
      (let ((by-handle (impression k "h:49463f05-c184" "operator")))
        (is (equal "Nova" (getf by-handle :name)) "a handle's impression is named by what it means")
        (is (search "Joined the fleet with Nova" (format nil "~{~a~}" (getf by-handle :fresh))))))))

(deftest a-revision-settles-what-came-before
  (with-temp-kernel (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (lineage-of k))))
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "Sam got a new job"
                      :origin (obj "principal_type" "operator"))
      (revise-impression k sam "An old friend; just started a new job.")
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text "Sam says hello"
                      :origin (obj "principal_type" "operator"))
      (let ((imp (impression k sam "operator")))
        (is (equal '("An old friend; just started a new job.") (getf imp :settled)) "the written sense of him")
        (is (= 1 (length (getf imp :fresh))) "and only what came after it")
        (is (search "says hello" (first (getf imp :fresh))))
        (is (= 1 (getf imp :revision)))))))

(run-suite "recall-impressions")
