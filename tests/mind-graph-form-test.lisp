(load "/v3/tests/support-graph.lisp")
(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Forming the graph from conversation (bead v3-clp.6, stage 3): one call,
;;; every item checked on its own, the rest kept, what was dropped recorded.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun proposal-provider (proposal)
  "A provider that answers with one record_knowledge call carrying PROPOSAL."
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers body stream-fn timeout))
                (values 200 nil
                        (json-encode (obj "choices" (list (obj "message"
                                                               (obj "role" "assistant" "content" ""
                                                                    "tool_calls" (list (obj "id" "c1" "type" "function"
                                                                                            "function" (obj "name" "record_knowledge"
                                                                                                            "arguments" (json-encode proposal)))))
                                                               "finish_reason" "tool_calls"))
                                          "usage" (obj "prompt_tokens" 100 "completion_tokens" 50)))))))

(defun say (k text)
  (receive-turn k :channel "web" :audience "operator" :speaker "Operator" :text text
                  :origin (obj "principal_type" "operator")))

(defmacro with-formation ((k) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (advance 43200)
       (let ((*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
         ,@body))))

(deftest formation-keeps-what-checks-out-and-says-what-it-dropped
  (with-formation (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (list (lineage-hop "beta" "formed"))))
           (turn (say k "I finished the third draft of North Wind last night, and Mira is my favourite character."))
           (s (format nil "S~d" (event-id turn)))
           (*thought-provider*
             (proposal-provider
              (obj "entities" (list (obj "ref" "e1" "name" "Sam" "kind" "person" "known_id" sam)
                                    (obj "ref" "e2" "name" "North Wind" "kind" "artifact")
                                    (obj "ref" "e3" "name" "Mira" "kind" "person"))
                   "facts" (list (obj "subject" "e1" "predicate" "authored" "object" "e2" "statement" "Sam is writing North Wind."
                                      "time" "ongoing-state" "source" s "quote" "finished the third draft of North Wind")
                                 (obj "subject" "e3" "predicate" "part_of" "object" "e2" "statement" "Mira is a character in North Wind."
                                      "source" s "quote" "Mira is my favourite character")
                                 (obj "subject" "e1" "predicate" "related_to" "object" "e3" "statement" "Sam likes Mira."
                                      "source" s "quote" "Mira is my absolute favourite")
                                 (obj "subject" "e2" "predicate" "authored" "object" "e1" "statement" "Backwards."
                                      "source" s "quote" "North Wind"))))))
      (form-graph k (list :id nil))
      (is (= 1 (length (find-entities k "sam"))) "the known entity is matched, not duplicated")
      (is (null (find-entities k "mira")) "an entity whose facts were all dropped is never formed")
      (is (find-entities k "North Wind"))
      (let* ((book (first (first (find-entities k "north wind"))))
             (facts (entity-facts k book)))
        (is (= 1 (length facts)) (format nil "only the fact that checks out; got ~s" facts))
        (is (equal "Sam is writing North Wind." (tenth (first facts))))
        (is (equal (kernel-agent-id k) (sql-value (kernel-db k) "SELECT path FROM graph_facts WHERE id=?1 LIMIT 1" (first (first facts))))
            "formed here")
        (is (eql (event-id turn) (third (first (fact-evidence k (first (first facts))))))
            "its quote points at the turn it came from"))
      (let ((run (sql-row (kernel-db k) "SELECT facts_admitted,dropped,through_event_id FROM graph_formation_runs WHERE event_id>?1 LIMIT 1" 0)))
        (is (= 1 (first run)))
        (is (= 3 (second run)) "Mira's part_of (a person cannot be part of an artifact), the made-up quote, and the backwards fact")
        (is (= (event-id turn) (third run)) "the conversation is marked as read")))))

(deftest formation-waits-until-the-operator-is-quiet-and-is-off-by-default
  (with-formation (k)
    (say k "Something worth knowing.")
    (advance 600)
    (request-graph-formation k)
    (is (not (pai-v3.kernel:work-pending-p k "graph-form" "operator")) "off until switched on")
    (change-setting k "graph.formation" t :origin *operator*)
    (say k "Something else.")
    (request-graph-formation k)
    (is (not (pai-v3.kernel:work-pending-p k "graph-form" "operator")) "not while the operator is still talking")
    (advance 400)
    (request-graph-formation k)
    (is (pai-v3.kernel:work-pending-p k "graph-form" "operator") "after five quiet minutes")))

(deftest formation-puts-a-story-in-its-own-world
  (with-formation (k)
    (let* ((turn (say k "In my novel North Wind, Gamma is Ilsa Corran's assassin."))
           (s (format nil "S~d" (event-id turn)))
           (*thought-provider*
             (proposal-provider
              (obj "entities" (list (obj "ref" "e1" "name" "North Wind" "kind" "artifact")
                                    (obj "ref" "e2" "name" "Gamma" "kind" "person" "world" "e1")
                                    (obj "ref" "e3" "name" "Ilsa Corran" "kind" "person" "world" "e1")
                                    (obj "ref" "e4" "name" "Ghost" "kind" "person" "world" "e9"))
                   "facts" (list (obj "subject" "e2" "predicate" "related_to" "object" "e3" "world" "e1"
                                      "statement" "Gamma is Ilsa Corran's assassin." "source" s
                                      "quote" "Gamma is Ilsa Corran's assassin")
                                 (obj "subject" "e2" "predicate" "appears_in" "object" "e1"
                                      "statement" "Gamma is a character in North Wind." "source" s
                                      "quote" "In my novel North Wind"))))))
      (form-graph k (list :id nil))
      (let ((gamma (first (find-entities k "gamma")))
            (book (first (first (find-entities k "north wind")))))
        (is (equal book (tenth gamma)) "the character lives in the novel's world")
        (is (equal "real" (tenth (first (find-entities k "north wind")))) "the novel itself is real")
        (is (= 2 (length (entity-facts k (first gamma)))) "her in-story fact and where she appears"))
      (is (= 1 (sql-value (kernel-db k) "SELECT dropped FROM graph_formation_runs WHERE event_id>?1 LIMIT 1" 0))
          "an entity in an unknown world is dropped"))))

(deftest recovery-connects-an-orphan-only-from-its-own-mentions
  (with-formation (k)
    (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")
    (let* ((orphans (obj "orphans" (list (obj "entity_id" "cgf:loose" "label" "A loose idea" "kind" "concept"
                                              "mentions" (list (obj "event_id" 42 "speaker" "operator" "origin" "migrated"
                                                                    "at" "2026-08-02T10:00:00Z"
                                                                    "text" "I keep coming back to a loose idea I proposed last spring."))))))
           (*thought-provider*
             (proposal-provider
              (obj "entities" (list (obj "ref" "e1" "name" "A loose idea" "kind" "concept" "known_id" "cgf:loose")
                                    (obj "ref" "e2" "name" "Sam" "kind" "person" "known_id" "participant:op"))
                   "facts" (list (obj "subject" "e2" "predicate" "proposed" "object" "e1"
                                      "statement" "Sam proposed the loose idea." "time" "event"
                                      "source" "S42" "quote" "a loose idea I proposed last spring")
                                 (obj "subject" "e2" "predicate" "proposed" "object" "e1"
                                      "statement" "Made up." "source" "S42" "quote" "something never said"))))))
      (let ((r (pai-v3.mind::recover-orphans k orphans :source-agent "beta" :earlier-agent "alpha")))
        (is (= 1 (obj-get r "tried")))
        (is (= 1 (obj-get r "facts_admitted")) "only what the mention says")
        (is (= 1 (obj-get r "dropped")) "the made-up quote is dropped"))
      (let* ((fact (first (entity-facts k "cgf:loose")))
             (evidence (first (fact-evidence k (first fact)))))
        (is (equal "Sam proposed the loose idea." (tenth fact)))
        (is (equal (kernel-agent-id k) (seventh fact)) "formed here")
        (is (equal "alpha-v1 via beta" (fifth evidence)) "the quote keeps its true origin")
        (is (equal "beta:event:42" (sixth evidence)))
        (is (null (third evidence)) "and points at no event of this agent's"))
      (is (= 1 (obj-get (pai-v3.mind::recover-orphans k orphans :source-agent "beta" :earlier-agent "alpha") "skipped")) "a connected entity is not tried again"))))

(deftest a-labelled-source-is-read-and-a-reset-reads-again
  (with-formation (k)
    (let* ((turn (say k "We moved to Porto this summer."))
           (*thought-provider*
             (proposal-provider
              (obj "entities" (list (obj "ref" "e1" "name" "Sam" "kind" "person")
                                    (obj "ref" "e2" "name" "Porto" "kind" "place"))
                   "facts" (list (obj "subject" "e1" "predicate" "located_at" "object" "e2"
                                      "statement" "Sam lives in Porto." "time" "ongoing-state"
                                      "source" (format nil "[S~d · from the operator]" (event-id turn))
                                      "quote" "We moved to Porto"))))))
      (form-graph k (list :id nil))
      (is (find-entities k "porto") "the label is read even with the speaker after it")
      (is (plusp (pai-v3.mind::%formed-through k)))
      (pai-v3.mind::reset-graph k "test")
      (is (zerop (pai-v3.mind::%formed-through k)) "a reset means the conversation is read again"))))

(deftest a-fact-may-name-a-known-entity-by-its-id
  (with-formation (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (list (lineage-hop "beta" "formed"))))
           (turn (say k "Phoebe is my dog."))
           (s (format nil "S~d" (event-id turn))))
      (let ((*thought-provider*
              (proposal-provider
               (obj "entities" (list (obj "ref" "e1" "name" "Phoebe" "kind" "organism"))
                    "facts" (list (obj "subject" sam "predicate" "companion_of" "object" "e1"
                                       "statement" "Phoebe is Sam's dog." "source" s "quote" "Phoebe is my dog"))))))
        (form-graph k (list :id nil)))
      (is (find "companion_of" (entity-facts k sam) :key #'third :test #'equal)
          "the known entity's id resolves; the fact is kept"))))

(run-suite "mind-graph-form")
