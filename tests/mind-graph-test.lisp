(load "/v3/tests/support-graph.lisp")
(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; The knowledge graph's state (bead v3-clp.6, stage 1): one ontology,
;;; evidence on every fact, lineage on every record, corrections kept.

(defun own (k) (list (lineage-hop (kernel-agent-id k) "formed")))

(defun quote-of (text) (list (obj "kind" "original-utterance" "quote" text)))

(defmacro with-graph ((k) &body body) `(with-temp-kernel (,k) ,@body))

(deftest entities-need-a-known-kind-a-name-and-lineage
  (with-graph (k)
    (is (form-entity k :kind "person" :name "Sam" :lineage (own k)))
    (signals graph-refused (form-entity k :kind "dragon" :name "Smaug" :lineage (own k)))
    (signals graph-refused (form-entity k :kind "person" :name "" :lineage (own k)))
    (signals graph-refused (form-entity k :kind "person" :name "Nobody" :lineage nil))
    (signals graph-refused (form-entity k :kind "person" :name "Imported"
                                          :lineage (list (lineage-hop "beta" "imported")))
             "a lineage starts where the record was formed")))

(deftest names-and-aliases-find-an-entity
  (with-graph (k)
    (let ((id (form-entity k :kind "artifact" :name "North Wind" :aliases '("the book") :lineage (own k))))
      (is (equal id (first (first (find-entities k "north wind")))))
      (is (equal id (first (first (find-entities k "  The Book ")))))
      (is (null (find-entities k "another book"))))))

(deftest facts-follow-the-ontology-and-carry-evidence
  (with-graph (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
          (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k)))
          (place (form-entity k :kind "place" :name "Lisbon" :lineage (own k))))
      (multiple-value-bind (id outcome)
          (admit-fact k :subject sam :predicate "authored" :object book
                        :temporal (obj "character" "event") :evidence (quote-of "I wrote North Wind") :lineage (own k))
        (is id) (is (eq :admitted outcome))
        (is (equal "I wrote North Wind" (second (first (fact-evidence k id))))))
      (signals graph-refused (admit-fact k :subject book :predicate "authored" :object sam
                                           :evidence (quote-of "x") :lineage (own k))
               "an artifact cannot author a person")
      (signals graph-refused (admit-fact k :subject sam :predicate "located_at" :object place
                                           :evidence nil :lineage (own k))
               "no evidence, no fact")
      (signals graph-refused (admit-fact k :subject sam :predicate "located_at" :object place
                                           :temporal (obj "character" "forever") :evidence (quote-of "x") :lineage (own k))))))

(deftest the-same-fact-again-adds-evidence-not-a-duplicate
  (with-graph (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
          (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k))))
      (let ((a (admit-fact k :subject sam :predicate "authored" :object book :evidence (quote-of "I wrote it") :lineage (own k))))
        (multiple-value-bind (b outcome)
            (admit-fact k :subject sam :predicate "authored" :object book :evidence (quote-of "my book") :lineage (own k))
          (is (equal a b)) (is (eq :supported outcome))
          (is (= 2 (length (fact-evidence k a)))))))))

(deftest lineage-says-who-formed-it-and-who-carried-it
  (with-graph (k)
    (let* ((alpha-via-beta (list (lineage-hop "alpha" "formed" :generation "v1") (lineage-hop "beta" "imported")))
           (id (form-entity k :kind "person" :name "Mira" :lineage alpha-via-beta)))
      (is (equal "Alpha (v1) via Beta" (render-lineage alpha-via-beta)))
      (is (equal "alpha>beta" (lineage-path alpha-via-beta)))
      (is (equal "alpha" (seventh (graph-entity k id))) "origin is the first hop")
      (is (equal "alpha>beta" (eighth (graph-entity k id)))))))

(deftest different-statements-under-one-predicate-stay-apart
  (with-graph (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
          (pai (form-entity k :kind "system" :name "pAI" :lineage (own k))))
      (let ((a (admit-fact k :subject sam :predicate "related_to" :object pai :text "Sam builds pAI."
                             :evidence (quote-of "I'm building pAI") :lineage (own k)))
            (b (admit-fact k :subject sam :predicate "related_to" :object pai :text "Sam worries pAI is too complex."
                             :evidence (quote-of "it's too complex") :lineage (own k))))
        (is (not (equal a b)) "two statements, two facts")
        (multiple-value-bind (c outcome)
            (admit-fact k :subject sam :predicate "related_to" :object pai :text "  sam builds pAI. "
                          :evidence (quote-of "still building it") :lineage (own k))
          (is (equal a c)) (is (eq :supported outcome) "the same statement again is support"))))))

(deftest a-reset-starts-the-graph-again-and-is-recorded
  (with-graph (k)
    (form-entity k :kind "person" :name "Sam" :lineage (own k))
    (pai-v3.mind::reset-graph k "re-import with a corrected rule")
    (is (= 0 (obj-get (graph-health k) "entities")))
    (is (sql-value (kernel-db k) "SELECT 1 FROM pai_events WHERE event_type=?1 LIMIT 1" "graph.reset")
        "the reset itself is in the ledger")))

(deftest corrections-keep-the-record
  (with-graph (k)
    (let* ((a (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (b (form-entity k :kind "person" :name "J. Smith" :lineage (own k)))
           (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k)))
           (fact (admit-fact k :subject b :predicate "authored" :object book :evidence (quote-of "I wrote it") :lineage (own k))))
      (merge-entities k b a "the same person")
      (is (equal a (first (graph-entity k b))) "the merged entity resolves to the survivor")
      (is (equal a (second (first (entity-facts k a)))) "its facts moved")
      (is (equal a (first (first (find-entities k "j. smith")))) "and its names")
      (retract-fact k fact "wrong book")
      (is (null (entity-facts k a)) "a retracted fact is no longer live")
      (is (equal "retracted" (sql-value (kernel-db k) "SELECT status FROM graph_facts WHERE id=?1 LIMIT 1" fact))
          "but it stays in the record"))))

(deftest health-uses-v2s-measures
  (with-graph (k)
    (let ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
          (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k))))
      (form-entity k :kind "concept" :name "Loose idea" :lineage (own k))
      (admit-fact k :subject sam :predicate "related_to" :object book :evidence (quote-of "x") :lineage (own k))
      (let ((h (graph-health k)))
        (is (= 3 (obj-get h "entities")))
        (is (= 1 (obj-get h "unconnected")))
        (is (= 1d0 (obj-get h "related_to_share")))))))

(deftest an-imported-graph-keeps-who-formed-what
  (with-graph (k)
    (let ((result (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")))
      (is (= 4 (obj-get result "entities")))
      (is (= 2 (obj-get result "facts_admitted")))
      (is (= 0 (obj-get result "refused_count")) (format nil "~a" (obj-get result "refused")))
      (let ((me (kernel-agent-id k)))
        (is (equal (format nil "alpha>beta>~a" me) (eighth (graph-entity k "cgf:book")))
            "an entity known only from Alpha-era conversation is Alpha's, via Beta")
        (is (equal (format nil "beta>~a" me) (eighth (graph-entity k "cgf:city"))) "native to Beta")
        (is (equal (format nil "beta>~a" me) (eighth (graph-entity k "participant:op")))
            "an entity in both Alpha-era and native facts is Beta's")
        (is (equal (format nil "beta>~a" me) (eighth (graph-entity k "cgf:loose"))) "unconnected: Beta's")
        (is (equal (format nil "Alpha (v1) via Beta via ~:(~a~)" me)
                   (render-lineage (json-parse (sql-value (kernel-db k) "SELECT lineage FROM graph_facts WHERE id=?1 LIMIT 1" "cgf:f1"))))))
      (destructuring-bind (scope polarity derivation)
          (sql-row (kernel-db k) "SELECT scope,polarity,derivation FROM graph_facts WHERE id=?1 LIMIT 1" "cgf:f2")
        (is (equal "question" scope) "a question stays a question")
        (is (equal "unknown" polarity))
        (is (equal "inferred" derivation)))
      (is (equal "alpha-v1" (fifth (first (fact-evidence k "cgf:f1")))) "each quote keeps its origin")
      (is (equal "cgf:book" (first (first (find-entities k "the book")))))
      (let ((again (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")))
        (is (= 0 (obj-get again "entities")) "a second run adds no entities")
        (is (= 4 (obj-get again "entities_already_here")))
        (is (= 2 (obj-get again "facts_already_here")) "and its facts are skipped")
        (is (= 1 (length (fact-evidence k "cgf:f1"))) "not reinforced with the same quote")))))

(defun knowledge-tool (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "knowledge" pai-v3.mind::*tools*)) k 1 (apply #'obj args)))

(defun knowledge-slot (k audience task)
  "What the graph tells of what TASK names, as impressions show it: the
operator's knowledge, so nothing for another audience."
  (when (equal audience "operator")
    (let* ((named (entities-named-in k task)) (worlds (open-worlds-for k named)))
      (remove nil (mapcar (lambda (e) (entity-knowledge k e worlds)) named)))))

(deftest what-i-know-comes-from-the-names-in-the-conversation
  (with-graph (k)
    (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")
    (let ((lines (knowledge-slot k "operator" "Tell me about North Wind")))
      (is (= 1 (length lines)) (format nil "~s" lines))
      (is (search "North Wind (artifact):" (first lines)))
      (is (search "Sam wrote North Wind. (event) — Alpha (v1) via Beta" (first lines))
          "the statement, its time and where it came from"))
    (let ((lines (knowledge-slot k "operator" "Do I live in Lisbon?")))
      (is (search "[question]" (first lines)) "a question is marked as one"))
    (is (null (knowledge-slot k "guest:alex" "Tell me about North Wind")) "guests see none of it")
    (is (null (knowledge-slot k "operator" "Nothing named here")))
    (is (equal (list (format nil "A loose idea (concept): no known connections.~%"))
               (knowledge-slot k "operator" "Tell me about a loose idea."))
        "an unconnected entity stays visible, marked as such")))

(deftest the-knowledge-tool-searches-and-shows-an-entity
  (with-graph (k)
    (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")
    (is (search "North Wind (artifact, cgf:book)" (knowledge-tool k "action" "search" "query" "wind")) "full-text match on a word")
    (let ((card (knowledge-tool k "action" "entity" "name" "the book")))
      (is (search "from Alpha (v1) via Beta" card))
      (is (search "\"I wrote North Wind\"" card) "with a quote for each fact"))
    (is (search "4 entities, 2 facts" (knowledge-tool k "action" "health")))
    (is (search "Nothing in my graph" (knowledge-tool k "action" "search" "query" "zebra")))))

(deftest the-explorer-search-draws-a-neighbourhood
  (with-graph (k)
    (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")
    (let ((r (graph-search k :query "north wind")))
      (is (equal "exact-identity" (obj-get r "query_match_kind")))
      (is (= 3 (obj-get r "node_count")) "the book, Sam, and Lisbon two steps out")
      (is (= 2 (obj-get r "edge_count")))
      (let ((book (find "cgf:book" (obj-get r "nodes") :key (lambda (n) (obj-get n "node_id")) :test #'equal)))
        (is (equal '("alpha" "beta") (obj-get book "origin_agent_ids")) "origins shown, this agent left out")))
    (is (equal "related-suggestions" (obj-get (graph-search k :query "wind") "query_match_kind")))
    (is (= 1 (obj-get (graph-search k :start-id "cgf:loose") "node_count")))))

(deftest a-reviewer-sees-the-shape-not-the-content
  (with-graph (k)
    (pai-v3.mind::import-graph k (synthetic-export) :source-agent "beta" :earlier-agent "alpha")
    (let ((shape (pai-v3.mind::graph-shape k)) (me (kernel-agent-id k)))
      (is (search (format nil "alpha>beta>~a 1" me) shape) "counts by origin path")
      (is (search "question 1" shape))
      (is (not (search "North Wind" shape)) "no names")
      (is (not (search "I wrote" shape)) "no quotes"))
    (let ((tool (gethash "knowledge_shape" *tools*)))
      (is (member tool (tools-for (list :audience "coding-agent"))))
      (is (not (member (gethash "knowledge" *tools*) (tools-for (list :audience "coding-agent" :communicating t))))
          "the content stays the operator's"))))

(deftest fiction-stays-in-its-world
  (with-graph (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k)))
           (mira (form-entity k :kind "person" :name "Mira" :world book :lineage (own k)))
           (ilsa (form-entity k :kind "person" :name "Ilsa Corran" :world book :lineage (own k))))
      (signals graph-refused (form-entity k :kind "person" :name "X" :world sam :lineage (own k))
               "only a work has a world")
      (admit-fact k :subject sam :predicate "authored" :object book :evidence (quote-of "I wrote it") :lineage (own k))
      (is (admit-fact k :subject ilsa :predicate "related_to" :object mira :world book :text "Ilsa controls Mira."
                        :evidence (quote-of "Ilsa's mesmer power") :lineage (own k))
          "a fact inside the story")
      (signals graph-refused (admit-fact k :subject sam :predicate "related_to" :object mira :text "Sam loves Mira."
                                           :evidence (quote-of "x") :lineage (own k))
               "a real fact cannot use a character as if she were real")
      (is (admit-fact k :subject mira :predicate "appears_in" :object book :evidence (quote-of "Mira, in my book")
                        :lineage (own k))
          "but it can say which work she is in")
      (let ((plain (knowledge-slot k "operator" "How is Sam doing?")))
        (is (search "Sam (person)" (first plain)))
        (is (not (search "Ilsa" (format nil "~{~a~}" plain))) "the story stays shut when nobody names it"))
      (let ((story (format nil "~{~a~}" (knowledge-slot k "operator" "Tell me about Mira"))))
        (is (search "Mira (person, in North Wind)" story))
        (is (search "[in North Wind] Ilsa controls Mira." story) "and is marked when it opens")))))

(deftest every-predicate-says-what-it-means
  (dolist (row pai-v3.mind::+graph-predicates+)
    (is (assoc (first row) pai-v3.mind::+predicate-meanings+ :test #'equal) (first row))))

(deftest a-fact-that-has-ended-is-marked-and-kept-out-of-context
  (with-graph (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (lee (form-entity k :kind "person" :name "Lee" :lineage (own k)))
           (old (admit-fact k :subject sam :predicate "knows" :object lee :text "Sam and Lee were flatmates."
                              :temporal (obj "character" "temporary-state" "valid_until" "2020-06-30")
                              :evidence (quote-of "we shared a flat") :lineage (own k)))
           (now (admit-fact k :subject sam :predicate "friend_of" :object lee :text "Sam and Lee are friends."
                              :evidence (quote-of "my friend Lee") :lineage (own k))))
      (is (equal "2020-06-30" (pai-v3.mind::fact-ended k old)))
      (is (null (pai-v3.mind::fact-ended k now)))
      (is (search "[ended 2020-06-30]" (render-fact k (find old (entity-facts k sam) :key #'first :test #'equal)))
          "shown as ended where it is shown")
      (let ((slot (format nil "~{~a~}" (knowledge-slot k "operator" "Tell me about Sam"))))
        (is (search "friends" slot))
        (is (not (search "flatmates" slot)) "and left out of what is so now")))))

(run-suite "mind-graph")
