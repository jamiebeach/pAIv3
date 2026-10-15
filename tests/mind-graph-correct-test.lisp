(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Correcting the graph when the operator points something out (bead
;;; v3-clp.22): only in conversation with the operator, always with a reason,
;;; and the record keeps what was corrected.

(defun own (k) (list (lineage-hop (kernel-agent-id k) "formed")))
(defun quote-of (text) (list (obj "kind" "original-utterance" "quote" text)))

(defun answering (k text)
  "A thought answering the operator's TEXT. Returns its id."
  (let ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text text
                              :origin (obj "principal_type" "operator"))))
    (request-thought k "responding" text :channel "web" :source-event-id (event-id turn))))

(defun correct (k thought &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "correct_knowledge" *tools*)) k thought (apply #'obj args)))

(defun fact-status (k id)
  (sql-value (kernel-db k) "SELECT status FROM graph_facts WHERE id=?1 LIMIT 1" id))

(deftest only-a-conversation-with-the-operator-corrects
  (with-temp-kernel (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (city (form-entity k :kind "place" :name "Lisbon" :lineage (own k)))
           (fact (admit-fact k :subject sam :predicate "lives_at" :object city :evidence (quote-of "I live in Lisbon") :lineage (own k)))
           (quiet (request-thought k "introspection" "musing")))
      (is (search "only when the operator" (correct k quiet "action" "retract" "fact_id" fact "reason" "wrong")))
      (is (equal "active" (fact-status k fact)) "untouched")
      (let ((thought (answering k "I moved out of Lisbon last year, that's wrong.")))
        (is (search "Give the reason" (correct k thought "action" "retract" "fact_id" fact)))
        (is (search "Retracted" (correct k thought "action" "retract" "fact_id" fact "reason" "moved out last year")))
        (is (equal "retracted" (fact-status k fact)))
        (is (search "operator correction"
                    (sql-value (kernel-db k) "SELECT retracted_reason FROM graph_facts WHERE id=?1 LIMIT 1" fact)))
        (is (search "\"source_event_id\""
                    (sql-value (kernel-db k) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                               (kernel-agent-id k) "graph.fact.retracted"))
            "the operator's turn is the source")))))

(deftest duplicates-merge-and-names-are-added
  (with-temp-kernel (k)
    (let* ((a (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (b (form-entity k :kind "person" :name "Samuel" :lineage (own k)))
           (place (form-entity k :kind "place" :name "Porto" :lineage (own k)))
           (thought (answering k "Samuel is me.")))
      (admit-fact k :subject b :predicate "lives_at" :object place :evidence (quote-of "Samuel lives in Porto") :lineage (own k))
      (is (search "only the same kind" (correct k thought "action" "merge" "entity_id" place "into" a "reason" "x")))
      (is (search "Merged Samuel into Sam" (correct k thought "action" "merge" "entity_id" b "into" a "reason" "the same person")))
      (is (equal a (second (first (entity-facts k a)))) "the fact now belongs to Sam")
      (is (search "also known as" (correct k thought "action" "alias" "entity_id" a "alias" "Sammy")))
      (is (equal a (first (first (find-entities k "sammy"))))))))

(deftest a-fact-gets-the-right-relation-and-keeps-its-quote
  (with-temp-kernel (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (lee (form-entity k :kind "person" :name "Lee" :lineage (own k)))
           (fact (admit-fact k :subject sam :predicate "related_to" :object lee :text "Lee is Sam's sister."
                               :evidence (quote-of "my sister Lee") :lineage (own k)))
           (thought (answering k "Lee is my sister, not just related.")))
      (is (search "Not corrected" (correct k thought "action" "predicate" "fact_id" fact "predicate" "no_such" "reason" "x")))
      (is (search "now says sibling_of" (correct k thought "action" "predicate" "fact_id" fact "predicate" "sibling_of" "reason" "she is my sister")))
      (is (equal "retracted" (fact-status k fact)))
      (let ((new (first (first (entity-facts k sam)))))
        (is (equal "sibling_of" (third (first (entity-facts k sam)))))
        (is (equal "my sister Lee" (second (first (fact-evidence k new)))) "the quote comes along")
        (is (equal "Lee is Sam's sister." (tenth (first (entity-facts k sam)))) "and the statement")))))

(deftest fiction-taken-as-real-moves-into-its-work
  (with-temp-kernel (k)
    (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (own k)))
           (book (form-entity k :kind "artifact" :name "North Wind" :lineage (own k)))
           (mira (form-entity k :kind "person" :name "Mira" :lineage (own k)))
           (ilsa (form-entity k :kind "person" :name "Ilsa" :lineage (own k)))
           (story (admit-fact k :subject ilsa :predicate "knows" :object mira :evidence (quote-of "Ilsa knows Mira") :lineage (own k)))
           (crossing (admit-fact k :subject sam :predicate "knows" :object mira :evidence (quote-of "I know Mira") :lineage (own k)))
           (thought (answering k "Mira and Ilsa are characters in my novel, not real people.")))
      (let ((said (correct k thought "action" "world" "entity_ids" (list mira ilsa) "world" book "reason" "characters in the novel")))
        (is (search "Moved Mira, Ilsa into North Wind" said))
        (is (search "Still linked across worlds" said) "the fact tying Sam to Mira is named")
        (is (search crossing said)))
      (is (equal book (tenth (graph-entity k mira))))
      (is (equal book (sql-value (kernel-db k) "SELECT world FROM graph_facts WHERE id=?1 LIMIT 1" story))
          "the story's own fact moved with them")
      (is (search "Retracted" (correct k thought "action" "retract" "fact_id" crossing "reason" "Mira is not real")))
      (is (search "Moved Mira into the real world" (correct k thought "action" "world" "entity_id" mira "world" "real" "reason" "undo"))))))

(run-suite "mind-graph-correct")
