(in-package #:pai-v3.test)

;;; A small v2 graph export for the graph suites: Sam, the novel, a city and
;;; an unconnected idea; one fact from Alpha-era conversation, one native question.

(defun synthetic-export ()
  (flet ((ev (origin quote) (obj "quote" quote "kind" "original-utterance" "speaker" "principal:x"
                                 "at" "2026-08-01T10:00:00Z" "source_ref" "event:1" "origin" origin)))
    (obj "entities" (list (obj "entity_id" "participant:op" "kind" "person" "label" "Sam" "participant_role" "operator")
                          (obj "entity_id" "cgf:book" "kind" "artifact" "label" "North Wind" "aliases" (list "the book"))
                          (obj "entity_id" "cgf:city" "kind" "place" "label" "Lisbon")
                          (obj "entity_id" "cgf:loose" "kind" "concept" "label" "A loose idea"))
         "facts" (list (obj "fact_id" "cgf:f1" "subject" "participant:op" "predicate" "authored" "object" "cgf:book"
                            "text" "Sam wrote North Wind."
                            "grounding" (obj "scope" "assertion" "polarity" "positive" "source_basis" "original")
                            "temporal" (json-parse "{\"character\":\"event\",\"occurred_at\":null,\"valid_from\":null,\"valid_until\":null}")
                            "evidence" (list (ev "migrated" "I wrote North Wind")))
                       (obj "fact_id" "cgf:f2" "subject" "participant:op" "predicate" "located_at" "object" "cgf:city"
                            "grounding" (obj "scope" "question" "polarity" "unknown" "source_basis" "derived")
                            "temporal" (obj "character" "ongoing-state") "evidence" (list (ev "native" "Do I live in Lisbon?")))))))

