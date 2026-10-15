(in-package #:pai-v3.mind)

;;; Correcting the graph when the operator points something out (bead
;;; v3-clp.22; operator decision 2026-10-14: no sweeps, the operator corrects
;;; things as they come up). Only a thought answering the operator may
;;; correct, and each correction is an event that carries the reason and the
;;; operator's turn, so nothing is lost: a retracted fact stays in the record,
;;; a merged entity stays, marked, and a moved entity keeps its history.

(defun %by-operator (reason)
  (format nil "~a (operator correction)" (string-trim " " reason)))

(defun %live-fact (kernel fact-id)
  "(subject predicate object character occurred valid-from valid-until derivation lineage scope polarity attributed text world)"
  (or (and (stringp fact-id)
           (sql-row (kernel-db kernel)
                    "SELECT subject,predicate,object,character,occurred_at,valid_from,valid_until,derivation,lineage,scope,polarity,attributed_to,text,world FROM graph_facts WHERE id=?1 AND status=?2 LIMIT 1"
                    fact-id "active"))
      (%graph-refuse "there is no live fact ~a" fact-id)))

(defun correct-retract (kernel fact-id reason source)
  (%live-fact kernel fact-id)
  (append! kernel "graph.fact.retracted" (obj "fact_id" fact-id "reason" (%by-operator reason) "source_event_id" source))
  (format nil "Retracted ~a. It stays in the record with the reason." fact-id))

(defun correct-merge (kernel from into reason source)
  (let ((a (graph-entity kernel from)) (b (graph-entity kernel into)))
    (unless (and a b (not (equal (first a) (first b)))) (%graph-refuse "merge needs two different entities"))
    (unless (equal (second a) (second b)) (%graph-refuse "~a is a ~a and ~a a ~a; only the same kind merges" (third a) (second a) (third b) (second b)))
    (append! kernel "graph.entity.merged" (obj "entity_id" (first a) "into" (first b) "reason" (%by-operator reason) "source_event_id" source))
    (format nil "Merged ~a into ~a; its names and facts now belong to ~a." (third a) (third b) (third b))))

(defun correct-alias (kernel entity-id alias)
  (let ((e (graph-entity kernel entity-id)))
    (unless e (%graph-refuse "there is no entity ~a" entity-id))
    (unless (and (stringp alias) (< 0 (length (string-trim " " alias)) 201)) (%graph-refuse "give the other name"))
    (append! kernel "graph.entity.aliased" (obj "entity_id" (first e) "alias" (string-trim " " alias)))
    (format nil "~a is also known as ~a." (third e) alias)))

(defun correct-predicate (kernel fact-id predicate reason source)
  "Say what a fact means with the right predicate: a new fact with the same
ends, statement, evidence and lineage, and the old one retracted."
  (destructuring-bind (subject old object character occurred from until derivation lineage scope polarity attributed text world)
      (%live-fact kernel fact-id)
    (when (equal old predicate) (%graph-refuse "that fact already says ~a" predicate))
    (let ((new (admit-fact kernel :subject subject :predicate predicate :object object
                                  :temporal (obj "character" character "occurred_at" occurred "valid_from" from "valid_until" until)
                                  :derivation derivation :lineage (json-parse lineage)
                                  :grounding (obj "scope" scope "polarity" polarity "attributed_to" attributed)
                                  :text text :world world
                                  :evidence (mapcar (lambda (ev)
                                                      (destructuring-bind (kind quote event speaker origin ref at) ev
                                                        (obj "kind" kind "quote" quote "source_event_id" event "speaker" speaker
                                                             "origin" origin "source_ref" ref "at" at)))
                                                    (fact-evidence kernel fact-id)))))
      (append! kernel "graph.fact.retracted"
               (obj "fact_id" fact-id "reason" (%by-operator (format nil "re-predicated as ~a (~a): ~a" predicate new reason))
                    "source_event_id" source))
      (format nil "~a now says ~a, as ~a; the old fact is retracted." fact-id predicate new))))

(defun correct-world (kernel entity-ids world reason source)
  "Move ENTITY-IDS (together) into WORLD: a work's entity id, or real."
  (let ((world (%check-world kernel (if (equal world "real") +real-world+ world)))
        (entities (mapcar (lambda (id) (or (graph-entity kernel id) (%graph-refuse "there is no entity ~a" id))) entity-ids)))
    (when (null entities) (%graph-refuse "name the entities to move"))
    (dolist (e entities)
      (when (equal (first e) world) (%graph-refuse "a work cannot be moved into itself")))
    (dolist (e entities)
      (append! kernel "graph.entity.moved" (obj "entity_id" (first e) "world" world "reason" (%by-operator reason) "source_event_id" source)))
    (let ((across (loop for e in entities
                        append (loop for f in (entity-facts kernel (first e) :limit 200)
                                     unless (or (equal (eleventh-of f) world)
                                                (member (third f) +world-crossing-predicates+ :test #'equal))
                                       collect (format nil "~a [~a]" (render-fact kernel f) (first f))))))
      (format nil "Moved ~{~a~^, ~} into ~a.~@[~%Still linked across worlds (retract, or move the other end too):~%~{  ~a~^~%~}~]"
              (mapcar #'third entities) (if (equal world +real-world+) "the real world" (world-name kernel world))
              (remove-duplicates across :test #'equal)))))

(defun eleventh-of (row) (nth 10 row))

(define-tool "correct_knowledge"
  :description "Correct my knowledge graph when the operator points out a mistake, in the conversation where they do. action:
retract (fact_id, reason) when a fact is wrong; merge (entity_id into into, reason) for a duplicate; alias (entity_id,
alias) for another name; predicate (fact_id, predicate, reason) when the fact is right but its relation is wrong;
world (entity_ids, world: a work's entity id or real, reason) when something from a story was taken as real, or the
other way round. Look the ids up with the knowledge tool first. Every correction is kept with its reason."
  :summary "Correct my knowledge graph."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "retract" "merge" "alias" "predicate" "world"))
                                     "fact_id" (obj "type" "string") "entity_id" (obj "type" "string")
                                     "entity_ids" (obj "type" "array" "items" (obj "type" "string"))
                                     "into" (obj "type" "string") "alias" (obj "type" "string")
                                     "predicate" (obj "type" "string") "world" (obj "type" "string")
                                     "reason" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (let* ((thought (thought-row kernel thought-id))
                (source (getf thought :source-event-id))
                (action (obj-get args "action"))
                (reason (let ((r (obj-get args "reason"))) (and (stringp r) (plusp (length (string-trim " " r))) r))))
           (handler-case
               (cond ((not (and (equal "responding" (getf thought :origin)) source))
                      "I correct my graph only when the operator points something out in our conversation.")
                     ((and (not (equal action "alias")) (null reason)) "Give the reason, in the operator's words where you can.")
                     ((equal action "retract") (correct-retract kernel (obj-get args "fact_id") reason source))
                     ((equal action "merge") (correct-merge kernel (obj-get args "entity_id") (obj-get args "into") reason source))
                     ((equal action "alias") (correct-alias kernel (obj-get args "entity_id") (obj-get args "alias")))
                     ((equal action "predicate") (correct-predicate kernel (obj-get args "fact_id") (obj-get args "predicate") reason source))
                     ((equal action "world")
                      (correct-world kernel (let ((ids (obj-get args "entity_ids")))
                                              (cond ((consp ids) (remove-if-not #'stringp ids))
                                                    ((stringp (obj-get args "entity_id")) (list (obj-get args "entity_id")))))
                                     (obj-get args "world") reason source))
                     (t "action is retract, merge, alias, predicate or world."))
             (graph-refused (e) (format nil "Not corrected: ~a" e))))))
