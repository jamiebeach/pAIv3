(in-package #:pai-v3.mind)

;;; Bringing another agent's graph in (bead v3-clp.6). The source is an
;;; export of a v2 reviewed context graph (entities and facts with grounding,
;;; time and evidence). Data, not formation receipts: nothing is re-formed.
;;;
;;; Lineage is kept as the operator asked. The source agent formed what came
;;; from its own conversations; what it formed from conversation migrated in
;;; from an earlier agent reads as that earlier agent, via the source. This
;;; agent is the last hop: a fact Alpha v1's conversation gave Beta reads, here,
;;; as Alpha (v1) via Beta via this agent.

(defun %import-origin-hops (origin source-agent earlier-agent earlier-generation at)
  "The hops up to and including SOURCE-AGENT for a record whose evidence is
ORIGIN (\"migrated\" from EARLIER-AGENT, or \"native\")."
  (if (equal origin "migrated")
      (list (lineage-hop earlier-agent "formed" :generation earlier-generation :at at)
            (lineage-hop source-agent "migrated" :at at))
      (list (lineage-hop source-agent "formed" :at at))))

(defun %fact-origin (fact)
  "migrated when every quote came from migrated conversation, else native."
  (let ((origins (remove-duplicates (mapcar (lambda (e) (obj-get e "origin")) (obj-get fact "evidence"))
                                    :test #'equal)))
    (if (equal origins '("migrated")) "migrated" "native")))

(defun %earliest (fact)
  (let ((times (remove nil (mapcar (lambda (e) (obj-get e "at")) (obj-get fact "evidence")))))
    (or (first (sort (copy-list times) #'string<)) (now-iso8601))))

(defun import-graph (kernel export &key source-agent earlier-agent (earlier-generation "v1"))
  "Import EXPORT (the parsed object) into this agent's graph. Returns an
object of counts and the reasons anything was refused. Entities keep their
source ids, so a second run adds only what is new: records already here are
skipped, not reinforced with the same quotes. SOURCE-AGENT is the agent the
export comes from and EARLIER-AGENT the one before it; both are required,
since they become every record's lineage."
  (assert (and (stringp source-agent) (stringp earlier-agent)) () "Name the source and earlier agents.")
  (let* ((me (kernel-agent-id kernel))
         (facts (obj-get export "facts"))
         (entity-origins (make-hash-table :test 'equal))
         (refused '()) (entities 0) (admitted 0) (supported 0) (skipped 0) (facts-here 0))
    ;; An entity takes the origin of its facts: migrated only when all are.
    (dolist (f facts)
      (dolist (end (list (obj-get f "subject") (obj-get f "object")))
        (push (%fact-origin f) (gethash end entity-origins))))
    (dolist (e (obj-get export "entities"))
      (let* ((id (obj-get e "entity_id"))
             (origins (gethash id entity-origins))
             (origin (if (and origins (every (lambda (o) (equal o "migrated")) origins)) "migrated" "native"))
             (role (obj-get e "participant_role")))
        (if (graph-entity kernel id)
            (incf skipped)
            (handler-case
                (progn
                  (form-entity kernel :id id :kind (obj-get e "kind") :name (obj-get e "label")
                                      :aliases (obj-get e "aliases") :classifications (obj-get e "classifications")
                                      :summary (cond ((equal role "operator") "The operator.")
                                                     ((equal role "active-persona")
                                                      (format nil "~:(~a~) herself, as she was when this was formed." source-agent)))
                                      :lineage (append (%import-origin-hops origin source-agent earlier-agent earlier-generation
                                                                            (now-iso8601))
                                                       (list (lineage-hop me "imported"))))
                  (incf entities))
              (graph-refused (c) (push (format nil "entity ~a: ~a" id (graph-refused-reason c)) refused))))))
    (dolist (f facts)
      (let ((g (or (obj-get f "grounding") (obj))))
        (if (sql-value (kernel-db kernel) "SELECT 1 FROM graph_facts WHERE id=?1 LIMIT 1" (obj-get f "fact_id"))
            (incf facts-here)
        (handler-case
            (multiple-value-bind (id outcome)
                (admit-fact kernel
                            :id (obj-get f "fact_id")
                            :subject (obj-get f "subject") :predicate (obj-get f "predicate") :object (obj-get f "object")
                            :temporal (obj-get f "temporal")
                            :derivation (if (equal "derived" (obj-get g "source_basis")) "inferred" "direct")
                            :grounding (obj "scope" (obj-get g "scope") "polarity" (obj-get g "polarity")
                                            "attributed_to" (obj-get g "attributed_entity_id"))
                            :text (obj-get f "text")
                            :evidence (mapcar (lambda (ev)
                                                (obj "kind" (obj-get ev "kind") "quote" (obj-get ev "quote")
                                                     "speaker" (obj-get ev "speaker") "at" (obj-get ev "at")
                                                     "source_ref" (obj-get ev "source_ref")
                                                     "origin" (if (equal "migrated" (obj-get ev "origin"))
                                                                  (format nil "~a-~a" earlier-agent earlier-generation)
                                                                  source-agent)))
                                              (obj-get f "evidence"))
                            :lineage (append (%import-origin-hops (%fact-origin f) source-agent earlier-agent
                                                                  earlier-generation (%earliest f))
                                             (list (lineage-hop me "imported"))))
              (declare (ignore id))
              (if (eq outcome :admitted) (incf admitted) (incf supported)))
          (graph-refused (c) (push (format nil "fact ~a: ~a" (obj-get f "fact_id") (graph-refused-reason c)) refused))))))
    (let ((o (obj "entities" entities "entities_already_here" skipped "facts_admitted" admitted
                  "facts_merged_as_support" supported "facts_already_here" facts-here
                  "refused_count" (length refused))))
      (setf (gethash "refused" o) (reverse refused))
      o)))
