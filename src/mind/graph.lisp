(in-package #:pai-v3.mind)

;;; The knowledge graph (bead v3-clp.6): entities and the facts between them.
;;;
;;; Ported from v2's context graph, which kept its data model sound and its
;;; formation fragile, and kept fourteen protocol versions side by side, each
;;; re-forming the graph from scratch. Here there is one graph and no protocol
;;; version: entities and facts are recorded as events with their evidence and
;;; lineage, so how they are formed can change without the graph changing.
;;;
;;; Kept from v2:
;;;  - typed entities with lasting identity, and facts with a predicate whose
;;;    subject and object kinds the ontology allows;
;;;  - time on every fact: its character (event, temporary or ongoing state,
;;;    standing disposition, timeless) and when it held;
;;;  - nothing enters without evidence: every fact carries at least one quote
;;;    from its source;
;;;  - grounding on every fact: whether it is asserted, reported, proposed,
;;;    asked, intended or a hypothesis, its polarity, and who it is
;;;    attributed to; a question is not a fact about the world;
;;;  - corrections as their own records: a retracted fact or a merged entity
;;;    stays in the record, marked;
;;;  - worlds: an entity or fact is real, or belongs to the world of a work
;;;    (a story, a book), named by the work's own entity. Facts about a work
;;;    are real; facts inside it are in its world, and are never given as
;;;    real. Only appears_in links an in-world entity to the real world, by
;;;    naming its work.
;;; New:
;;;  - lineage on every entity and fact: the agents that formed it and that
;;;    carried it here, in order, so a fact Alpha formed and Beta held reads as
;;;    "Alpha via Beta", and the agent's own as its own;
;;;  - one ontology, v2's base and family extension together.

;;; Ontology (v2 personal-context-core v1.2 with the v1.3 family extension)

(defparameter +graph-kinds+
  '("person" "agent" "organization" "organism" "object" "place" "artifact" "system"
    "project" "concept" "condition" "event" "phenomenon" "gap" "other_thing" "attribute_value"))

(defparameter +graph-all-kinds+ (remove "attribute_value" +graph-kinds+ :test #'string=)
  "Every kind but literal attribute values.")

(defparameter +graph-predicates+
  `(("part_of" ("artifact" "system" "project" "event" "organism" "object" "place" "organization" "concept")
               ("artifact" "system" "project" "organism" "object" "place" "organization" "event" "concept"))
    ("has_part" ("artifact" "system" "project" "organism" "object" "place" "organization")
                ("artifact" "system" "project" "event" "organism" "object" "place" "organization"))
    ("works_at" ("person") ("organization"))
    ("formerly_at" ("person") ("organization"))
    ("operates" ("person" "agent") ("agent" "system"))
    ("owns" ("person" "agent" "organization") ("artifact" "organism" "system" "object"))
    ("authored" ("person" "agent" "organization") ("artifact" "system"))
    ("leads" ("person" "agent") ("project"))
    ("works_on" ("person" "agent") ("project" "artifact" "system"))
    ("addresses" ("artifact" "concept" "system" "project") ("gap" "concept"))
    ("affects" ,+graph-all-kinds+ ,+graph-all-kinds+)
    ("has_condition" ("person" "organism") ("condition"))
    ("uses" ("person" "agent" "system" "project" "organization") ("artifact" "system" "object" "organization" "concept"))
    ("considered_using" ("person" "agent") ("artifact" "system" "object"))
    ("implements" ("artifact" "system") ("concept" "artifact"))
    ("occurs_after" ("event" "project" "concept") ("event" "project" "concept"))
    ("related_to" ,+graph-all-kinds+ ,+graph-all-kinds+)
    ("proposed" ("person" "agent") ("concept" "artifact" "project" "system"))
    ("recommends" ("person" "agent") ("artifact" "concept" "system"))
    ("warned_about" ("person" "agent") ("concept"))
    ("keeps_secret_from" ("person" "agent") ("person" "agent"))
    ("runs_on" ("agent" "system" "artifact") ("system" "artifact"))
    ("managed_with" ("condition") ("artifact" "system" "concept" "object"))
    ("has_gap" ("person" "agent" "system" "project") ("gap"))
    ("plans_migration_of" ("person" "agent" "organization") ("agent" "system"))
    ("migrates_to" ("agent" "system") ("system"))
    ("classified_as" ,(remove "concept" +graph-all-kinds+ :test #'string=) ("concept"))
    ("located_at" ,(remove "gap" (remove "concept" +graph-all-kinds+ :test #'string=) :test #'string=)
                  ("place"))
    ("parent_of" ("person") ("person"))
    ("daughter_of" ("person") ("person"))
    ("son_of" ("person") ("person"))
    ("spouse_of" ("person") ("person"))
    ("companion_of" ("person" "organism") ("person" "organism"))
    ("has_age" ("person" "organism") ("attribute_value"))
    ("has_gender" ("person" "organism") ("attribute_value"))
    ("has_appearance" ,+graph-all-kinds+ ("attribute_value"))
    ("appears_in" ,+graph-all-kinds+ ("artifact"))
    ;; Everyday relations v2 had no word for, found by reading what its
    ;; related_to facts said (bead v3-clp.15): 81% of its facts were related_to.
    ("sibling_of" ("person") ("person"))
    ("relative_of" ("person") ("person"))
    ("friend_of" ("person" "agent") ("person" "agent"))
    ("knows" ("person" "agent") ("person" "agent" "organization"))
    ("met_with" ("person" "agent") ("person" "agent" "organization" "organism"))
    ("spent_time_with" ("person" "agent") ("person" "agent" "organism"))
    ("helped" ("person" "agent" "organization") ("person" "agent" "organization" "project" "system"))
    ("trusts" ("person" "agent") ("person" "agent" "organization" "system"))
    ("likes" ("person" "agent") ,+graph-all-kinds+)
    ("prefers" ("person" "agent") ("concept" "artifact" "system" "object" "place" "event"))
    ("interested_in" ("person" "agent" "organization") ,+graph-all-kinds+)
    ("watched" ("person" "agent") ("artifact" "event"))
    ("read" ("person" "agent") ("artifact"))
    ("attended" ("person" "agent" "organization") ("event"))
    ("plans" ("person" "agent" "organization") ("event" "project" "concept"))
    ("lives_at" ("person" "organism" "agent") ("place"))
    ("visited" ("person" "agent") ("place" "organization" "event"))
    ("named_after" ,+graph-all-kinds+ ,+graph-all-kinds+)
    ("member_of" ("person" "agent" "organization") ("organization" "project"))
    ("involved_in" ("person" "agent" "organization") ("event" "project" "concept" "condition"))
    ("worked_with" ("person" "agent" "organization") ("person" "agent" "organization"))
    ("scheduled" ("person" "agent") ("event" "concept")))
  "Predicate, subject kinds, object kinds.")

(defparameter +predicate-meanings+
  '(("part_of" . "is a part or component of") ("has_part" . "contains as a part")
    ("works_at" . "is employed by") ("formerly_at" . "used to be employed by")
    ("operates" . "runs or controls") ("owns" . "owns or has")
    ("authored" . "wrote, made or created") ("leads" . "leads or directs")
    ("works_on" . "is working on") ("addresses" . "is meant to solve or deal with")
    ("affects" . "has an effect on") ("has_condition" . "has this health condition")
    ("uses" . "uses") ("considered_using" . "thought about using")
    ("implements" . "is an implementation of") ("occurs_after" . "happens after")
    ("proposed" . "suggested or put forward") ("recommends" . "recommends")
    ("warned_about" . "warned about") ("keeps_secret_from" . "keeps something secret from")
    ("runs_on" . "runs on") ("managed_with" . "is managed or treated with")
    ("has_gap" . "lacks or is missing") ("plans_migration_of" . "plans to migrate")
    ("migrates_to" . "is moving to") ("classified_as" . "is a kind of")
    ("located_at" . "is located at") ("parent_of" . "is the parent of")
    ("daughter_of" . "is the daughter of") ("son_of" . "is the son of")
    ("spouse_of" . "is married to or the partner of") ("companion_of" . "is a companion (or pet) of")
    ("has_age" . "is this age") ("has_gender" . "has this gender")
    ("has_appearance" . "looks like this (the description is in the statement; cite the picture when there is one)")
    ("appears_in" . "is a character, place or thing in this work")
    ("sibling_of" . "is a brother or sister of") ("relative_of" . "is related by family to (other than parent, child, sibling, spouse)")
    ("friend_of" . "is a friend of") ("knows" . "is acquainted with")
    ("met_with" . "met, talked or presented to (a meeting, call or conversation)")
    ("spent_time_with" . "was with or spent time with")
    ("helped" . "helped, fixed something for, or set something up for")
    ("trusts" . "trusts") ("likes" . "likes, enjoys or cares for (negative polarity: dislikes)")
    ("prefers" . "prefers") ("interested_in" . "is interested in or curious about")
    ("watched" . "watched (a film, show, game or event)") ("read" . "read (a book or document)")
    ("attended" . "went to or will go to (an event)") ("plans" . "plans or intends to hold or do")
    ("lives_at" . "lives at") ("visited" . "visited") ("named_after" . "is named after")
    ("member_of" . "belongs to (a group, team or organization)")
    ("involved_in" . "takes part in or has a role in")
    ("worked_with" . "worked or collaborated with")
    ("scheduled" . "arranged or set a time for (a meeting, reminder or appointment)")
    ("related_to" . "LAST RESORT: linked in some way no other predicate describes"))
  "What each predicate means, for the formation call.")

(defparameter +real-world+ "real")
(defparameter +world-crossing-predicates+ '("appears_in")
  "The only predicates a real fact may use with an in-world entity: to say which work it is in.")

(defparameter +temporal-characters+
  '("event" "temporary-state" "ongoing-state" "standing-disposition" "timeless" "unspecified"))
(defparameter +evidence-kinds+
  '("original-utterance" "prior-agent-utterance" "tool-observation" "generated-summary" "retrieval-metadata"
    ;; A picture the fact was read from (bead v3-3za): its quote describes
    ;; what the image shows, and its source_ref is image:<stored image id>.
    "image"
    ;; A passage of a book or document the agent read (bead v3-clp.21); its
    ;; source_ref is book:<id>#<passage>.
    "document"))
(defparameter +lineage-hows+ '("formed" "imported" "migrated"))
(defparameter +grounding-scopes+ '("assertion" "reported-speech" "proposal" "intention" "question" "hypothesis"))
(defparameter +polarities+ '("positive" "negative" "unknown"))

(defun predicate-allows-p (predicate subject-kind object-kind)
  (let ((row (assoc predicate +graph-predicates+ :test #'equal)))
    (and row (member subject-kind (second row) :test #'equal) (member object-kind (third row) :test #'equal))))

;;; Lineage

(defun lineage-hop (agent how &key generation (at (now-iso8601)))
  (obj "agent" agent "how" how "generation" generation "at" at))

(defun %lineage-problem (lineage)
  "Why LINEAGE is not a valid chain of hops, or NIL."
  (cond ((not (and (listp lineage) (plusp (length lineage)))) "lineage needs at least one hop")
        ((not (equal "formed" (obj-get (first lineage) "how"))) "the first hop of a lineage is where it was formed")
        (t (loop for hop in lineage
                 unless (and (hash-table-p hop) (stringp (obj-get hop "agent")) (< 0 (length (obj-get hop "agent")) 41)
                             (member (obj-get hop "how") +lineage-hows+ :test #'equal)
                             (stringp (obj-get hop "at")))
                   return "each hop needs agent, how (formed, imported or migrated) and at"))))

(defun lineage-path (lineage)
  "The agents a record passed through, as stored for queries: alpha>beta>gamma."
  (format nil "~{~(~a~)~^>~}" (mapcar (lambda (h) (obj-get h "agent")) lineage)))

(defun render-lineage (lineage)
  "Alpha (v1) via Beta, for people and prompts."
  (let ((names (mapcar (lambda (h) (format nil "~:(~a~)~@[ (~a)~]" (obj-get h "agent") (obj-get h "generation")))
                       lineage)))
    (format nil "~a~@[ via ~{~a~^ via ~}~]" (first names) (rest names))))

;;; Events

(define-event-type "graph.entity.formed"
  :schema (("entity_id" :string) ("kind" :string) ("name" :string) ("aliases" :array :optional)
           ("classifications" :array :optional) ("summary" :string :optional) ("lineage" :array)
           ("world" :string :optional)))
(define-event-type "graph.entity.aliased"
  :schema (("entity_id" :string) ("alias" :string)))
(define-event-type "graph.entity.merged"
  :schema (("entity_id" :string) ("into" :string) ("reason" :string) ("source_event_id" :integer :optional)))
(define-event-type "graph.entity.moved"
  ;; An operator's correction: the entity belongs in another world (a work, or real).
  :schema (("entity_id" :string) ("world" :string) ("reason" :string) ("source_event_id" :integer :optional)))
(define-event-type "graph.fact.admitted"
  :schema (("fact_id" :string) ("subject" :string) ("predicate" :string) ("object" :string)
           ("temporal" :object) ("derivation" :string) ("evidence" :array) ("lineage" :array)
           ("grounding" :object :optional) ("text" :string :optional) ("world" :string :optional)))
(define-event-type "graph.fact.supported"
  :schema (("fact_id" :string) ("evidence" :array)))
(define-event-type "graph.fact.retracted"
  :schema (("fact_id" :string) ("reason" :string) ("source_event_id" :integer :optional)))
(define-event-type "graph.reset"
  :schema (("reason" :string)))

(defun %canonical (text)
  (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))

(defun %add-evidence (db fact-id evidence)
  (let ((next (or (sql-value db "SELECT max(seq) FROM graph_evidence WHERE fact_id=?1 LIMIT 1" fact-id) 0)))
    (loop for ev in evidence for seq from (1+ next)
          do (sql-exec db "INSERT INTO graph_evidence(fact_id,seq,kind,quote,source_event_id,speaker,origin,source_ref,at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
                       fact-id seq (obj-get ev "kind") (obj-get ev "quote") (obj-get ev "source_event_id")
                       (obj-get ev "speaker") (obj-get ev "origin") (obj-get ev "source_ref") (obj-get ev "at")))))

(defun %reduce-graph (db e)
  (let ((p (event-payload e)) (type (event-type e)) (at (event-at e)))
    (flet ((f (k) (obj-get p k)))
      (cond
        ((string= type "graph.entity.formed")
         (let ((lineage (f "lineage")))
           (sql-exec db "INSERT INTO graph_entities(id,kind,name,summary,classifications,status,merged_into,origin_agent,path,lineage,created_at,world) VALUES(?1,?2,?3,?4,?5,'active',NULL,?6,?7,?8,?9,?10)"
                     (f "entity_id") (f "kind") (f "name") (f "summary")
                     (and (f "classifications") (json-encode (f "classifications")))
                     (%canonical (obj-get (first lineage) "agent")) (lineage-path lineage) (json-encode lineage) at
                     (or (f "world") +real-world+))
           (dolist (name (cons (f "name") (f "aliases")))
             (sql-exec db "INSERT OR IGNORE INTO graph_names(canonical,entity_id) VALUES(?1,?2)" (%canonical name) (f "entity_id"))
             (sql-exec db "INSERT INTO graph_names_fts(name,entity_id) VALUES(?1,?2)" name (f "entity_id")))))
        ((string= type "graph.entity.aliased")
         (sql-exec db "INSERT OR IGNORE INTO graph_names(canonical,entity_id) VALUES(?1,?2)" (%canonical (f "alias")) (f "entity_id"))
         (sql-exec db "INSERT INTO graph_names_fts(name,entity_id) VALUES(?1,?2)" (f "alias") (f "entity_id")))
        ((string= type "graph.entity.merged")
         ;; The merged entity stays, marked; its facts and names move to the survivor.
         (sql-exec db "UPDATE graph_entities SET status='merged',merged_into=?2 WHERE id=?1" (f "entity_id") (f "into"))
         (sql-exec db "UPDATE graph_facts SET subject=?2 WHERE subject=?1" (f "entity_id") (f "into"))
         (sql-exec db "UPDATE graph_facts SET object=?2 WHERE object=?1" (f "entity_id") (f "into"))
         (sql-exec db "UPDATE OR IGNORE graph_names SET entity_id=?2 WHERE entity_id=?1" (f "entity_id") (f "into"))
         (sql-exec db "UPDATE graph_names_fts SET entity_id=?2 WHERE entity_id=?1" (f "entity_id") (f "into")))
        ((string= type "graph.fact.admitted")
         (let ((lineage (f "lineage")) (temporal (f "temporal")) (grounding (or (f "grounding") (obj))))
           (sql-exec db "INSERT INTO graph_facts(id,subject,predicate,object,character,occurred_at,valid_from,valid_until,derivation,status,origin_agent,path,lineage,created_at,retracted_reason,scope,polarity,attributed_to,text,world) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,'active',?10,?11,?12,?13,NULL,?14,?15,?16,?17,?18)"
                     (f "fact_id") (f "subject") (f "predicate") (f "object")
                     (obj-get temporal "character") (obj-get temporal "occurred_at") (obj-get temporal "valid_from")
                     (obj-get temporal "valid_until") (f "derivation")
                     (%canonical (obj-get (first lineage) "agent")) (lineage-path lineage) (json-encode lineage) at
                     (obj-get grounding "scope" "assertion") (obj-get grounding "polarity" "positive")
                     (obj-get grounding "attributed_to") (f "text") (or (f "world") +real-world+))
           (%add-evidence db (f "fact_id") (f "evidence"))))
        ((string= type "graph.fact.supported")
         (%add-evidence db (f "fact_id") (f "evidence")))
        ((string= type "graph.reset")
         ;; The record of what was there stays in the ledger; the graph starts again.
         (dolist (table '("graph_evidence" "graph_facts" "graph_names" "graph_names_fts" "graph_entities"))
           (sql-exec db (format nil "DELETE FROM ~a" table))))
        ((string= type "graph.entity.moved")
         ;; Its facts follow it where the other end is already in that world;
         ;; a fact saying which work it appears in stays real.
         (let ((id (f "entity_id")) (world (f "world")))
           (sql-exec db "UPDATE graph_entities SET world=?2 WHERE id=?1" id world)
           (dolist (r (append (sql-rows db "SELECT id,object FROM graph_facts WHERE subject=?1 AND status=?2 LIMIT 500" id "active")
                              (sql-rows db "SELECT id,subject FROM graph_facts WHERE object=?1 AND status=?2 LIMIT 500" id "active")))
             (let ((fact-predicate (sql-value db "SELECT predicate FROM graph_facts WHERE id=?1 LIMIT 1" (first r)))
                   (other-world (sql-value db "SELECT world FROM graph_entities WHERE id=?1 LIMIT 1" (second r))))
               (when (and (not (member fact-predicate +world-crossing-predicates+ :test #'equal))
                          (or (equal (second r) id) (equal other-world world)))
                 (sql-exec db "UPDATE graph_facts SET world=?2 WHERE id=?1" (first r) world))))))
        ((string= type "graph.fact.retracted")
         (sql-exec db "UPDATE graph_facts SET status='retracted',retracted_reason=?2 WHERE id=?1" (f "fact_id") (f "reason")))))))

(define-projection "graph"
  :kind :inline
  :version 3                            ; 2: names in full text; 3: worlds
  :consumes ("graph.entity.formed" "graph.entity.aliased" "graph.entity.merged" "graph.entity.moved"
             "graph.fact.admitted" "graph.fact.supported" "graph.fact.retracted" "graph.reset")
  :tables ("CREATE TABLE IF NOT EXISTS graph_entities (id TEXT PRIMARY KEY, kind TEXT NOT NULL, name TEXT NOT NULL, summary TEXT, classifications TEXT, status TEXT NOT NULL, merged_into TEXT, origin_agent TEXT NOT NULL, path TEXT NOT NULL, lineage TEXT NOT NULL, created_at TEXT NOT NULL, world TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS graph_entities_status ON graph_entities(status,kind)"
           "CREATE INDEX IF NOT EXISTS graph_entities_origin ON graph_entities(origin_agent)"
           "CREATE TABLE IF NOT EXISTS graph_names (canonical TEXT NOT NULL, entity_id TEXT NOT NULL, PRIMARY KEY(canonical,entity_id))"
           "CREATE INDEX IF NOT EXISTS graph_names_entity ON graph_names(entity_id)"
           "CREATE VIRTUAL TABLE IF NOT EXISTS graph_names_fts USING fts5(name, entity_id UNINDEXED, tokenize='unicode61')"
           "CREATE TABLE IF NOT EXISTS graph_facts (id TEXT PRIMARY KEY, subject TEXT NOT NULL, predicate TEXT NOT NULL, object TEXT NOT NULL, character TEXT NOT NULL, occurred_at TEXT, valid_from TEXT, valid_until TEXT, derivation TEXT NOT NULL, status TEXT NOT NULL, origin_agent TEXT NOT NULL, path TEXT NOT NULL, lineage TEXT NOT NULL, created_at TEXT NOT NULL, retracted_reason TEXT, scope TEXT NOT NULL, polarity TEXT NOT NULL, attributed_to TEXT, text TEXT, world TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS graph_facts_world ON graph_facts(world,status)"
           "CREATE INDEX IF NOT EXISTS graph_facts_subject ON graph_facts(subject,status)"
           "CREATE INDEX IF NOT EXISTS graph_facts_object ON graph_facts(object,status)"
           "CREATE INDEX IF NOT EXISTS graph_facts_predicate ON graph_facts(predicate,status)"
           "CREATE INDEX IF NOT EXISTS graph_facts_origin ON graph_facts(origin_agent,status)"
           "CREATE INDEX IF NOT EXISTS graph_facts_shape ON graph_facts(status,path,scope,derivation)"
           "CREATE TABLE IF NOT EXISTS graph_evidence (fact_id TEXT NOT NULL, seq INTEGER NOT NULL, kind TEXT NOT NULL, quote TEXT NOT NULL, source_event_id INTEGER, speaker TEXT, origin TEXT, source_ref TEXT, at TEXT, PRIMARY KEY(fact_id,seq))")
  :entity (lambda (e) (or (obj-get (event-payload e) "fact_id") (obj-get (event-payload e) "entity_id") "graph"))
  :reduce #'%reduce-graph)

;;; Admission: nothing enters that the ontology or the evidence rule refuses.

(define-condition graph-refused (error)
  ((reason :initarg :reason :reader graph-refused-reason))
  (:report (lambda (c s) (format s "Graph refused: ~a" (graph-refused-reason c)))))

(defun %graph-refuse (fmt &rest args) (error 'graph-refused :reason (apply #'format nil fmt args)))

(defun %new-id (prefix) (format nil "~a-~a" prefix (base64url-encode (ironclad:random-data 9))))

(defun graph-entity (kernel id)
  "(id kind name summary status merged-into origin path lineage world) of entity ID, following merges."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT id,kind,name,summary,status,merged_into,origin_agent,path,lineage,world FROM graph_entities WHERE id=?1 LIMIT 1" id)))
    (if (and row (equal "merged" (fifth row)) (sixth row))
        (graph-entity kernel (sixth row))
        row)))

(defun find-entities (kernel name)
  "Active entities answering to NAME (a name or an alias), most recent first."
  (remove-duplicates
   (loop for (id) in (sql-rows (kernel-db kernel) "SELECT entity_id FROM graph_names WHERE canonical=?1 LIMIT 20" (%canonical name))
         for entity = (graph-entity kernel id)
         when (and entity (equal "active" (fifth entity))) collect entity)
   :key #'first :test #'equal))

(defun %check-world (kernel world)
  "WORLD, real or the id of a work (an artifact), or refuse."
  (cond ((or (null world) (equal world +real-world+)) +real-world+)
        ((let ((work (graph-entity kernel world)))
           (and work (equal "active" (fifth work)) (equal "artifact" (second work))
                (equal +real-world+ (tenth work))))
         (first (graph-entity kernel world)))
        (t (%graph-refuse "~a is not a work that has a world of its own" world))))

(defun world-name (kernel world)
  (if (equal world +real-world+) nil (or (third (graph-entity kernel world)) world)))

(defun form-entity (kernel &key kind name aliases classifications summary lineage id world)
  "Record a new entity, real or in the world of a work. Returns its id: ID
when given (an import keeps its source's ids), else a new one."
  (unless (member kind +graph-kinds+ :test #'equal) (%graph-refuse "~s is not an entity kind" kind))
  (unless (and (stringp name) (< 0 (length (string-trim " " name)) 201)) (%graph-refuse "an entity needs a name of 1 to 200 characters"))
  (let ((problem (%lineage-problem lineage))) (when problem (%graph-refuse "~a" problem)))
  (when (and id (graph-entity kernel id)) (%graph-refuse "entity ~a already exists" id))
  (let ((id (or id (%new-id "e"))) (world (%check-world kernel world)))
    (append! kernel "graph.entity.formed"
             (obj "entity_id" id "kind" kind "name" name "summary" summary
                  "classifications" (remove-if-not #'stringp classifications)
                  "aliases" (remove-if-not (lambda (a) (and (stringp a) (< 0 (length a) 201))) aliases)
                  "lineage" lineage "world" world))
    id))

(defun %present (v) (if (eq v :null) nil v))

(defun %temporal (temporal)
  (let* ((given (let ((o (obj)))          ; JSON null reads as :null: absent
                  (when (hash-table-p temporal)
                    (maphash (lambda (k v) (when (%present v) (setf (gethash k o) v))) temporal))
                  o))
         (character (or (obj-get given "character") "unspecified")))
    (unless (member character +temporal-characters+ :test #'equal)
      (%graph-refuse "~s is not a temporal character" character))
    (dolist (k '("occurred_at" "valid_from" "valid_until"))
      (let ((v (obj-get given k)))
        (unless (or (null v) (and (stringp v) (<= (length v) 80))) (%graph-refuse "~a must be a short time" k))))
    (obj "character" character "occurred_at" (obj-get given "occurred_at")
         "valid_from" (obj-get given "valid_from") "valid_until" (obj-get given "valid_until"))))

(defun %image-ref (ev)
  "The stored image id an image piece of evidence cites, or NIL."
  (let ((ref (obj-get ev "source_ref")))
    (and (stringp ref) (> (length ref) 6) (string= "image:" ref :end2 6) (subseq ref 6))))

(defun %evidence (evidence kernel)
  (unless (and (listp evidence) (plusp (length evidence))) (%graph-refuse "a fact needs evidence: at least one quote"))
  (loop for ev in evidence
        unless (and (hash-table-p ev) (member (obj-get ev "kind") +evidence-kinds+ :test #'equal)
                    (stringp (obj-get ev "quote")) (< 0 (length (string-trim " " (obj-get ev "quote"))) 1001))
          do (%graph-refuse "each piece of evidence needs a kind and a quote of 1 to 1000 characters")
        when (equal "image" (obj-get ev "kind"))
          do (unless (and (%image-ref ev) (image-row kernel (%image-ref ev)))
               (%graph-refuse "image evidence must cite a stored image as image:<id>")))
  evidence)

(defun %grounding (grounding)
  (let* ((g (or grounding (obj)))
         (scope (obj-get g "scope" "assertion")) (polarity (obj-get g "polarity" "positive")))
    (unless (member scope +grounding-scopes+ :test #'equal) (%graph-refuse "~s is not a grounding scope" scope))
    (unless (member polarity +polarities+ :test #'equal) (%graph-refuse "~s is not a polarity" polarity))
    (obj "scope" scope "polarity" polarity "attributed_to" (%present (obj-get g "attributed_to")))))

(defun admit-fact (kernel &key subject predicate object temporal (derivation "direct") evidence lineage
                               grounding text id world)
  "Admit a fact between two entities, or add its evidence to the same live
fact: same subject, predicate, object, time character, scope and polarity,
and the same statement. Two statements under one vague predicate (related_to
most of all) are two facts; an import once folded 37 of them into one.
Returns (values fact-id :admitted|:supported). Signals GRAPH-REFUSED."
  (let ((s (graph-entity kernel subject)) (o (graph-entity kernel object)))
    (unless (and s (equal "active" (fifth s))) (%graph-refuse "no entity ~a" subject))
    (unless (and o (equal "active" (fifth o))) (%graph-refuse "no entity ~a" object))
    (unless (predicate-allows-p predicate (second s) (second o))
      (%graph-refuse "~a cannot link a ~a to a ~a" predicate (second s) (second o)))
    (setf world (%check-world kernel world))
    ;; A fact sits in one world. Its ends are of that world or real; a real
    ;; fact touches an in-world entity only to say which work it appears in.
    (dolist (end (list s o))
      (let ((end-world (tenth end)))
        (unless (or (equal end-world world) (equal end-world +real-world+)
                    (and (equal world +real-world+) (member predicate +world-crossing-predicates+ :test #'equal)
                         (equal end-world (first o))))
          (%graph-refuse "~a is in the world of ~a, so a fact in ~a cannot use it"
                         (third end) (world-name kernel end-world)
                         (if (equal world +real-world+) "the real world" (world-name kernel world))))))
    (unless (member derivation '("direct" "inferred") :test #'equal) (%graph-refuse "derivation is direct or inferred"))
    (let ((problem (%lineage-problem lineage))) (when problem (%graph-refuse "~a" problem)))
    (let* ((temporal (%temporal temporal))
           (grounding (%grounding grounding))
           (evidence (%evidence evidence kernel))
           (same (sql-value (kernel-db kernel)
                            "SELECT id FROM graph_facts WHERE subject=?1 AND predicate=?2 AND object=?3 AND status=?4 AND character=?5 AND scope=?6 AND polarity=?7 AND coalesce(lower(trim(text)),'')=?8 AND world=?9 LIMIT 1"
                            (first s) predicate (first o) "active" (obj-get temporal "character")
                            (obj-get grounding "scope") (obj-get grounding "polarity")
                            (string-downcase (string-trim " " (if (stringp text) text ""))) world)))
      (if same
          (progn (append! kernel "graph.fact.supported" (obj "fact_id" same "evidence" evidence))
                 (values same :supported))
          (let ((id (or id (%new-id "f"))))
            (append! kernel "graph.fact.admitted"
                     (obj "fact_id" id "subject" (first s) "predicate" predicate "object" (first o)
                          "temporal" temporal "derivation" derivation "evidence" evidence "lineage" lineage
                          "grounding" grounding "text" (and (stringp text) (subseq text 0 (min 1000 (length text))))
                          "world" world))
            (values id :admitted))))))

(defun reset-graph (kernel reason)
  "Start the graph again. The ledger keeps everything that was there, and why it was reset."
  (append! kernel "graph.reset" (obj "reason" reason)))

(defun retract-fact (kernel fact-id reason)
  (append! kernel "graph.fact.retracted" (obj "fact_id" fact-id "reason" reason)))

(defun merge-entities (kernel from into reason)
  "Correct a duplicate: FROM becomes INTO."
  (unless (and (graph-entity kernel from) (graph-entity kernel into) (not (equal from into)))
    (%graph-refuse "merge needs two different entities"))
  (append! kernel "graph.entity.merged" (obj "entity_id" from "into" into "reason" reason)))

;;; Reading

(defun entity-facts (kernel entity-id &key (limit 50))
  "Live facts about ENTITY-ID either way round:
(id subject predicate object character derivation path scope polarity text world)."
  (let ((db (kernel-db kernel)))
    (append (sql-rows db "SELECT id,subject,predicate,object,character,derivation,path,scope,polarity,text,world FROM graph_facts WHERE subject=?1 AND status=?2 LIMIT ?3"
                      entity-id "active" limit)
            (sql-rows db "SELECT id,subject,predicate,object,character,derivation,path,scope,polarity,text,world FROM graph_facts WHERE object=?1 AND status=?2 LIMIT ?3"
                      entity-id "active" limit))))

(defun entity-names (kernel entity-id)
  "Every name ENTITY-ID goes by, as kept (lower case), its own name first."
  (let ((own (third (graph-entity kernel entity-id))))
    (remove-duplicates
     (cons (%canonical own)
           (mapcar #'first (sql-rows (kernel-db kernel) "SELECT canonical FROM graph_names WHERE entity_id=?1 LIMIT 50" entity-id)))
     :test #'string=)))

(defun live-fact (kernel fact-id)
  "Fact FACT-ID while it is active, in the shape ENTITY-FACTS gives; else NIL."
  (sql-row (kernel-db kernel) "SELECT id,subject,predicate,object,character,derivation,path,scope,polarity,text,world FROM graph_facts WHERE id=?1 AND status=?2 LIMIT 1"
           fact-id "active"))

(defun fact-evidence (kernel fact-id)
  (sql-rows (kernel-db kernel) "SELECT kind,quote,source_event_id,speaker,origin,source_ref,at FROM graph_evidence WHERE fact_id=?1 ORDER BY seq LIMIT 20" fact-id))

(defun graph-health (kernel)
  "v2's health measures: entities, facts, entities with no facts, and the share of related_to."
  (let* ((db (kernel-db kernel))
         (entities (or (sql-value db "SELECT count(*) FROM graph_entities WHERE status=?1 LIMIT 1" "active") 0))
         (facts (or (sql-value db "SELECT count(*) FROM graph_facts WHERE status=?1 LIMIT 1" "active") 0))
         (related (or (sql-value db "SELECT count(*) FROM graph_facts WHERE predicate=?1 AND status=?2 LIMIT 1" "related_to" "active") 0))
         (connected (or (sql-value db "SELECT count(*) FROM graph_entities e WHERE e.status=?1 AND (EXISTS (SELECT 1 FROM graph_facts f WHERE f.subject=e.id AND f.status=?1) OR EXISTS (SELECT 1 FROM graph_facts f WHERE f.object=e.id AND f.status=?1)) LIMIT 1" "active") 0)))
    (obj "entities" entities "facts" facts "unconnected" (- entities connected)
         "unconnected_share" (if (plusp entities) (/ (- entities connected) (float entities 1d0)) 0d0)
         "related_to_share" (if (plusp facts) (/ related (float facts 1d0)) 0d0))))
