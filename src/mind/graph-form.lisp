(in-package #:pai-v3.mind)

;;; Forming the knowledge graph from conversation (bead v3-clp.6, stage 3).
;;;
;;; v2 formed its graph in eight phases and up to fourteen paid calls per
;;; episode, with closed schemas: one bad item rejected the whole reply and
;;; ended the attempt. From 2026-09-25 to 2026-10-08 that completed 3 of 209
;;; attempts. Here one call reads a stretch of the operator's conversation,
;;; with the entities already known that it names, and proposes entities and
;;; facts. Each item is then checked on its own: a fact whose quote is not in
;;; its source, whose predicate the ontology does not allow, or whose entity
;;; is unclear is dropped with its reason, and the rest are kept. What was
;;; kept and dropped is recorded, so the yield can be measured.

(define-setting "graph.formation"
  :type :boolean :default :false :group "Knowledge graph"
  :description "Form the knowledge graph from conversation with the operator.")
(define-setting "graph.formation_after_seconds"
  :type :integer :min 0 :max 86400 :default 300 :group "Knowledge graph"
  :description "How long after the operator last spoke before the conversation is read into the graph.")
(define-setting "graph.daily_usd"
  :type :number :min 0 :max 100 :default 1.0d0 :group "Knowledge graph"
  :description "Most graph formation may spend in a UTC day.")
(define-setting "graph.batch_characters"
  :type :integer :min 1000 :max 60000 :default 8000 :group "Knowledge graph"
  :description "About how much conversation one formation call reads.")

(define-event-type "graph.formation.done"
  :schema (("through_event_id" :integer) ("turns" :integer) ("entities_formed" :integer)
           ("entities_matched" :integer) ("facts_admitted" :integer) ("facts_supported" :integer)
           ("dropped" :array :optional) ("model" :string)))

(define-projection "graph_formation"
  :kind :inline
  :consumes ("graph.formation.done" "graph.reset")
  :tables ("CREATE TABLE IF NOT EXISTS graph_formation_runs (event_id INTEGER PRIMARY KEY, through_event_id INTEGER NOT NULL, turns INTEGER NOT NULL, entities_formed INTEGER NOT NULL, entities_matched INTEGER NOT NULL, facts_admitted INTEGER NOT NULL, facts_supported INTEGER NOT NULL, dropped INTEGER NOT NULL, at TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS graph_formation_mark (audience TEXT PRIMARY KEY, through_event_id INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (string= (event-type e) "graph.reset")
                  ;; The graph starts again, so its conversation is read again.
                  (progn (sql-exec db "DELETE FROM graph_formation_mark")
                         (sql-exec db "DELETE FROM graph_formation_runs"))
                  (progn
                    (sql-exec db "INSERT INTO graph_formation_mark(audience,through_event_id) VALUES('operator',?1) ON CONFLICT(audience) DO UPDATE SET through_event_id=max(through_event_id,excluded.through_event_id)"
                              (obj-get p "through_event_id"))
                    (sql-exec db "INSERT INTO graph_formation_runs(event_id,through_event_id,turns,entities_formed,entities_matched,facts_admitted,facts_supported,dropped,at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
                              (event-id e) (obj-get p "through_event_id") (obj-get p "turns") (obj-get p "entities_formed")
                              (obj-get p "entities_matched") (obj-get p "facts_admitted") (obj-get p "facts_supported")
                              (length (obj-get p "dropped")) (event-at e)))))))

(defun %formed-through (kernel)
  (or (sql-value (kernel-db kernel) "SELECT through_event_id FROM graph_formation_mark WHERE audience=?1 LIMIT 1" "operator") 0))

(defun %unformed-turns (kernel)
  "The operator's conversation not yet read into the graph, oldest first, as
(event-id kind speaker rendered), about one batch of it."
  (let ((limit (setting kernel "graph.batch_characters")) (used 0))
    (loop for row in (sql-rows (kernel-db kernel)
                               "SELECT event_id,kind,speaker,rendered FROM working_dialogue WHERE audience=?1 AND event_id>?2 AND kind IN (?3,?4) ORDER BY event_id LIMIT 200"
                               "operator" (%formed-through kernel) "turn" "reply")
          while (or (zerop used) (<= (+ used (length (fourth row))) limit))
          do (incf used (length (fourth row)))
          collect row)))

;;; The request

(defparameter +formation-tool+
  (obj "type" "function"
       "function"
       (obj "name" "record_knowledge"
            "description" "Record the entities and facts this conversation establishes."
            "parameters"
            (obj "type" "object"
                 "properties"
                 (obj "entities"
                      (obj "type" "array"
                           "items" (obj "type" "object"
                                        "properties" (obj "ref" (obj "type" "string")
                                                          "name" (obj "type" "string")
                                                          "kind" (obj "type" "string")
                                                          "known_id" (obj "type" "string")
                                                          "world" (obj "type" "string")
                                                          "aliases" (obj "type" "array" "items" (obj "type" "string")))
                                        "required" (list "ref" "name" "kind")))
                      "facts"
                      (obj "type" "array"
                           "items" (obj "type" "object"
                                        "properties" (obj "subject" (obj "type" "string")
                                                          "predicate" (obj "type" "string")
                                                          "object" (obj "type" "string")
                                                          "statement" (obj "type" "string")
                                                          "scope" (obj "type" "string")
                                                          "polarity" (obj "type" "string")
                                                          "time" (obj "type" "string")
                                                          "when" (obj "type" "string")
                                                          "inferred" (obj "type" "boolean")
                                                          "world" (obj "type" "string")
                                                          "source" (obj "type" "string" "description" "The source's label, like S123: the part before the dot in its brackets.")
                                                          "quote" (obj "type" "string"))
                                        "required" (list "subject" "predicate" "object" "statement" "source" "quote"))))
                 "required" (list "entities" "facts")))))

(defun %ontology-text ()
  (format nil "Entity kinds: ~{~a~^, ~}.~%Predicates (meaning; subject kinds -> object kinds):~%~{~a~%~}"
          +graph-kinds+
          (mapcar (lambda (row) (format nil "- ~a: ~a; ~{~a~^/~} -> ~{~a~^/~}" (first row)
                                        (or (cdr (assoc (first row) +predicate-meanings+ :test #'equal)) "")
                                        (if (equal (second row) +graph-all-kinds+) '("any") (second row))
                                        (if (equal (third row) +graph-all-kinds+) '("any") (third row))))
                  +graph-predicates+)))

(defun formation-messages (kernel turns &key focus)
  "The one formation request: numbered sources, the entities already known
that they name, and the rules. With FOCUS (an entity row), only facts that
involve that entity are asked for."
  (let* ((text (format nil "~{~a~^~%~}" (mapcar #'fourth turns)))
         (named (entities-named-in kernel text))
         (known (let ((k (subseq named 0 (min 40 (length named)))))
                  (if (and focus (not (find (first focus) k :key #'first :test #'equal))) (cons focus k) k))))
    (list (obj "role" "system"
               "content" (format nil "You maintain a personal knowledge graph for ~a, an AI companion, about the operator ~
(the person they talk with) and the people, things, projects and ideas in their life. From the conversation, record what it ~
establishes: entities, and facts linking two entities. Rules:~%~
- Only what the conversation says or plainly implies. Each fact needs a quote copied exactly from one source, ~
and that source's label.~%~
- Use the predicate whose meaning fits; family, friends, meetings, likes, what someone watched, read or attended ~
all have their own. related_to is a last resort: a vague link adds little.~%~
- Conversation itself is not knowledge: someone greeting, thanking, addressing, checking in with or acknowledging ~
someone is not a fact about the world. Record what was said, not that it was said.~%~
- statement: the fact as one plain sentence, e.g. \"Sam wrote the novel North Wind.\"~%~
- scope: assertion (stated as true), reported-speech (someone said it), proposal, intention, question or hypothesis. ~
polarity: positive, negative or unknown.~%~
- time: event, temporary-state, ongoing-state, standing-disposition, timeless or unspecified; when: a date if one is given.~%~
- inferred: true when the quote implies the fact rather than stating it.~%~
- Entities: give each a short ref (e1, e2...). When it is one of the known entities below, set known_id to its id. ~
The operator is a person; refer to them by name if known.~%~
- Fiction is never fact. A story, novel, game or other invented work is a real artifact, and what happens in it ~
belongs to its world: give such entities and facts world set to the work's ref (e.g. the novel's ref), and leave world ~
\"real\" for everything true in the real world. Facts about the work itself (who wrote it, when) are real. To say a ~
character or place is in a work, use appears_in with world \"real\". List the work before the entities in its world.~%~
- Not knowledge: small talk, greetings, the AI's own guesses, and how software (this AI included) is built: files, ~
functions, code paths, log lines, event counts, configuration, failure modes. Record such work only at the level of ~
decisions and directions, e.g. \"Sam decided pAI v3 keeps one knowledge graph\", never its mechanics.~%~
- At most 15 facts, the ones that matter most. Recording nothing is fine.~%~%~a"
                                     (kernel-agent-id kernel) (%ontology-text)))
          (obj "role" "user"
               "content" (format nil "~@[~a~%~%~]~:[No known entities are named.~;Known entities named here:~%~:*~{~a~%~}~]~%Conversation:~%~{~a~%~}~%Call record_knowledge."
                                 (and focus (format nil "Focus: ~a (~a, known_id ~a). Record only facts that involve it, linking it to other entities; it is already recorded, so refer to it by its known_id."
                                                    (third focus) (second focus) (first focus)))
                                 (mapcar (lambda (e) (format nil "- ~a: ~a (~a~@[, in the world of ~a~])" (first e) (third e) (second e)
                                                             (world-name kernel (tenth e))))
                                         known)
                                 (loop for (id kind nil rendered) in turns
                                       collect (format nil "[S~d · from the ~:[agent~;operator~]] ~a"
                                                       id (equal kind "turn") rendered)))))))

;;; Checking each item

(defun %normalise-quote (text)
  "For comparing quotes: typographic quotes and dashes plain, markdown emphasis
and code marks gone (a model quotes the words, not the asterisks), space
collapsed, lower-case."
  (let ((out (make-string-output-stream)) (space nil))
    (loop for c across (remove-if (lambda (c) (member c '(#\* #\`))) (or text ""))
          for plain = (case c ((#\LEFT_SINGLE_QUOTATION_MARK #\RIGHT_SINGLE_QUOTATION_MARK) #\')
                              ((#\LEFT_DOUBLE_QUOTATION_MARK #\RIGHT_DOUBLE_QUOTATION_MARK) #\")
                              ((#\EN_DASH #\EM_DASH) #\-) (t c))
          do (if (member plain '(#\Space #\Tab #\Newline #\Return))
                 (setf space t)
                 (progn (when space (write-char #\Space out)) (setf space nil)
                        (write-char (char-downcase plain) out))))
    (string-trim " " (get-output-stream-string out))))

(defun %source-label (text)
  "The S123 a model gave as a source, even when it added the speaker after it."
  (and (stringp text)
       (let* ((trimmed (string-trim "[] " text))
              (end (position-if-not (lambda (c) (or (alphanumericp c))) trimmed)))
         (subseq trimmed 0 (or end (length trimmed))))))

(defun %resolve-end (kernel specs ref)
  "REF's entity spec; an id of a known entity is recorded as one, so the
rest of the checks treat it like a declared, matched entity."
  (or (gethash ref specs)
      (let ((e (and (stringp ref) (graph-entity kernel ref))))
        (and e (equal "active" (fifth e)) (setf (gethash ref specs) (list :id (first e)))))))

(defun apply-formation (kernel turns proposal)
  "Check every proposed fact on its own and record what passes. An entity is
formed only when a fact that uses it passes every check, so a dropped fact
leaves nothing behind: forming entities first left v2 a graph of orphans,
and a first recovery trial here made 47 of them.
Returns (values entities-formed entities-matched admitted supported dropped)."
  (let* ((me (kernel-agent-id kernel))
         (sources (make-hash-table :test 'equal))
         (specs (make-hash-table :test 'equal))      ; ref -> (:id id) or (:new kind name aliases world)
         (formed-ids (make-hash-table :test 'equal)) ; ref -> id, once formed
         (formed 0) (matched 0) (admitted 0) (supported 0) (dropped '()))
    (dolist (turn turns)
      (setf (gethash (format nil "S~d" (first turn)) sources) turn))
    (labels ((drop (fmt &rest args) (push (apply #'format nil fmt args) dropped))
             (world-key (w)
               ;; real, the id of a recorded work, or (:ref r) for a work proposed here.
               (cond ((or (not (stringp w)) (member w '("" "real") :test #'equal)) +real-world+)
                     ((gethash w specs) (let ((sp (gethash w specs)))
                                          (if (eq :id (first sp)) (second sp) (list :ref w))))
                     ((graph-entity kernel w) (first (graph-entity kernel w)))
                     (t nil)))
             (spec-kind (sp) (if (eq :id (first sp)) (second (graph-entity kernel (second sp))) (second sp)))
             (spec-world (sp) (if (eq :id (first sp)) (tenth (graph-entity kernel (second sp))) (fifth sp)))
             (spec-key (ref sp) (if (eq :id (first sp)) (second sp) (list :ref ref)))
             (ensure (ref)
               ;; The entity's id, forming it (and the work its world is) now.
               (let ((sp (gethash ref specs)))
                 (cond ((eq :id (first sp)) (second sp))
                       ((gethash ref formed-ids))
                       (t (destructuring-bind (kind name aliases world) (rest sp)
                            (let ((id (form-entity kernel :kind kind :name name :aliases aliases
                                                          :world (if (consp world) (ensure (second world)) world)
                                                          :lineage (list (lineage-hop me "formed")))))
                              (incf formed)
                              (setf (gethash ref formed-ids) id))))))))
      ;; Entities: match what is known; plan the rest, forming nothing yet.
      (dolist (e (obj-get proposal "entities"))
        (let ((ref (obj-get e "ref")) (name (obj-get e "name")) (kind (obj-get e "kind")) (known (obj-get e "known_id")))
          (cond
            ((not (and (stringp ref) (stringp name) (plusp (length name)))) (drop "entity without a ref or name"))
            ((not (member kind +graph-kinds+ :test #'equal)) (drop "entity ~a: ~s is not an entity kind" name kind))
            (t (let ((world (world-key (obj-get e "world"))))
                 (if (null world)
                     (drop "entity ~a: its world ~a is not a work" name (obj-get e "world"))
                     (let ((same (and (not (consp world))
                                      (or (and (stringp known) (graph-entity kernel known)
                                               (equal (tenth (graph-entity kernel known)) world)
                                               (graph-entity kernel known))
                                          (find-if (lambda (x) (and (equal (second x) kind) (equal (tenth x) world)))
                                                   (find-entities kernel name))))))
                       (setf (gethash ref specs)
                             (if same
                                 (progn (incf matched) (list :id (first same)))
                                 (list :new kind name (remove-if-not #'stringp (obj-get e "aliases")) world))))))))))
      (dolist (f (obj-get proposal "facts"))
        (let* ((label (%source-label (obj-get f "source")))
               (source (gethash label sources))
               (quote (obj-get f "quote"))
               (statement (obj-get f "statement"))
               ;; An end is a ref this proposal declared, or the id of an entity
               ;; already in the graph, which a model may name directly.
               (s (%resolve-end kernel specs (obj-get f "subject")))
               (o (%resolve-end kernel specs (obj-get f "object")))
               (predicate (obj-get f "predicate"))
               (world (world-key (obj-get f "world"))))
          (cond
            ((null source) (drop "fact ~a: no source ~a" statement (obj-get f "source")))
            ((not (and (stringp quote) (plusp (length quote))
                       (search (%normalise-quote quote) (%normalise-quote (fourth source)))))
             (drop "fact ~a: its quote is not in ~a" statement label))
            ((not (and s o)) (drop "fact ~a: an entity was not recorded" statement))
            ((null world) (drop "fact ~a: its world ~a is not a work" statement (obj-get f "world")))
            ((not (predicate-allows-p predicate (spec-kind s) (spec-kind o)))
             (drop "fact ~a: ~a cannot link a ~a to a ~a" statement predicate (spec-kind s) (spec-kind o)))
            ((notevery (lambda (end-world)
                         (or (equal end-world world) (equal end-world +real-world+)
                             (and (equal world +real-world+) (member predicate +world-crossing-predicates+ :test #'equal)
                                  (equal end-world (spec-key (obj-get f "object") o)))))
                       (list (spec-world s) (spec-world o)))
             (drop "fact ~a: its entities are not of its world" statement))
            (t (handler-case
                   (let ((temporal (obj "character" (or (obj-get f "time") "unspecified")
                                        "occurred_at" (let ((w (obj-get f "when"))) (and (stringp w) (plusp (length w)) w))))
                         (grounding (obj "scope" (or (obj-get f "scope") "assertion")
                                         "polarity" (or (obj-get f "polarity") "positive"))))
                     ;; Refuse a bad time or scope before anything is formed.
                     (%temporal temporal) (%grounding grounding)
                     ;; Both ends already in the graph: a spec of (:id <entity>).
                     (let ((same (and (eq :id (first s)) (eq :id (first o)) (stringp statement)
                                      (near-duplicate-fact kernel statement (second s) (second o)
                                                           (obj-get grounding "scope") (obj-get grounding "polarity"))))
                           (evidence (list (obj "kind" (cond ((equal "turn" (second source)) "original-utterance")
                                                                 ((equal "document" (second source)) "document")
                                                                 (t "prior-agent-utterance"))
                                                "quote" quote "speaker" (third source)
                                                "source_event_id" (and (null (fifth source)) (first source))
                                                "origin" (or (fifth source) me) "source_ref" (sixth source)))))
                     ;; What a live fact already says, in other words, supports it (§7a).
                     (if same
                         (progn (append! kernel "graph.fact.supported" (obj "fact_id" same "evidence" evidence))
                                (incf supported))
                     (multiple-value-bind (id outcome)
                         (admit-fact kernel :subject (ensure (obj-get f "subject")) :predicate predicate
                                            :object (ensure (obj-get f "object"))
                                            :text statement :temporal temporal :grounding grounding
                                            :derivation (if (eq t (obj-get f "inferred")) "inferred" "direct")
                                            ;; A source from this agent's own dialogue points at its event; one
                                            ;; from elsewhere carries its own origin and reference.
                                            :evidence evidence
                                            :world (if (consp world) (ensure (second world)) world)
                                            :lineage (list (lineage-hop me "formed")))
                       (declare (ignore id))
                       (if (eq outcome :admitted) (incf admitted) (incf supported))))))
                 (graph-refused (c) (drop "fact ~a: ~a" statement (graph-refused-reason c)))))))))
    (values formed matched admitted supported (reverse dropped))))

;;; The work

(defun form-graph (kernel work)
  "Read one batch of the operator's conversation into the graph."
  (let ((turns (%unformed-turns kernel)))
    (when turns
      (let* ((profile (profile-for kernel "graph"))
             (reply (complete kernel profile (formation-messages kernel turns)
                              :tools (list +formation-tool+) :work-id (getf work :id) :lane "graph"))
             (call (find "record_knowledge" (obj-get reply "tool_calls")
                         :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal))
             (proposal (if call
                           (or (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments")))
                               (error 'handler-failure :class :model :reason "the formation call's arguments are not JSON"))
                           (obj "entities" nil "facts" nil))))
        (multiple-value-bind (formed matched admitted supported dropped) (apply-formation kernel turns proposal)
          (append! kernel "graph.formation.done"
                   (obj "through_event_id" (first (car (last turns))) "turns" (length turns)
                        "entities_formed" formed "entities_matched" matched
                        "facts_admitted" admitted "facts_supported" supported
                        "dropped" (subseq dropped 0 (min 40 (length dropped)))
                        "model" (provider-profile-model profile)))
          (obj "through" (first (car (last turns))) "admitted" admitted "dropped" (length dropped)))))))

(define-handler "graph-form" :run #'form-graph :paid t :estimate-microusd 20000)

(defun request-graph-formation (kernel)
  "After the operator has been quiet a while, read what they said into the graph."
  (when (and (eq t (setting kernel "graph.formation"))
             (not (work-pending-p kernel "graph-form" "operator"))
             (< (spent-today kernel "graph") (* 1000000 (setting kernel "graph.daily_usd"))))
    (let ((last-turn (sql-row (kernel-db kernel)
                              "SELECT event_id,at FROM working_dialogue WHERE audience=?1 AND kind=?2 ORDER BY event_id DESC LIMIT 1"
                              "operator" "turn")))
      (when (and last-turn (> (first last-turn) (%formed-through kernel))
                 (>= (- (now) (parse-iso8601 (second last-turn))) (setting kernel "graph.formation_after_seconds")))
        (request-work kernel "graph-form" "operator" (obj) :priority :maintenance)))))
