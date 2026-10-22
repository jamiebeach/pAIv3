(in-package #:pai-v3.mind)

;;; Reading the knowledge graph (bead v3-clp.6, stage 2): what the agent
;;; knows about the people and things in the conversation, a tool to look
;;; things up, and the search behind the operator's graph explorer page.
;;; The graph is the operator's: guests never see it.

(defparameter *graph-slot-facts* 5 "Facts entity-knowledge gives per entity.")

(defun %name-grams (text &key (longest 4) (most 400))
  "Every run of one to LONGEST words in TEXT, lower-case, for exact name lookup."
  (let* ((words (loop with start = 0
                      for end = (position-if-not (lambda (c) (or (alphanumericp c) (member c '(#\' #\- #\.)))) text :start start)
                      for word = (string-trim ".'-" (subseq text start (or end (length text))))
                      when (plusp (length word)) collect (string-downcase word)
                      while end do (setf start (1+ end))))
         (grams '()))
    (loop for i from 0 below (length words)
          do (loop for n from 1 to longest
                   while (<= (+ i n) (length words))
                   do (push (format nil "~{~a~^ ~}" (subseq words i (+ i n))) grams)))
    (subseq (remove-duplicates grams :test #'string=) 0 (min most (length (remove-duplicates grams :test #'string=))))))

(defun entities-named-in (kernel text)
  "Active entities whose name or alias appears in TEXT, those with most facts first."
  (let ((grams (%name-grams (or text ""))) (ids '()))
    (loop while grams
          do (let ((batch (subseq grams 0 (min 100 (length grams)))))
               (setf grams (nthcdr (length batch) grams))
               (dolist (r (apply #'sql-rows (kernel-db kernel)
                                 (format nil "SELECT entity_id FROM graph_names WHERE canonical IN (~a) LIMIT 200"
                                         (%in-list 1 (length batch)))
                                 batch))
                 (pushnew (first r) ids :test #'equal))))
    (let ((entities (remove-if-not (lambda (e) (and e (equal "active" (fifth e))))
                                   (remove-duplicates (mapcar (lambda (id) (graph-entity kernel id)) ids)
                                                      :key #'first :test #'equal))))
      (sort entities #'> :key (lambda (e) (length (entity-facts kernel (first e) :limit 100)))))))

(defun search-entities (kernel query &key (limit 12))
  "Entities whose names match QUERY in full text: all words, else any."
  (dolist (joiner '(" " " OR ") nil)
    (let* ((match (%fts-query query joiner))
           (ids (and match (mapcar #'first (sql-rows (kernel-db kernel)
                                                     "SELECT DISTINCT entity_id FROM graph_names_fts WHERE graph_names_fts MATCH ?1 ORDER BY rank LIMIT ?2"
                                                     match limit))))
           (entities (remove-if-not (lambda (e) (and e (equal "active" (fifth e))))
                                    (remove-duplicates (mapcar (lambda (id) (graph-entity kernel id)) ids)
                                                       :key #'first :test #'equal))))
      (when entities (return entities)))))

(defun %entity-name (kernel id) (or (third (graph-entity kernel id)) id))

(defun %lineage-of (lineage-json me)
  "Where a record came from, leaving out this agent's own last hop."
  (let ((hops (ignore-errors (json-parse lineage-json))))
    (when (and hops (equal (obj-get (car (last hops)) "agent") me) (rest hops))
      (setf hops (butlast hops)))
    (if hops (render-lineage hops) "unknown")))

(defun fact-ended (kernel fact-id)
  "The date FACT-ID stopped being true, when its valid_until is a date that
has passed; else NIL. A fact that has ended stays in the record."
  (let ((until (sql-value (kernel-db kernel) "SELECT valid_until FROM graph_facts WHERE id=?1 LIMIT 1" fact-id)))
    (and (stringp until) (>= (length until) 10)
         (every #'digit-char-p (remove #\- (subseq until 0 10)))
         (string< (subseq until 0 10) (subseq (now-iso8601) 0 10))
         (subseq until 0 10))))

(defun fact-first-hand (kernel fact-id)
  "The date of the newest first-hand evidence for FACT-ID (the operator's own
words in conversation), or NIL."
  (let ((dates (loop for ev in (fact-evidence kernel fact-id)
                     when (and (equal "original-utterance" (first ev)) (stringp (seventh ev)) (>= (length (seventh ev)) 10))
                       collect (subseq (seventh ev) 0 10))))
    (and dates (first (sort dates #'string>)))))

(defun render-fact (kernel row &key lineage)
  "One fact as a line: its statement (or subject predicate object), marked
when it is in a story's world or is not a plain positive assertion, with
where it came from."
  (destructuring-bind (id subject predicate object character derivation path scope polarity text world) row
    (declare (ignore path))
    (format nil "~@[[ended ~a] ~]~@[[in ~a] ~]~@[[~a] ~]~@[[not] ~]~a~@[ (~a)~]~:[~; (inferred)~]~@[ — ~a~]"
            (fact-ended kernel id)
            (world-name kernel world)
            (and (not (equal scope "assertion")) scope)
            (equal polarity "negative")
            (if (and text (plusp (length text)))
                text
                (format nil "~a ~a ~a" (%entity-name kernel subject) (substitute #\Space #\_ predicate) (%entity-name kernel object)))
            (and (not (member character '("unspecified" "timeless") :test #'equal)) character)
            (equal derivation "inferred")
            lineage)))

(defun %fact-lineage (kernel fact-id)
  (%lineage-of (sql-value (kernel-db kernel) "SELECT lineage FROM graph_facts WHERE id=?1 LIMIT 1" fact-id)
               (kernel-agent-id kernel)))

(defun open-worlds-for (kernel entities)
  "The worlds open given ENTITIES named: the real one, and a story's world
only when the story or someone in it is named."
  (declare (ignore kernel))
  (cons +real-world+
        (remove-duplicates
         (loop for e in entities
               collect (if (equal (tenth e) +real-world+) (first e) (tenth e)))
         :test #'equal)))

(defun entity-knowledge (kernel entity open-worlds)
  "What I know of ENTITY (a graph-entity row) within OPEN-WORLDS, as one
block: its live facts that have not ended, each with where it came from;
an entity with no facts at all says so, so it is never hidden or guessed
at. NIL when its facts are all in closed worlds or have ended."
  (let* ((all (entity-facts kernel (first entity) :limit (* 3 *graph-slot-facts*)))
         ;; What has ended is not what is so now.
         (facts (remove-if (lambda (f) (fact-ended kernel (first f)))
                           (remove-if-not (lambda (f) (member (nth 10 f) open-worlds :test #'equal)) all))))
    (when (or facts (null all))
      ;; First-hand facts lead, newest first, and say so: when two facts
      ;; disagree, the operator's own words outrank an import or an
      ;; inference, and newer first-hand words outrank older (bead v3-u3x).
      (let* ((dated (mapcar (lambda (f) (cons (fact-first-hand kernel (first f)) f)) facts))
             (ordered (stable-sort dated (lambda (a b) (and (car a) (or (null (car b)) (string> (car a) (car b)))))))
             (shown (subseq ordered 0 (min *graph-slot-facts* (length ordered)))))
        (format nil "~a (~a~@[, in ~a~]):~:[ no known connections.~%~;~:*~%~{  - ~a~%~}~]~:[~;  (Where these disagree, a first-hand fact outranks the rest, and a newer first-hand fact an older one.)~%~]"
                (third entity) (second entity) (world-name kernel (tenth entity))
                (mapcar (lambda (d) (format nil "~@[[first-hand ~a] ~]~a" (car d)
                                            (render-fact kernel (cdr d) :lineage (%fact-lineage kernel (second d)))))
                        shown)
                (some #'car shown))))))

;;; The tool

(defun %entity-card (kernel entity)
  (destructuring-bind (id kind name summary status merged origin path lineage world) entity
    (declare (ignore status merged origin path))
    (let ((facts (entity-facts kernel id :limit 40)))
      (format nil "~a (~a, ~a~@[, in the world of ~a~]) — from ~a~@[~%~a~]~@[~%Pictures: ~{~a~^, ~}~]~%~:[No facts.~;~:*~{~a~^~%~}~]"
              name kind id (world-name kernel world) (%lineage-of lineage (kernel-agent-id kernel)) summary
              (pictures-of kernel id)
              (mapcar (lambda (f)
                        (let ((evidence (fact-evidence kernel (first f))))
                          (format nil "- ~a [~a]~@[~%    \"~a\"~]" (render-fact kernel f) (first f)
                                  (and evidence (let ((q (second (first evidence))))
                                                  (if (> (length q) 200) (format nil "~a…" (subseq q 0 200)) q))))))
                      facts)))))

(define-tool "knowledge"
  :description "Look things up in my knowledge graph: who and what I know about, and what I know of them.
action: search (query: a name or words in one), entity (id, or name: the entity with its facts, a quote for
each, and where it came from), health (how complete the graph is), suggest (vague related_to facts that read like
a precise relation, and entities that may be one: for the operator to decide, then correct_knowledge). Facts are marked when they are not a plain
assertion ([question], [proposal], [reported-speech]...), negative ([not]) or inferred. Where a fact came from
reads like \"Alpha (v1) via Beta\": formed by Alpha and carried here by Beta."
  :summary "Look things up in my knowledge graph."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "search" "entity" "health" "suggest"))
                                     "query" (obj "type" "string") "id" (obj "type" "string") "name" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel thought-id args)
         (declare (ignore thought-id))
         (let ((action (obj-get args "action")))
           (cond
             ((equal action "suggest")
              (let ((lines (graph-suggestions kernel)))
                (if lines
                    (format nil "Suggestions only; nothing is changed until the operator agrees:~%~{- ~a~^~%~}" lines)
                    "Nothing to suggest: no vague relations or likely duplicates stand out.")))
             ((equal action "health")
              (let ((h (graph-health kernel)))
                (format nil "~d entities, ~d facts; ~d entities (~,0f%) have no facts; ~,0f% of facts are only related_to."
                        (obj-get h "entities") (obj-get h "facts") (obj-get h "unconnected")
                        (* 100 (obj-get h "unconnected_share")) (* 100 (obj-get h "related_to_share")))))
             ((equal action "search")
              (let ((found (search-entities kernel (or (obj-get args "query") ""))))
                (if found
                    (format nil "~{~a~^~%~}"
                            (mapcar (lambda (e) (format nil "~a (~a, ~a~@[, in ~a~]) — ~d facts, from ~a" (third e) (second e) (first e)
                                                        (world-name kernel (tenth e))
                                                        (length (entity-facts kernel (first e) :limit 200))
                                                        (%lineage-of (ninth e) (kernel-agent-id kernel))))
                                    found))
                    "Nothing in my graph by that name.")))
             ((equal action "entity")
              (let ((entity (or (and (obj-get args "id") (graph-entity kernel (obj-get args "id")))
                                (first (find-entities kernel (or (obj-get args "name") "")))
                                (first (search-entities kernel (or (obj-get args "name") ""))))))
                (if entity (%entity-card kernel entity) "No entity by that id or name.")))
             (t "action must be search, entity or health")))))

;;; The graph's shape, without its content: what a reviewer may see. Counts
;;; by origin, scope, predicate and kind and the health shares, never a name
;;; or a quote, so a coding agent can check lineage and health from inside
;;; without learning who or what the operator knows.

(defun %counts (kernel sql &rest params)
  (format nil "~{~a~^, ~}" (mapcar (lambda (r) (format nil "~a ~d" (first r) (second r)))
                                   (apply #'sql-rows (kernel-db kernel) sql params))))

(defun graph-shape (kernel)
  (let ((h (graph-health kernel)))
    (format nil "~d entities, ~d facts; ~d entities (~,1f%) have no facts; ~,1f% of facts are related_to.~%~
Facts by origin path: ~a~%Entities by origin path: ~a~%Facts by scope: ~a~%Facts by derivation: ~a~%~
Facts by predicate: ~a~%Entities by kind: ~a~%Facts by world: ~a"
            (obj-get h "entities") (obj-get h "facts") (obj-get h "unconnected")
            (* 100 (obj-get h "unconnected_share")) (* 100 (obj-get h "related_to_share"))
            (%counts kernel "SELECT path,count(*) FROM graph_facts WHERE status=?1 GROUP BY path ORDER BY 2 DESC LIMIT 20" "active")
            (%counts kernel "SELECT path,count(*) FROM graph_entities WHERE status=?1 GROUP BY path ORDER BY 2 DESC LIMIT 20" "active")
            (%counts kernel "SELECT scope,count(*) FROM graph_facts WHERE status=?1 GROUP BY scope ORDER BY 2 DESC LIMIT 20" "active")
            (%counts kernel "SELECT derivation,count(*) FROM graph_facts WHERE status=?1 GROUP BY derivation ORDER BY 2 DESC LIMIT 5" "active")
            (%counts kernel "SELECT predicate,count(*) FROM graph_facts WHERE status=?1 GROUP BY predicate ORDER BY 2 DESC LIMIT 40" "active")
            (%counts kernel "SELECT kind,count(*) FROM graph_entities WHERE status=?1 GROUP BY kind ORDER BY 2 DESC LIMIT 20" "active")
            (%counts kernel "SELECT world,count(*) FROM graph_facts WHERE world IN (SELECT world FROM graph_facts WHERE status=?1 LIMIT 100) AND status=?1 GROUP BY world ORDER BY 2 DESC LIMIT 20" "active"))))

(define-tool "knowledge_shape"
  :description "The shape of my knowledge graph without its content: counts by origin path, scope,
derivation, predicate and kind, and the health shares. No names and no quotes, so it is safe to use
when a coding agent asks me to check the graph."
  :summary "The shape of my knowledge graph, without its content."
  :available '("operator" "coding-agent")
  :run (lambda (kernel thought-id args) (declare (ignore thought-id args)) (graph-shape kernel)))

;;; The explorer page's search (v2's /api/v2/graph/search shape)

(defun graph-search (kernel &key query start-id (depth 2) (max-nodes 60))
  "Nodes and edges around START-ID, or around the entities QUERY names
exactly, or else those it matches in full text, as the explorer draws them."
  (let* ((exact (and (null start-id) query (find-entities kernel query)))
         (seeds (cond (start-id (let ((e (graph-entity kernel start-id))) (and e (list e))))
                      (exact exact)
                      (query (search-entities kernel query :limit 6))))
         (nodes (make-hash-table :test 'equal)) (edges (make-hash-table :test 'equal))
         (frontier (mapcar #'first seeds)) (truncated nil))
    (dolist (e seeds) (setf (gethash (first e) nodes) e))
    (loop repeat (max 1 (min 4 depth))
          while frontier
          do (let ((next '()))
               (dolist (id frontier)
                 (dolist (f (entity-facts kernel id :limit 40))
                   (destructuring-bind (fid subject predicate object &rest ignore) f
                     (declare (ignore ignore))
                     (setf (gethash fid edges) (list subject predicate object))
                     (dolist (other (list subject object))
                       (unless (gethash other nodes)
                         (if (>= (hash-table-count nodes) max-nodes)
                             (setf truncated t)
                             (let ((e (graph-entity kernel other)))
                               (when e (setf (gethash other nodes) e) (push other next)))))))))
               (setf frontier next)))
    (let ((node-list (loop for e being the hash-values of nodes
                           collect (let ((n (obj "node_id" (first e) "kind" (second e)
                                                 "label" (format nil "~a~@[ (in ~a)~]" (third e) (world-name kernel (tenth e))))))
                                     (setf (gethash "origin_agent_ids" n)
                                           (let ((hops (ignore-errors (json-parse (ninth e)))))
                                             (mapcar (lambda (h) (obj-get h "agent"))
                                                     (butlast hops))))
                                     n)))
          (edge-list (loop for (s p o) being the hash-values of edges
                           when (and (gethash s nodes) (gethash o nodes))
                             collect (obj "from_node_id" s "predicate" p "to_node_id" o))))
      (let ((result (obj "query_match_kind" (cond (start-id "starting-node") (exact "exact-identity")
                                                  (seeds "related-suggestions") (t "none"))
                         "node_count" (length node-list) "edge_count" (length edge-list)
                         "non_exhaustive" (if truncated t :false))))
        (setf (gethash "nodes" result) node-list (gethash "edges" result) edge-list)
        result))))
