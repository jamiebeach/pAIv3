(in-package #:pai-v3.mind)

;;; Recovering unconnected entities (bead v3-clp.6). An entity with no facts
;;; stays visible and unconnected: nothing is deleted, and no fact is made up
;;; to connect it (v2's rule, pai-gmw.4.5). Recovery reads the messages that
;;; actually mention it and, with the formation call focused on it, records
;;; the facts those messages support, each with a quote copied from them.
;;;
;;; The facts are formed here, from conversation that came from elsewhere, so
;;; their lineage is this agent and each quote keeps its true origin: an
;;; Alpha-era message says alpha-v1, Beta's own says beta.

(define-event-type "graph.recovery.done"
  :schema (("entity_id" :string) ("mentions" :integer) ("facts_admitted" :integer)
           ("facts_supported" :integer) ("entities_formed" :integer) ("dropped" :array :optional)
           ("model" :string)))

(define-projection "graph_recovery"
  :kind :inline
  :consumes ("graph.recovery.done" "graph.reset")
  :tables ("CREATE TABLE IF NOT EXISTS graph_recovery_runs (entity_id TEXT PRIMARY KEY, mentions INTEGER NOT NULL, facts_admitted INTEGER NOT NULL, facts_supported INTEGER NOT NULL, entities_formed INTEGER NOT NULL, dropped INTEGER NOT NULL, at TEXT NOT NULL)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (string= (event-type e) "graph.reset")
                  (sql-exec db "DELETE FROM graph_recovery_runs")
                  (sql-exec db "INSERT INTO graph_recovery_runs(entity_id,mentions,facts_admitted,facts_supported,entities_formed,dropped,at) VALUES(?1,?2,?3,?4,?5,?6,?7) ON CONFLICT(entity_id) DO UPDATE SET mentions=excluded.mentions,facts_admitted=facts_admitted+excluded.facts_admitted,facts_supported=facts_supported+excluded.facts_supported,entities_formed=entities_formed+excluded.entities_formed,dropped=excluded.dropped,at=excluded.at"
                            (obj-get p "entity_id") (obj-get p "mentions") (obj-get p "facts_admitted")
                            (obj-get p "facts_supported") (obj-get p "entities_formed") (length (obj-get p "dropped"))
                            (event-at e))))))

(defparameter *recovery-characters* 8000 "Most message text one recovery call reads.")

(defun %mention-turns (orphan source-agent earlier-agent earlier-generation)
  "The messages that mention ORPHAN as formation sources, within the budget;
a long message is cut around its first mention of the name."
  (let ((names (remove-if-not (lambda (n) (and (stringp n) (>= (length n) 3)))
                              (cons (obj-get orphan "label") (obj-get orphan "aliases"))))
        (used 0))
    (loop for m in (obj-get orphan "mentions")
          for text = (obj-get m "text")
          for at = (or (loop for n in names thereis (search n text :test #'char-equal)) 0)
          for cut = (if (> (length text) 1500)
                        (subseq text (max 0 (- at 600)) (min (length text) (+ at 900)))
                        text)
          while (<= (+ used (length cut)) *recovery-characters*)
          do (incf used (length cut))
          collect (list (obj-get m "event_id")
                        (if (equal "operator" (obj-get m "speaker")) "turn" "reply")
                        (obj-get m "speaker")
                        (format nil "~:[Agent~;Operator~]: ~a" (equal "operator" (obj-get m "speaker")) cut)
                        (if (equal "migrated" (obj-get m "origin"))
                            (format nil "~a-~a via ~a" earlier-agent earlier-generation source-agent)
                            source-agent)
                        (format nil "~a:event:~a" source-agent (obj-get m "event_id"))))))

(defun recover-orphans (kernel export &key (limit 200) source-agent earlier-agent
                                        (earlier-generation "v1"))
  "For each unconnected entity in EXPORT that is in this graph, still
unconnected and not yet tried, one focused formation call over the messages
that mention it. Returns counts. SOURCE-AGENT and EARLIER-AGENT are required,
as for IMPORT-GRAPH."
  (assert (and (stringp source-agent) (stringp earlier-agent)) () "Name the source and earlier agents.")
  (let ((tried 0) (admitted 0) (supported 0) (dropped 0) (skipped 0))
    (dolist (orphan (obj-get export "orphans"))
      (let* ((id (obj-get orphan "entity_id"))
             (entity (graph-entity kernel id)))
        (cond
          ((>= tried limit) (return))
          ((or (null entity) (entity-facts kernel id :limit 1)
               (sql-value (kernel-db kernel) "SELECT 1 FROM graph_recovery_runs WHERE entity_id=?1 LIMIT 1" id))
           (incf skipped))
          (t
           (let ((turns (%mention-turns orphan source-agent earlier-agent earlier-generation)))
             (when turns
               (incf tried)
               (let* ((profile (profile-for kernel "graph"))
                      (reply (handler-case
                                 (complete kernel profile (formation-messages kernel turns :focus entity)
                                           :tools (list +formation-tool+) :lane "graph")
                               (error (e) (format *error-output* "~&recovery ~a: ~a~%" id e) nil)))
                      (call (and reply (find "record_knowledge" (obj-get reply "tool_calls")
                                             :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal)))
                      (proposal (or (and call (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments"))))
                                    (obj "entities" nil "facts" nil))))
                 (multiple-value-bind (formed matched a s d) (apply-formation kernel turns proposal)
                   (declare (ignore matched))
                   (incf admitted a) (incf supported s) (incf dropped (length d))
                   (append! kernel "graph.recovery.done"
                            (obj "entity_id" id "mentions" (length turns) "facts_admitted" a "facts_supported" s
                                 "entities_formed" formed "dropped" (subseq d 0 (min 20 (length d)))
                                 "model" (provider-profile-model profile))))))))))
      (when (and (>= tried limit)) (return)))
    (obj "tried" tried "skipped" skipped "facts_admitted" admitted "facts_supported" supported "dropped" dropped
         "health" (graph-health kernel))))
