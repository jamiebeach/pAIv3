(in-package #:pai-v3.recall)

;;; Interaction recall (design note interaction-recall §2-§5): anything the
;;; agent perceives or does is a cue; what the cue names exactly, and what
;;; it resembles in meaning, bring their impressions to mind, once per
;;; thought, within a budget.
;;;
;;; Two hooks, both the mind's general extension points:
;;; - at a stimulus, the context slot :recollection: the task and the last
;;;   lines of conversation, by name and identifier; the task also by
;;;   meaning (graph facts and interactions), with the memories nearest it;
;;; - at each tool call, a step note: the call's arguments and result, by
;;;   name and identifier only (cheap; no embedding per step).
;;;
;;; Every recollection is an event, so what came to mind, and from what,
;;; can be seen and the thresholds tuned.

(define-setting "recall.anchors_per_cue"
  :type :integer :min 1 :max 8 :default 3 :group "Memory"
  :description "Most things one cue brings to mind (a thought's start; a tool call brings at most two).")
(define-setting "recall.association_distance"
  :type :number :min 0 :max 1 :default 0.3d0 :group "Memory"
  :description "How close in meaning a fact or interaction must be to a cue to bring what it names to mind (cosine distance; lower is stricter). Start strict and loosen on evidence.")

(define-event-type "recall.recollected"
  :schema (("thought_id" :integer :optional) ("hook" :string) ("anchors" :array) ("cue" :string)))

(define-projection "recollections"
  :kind :inline
  :consumes ("recall.recollected")
  :tables ("CREATE TABLE IF NOT EXISTS recollections (thought_id INTEGER NOT NULL, anchor TEXT NOT NULL, hook TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(thought_id,anchor))")
  :reduce (lambda (db e)
            (let* ((p (event-payload e)) (thought (obj-get p "thought_id")))
              (when (integerp thought)
                (dolist (anchor (obj-get p "anchors"))
                  (sql-exec db "INSERT OR IGNORE INTO recollections(thought_id,anchor,hook,at) VALUES(?1,?2,?3,?4)"
                            thought anchor (obj-get p "hook") (event-at e)))))))

(defun recalled-in (kernel thought-id)
  "The anchors already brought to mind in THOUGHT-ID."
  (and (integerp thought-id)
       (mapcar #'first (sql-rows (kernel-db kernel) "SELECT anchor FROM recollections WHERE thought_id=?1 LIMIT 200" thought-id))))

(defun %record (kernel thought-id hook anchors cue)
  (append! kernel "recall.recollected"
           (obj "thought_id" thought-id "hook" hook "anchors" anchors
                "cue" (let ((c (substitute #\Space #\Newline cue))) (subseq c 0 (min 300 (length c)))))))

;;; From a cue to anchors

(defun %exact-anchors (kernel text)
  "Anchors TEXT names: graph entities by name or alias (those with most
facts first), and identifiers bound to a meaning, as the entity they name
or as an anchor of their own. Second value: the entity rows named."
  (let ((named (entities-named-in kernel text)))
    (values (remove-duplicates
             (append (mapcar #'first named)
                     (loop for (handle label) in (handles-in kernel text)
                           collect (let ((e (first (find-entities kernel label))))
                                     (if e (first e) (format nil "h:~a" handle)))))
             :test #'equal :from-end t)
            named)))

(defun %associated-anchors (kernel vector audience)
  "Anchors brought up by meaning: those named by the facts and interactions
nearest VECTOR, within the association distance."
  (let ((limit (setting kernel "recall.association_distance")))
    (remove-duplicates
     (append
      ;; Graph facts are the operator's knowledge.
      (when (equal audience "operator")
        (loop for (id d) in (facts-near kernel vector :pool 6)
              for row = (live-fact kernel id)
              when (and row (< d limit)) append (list (second row) (fourth row))))
      (loop for (nil d gist) in (experience-near kernel vector audience :pool 6)
            when (and gist (< d limit)) append (%exact-anchors kernel gist)))
     :test #'equal :from-end t)))

(defun %impressions (kernel anchors audience open-worlds)
  "(anchor text) for each of ANCHORS that has an impression."
  (loop for anchor in anchors
        for imp = (ignore-errors (impression kernel anchor audience :open-worlds open-worlds))
        when imp collect (list anchor (render-impression imp))))

;;; At a stimulus: what comes to mind as a thought starts

(define-slot-source :recollection (kernel request max)
  (let* ((audience (getf request :audience))
         (thought (getf request :thought-id))
         (task (or (getf request :query) (getf request :task) ""))
         (recent (format nil "~{~a~%~}" (mapcar #'dialogue-row-text
                                                (recent-dialogue kernel audience :token-budget 600 :max-rows 4))))
         (vector (ignore-errors (embed task kernel)))
         (already (recalled-in kernel thought))
         (most (setting kernel "recall.anchors_per_cue")))
    (multiple-value-bind (exact named) (%exact-anchors kernel (format nil "~a~%~a" task recent))
      (let* ((anchors (remove-if (lambda (a) (member a already :test #'equal))
                                 (remove-duplicates (append exact (and vector (%associated-anchors kernel vector audience)))
                                                    :test #'equal :from-end t)))
             (shown (%impressions kernel (subseq anchors 0 (min most (length anchors))) audience
                                  (open-worlds-for kernel named)))
             (texts (mapcar #'second shown))
             (memories (and vector
                            (remove-if (lambda (row) (some (lambda (text) (search (second row) text)) texts))
                                       (recall kernel vector audience :k 5)))))
        (when shown (%record kernel thought "stimulus" (mapcar #'first shown) task))
        (when memories
          (append! kernel "memory.recalled" (obj "memory_ids" (mapcar #'first memories) "thought_id" thought)))
        (append (mapcar (lambda (text) (list text (estimate-tokens text))) texts)
                (mapcar (lambda (row) (list (second row) (estimate-tokens (second row)))) memories)
                (unless vector
                  (let ((note "(Remembering by meaning is unavailable just now; only what was named comes to mind.)"))
                    (list (list note (estimate-tokens note))))))))))

;;; At each tool call: what the agent's own action brings to mind

(defparameter *step-anchors-most* 2 "Most things one tool call brings to mind.")

(define-step-note recollection-note (kernel thought-id name arguments prior result)
  (let* ((audience (getf (thought-row kernel thought-id) :audience))
         (cue (format nil "~a~%~a" (or arguments "") (subseq result 0 (min 4000 (length result)))))
         (already (recalled-in kernel thought-id))
         (anchors (remove-if (lambda (a) (member a already :test #'equal)) (%exact-anchors kernel cue)))
         (shown (%impressions kernel (subseq anchors 0 (min *step-anchors-most* (length anchors))) audience nil)))
    (when shown
      (%record kernel thought-id (format nil "step:~a" name) (mapcar #'first shown) cue)
      (format nil "~{~a~^~%~%~}" (mapcar #'second shown)))))
