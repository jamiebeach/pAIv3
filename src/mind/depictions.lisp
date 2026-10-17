(in-package #:pai-v3.mind)

;;; Depictions (bead v3-3za): which stored images show which graph
;;; entities. Images are not entities, so this is not a fact: it is a link
;;; kept beside the graph, following its merges, so that "a picture of
;;; someone" is a lookup. Images an agent makes for or from someone are
;;; linked as they are made; a picture the operator sends is linked when the
;;; agent is told who is in it.

(define-event-type "image.depicts"
  :schema (("image_id" :string) ("entity_id" :string) ("how" :string)
           ("source_event_id" :integer :optional)))

(define-projection "depictions"
  :kind :inline
  :consumes ("image.depicts" "graph.entity.merged" "graph.reset")
  :tables ("CREATE TABLE IF NOT EXISTS depictions (image_id TEXT NOT NULL, entity_id TEXT NOT NULL, how TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(image_id,entity_id))"
           "CREATE INDEX IF NOT EXISTS depictions_entity_idx ON depictions(entity_id,at)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (cond ((string= type "graph.reset") (sql-exec db "DELETE FROM depictions"))
                    ((string= type "graph.entity.merged")
                     (sql-exec db "UPDATE OR IGNORE depictions SET entity_id=?2 WHERE entity_id=?1"
                               (obj-get p "entity_id") (obj-get p "into"))
                     (sql-exec db "DELETE FROM depictions WHERE entity_id=?1" (obj-get p "entity_id")))
                    (t (sql-exec db "INSERT OR IGNORE INTO depictions(image_id,entity_id,how,at) VALUES(?1,?2,?3,?4)"
                                 (obj-get p "image_id") (obj-get p "entity_id") (obj-get p "how") (event-at e)))))))

(defparameter +depiction-hows+ '("made" "shown" "said")
  "made: the agent made it of the entity; shown: it came with an image of
the entity as a reference; said: the operator said who or what it shows.")

(defun note-depiction (kernel image-id entity-id &key (how "said") source-event-id)
  "Link stored image IMAGE-ID to graph entity ENTITY-ID. Returns the entity's id."
  (unless (image-row kernel image-id) (%graph-refuse "there is no stored image ~a" image-id))
  (let ((entity (graph-entity kernel entity-id)))
    (unless entity (%graph-refuse "there is no entity ~a" entity-id))
    (append! kernel "image.depicts" (obj "image_id" image-id "entity_id" (first entity) "how" how
                                         "source_event_id" source-event-id))
    (first entity)))

(defun pictures-of (kernel entity-id &key (limit 3))
  "Stored images showing ENTITY-ID, newest first."
  (let ((entity (graph-entity kernel entity-id)))
    (and entity
         (mapcar #'first (sql-rows (kernel-db kernel)
                                   "SELECT image_id FROM depictions WHERE entity_id=?1 ORDER BY at DESC LIMIT ?2"
                                   (first entity) limit)))))

(defun depicted-in (kernel image-ids)
  "The entity ids the images IMAGE-IDS show."
  (when image-ids
    (remove-duplicates
     (mapcar #'first (apply #'sql-rows (kernel-db kernel)
                            (format nil "SELECT entity_id FROM depictions WHERE image_id IN (~a) LIMIT 50"
                                    (%in-list 1 (length image-ids)))
                            image-ids))
     :test #'equal)))

(defun %entity-by-name-or-id (kernel ref)
  (or (let ((e (graph-entity kernel ref))) (and e (first e)))
      (let ((found (find-entities kernel ref)))
        (cond ((null found) (%graph-refuse "I know no one and nothing named ~a" ref))
              ((rest found) (%graph-refuse "~a could be ~{~a~^ or ~}; give the id" ref
                                           (mapcar (lambda (e) (format nil "~a (~a, ~a)" (third e) (second e) (first e))) found)))
              (t (first (first found)))))))

(defun note-depictions (kernel image-id refs &key (how "said") source-event-id)
  "Link IMAGE-ID to each of REFS (entity ids or names). Returns the names linked."
  (loop for ref in refs
        for id = (%entity-by-name-or-id kernel ref)
        do (note-depiction kernel image-id id :how how :source-event-id source-event-id)
        collect (third (graph-entity kernel id))))

(define-tool "note_picture"
  :description "Say who or what a stored image shows, so I can find a picture of them later (to look at, or as a
reference when I make an image of them). image: the stored image id (from a picture the operator sent, or one I
made); shows: names or entity ids from my knowledge graph. Use it when the operator tells me who is in a picture."
  :summary "Record who or what a stored image shows."
  :available :operator-trust
  :parameters (obj "type" "object"
                   "properties" (obj "image" (obj "type" "string")
                                     "shows" (obj "type" "array" "items" (obj "type" "string")))
                   "required" (list "image" "shows"))
  :run (lambda (kernel thought-id args)
         (handler-case
             (let ((names (note-depictions kernel (obj-get args "image")
                                           (remove-if-not #'stringp (or (obj-get args "shows") '()))
                                           :source-event-id thought-id)))
               (if names
                   (format nil "Noted: image ~a shows ~{~a~^, ~}." (obj-get args "image") names)
                   "Name who or what it shows."))
           (graph-refused (e) (format nil "Not noted: ~a" e)))))
