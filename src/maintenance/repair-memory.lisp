(in-package #:pai-v3.maintenance)

;;; Memories lost to repeated ids (bead v3-clp.2.1). Memory ids came from
;;; Lisp's RANDOM, which starts from the same state in every process, so after
;;; a restart a new memory could take an id already in use. The projection
;;; kept the first and quarantined the rest; their content is only in the
;;; ledger. This forms each lost memory again under a new id, with the same
;;; kind, content, sensitivity, owner, strength and source, and clears its
;;; quarantine entry. A memory whose content is already held is not formed
;;; twice, so running it again changes nothing.

(defun reform-collided-memories (kernel &key (most 500))
  "Re-form memory.formed events the memory projection quarantined for a
repeated id. Returns (values re-formed already-held other)."
  (let ((db (kernel-db kernel)) (reformed 0) (held 0) (other 0))
    (dolist (row (sql-rows db "SELECT storage_sequence,event_id,message FROM v3_quarantine WHERE projection=?1 ORDER BY storage_sequence LIMIT ?2"
                           "memory" most))
      (destructuring-bind (seq event-id message) row
        (let* ((e (event-by-id kernel event-id))
               (p (and e (event-payload e))))
          (if (not (and e (equal "memory.formed" (event-type e)) (search "UNIQUE" message)))
              (incf other)
              (flet ((f (k) (let ((v (obj-get p k))) (if (eq v :null) nil v))))
                (if (sql-value db "SELECT 1 FROM memory_nodes WHERE content=?1 AND kind=?2 LIMIT 1" (f "content") (f "kind"))
                    (incf held)
                    (progn
                      (pai-v3.mind:form-memory kernel :kind (f "kind") :content (f "content")
                                                      :sensitivity (f "sensitivity") :owner (f "owner")
                                                      :strength (f "strength") :topic (f "topic")
                                                      :source-event-id (f "source_event_id"))
                      (incf reformed)))
                (sql-exec db "DELETE FROM v3_quarantine WHERE projection=?1 AND storage_sequence=?2" "memory" seq)
                (sql-exec db "DELETE FROM v3_degraded WHERE projection=?1 AND event_id=?2" "memory" event-id))))))
    (values reformed held other)))
