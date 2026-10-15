(in-package #:pai-v3.maintenance)

;;; Explicit rebuild and verification (engineering spec §4.3, §14).
;;; Maintenance reads history deliberately, in bounded pages, one transaction
;;; per page, resumable after interruption. Never called by the runtime.

(define-condition model-derived-rebuild-refused (error)
  ((name :initarg :name :reader refused-name))
  (:report (lambda (c s)
             (format s "Projection ~s is model-derived; rebuilding it would repeat paid cognition. Adopt existing rows or run an approved regeneration."
                     (refused-name c)))))

(defun %begin-rebuild (db projection)
  (dolist (table (owned-tables projection))
    (sql-exec db (format nil "DELETE FROM ~a" table)))
  (sql-exec db "DELETE FROM v3_quarantine WHERE projection=?1" (projection-name projection))
  (sql-exec db "DELETE FROM v3_degraded WHERE projection=?1" (projection-name projection))
  (%set-state db (projection-name projection) (projection-version projection)
              (projection-kind projection) 0 "rebuilding"))

(defun %rebuild-page (kernel projection cursor page-size)
  "Apply one page after CURSOR; return the new cursor and the page length."
  (let ((events (%events-after kernel cursor (projection-consumes projection) page-size))
        (db (kernel-db kernel)))
    (dolist (event events) (%apply-one db projection event))
    (let ((next (if events (event-seq (car (last events))) cursor)))
      (%set-state db (projection-name projection) (projection-version projection)
                  (projection-kind projection) next "rebuilding")
      (values next (length events)))))

(defun rebuild-projection (kernel name &key (page-size 500) max-pages)
  "Rebuild pure projection NAME from the ledger. Resumes an interrupted
rebuild from its persisted cursor. Returns :DONE, or :PAUSED when MAX-PAGES
pages have been applied. Working memory is one page."
  (let* ((projection (find-projection name))
         (db (kernel-db kernel)))
    (unless (eq (projection-derivation projection) :pure)
      (error 'model-derived-rebuild-refused :name name))
    (with-transaction (db)
      (unless (equal "rebuilding" (fourth (%state db name)))
        (%begin-rebuild db projection)))
    (loop with pages = 0
          for cursor = (third (%state db name))
          do (when (and max-pages (>= pages max-pages))
               (return :paused))
             (multiple-value-bind (next count)
                 (with-transaction (db) (%rebuild-page kernel projection cursor page-size))
               (declare (ignore next))
               (incf pages)
               (when (< count page-size)
                 ;; Finish under the kernel lock so no append slips between the
                 ;; last page read and the projection becoming ready.
                 (bt:with-recursive-lock-held ((kernel-lock kernel))
                   (with-transaction (db)
                     (loop for c = (third (%state db name))
                           for (nil n) = (multiple-value-list
                                          (%rebuild-page kernel projection c page-size))
                           while (= n page-size))
                     (%set-state db name (projection-version projection)
                                 (projection-kind projection) (ledger-head kernel) "ready")))
                 (return :done))))))

(defun verify-projection (kernel name scratch-path)
  "Rebuild NAME on a scratch copy of the store and compare digests with the
live projection. Returns T when they match, plus both digests."
  (sql-exec (kernel-db kernel) "VACUUM INTO ?1" scratch-path)
  (let ((copy (open-kernel scratch-path :agent-id (kernel-agent-id kernel)
                                         :key (pai-v3.kernel::db-key (kernel-db kernel)))))
    (unwind-protect
         (progn
           (rebuild-projection copy name)
           (let ((live (projection-digest kernel name))
                 (rebuilt (projection-digest copy name)))
             (values (string= live rebuilt) live rebuilt)))
      (close-kernel copy)
      (ignore-errors (delete-file scratch-path)))))
