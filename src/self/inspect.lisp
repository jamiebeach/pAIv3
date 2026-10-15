(in-package #:pai-v3.self)

;;; Inspection from inside (spec §16.6; agent doc §14 term 7). One read-only
;;; tool over the agent's own derived state: projection status and cursors,
;;; rows by bounded query, quarantine and degraded entities, live changes,
;;; and the counts and digests that migration verification compares (§9
;;; step 6b). It never reads the ledger, and credential tables show only
;;; their status, counts and digest. One tool keeps the index to one line.

(defun find-projection-p (name)
  (ignore-errors (find-projection name)))

(defun %inspect-overview (kernel)
  (format nil "Ledger head ~d.~%~{~a~^~%~}" (ledger-head kernel)
          (mapcar (lambda (p)
                    (format nil "~a (~a, ~a, v~d): ~a~@[, ~d behind~]~@[, ~d quarantined~]~@[, ~d degraded~]"
                            (obj-get p "name") (obj-get p "kind") (obj-get p "derivation")
                            (obj-get p "version") (obj-get p "status")
                            (let ((b (obj-get p "behind"))) (and b (plusp b) b))
                            (let ((q (obj-get p "quarantined"))) (and (plusp q) q))
                            (let ((d (obj-get p "degraded"))) (and (plusp d) d))))
                  (projections-overview kernel))))

(defun %inspect-quarantine (kernel name)
  (let ((events (quarantined-events kernel :projection name :limit 20))
        (degraded (and name (degraded-entities kernel name :limit 20))))
    (if (or events degraded)
        (format nil "~{~a~^~%~}"
                (append (mapcar (lambda (r) (destructuring-bind (projection event class message attempts) r
                                              (format nil "~a: event ~d, ~a (~d attempts): ~a"
                                                      projection event class attempts message)))
                                events)
                        (mapcar (lambda (r) (format nil "degraded ~a since event ~d: ~a" (first r) (second r) (third r)))
                                degraded)))
        "Nothing is quarantined.")))

(defun %inspect-live-changes (kernel)
  (let ((rows (append (live-changes kernel) (live-changes kernel :status "lost-on-restart" :limit 20))))
    (if rows
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (r) (format nil "~a (~a, ~a)" (first r) (second r) (subseq (fourth r) 0 12)))
                        rows))
        "No live changes.")))

(defun %inspect-digest (kernel name)
  (format nil "~a: rows ~a, digest ~a" name
          (json-encode (projection-counts kernel name)) (projection-digest kernel name)))

(define-tool "inspect"
  :description "Look at my own derived state, read-only. what: overview (every projection's status, cursor and problems), rows (a projection's rows; give name, and optionally table, column and value), quarantine (events a projection could not apply; optionally name), live_changes (my skills and repairs), digest (row counts and a digest of a projection, for verification; give name)."
  :parameters (obj "type" "object"
                   "properties" (obj "what" (obj "type" "string" "enum" (list "overview" "rows" "quarantine" "live_changes" "digest"))
                                     "name" (obj "type" "string" "description" "A projection name, from the overview.")
                                     "table" (obj "type" "string")
                                     "column" (obj "type" "string")
                                     "value" (obj "type" "string")
                                     "limit" (obj "type" "integer"))
                   "required" (list "what"))
  :available :operator-trust
  :core '("introspection")
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((name (obj-get args "name")))
           (when (equal name "") (setf name nil))
           (handler-case
               (let ((what (obj-get args "what" "overview")))
                 (cond ((string= what "overview") (%inspect-overview kernel))
                       ((string= what "quarantine") (%inspect-quarantine kernel name))
                       ((string= what "live_changes") (%inspect-live-changes kernel))
                       ((null name) (format nil "~a needs a projection name; see the overview." what))
                       ((not (find-projection-p name)) (format nil "There is no projection ~a." name))
                       ((string= what "digest") (%inspect-digest kernel name))
                       ((string= what "rows")
                        (let ((rows (projection-rows kernel name
                                                     :table (obj-get args "table")
                                                     :column (obj-get args "column")
                                                     :value (obj-get args "value")
                                                     :limit (max 1 (min 50 (obj-get args "limit" 10))))))
                          (if rows
                              (format nil "~{~a~^~%~}" (mapcar #'json-encode rows))
                              "No rows match.")))
                       (t (format nil "I can inspect overview, rows, quarantine, live_changes or digest, not ~a." what))))
             (not-inspectable (e) (princ-to-string e))))))
