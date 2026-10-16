(in-package #:pai-v3.maintenance)

;;; Event-type census (migration checklist B3; engineering spec §9 step 2).
;;; Reads a COPY of an agent's events store, read-only, in storage order and
;;; bounded pages, and reports what is in it: every event type with counts,
;;; first and last time seen, envelope kinds, payload key-set variants, size
;;; distribution, and the edge cases the type census must decide on
;;; (oversized payloads, empty timestamps, event-id gaps and duplicates).
;;; Content is never copied into the report; only key names and numbers.

(defparameter *variant-limit* 12 "Key-set variants kept per type; the rest are counted.")

(defstruct type-stats
  (count 0) first-seen last-seen (v2 0) (v3 0) (other-envelope 0)
  (sizes '()) (oversized 0) (empty-time 0) (caused 0) (bad-json 0)
  (variants (make-hash-table :test 'equal)) (extra-variants 0))

(defun %keys (payload)
  (if (hash-table-p payload)
      (format nil "~{~a~^,~}" (sort (loop for k being the hash-keys of payload collect k) #'string<))
      (format nil "<~(~a~)>" (type-of payload))))

(defun %note (stats seq-time size json limit)
  (incf (type-stats-count stats))
  (when (and seq-time (plusp (length seq-time)))
    (unless (type-stats-first-seen stats) (setf (type-stats-first-seen stats) seq-time))
    (setf (type-stats-last-seen stats) seq-time))
  (when (or (null seq-time) (zerop (length seq-time))) (incf (type-stats-empty-time stats)))
  (push size (type-stats-sizes stats))
  (when (> size limit) (incf (type-stats-oversized stats)))
  (when (null json) (return-from %note))
  (handler-case
      (let* ((raw (json-parse json))
             (variant (%keys (obj-get raw "payload")))
             (variants (type-stats-variants stats)))
        (cond ((eql 3 (obj-get raw "envelope")) (incf (type-stats-v3 stats)))
              ((eql 1 (obj-get raw "schema_version")) (incf (type-stats-v2 stats)))
              (t (incf (type-stats-other-envelope stats))))
        (when (integerp (obj-get raw "caused_by")) (incf (type-stats-caused stats)))
        (if (or (gethash variant variants) (< (hash-table-count variants) *variant-limit*))
            (incf (gethash variant variants 0))
            (incf (type-stats-extra-variants stats))))
    (error () (incf (type-stats-bad-json stats)))))

(defun %percentile (sorted p)
  (if sorted (nth (min (1- (length sorted)) (floor (* p (length sorted)))) sorted) 0))

(defun census (path &key agent-id (page-size 200) (limit-bytes *payload-limit-bytes*) (read-limit (* 1024 1024)))
  "Census of the events store at PATH (opened read-only). With AGENT-ID,
only that agent's partition. Returns a report object."
  (let ((db (db-open path :read-only t))
        (types (make-hash-table :test 'equal))
        (total 0) (previous-id nil) (gaps 0) (missing 0) (duplicates 0)
        (agents (make-hash-table :test 'equal)))
    (unwind-protect
         (loop with after = 0
               for rows = (sql-rows db "SELECT storage_sequence,event_id,agent_id,event_type,occurred_at,length(CAST(event_json AS BLOB)),CASE WHEN length(CAST(event_json AS BLOB))<=?3 THEN event_json END FROM pai_events WHERE storage_sequence>?1 ORDER BY storage_sequence LIMIT ?2"
                                    ;; An oversized event's body is not read: a page of
                                    ;; them once needed more memory than there was.
                                    after page-size read-limit)
               while rows
               do (loop for (seq id agent type at size json) in rows
                        ;; Event ids are global across agents: gaps and
                        ;; duplicates are measured over the whole store.
                        do (when previous-id
                             (cond ((= id previous-id) (incf duplicates))
                                   ((> id (1+ previous-id)) (incf gaps) (incf missing (- id previous-id 1)))))
                           (setf previous-id (max id (or previous-id id)))
                        when (or (null agent-id) (equal agent agent-id))
                          do (incf total)
                             (incf (gethash agent agents 0))
                             (%note (or (gethash type types)
                                        (setf (gethash type types) (make-type-stats)))
                                    at size json limit-bytes))
                  (setf after (first (car (last rows)))))
      (db-close db))
    (obj "agent_id" (or agent-id :null)
         "events" total
         "agents" (loop for a being the hash-keys of agents using (hash-value n) collect (obj "agent_id" a "events" n))
         "event_id_gaps" gaps "event_ids_missing" missing "event_id_duplicates" duplicates
         "types"
         (sort (loop for type being the hash-keys of types using (hash-value s)
                     collect (let ((sizes (sort (type-stats-sizes s) #'<)))
                               (obj "type" type "count" (type-stats-count s)
                                    "first_seen" (or (type-stats-first-seen s) :null)
                                    "last_seen" (or (type-stats-last-seen s) :null)
                                    "v2_envelopes" (type-stats-v2 s) "v3_envelopes" (type-stats-v3 s)
                                    "other_envelopes" (type-stats-other-envelope s)
                                    "bytes_p50" (%percentile sizes 0.5) "bytes_p95" (%percentile sizes 0.95)
                                    "bytes_max" (if sizes (car (last sizes)) 0)
                                    "oversized" (type-stats-oversized s)
                                    "empty_occurred_at" (type-stats-empty-time s)
                                    "with_caused_by" (type-stats-caused s)
                                    "unparseable" (type-stats-bad-json s)
                                    "key_variants" (sort (loop for v being the hash-keys of (type-stats-variants s)
                                                                 using (hash-value n)
                                                               collect (obj "keys" v "count" n))
                                                         #'> :key (lambda (o) (obj-get o "count")))
                                    "other_variants" (type-stats-extra-variants s)
                                    "declared" (if (find-event-type type nil) t :false))))
               #'> :key (lambda (o) (obj-get o "count"))))))

(defun census-markdown (report)
  "Human-readable summary of a census REPORT."
  (with-output-to-string (s)
    (format s "# Event-type census~%~%Events: ~d. Event-id gaps: ~d (~d ids missing). Duplicate ids: ~d.~%~%"
            (obj-get report "events") (obj-get report "event_id_gaps")
            (obj-get report "event_ids_missing") (obj-get report "event_id_duplicates"))
    (format s "| Type | Count | Declared | v2/v3 | p95 bytes | Max bytes | Oversized | Empty time | Key variants |~%")
    (format s "| --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: |~%")
    (dolist (row (obj-get report "types"))
      (flet ((g (k) (obj-get row k)))
        (format s "| `~a` | ~d | ~:[no~;yes~] | ~d/~d | ~d | ~d | ~d | ~d | ~d~:[~;+~] |~%"
                (g "type") (g "count") (eq t (g "declared")) (g "v2_envelopes") (g "v3_envelopes")
                (g "bytes_p95") (g "bytes_max") (g "oversized") (g "empty_occurred_at")
                (length (g "key_variants")) (plusp (g "other_variants")))))))
