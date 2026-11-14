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

(defun %reasoning-burn-p (reason)
  "True when REASON reports an empty reply whose completion tokens were nearly
all reasoning, such as \"6148 completion tokens, 6144 reasoning\"."
  (let ((c (search " completion tokens, " reason)) (r (search " reasoning" reason)))
    (when (and c r (search "empty response" reason))
      (let* ((start (1+ (or (position #\Space reason :end c :from-end t) -1)))
             (completion (ignore-errors (parse-integer reason :start start :end c)))
             (reasoned (ignore-errors (parse-integer reason :start (+ c (length " completion tokens, "))
                                                    :end (search " reasoning" reason :start2 c)))))
        (and completion reasoned (plusp completion) (>= reasoned (* 0.9 completion)))))))

(defun %failure-tally (kernel &optional from)
  "A line counting failed calls by profile and kind, so a pattern older than
the five shown can be seen and checked from inside (bead v3-vpp: 36 of one
agent's last 50 were reasoning burns, none among her latest five). Without
FROM, the last 100; with FROM (a day, YYYY-MM-DD), every failure from that day
on (up to 2000), so a before and an after can be read over fixed spans rather
than a window that drifts back as failures grow rare."
  (let* ((counts (make-hash-table :test 'equal)) (n 0) (since from)
         (ids (latest-event-ids kernel "provider.call.failed" (if from 2000 100)))
         ;; Every id read and still inside the span: the span may hold more than were counted.
         (full nil))
    (dolist (id ids)
      (let* ((e (event-by-id kernel id))
             (p (event-payload e))
             (reason (obj-get p "reason" ""))
             ;; A burn is told by its tokens too: some providers end one with
             ;; finish error, not length (Angel's October deepseek-graph empties).
             (kind (cond ((or (search "finish: length" reason) (%reasoning-burn-p reason)) "out of room (reasoning)")
                         ((search "empty response" reason) "empty")
                         (t (obj-get p "class" "other")))))
        (when (or (null from) (string>= (event-at e) from))
          (when (and from (= (length ids) 2000) (eql id (car (last ids)))) (setf full t))
          (incf n)
          (unless from (setf since (subseq (event-at e) 0 10)))  ; newest first: ends as the oldest day
          (incf (gethash (format nil "~a ~a" (obj-get p "profile" "?") kind) counts 0)))))
    (when (or (plusp n) from)
      ;; Against the calls that worked over the same days: a count that stops
      ;; growing may be a fix or only a quiet spell; a rate tells them apart.
      (let ((ok (sql-rows (kernel-db kernel) "SELECT profile,count(*) FROM cost_calls WHERE day>=?1 GROUP BY profile LIMIT 64" since)))
        (format nil "~:[Last ~d~;~d~]~:[~; or more (the count stopped at 2000)~] failed calls, since ~a~:[ (the oldest of the last 100; give since to fix the span)~;~], by profile and kind: ~:[none~;~:*~{~a~^; ~}~].~@[~%Calls that worked over the same days, by profile: ~{~{~a ~d~}~^; ~}.~]"
                from n full since from
                (sort (loop for k being the hash-keys of counts using (hash-value v)
                            collect (format nil "~a ×~d" k v))
                      #'string<)
                ok)))))

(defun %inspect-failures (kernel &optional since)
  "Recent failed model calls: why, and what the provider actually sent."
  (let ((ids (latest-event-ids kernel "provider.call.failed" 5)))
    (if (null ids)
        "No failed model calls on record."
        (format nil "~@[~a~%~%~]~{~a~^~%~%~}" (%failure-tally kernel since)
                (loop for id in ids
                      for e = (event-by-id kernel id)
                      for p = (event-payload e)
                      for raw = (sql-value (kernel-db kernel) "SELECT response FROM model_captures WHERE call_event_id=?1 LIMIT 1" id)
                      collect (format nil "Call ~d at ~a, ~a (~a): ~a~@[~%What was sent back: ~a~]"
                                      id (event-at e) (obj-get p "model") (obj-get p "class") (obj-get p "reason")
                                      (and raw (if (> (length raw) 3000) (format nil "~a…" (subseq raw 0 3000)) raw))))))))

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
  :description "Look at my own derived state, read-only. what: overview (every projection's status, cursor and problems), failures (recent failed model calls and what the provider sent back, opening with a tally by profile and kind against calls that worked; since, a day like 2026-11-13, fixes the tally's span so a before and an after compare fairly), flags (how often the provenance check flagged a claim of what I read, checked or saw without naming the record, by site; since as for failures), rows (a projection's rows; give name, and optionally table, column and value), quarantine (events a projection could not apply; optionally name), live_changes (code I defined or repaired in myself with lisp_eval: skills and repairs, and which were lost on restart; not recent ledger events), digest (row counts and a digest of a projection, for verification; give name)."
  :parameters (obj "type" "object"
                   "properties" (obj "what" (obj "type" "string" "enum" (list "overview" "failures" "flags" "rows" "quarantine" "live_changes" "digest"))
                                     "name" (obj "type" "string" "description" "A projection name, from the overview.")
                                     "table" (obj "type" "string")
                                     "column" (obj "type" "string")
                                     "value" (obj "type" "string")
                                     "limit" (obj "type" "integer")
                                     "since" (obj "type" "string" "description" "For failures and flags: count from this day (YYYY-MM-DD)."))
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
                       ((string= what "failures")
                        (let ((since (obj-get args "since")))
                          (%inspect-failures kernel (and (stringp since) (= (length since) 10) since))))
                       ((string= what "flags")
                        (let* ((since (obj-get args "since"))
                               (from (and (stringp since) (= (length since) 10) since))
                               (counts (pai-v3.mind::provenance-flag-counts kernel from)))
                          (format nil "Provenance flags~@[ since ~a~], by site (reply, forum, note; noted, not acted on): ~:[none~;~:*~{~{~a ~d~}~^; ~}~]."
                                  from (mapcar (lambda (c) (list (car c) (cdr c))) counts))))
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
                       (t (format nil "I can inspect overview, failures, flags, rows, quarantine, live_changes or digest, not ~a." what))))
             (not-inspectable (e) (princ-to-string e))))))
