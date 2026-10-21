(handler-bind ((warning #'muffle-warning))
  (ql:quickload "pai-v3-maintenance" :silent t))

(in-package #:pai-v3.test)
(use-package '#:pai-v3.maintenance)

(define-event-type "test.tick" :schema (("n" :integer)))

(define-projection "test.sum"
  :kind :inline :consumes ("test.tick")
  :tables ("CREATE TABLE IF NOT EXISTS t_sum (k INTEGER PRIMARY KEY CHECK(k=1), total INTEGER NOT NULL, count INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (sql-exec db "INSERT INTO t_sum(k,total,count) VALUES(1,?1,1) ON CONFLICT(k) DO UPDATE SET total=total+excluded.total,count=count+1"
                      (obj-get (event-payload e) "n"))))

(define-projection "test.model"
  :kind :async :consumes ("test.tick") :derivation :model
  :reduce (lambda (db e) (declare (ignore db e))))

(defun sum-row (k)
  (sql-row (kernel-db k) "SELECT total,count FROM t_sum WHERE k=1 LIMIT 1"))

(defun bump-version (name delta)
  (incf (pai-v3.kernel::projection-version (find-projection name)) delta))

(deftest rebuild-restores-a-needs-rebuild-projection
  (let ((path (format nil "/tmp/pai-v3-rb-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path)
      (dotimes (i 25) (append! k "test.tick" (obj "n" i))))
    (bump-version "test.sum" 1)
    (unwind-protect
         (with-temp-kernel (k :path path)
           (is (equal "needs-rebuild" (projection-status k "test.sum")))
           (is (eq :done (rebuild-projection k "test.sum" :page-size 7)))
           (is (equal "ready" (projection-status k "test.sum")))
           (is (equal '(300 25) (sum-row k)))
           (append! k "test.tick" (obj "n" 1000))
           (is (equal '(1300 26) (sum-row k)) "inline application resumes"))
      (bump-version "test.sum" -1))))

(deftest rebuild-is-resumable-after-interruption
  (with-temp-kernel (k)
    (dotimes (i 25) (append! k "test.tick" (obj "n" 1)))
    (is (eq :paused (rebuild-projection k "test.sum" :page-size 5 :max-pages 2)))
    (is (equal "rebuilding" (projection-status k "test.sum")))
    (is (= 10 (projection-cursor k "test.sum")) "cursor persisted per page")
    (append! k "test.tick" (obj "n" 1))
    (is (eq :done (rebuild-projection k "test.sum" :page-size 5)))
    (is (equal '(26 26) (sum-row k)) "no event lost or applied twice")))

(deftest model-derived-projections-are-never-rebuilt
  (with-temp-kernel (k)
    (signals model-derived-rebuild-refused (rebuild-projection k "test.model"))))

(deftest verify-matches-incremental-state
  (with-temp-kernel (k)
    (dotimes (i 40) (append! k "test.tick" (obj "n" (* i i))))
    (multiple-value-bind (ok live rebuilt)
        (verify-projection k "test.sum" (format nil "/tmp/pai-v3-verify-~d.sqlite3" (random 1000000)))
      (is ok (format nil "~a vs ~a" live rebuilt)))
    ;; Corrupt the live row: verification must notice.
    (sql-exec (kernel-db k) "UPDATE t_sum SET total=total+1 WHERE k=1")
    (is (not (verify-projection k "test.sum" (format nil "/tmp/pai-v3-verify-~d.sqlite3" (random 1000000)))))))

(deftest concern-horizons-survive-replay-with-old-notes
  (with-temp-kernel (k)
    (let ((c (pai-v3.mind:raise-concern k "Await the synthetic report")))
      (append! k "concern.noted" (obj "concern_id" c "text" "Old note without a horizon"))
      (append! k "concern.noted" (obj "concern_id" c "text" "Wait"
                                     "next_look_at" "2027-01-01T12:00:00Z"))
      (append! k "concern.noted" (obj "concern_id" c "text" "A later note preserves it"))
      (is (eq :done (rebuild-projection k "concerns" :page-size 2)))
      (is (equal "2027-01-01T12:00:00Z" (nth 10 (pai-v3.mind:concern-row k c))))
      (append! k "concern.noted" (obj "concern_id" c "text" "Clear" "next_look_at" ""))
      (is (eq :done (rebuild-projection k "concerns" :page-size 2)))
      (is (null (nth 10 (pai-v3.mind:concern-row k c)))))))

(deftest reflection-projections-rebuild-with-conclusions-and-handoff-intact
  (with-temp-kernel (k)
    (let* ((reflection (pai-v3.mind:request-thought k "introspection" "PRIVATE reflection"))
           (made (append! k "fleet.draft.made"
                          (obj "draft_id" "d-synthetic" "thought_id" reflection "peer_id" "beta"
                               "intent" "fyi" "text" "The public map is ready." "why" "PRIVATE reason"))))
      (append! k "thought.ended" (obj "thought_id" reflection "outcome" "concluded" "summary" "A map is already ready"))
      (let ((speaking (pai-v3.mind:request-thought k "work" "Check the public draft"
                                                 :audience "peer:beta" :source-event-id (event-id made))))
        (is (eq :done (rebuild-projection k "fleet_drafts" :page-size 2)))
        (is (equal (list "speaking" speaking)
                   (sql-row (kernel-db k) "SELECT state,speaking_thought_id FROM fleet_drafts WHERE draft_id=?1 LIMIT 1" "d-synthetic")))
        (is (eq :done (rebuild-projection k "self_activity" :page-size 2)))
        (is (equal "A map is already ready"
                   (sql-value (kernel-db k) "SELECT conclusion FROM self_activity WHERE thought_id=?1 LIMIT 1" reflection)))))))

(deftest work-admission-frontier-survives-explicit-rebuild
  (with-temp-kernel (k)
    (let* ((id (request-work k "synthetic.paid" "resume-me" nil :priority :operator))
           (deferred (append! k "work.deferred"
                             (obj "work_id" id "reason" "daily budget exhausted"
                                  "next_at" "2099-01-01T00:00:00Z"))))
      (change-setting k "budget.daily_microusd" 3000000
                      :origin (obj "principal_type" "operator"))
      (is (eq :done (rebuild-projection k "kernel.work" :page-size 2)))
      (is (= (event-id deferred)
             (sql-value (kernel-db k) "SELECT admission_event FROM v3_work WHERE id=?1 LIMIT 1" id)))
      (is (= id (first (pai-v3.kernel::%next-due k))))
      (is (equal "queued" (getf (work-status k id) :status))))))

(deftest artifact-revisions-and-disposition-survive-rebuild
  (with-temp-kernel (k)
    (let* ((thought (pai-v3.mind:request-thought k "responding" "Synthetic document"))
           (doc (obj "title" "Synthetic guide" "sections" (list (obj "text" "Useful notes"))))
           (saved (pai-v3.mind::save-document k thought doc))
           (id (obj-get saved "artifact_id")) (revision (obj-get saved "revision")))
      (pai-v3.mind::set-artifact-state k thought id revision "kept" "Draft for later")
      (is (eq :done (rebuild-projection k "artifacts" :page-size 2)))
      (let ((row (pai-v3.mind::artifact-row k id revision)))
        (is (equal "kept" (seventh row)))
        (is (= thought (fifth row)))
        (is (search "Useful notes" (second (pai-v3.mind::artifact-content k row))))))))

(deftest affect-personal-dynamics-rebuild-identically-across-setting-boundaries
  (with-temp-kernel (k)
    (let* ((start (now)) (*clock* (lambda () start)) (operator (obj "principal_type" "operator")))
      (append! k "affect.set" (obj "drives" (obj "connection" 0.3 "curiosity" 0.4) "cause" "Fixture"))
      (incf start 3600)
      (change-setting k "affect.drives"
                      (obj "connection" (obj "rise_per_hour" 0.2 "satisfaction_drop" 0.4)
                           "curiosity" (obj "resting_level" 0.1 "retention_per_five_minutes" 0.98)) :origin operator)
      (incf start 1800)
      (append! k "drive.satisfied" (obj "drive" "connection"))
      (append! k "drive.triggered" (obj "drive" "curiosity" "amount" 0.3 "target" "Synthetic question"))
      (change-setting k "affect.modulators" (obj "social_need" (obj "silence_seconds" 7200)) :origin operator)
      (incf start 600)
      (let ((before (json-encode (multiple-value-list (pai-v3.mind:affect-now k)))))
        (is (eq :done (rebuild-projection k "affect" :page-size 2)))
        (is (equal before (json-encode (multiple-value-list (pai-v3.mind:affect-now k)))))
        (is (equal "ready" (projection-status k "affect")))))))

(deftest semantic-appraisal-state-and-source-deduplication-rebuild
  (with-temp-kernel (k)
    (let* ((source (event-id (append! k "notice.posted" (obj "channel" "fixture" "audience" "operator" "text" "source" "importance" "normal"))))
           (payload (obj "source_key" "synthetic-appraisal" "source_id" source "status" "applied"
                         "reason" "Synthetic decision" "changes" (obj "valence" 0.05)
                         "triggers" (obj "curiosity" 0.2) "satisfactions" (obj "appreciation" 0.5))))
      (append! k "affect.appraisal.requested" (obj "source_key" "synthetic-appraisal" "source_id" source
                                                "audience" "operator" "speaker" "Operator" "text" "Synthetic"
                                                "truncated" :false "context" ""))
      (append! k "affect.appraisal.finished" payload)
      (append! k "affect.appraisal.finished" payload)
      (let ((affect (projection-digest k "affect")) (appraisals (projection-digest k "affect_appraisals")))
        (is (eq :done (rebuild-projection k "affect" :page-size 2)))
        (is (eq :done (rebuild-projection k "affect_appraisals" :page-size 2)))
        (is (equal affect (projection-digest k "affect")))
        (is (equal appraisals (projection-digest k "affect_appraisals")))))))

(deftest old-agent-startup-adds-empty-appraisal-tables-without-history-replay
  (let ((path (temp-path))
        (app (gethash "affect_appraisals" pai-v3.kernel::*projections*))
        (decisions (gethash "provider_decisions" pai-v3.kernel::*projections*))
        (head nil))
    (unwind-protect
         (progn
           (remhash "affect_appraisals" pai-v3.kernel::*projections*)
           (remhash "provider_decisions" pai-v3.kernel::*projections*)
           (with-temp-kernel (k :path path)
             (append! k "turn.received" (obj "channel" "fixture" "audience" "operator" "speaker" "Operator"
                                           "text" "Thank you" "origin" (obj "principal_type" "operator")))
             (setf head (ledger-head k))
             (sql-exec (kernel-db k) "DROP TABLE affect_appraisal_seen")))
      (setf (gethash "affect_appraisals" pai-v3.kernel::*projections*) app
            (gethash "provider_decisions" pai-v3.kernel::*projections*) decisions))
    (with-temp-kernel (k :path path)
      (is (= head (ledger-head k)))
      (is (equal "ready" (projection-status k "affect_appraisals")))
      (is (equal "ready" (projection-status k "provider_decisions")))
      (is (null (projections-not-ready k))))))

(deftest draft-envelopes-rebuild-and-old-store-startup-needs-no-replay
  (let ((path (temp-path))
        (projection (gethash "fleet_draft_envelopes" pai-v3.kernel::*projections*)) (head nil))
    (unwind-protect
         (progn
           (remhash "fleet_draft_envelopes" pai-v3.kernel::*projections*)
           (with-temp-kernel (k :path path)
             (pai-v3.mind:request-thought k "work" "Historical peer work" :audience "peer:synthetic")
             (setf head (ledger-head k))))
      (setf (gethash "fleet_draft_envelopes" pai-v3.kernel::*projections*) projection))
    (with-temp-kernel (k :path path)
      (is (= head (ledger-head k)))
      (is (equal "ready" (projection-status k "fleet_draft_envelopes")))
      (is (null (projections-not-ready k)))
      (append! k "fleet.draft.envelope" (obj "draft_id" "synthetic" "images" '("synthetic-image") "sharing_topics" '("daily-brief")))
      (let ((before (projection-digest k "fleet_draft_envelopes")))
        (is (eq :done (rebuild-projection k "fleet_draft_envelopes" :page-size 1)))
        (is (equal before (projection-digest k "fleet_draft_envelopes")))))))

(deftest board-disclosure-projection-rebuilds-from-its-events
 (with-temp-kernel (k)
  (append! k "fleet.board.disclosure" (obj "msg_id" "synthetic-message" "peer_id" "synthetic-recipient"))
  (let ((before (projection-digest k "fleet_board_disclosures")))
   (is (eq :done (rebuild-projection k "fleet_board_disclosures" :page-size 1)))
   (is (equal before (projection-digest k "fleet_board_disclosures"))))))

(deftest private-peer-reasoning-stays-private-after-rebuild
 (with-temp-kernel (k)
  (let ((id (pai-v3.mind:request-thought k "work" "Synthetic peer stimulus" :audience "peer:synthetic")))
   (pai-v3.mind::%record-text k id "model" "PRIVATE-REPLAY-CANARY")
   (let ((before (projection-digest k "experience")))
    (is (eq :done (rebuild-projection k "experience" :page-size 1)))
    (is (equal before (projection-digest k "experience")))
    (is (null (pai-v3.mind::search-experience k "PRIVATE-REPLAY-CANARY" "peer:synthetic")))
    (is (pai-v3.mind::search-experience k "PRIVATE-REPLAY-CANARY" "operator"))))))

(deftest old-store-adds-empty-board-disclosure-state-without-replay
 (let ((path (temp-path)) (head nil)
       (projection (gethash "fleet_board_disclosures" pai-v3.kernel::*projections*)))
  (unwind-protect
   (progn
    (remhash "fleet_board_disclosures" pai-v3.kernel::*projections*)
    (with-temp-kernel (k :path path)
     (pai-v3.mind:request-thought k "work" "Historical synthetic work")
     (setf head (ledger-head k))))
   (setf (gethash "fleet_board_disclosures" pai-v3.kernel::*projections*) projection))
  (with-temp-kernel (k :path path)
   (is (= head (ledger-head k)))
   (is (equal "ready" (projection-status k "fleet_board_disclosures")))
   (is (null (projections-not-ready k))))))

(run-suite "maintenance-rebuild")
