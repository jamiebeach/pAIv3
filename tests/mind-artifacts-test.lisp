(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defun sample-document (&optional (title "Field notes"))
  (obj "title" title "subtitle" "A small useful thing" "theme" "forest" "layout" "report"
       "sections" (list (obj "heading" "The next step" "text" "Observe. Make. Share."))))

(deftest drafts-revise-without-losing-the-delivered-version
  (with-temp-kernel (k)
    (let* ((thought (request-thought k "responding" "Synthetic document request"))
           (a (pai-v3.mind::save-document k thought (sample-document)))
           (id (obj-get a "artifact_id")) (v1 (obj-get a "revision")))
      (is (equal "draft" (obj-get a "state")))
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice")))
      (pai-v3.mind::set-artifact-state k thought id v1 "kept" "For later")
      (pai-v3.mind::set-artifact-state k thought id v1 "delivered" "Here are the field notes.")
      (pai-v3.mind::set-artifact-state k thought id v1 "delivered" "Retry after interruption")
      (is (= 1 (sql-value (kernel-db k) "SELECT count(*) FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice")))
      (is (search (format nil "revision=~d" v1)
                  (sql-value (kernel-db k) "SELECT rendered FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice")))
      (let* ((b (pai-v3.mind::save-document k thought (sample-document "Revised notes") id))
             (old (pai-v3.mind::artifact-row k id v1)))
        (is (> (obj-get b "revision") v1))
        (is (equal "draft" (obj-get b "state")))
        (is (equal "delivered" (seventh old)))
        (is (search "Field notes" (second (pai-v3.mind::artifact-content k old))))))))

(deftest documents-embed-owned-images-and-escape-every-text-field
  (with-temp-kernel (k)
    (let* ((image (pai-v3.mind::store-image k "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jJz0AAAAASUVORK5CYII="))
           (doc (obj "title" "<script>alert(1)</script>" "theme" "rose" "layout" "card"
                     "sections" (list (obj "text" "<iframe src=https://example.test>"
                                           "image_id" image "caption" "\" onload=bad <tag>"))))
           (html (pai-v3.mind::render-document k doc)))
      (is (search "data:image/png;base64," html))
      (is (not (search "<script>" html)))
      (is (not (search "<iframe " html)))
      (is (search "&lt;script&gt;" html))
      (is (search "&quot;" html))
      (is (search "default-src 'none'" html))
      (setf (gethash "theme" doc) "url(https://example.test)")
      (signals error (pai-v3.mind::render-document k doc)))))

(deftest rejected-documents-do-not-create-ledger-records
  (with-temp-kernel (k)
    (let* ((thought (request-thought k "responding" "Synthetic request"))
           (doc (sample-document)) (before (ledger-head k)))
      (setf (gethash "image_id" (first (obj-get doc "sections"))) "missing")
      (signals error (pai-v3.mind::save-document k thought doc))
      (is (= before (ledger-head k)))
      (is (null (pai-v3.mind::artifact-list k))))))

(deftest peer-stimulated-private-thinking-can-create-private-artifacts
  (with-temp-kernel (k)
    (let ((peer (request-thought k "work" "Synthetic peer request" :audience "peer:beta"))
          (tool (gethash "document" pai-v3.mind::*tools*)))
      (is (pai-v3.mind::tool-available-p tool (thought-row k peer)))
      (is (not (pai-v3.mind::tool-available-p tool (list :audience "peer:beta" :communicating t))))
      (is (stringp (funcall (pai-v3.mind::tool-run tool) k peer (obj "action" "list"))))
      (is (obj-get (pai-v3.mind::save-document k peer (sample-document)) "artifact_id")))))

(deftest docket-runs-link-documents-to-their-work-and-can-resume-source
  (with-temp-kernel (k)
    (let* ((work (open-work k :title "A field guide" :purpose "Produce guide" :operator-benefit "A useful reference" :next-step "Draft it"))
           (thought (pai-v3.mind::%start-work k))
           (doc (pai-v3.mind::save-document k thought (sample-document))))
      (is (equal work (obj-get doc "work_id")))
      (is (= 1 (length (pai-v3.mind::artifact-list k work))))
      (let* ((row (pai-v3.mind::artifact-row k (obj-get doc "artifact_id")))
             (source (json-parse (first (pai-v3.mind::artifact-content k row)))))
        (is (equal "Observe. Make. Share." (obj-get (first (obj-get source "sections")) "text")))))))

(deftest restart-preserves-provenance-and-content-in-separate-blob-store
  (let ((path (temp-path)) (blob-path (temp-path)) (id nil) (revision nil))
    (let ((pai-v3.mind::*image-store* (db-open blob-path :key *test-key*)))
      (unwind-protect
           (progn
             (with-temp-kernel (k :path path)
               (let* ((thought (request-thought k "responding" "Make a document"))
                      (saved (pai-v3.mind::save-document k thought (sample-document))))
                 (setf id (obj-get saved "artifact_id") revision (obj-get saved "revision"))
                 (is (null (sql-value (kernel-db k) "SELECT name FROM sqlite_master WHERE name=?1 LIMIT 1" "artifact_blobs")))
                 (is (not (search "Observe. Make. Share." (json-encode (event-payload (event-by-id k revision))))))))
             (with-temp-kernel (k :path path)
               (let ((row (pai-v3.mind::artifact-row k id revision)))
                 (is row)
                 (is (search "Observe. Make. Share." (second (pai-v3.mind::artifact-content k row)))))))
        (db-close pai-v3.mind::*image-store*)))))

(deftest missing-or-corrupt-content-cannot-be-delivered
  (with-temp-kernel (k)
    (let* ((thought (request-thought k "responding" "Synthetic document"))
           (saved (pai-v3.mind::save-document k thought (sample-document)))
           (id (obj-get saved "artifact_id")) (revision (obj-get saved "revision"))
           (row (pai-v3.mind::artifact-row k id revision)) (before (ledger-head k)))
      (sql-exec (kernel-db k) "UPDATE artifact_blobs SET html=?2 WHERE digest=?1" (fourth row) "corrupt")
      (signals error (pai-v3.mind::set-artifact-state k thought id revision "delivered" "Ready"))
      (is (= before (ledger-head k)))
      (sql-exec (kernel-db k) "DELETE FROM artifact_blobs WHERE digest=?1" (fourth row))
      (signals error (pai-v3.mind::set-artifact-state k thought id revision "delivered" "Ready"))
      (is (= before (ledger-head k)))
      (is (equal "draft" (seventh (pai-v3.mind::artifact-row k id revision)))))))

;;; A draft left behind (bead v3-3td).

(defun end-thought (k id)
  (pai-v3.mind::conclude k (thought-row k id) "thought.ended" (obj "outcome" "concluded" "summary" "done") "done"))

(defun follow-ups (k)
  (sql-rows (kernel-db k) "SELECT id,origin,reason FROM thoughts WHERE reason LIKE ?1 ORDER BY id LIMIT 10"
            "A document I saved was left unsettled.%"))

(deftest a-thought-that-ends-with-an-undelivered-draft-is-followed-up-once
  (with-temp-kernel (k)
    (let* ((thought (request-thought k "noticing" "Synthetic morning brief"))
           (a (pai-v3.mind::save-document k thought (sample-document "Morning brief"))))
      (end-thought k thought)
      (let ((f (follow-ups k)))
        (is (= 1 (length f)))
        (is (equal "noticing" (second (first f))) "in the same kind of thought")
        (is (search (obj-get a "artifact_id") (third (first f))))
        (is (search "Morning brief" (third (first f))))
        (end-thought k (first (first f)))
        (is (= 1 (length (follow-ups k))) "a follow-up never asks for another")))))

(deftest a-settled-or-superseded-draft-needs-no-follow-up
  (with-temp-kernel (k)
    (let* ((thought (request-thought k "noticing" "Synthetic brief"))
           (a (pai-v3.mind::save-document k thought (sample-document)))
           (id (obj-get a "artifact_id")))
      (pai-v3.mind::set-artifact-state k thought id (obj-get a "revision") "kept" "For later")
      (end-thought k thought)
      (is (null (follow-ups k)) "kept")
      (let* ((second-thought (request-thought k "noticing" "Synthetic revision"))
             (b (pai-v3.mind::save-document k second-thought (sample-document "v2") id)))
        (pai-v3.mind::save-document k second-thought (sample-document "v3") id)
        (pai-v3.mind::set-artifact-state k second-thought id (1+ (obj-get b "revision")) "delivered" "Here it is.")
        (end-thought k second-thought)
        (is (null (follow-ups k)) "an older revision of a delivered document is not left behind")))))

(run-suite "mind-artifacts")
