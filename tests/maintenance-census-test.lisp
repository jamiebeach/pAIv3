(handler-bind ((warning #'muffle-warning))
  (ql:quickload "pai-v3-maintenance" :silent t))
(in-package #:pai-v3.test)
(use-package '#:pai-v3.maintenance)

;;; A synthetic store shaped like v2's: schema_version 1 envelopes, several
;;; types, an id gap, a duplicate id, an empty timestamp, an oversized row,
;;; a second agent, and a few v3 envelopes.

(defun v2-row (db id agent type at payload)
  (let ((json (json-encode (obj "schema_version" 1 "id" id "agent_id" agent "timestamp" at
                                "type" type "payload" payload "caused_by" (and (> id 1) (1- id))))))
    (sql-exec db "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'native',?2,'verified',?3,?4,?5,?6)"
              id agent type at json (pai-v3.kernel::sha256-hex json))))

(deftest census-reports-types-shapes-and-edge-cases
  (let ((path (format nil "/tmp/pai-v3-census-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path :agent-id "beta" :key nil)   ; v2 stores are plaintext
      (let ((db (kernel-db k)))
        (loop for id from 1 to 30
              do (v2-row db id "beta" "user-message" "2026-09-01T00:00:00Z" (obj "text" "hi" "metadata" (obj))))
        (v2-row db 31 "beta" "user-message" "2026-09-02T00:00:00Z" (obj "text" "no metadata"))
        (v2-row db 35 "beta" "memory-operation-state" "" (obj "op" "x"))          ; gap 32-34, empty time
        (v2-row db 35 "beta" "memory-operation-state" "2026-09-03T00:00:00Z" (obj "op" "y")) ; duplicate id
        (v2-row db 36 "beta" "projection-state" "2026-09-03T00:00:00Z"
                (obj "blob" (make-string 300000 :initial-element #\z)))            ; oversized
        (v2-row db 37 "alpha" "user-message" "2026-09-03T00:00:00Z" (obj "text" "other agent")))
      (dotimes (i 3) (append! k "test.said" (obj "text" "v3 row"))))
    (let* ((report (census path :agent-id "beta"))
           (types (obj-get report "types")))
      (flet ((row (name) (find name types :key (lambda (o) (obj-get o "type")) :test #'equal)))
        (is (= 37 (obj-get report "events")) "only beta's partition")
        (is (= 31 (obj-get (row "user-message") "count")))
        (is (= 2 (length (obj-get (row "user-message") "key_variants"))) "two payload shapes")
        (is (= 1 (obj-get (row "memory-operation-state") "empty_occurred_at")))
        (is (= 1 (obj-get (row "projection-state") "oversized")))
        (is (> (obj-get (row "projection-state") "bytes_max") 300000))
        (is (= 3 (obj-get (row "test.said") "v3_envelopes")))
        (is (eq t (obj-get (row "test.said") "declared")))
        (is (eq :false (obj-get (row "user-message") "declared")) "census shows undeclared types")
        (is (= 1 (obj-get report "event_id_duplicates")))
        (is (= 3 (obj-get report "event_ids_missing")) "ids are global: 32-34")
        (is (null (search "no metadata" (json-encode report))) "no content in the report")
        (is (search "| `user-message` | 31 | no |" (census-markdown report)))))
    (ignore-errors (delete-file path))))

(deftest census-opens-read-only
  (let ((path (format nil "/tmp/pai-v3-census-ro-~d.sqlite3" (random 1000000))))
    (with-temp-kernel (k :path path :key nil) (append! k "test.said" (obj "text" "x")))
    (let ((db (db-open path :read-only t)))
      (unwind-protect
           (signals sqlite-error (sql-exec db "DELETE FROM pai_events"))
        (db-close db)))
    (ignore-errors (delete-file path))))

(define-event-type "test.said" :schema (("text" :string)))
(run-suite "maintenance-census")
