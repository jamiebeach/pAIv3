(defpackage #:pai-v3.kernel
  (:use #:cl)
  (:export
   ;; sqlite
   #:http-request #:check-outbound-url #:egress-refused #:egress-reason #:egress-transient-p #:*egress-guard*
   #:db-open #:db-close #:db-rekey #:db-backup #:store-locked #:sql-exec #:sql-rows #:sql-row #:sql-last-insert-rowid #:sql-value
   #:with-transaction #:with-savepoint #:sqlite-error #:sqlite-error-code
   #:*connection-hooks* #:db-handle
   ;; encoding
   #:base64url-encode #:base64url-decode #:cbor-decode
   ;; json
   #:json-parse #:json-encode #:obj #:obj-get
   ;; event types
   #:define-event-type #:find-event-type #:event-type-error
   #:*payload-limit-bytes*
   ;; events
   #:event #:event-id #:event-seq #:event-type #:event-version #:event-at
   #:event-caused-by #:event-root-id #:event-agent-id #:event-payload
   ;; kernel
   #:kernel #:open-kernel #:close-kernel #:kernel-db #:kernel-agent-id
   #:append! #:append-batch! #:event-by-id #:ledger-head #:latest-event-ids
   ;; projections
   #:define-projection #:find-projection #:projection-status
   #:projections-not-ready #:not-ready-warning
   #:projection-cursor #:step-async-projection #:quarantined-events
   #:degraded-entities #:owned-tables #:projection-digest #:projection-counts
   #:projections-overview #:projection-rows #:not-inspectable
   ;; time
   #:*clock* #:now #:now-iso8601 #:iso8601 #:parse-iso8601
   ;; handlers, scheduler, budgets
   #:define-handler #:request-work #:run-scheduler-step #:handler-failure
   #:failure-class #:failure-reason #:failure-retry-after
   #:work-status #:work-pending-p #:dead-letters #:record-spend #:spent-today #:*budget*
   ;; settings
   #:define-setting #:setting #:change-setting #:reset-setting #:settings-list #:setting-refused
   #:setting-refused-reason #:export-settings #:import-settings))
