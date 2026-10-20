(in-package #:pai-v3.api)

;;; Operator authentication is enforced by the caller. Preview has an opaque
;;; sandbox origin and no active content, network, forms or nested frames.
(defun artifacts-route (agent method path)
  (let* ((k (agent-kernel agent))
         (id (hunchentoot:get-parameter "id"))
         (revision (ignore-errors (parse-integer (hunchentoot:get-parameter "revision")))))
    (handler-case
        (cond ((not (eq method :get)) (reply 405 (obj "error" "read only")))
              ((equal path "/api/artifacts")
               (reply 200 (obj "items" (pai-v3.mind::artifact-list k (hunchentoot:get-parameter "work_id")))))
              ((member path '("/api/artifacts/preview" "/api/artifacts/download" "/api/artifacts/source") :test #'equal)
               (let ((row (and revision (pai-v3.mind::artifact-row k id revision))))
                 (unless row (return-from artifacts-route (reply 404 (obj "error" "no such artifact revision"))))
                 (let ((content (pai-v3.mind::artifact-content k row)))
                   (setf (hunchentoot:header-out "Cache-Control") "private, no-store"
                         (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
                   (if (equal path "/api/artifacts/source")
                       (reply 200 (obj "artifact" (pai-v3.mind::artifact-json row)
                                       "document" (json-parse (first content))))
                       (progn
                         (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
                               (hunchentoot:header-out "Content-Security-Policy")
                               "sandbox; default-src 'none'; img-src data:; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'; frame-ancestors 'self'")
                         (when (equal path "/api/artifacts/download")
                           (setf (hunchentoot:header-out "Content-Disposition")
                                 (format nil "attachment; filename=\"document-~d.html\"" revision)))
                         (babel:string-to-octets (second content) :encoding :utf-8))))))
              (t (reply 404 (obj "error" "no such artifact route"))))
      (error () (reply 503 (obj "error" "Artifact content is unavailable; its blob store may need restoring."))))))
