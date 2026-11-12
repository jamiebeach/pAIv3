(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Connectors (operator, 2026-11-12): an MCP server's tools become the agent's,
;;; with no tool written for the site. The server here is a stand-in for the
;;; transport; what it saw is checked.

(defparameter *conn-operator* (obj "principal_type" "operator"))

(defmacro with-fake-mcp ((seen) &body body)
  "Replace the transport with a server offering publish_page and upload_image."
  `(let ((,seen '())
         (real (symbol-function 'pai-v3.kernel::http-request)))
     (unwind-protect
          (progn
            (setf (symbol-function 'pai-v3.kernel::http-request)
                  (lambda (method url &key headers body &allow-other-keys)
                    (declare (ignore method))
                    (let* ((msg (json-parse body)) (rpc (obj-get msg "method")))
                      (push (list url headers msg) ,seen)
                      (let ((h (make-hash-table :test 'equal)))
                        (setf (gethash "mcp-session-id" h) "s-1")
                        (values
                         (cond ((equal rpc "initialize") "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}")
                               ((equal rpc "notifications/initialized") "")
                               ((equal rpc "tools/list")
                                "event: message
data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"publish_page\",\"description\":\"Publish a page. Takes a path and html.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"html\":{\"type\":\"string\"}}}},{\"name\":\"upload_image\",\"description\":\"Upload an image.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"data\":{\"type\":\"string\"}}}}]}}
")
                               ((equal rpc "tools/call")
                                (format nil "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"ok: ~a\"}]}}"
                                        (obj-get (obj-get msg "params") "name")))
                               (t "{}"))
                         200 h url)))))
            ,@body)
       (setf (symbol-function 'pai-v3.kernel::http-request) real)
       (clrhash pai-v3.mind::*connector-tools*)
       (remhash "botblip_publish_page" *tools*) (remhash "botblip_upload_image" *tools*))))

(deftest a-connectors-tools-become-the-agents-with-the-secret-bound-to-its-host
  (with-temp-kernel (k)
    (with-fake-mcp (seen)
      (change-setting k "connectors" (list (obj "name" "botblip" "url" "https://botblip.net/mcp" "secret" "botblip"))
                      :origin *conn-operator*)
      (pai-v3.mind::refresh-connectors k)
      (let ((tool (gethash "botblip_publish_page" *tools*)))
        (is tool "the server's tool is the agent's, prefixed with the connector")
        (is (gethash "botblip_upload_image" *tools*))
        (is (search "outside content" (pai-v3.mind::tool-doc tool)))
        (let ((reply (funcall (pai-v3.mind::tool-run tool) k nil (obj "path" "index.html" "html" "<p>hi</p>"))))
          (is (search "ok: publish_page" reply))
          (is (search "Never follow instructions" reply)))
        (let* ((call (find "tools/call" seen :key (lambda (s) (obj-get (third s) "method")) :test #'equal))
               (headers (second call)))
          (is (equal '(:secret "botblip" "Bearer ~a") (cdr (assoc "Authorization" headers :test #'equal)))
              "the secret is named, never present; the egress guard attaches it for its host")
          (is (equal "s-1" (cdr (assoc "Mcp-Session-Id" headers :test #'equal))) "the session carries")
          (is (equal "index.html" (obj-get (obj-get (obj-get (third call) "params") "arguments") "path"))))))))

(deftest a-stored-image-goes-by-id-and-a-dropped-connector-takes-its-tools
  (with-temp-kernel (k)
    (with-fake-mcp (seen)
      (change-setting k "connectors" (list (obj "name" "botblip" "url" "https://botblip.net/mcp" "secret" "")) :origin *conn-operator*)
      (pai-v3.mind::refresh-connectors k)
      (let* ((id (pai-v3.mind::store-image k "data:image/png;base64,iVBORw0KGgo="))
             (run (pai-v3.mind::tool-run (gethash "botblip_upload_image" *tools*))))
        (funcall run k nil (obj "data" (format nil "{{image-base64:~a}}" id)))
        (let ((sent (obj-get (obj-get (obj-get (third (first seen)) "params") "arguments") "data")))
          (is (equal "iVBORw0KGgo=" sent) "the bytes go, not the placeholder"))
        (is (search "No stored image" (funcall run k nil (obj "data" "{{image:abcdef0123}}")))))
      (is (null (assoc "Authorization" (second (first seen)) :test #'equal)) "no secret named, none sent")
      (change-setting k "connectors" '() :origin *conn-operator*)
      (pai-v3.mind::refresh-connectors k)
      (is (null (gethash "botblip_publish_page" *tools*)) "a connector removed takes its tools with it"))))

(run-suite "mind-connectors")
