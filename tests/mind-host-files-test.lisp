(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(deftest file-access-is-off-until-configured
  (with-temp-kernel (k)
    (is (obj-get (host-folder-request k (obj "action" "folders")) "error"))))

(deftest file-tool-cannot-use-the-administration-token
  (with-temp-kernel (k)
    (change-setting k "host_files.url" "http://host.docker.internal:8770"
                    :origin (obj "principal_type" "operator"))
    (let ((calls nil))
      (let ((*host-files-post* (lambda (url headers body)
                                (push (list url headers (json-parse body)) calls)
                                (values (json-encode (obj "text" "synthetic code")) 200))))
        (let ((tool (gethash "host_files" *tools*)))
          (is (member tool (tools-for (list :audience "operator"))))
          (is (not (member tool (tools-for (list :audience "peers" :communicating t)))))
          (is (search "synthetic code"
                      (funcall (pai-v3.mind::tool-run tool) k 1
                               (obj "action" "read" "folder" "demo" "path" ".git/HEAD" "admin" t))))
          (is (equal "http://host.docker.internal:8770/run" (first (first calls))))
          (is (equal '(:secret "host-files" "Bearer ~a")
                     (cdr (assoc "Authorization" (second (first calls)) :test #'equal))))
          (is (null (obj-get (third (first calls)) "admin")))
          (funcall (pai-v3.mind::tool-run tool) k 1 (obj "action" "write"))
          (is (= 1 (length calls))))
        (host-folder-request k (json-parse "{\"folders\":[]}") :admin t)
        (is (equal "http://host.docker.internal:8770/folders" (first (first calls))))
        (is (equal '(:secret "host-files-admin" "Bearer ~a")
                   (cdr (assoc "Authorization" (second (first calls)) :test #'equal))))))))

(run-suite "mind-host-files")
