(in-package #:pai-v3.api)

;;; Plugins (operator request 2026-10-31): capabilities that live outside
;;; this repository and are loaded into the agent at start, so the agent can
;;; grow without its source growing. A plugin is an ASDF system in its own
;;; folder under the plugins directory (PAI_PLUGINS, by default ~/plugins/):
;;; <dir>/<name>/<name>.asd. It runs in the agent's process, so it may use
;;; every extension point the agent has (define-tool, define-slot-source,
;;; define-projection, *periodic-hooks*), and may answer web requests under
;;; /plugins/<name>/ with define-plugin-route, behind the operator's own
;;; sign-in. A plugin that fails to load is recorded and skipped; it never
;;; stops the agent. Removing its folder removes it. See docs/behaviour/plugins.md.

(defvar *plugin-routes* (make-hash-table :test 'equal)
  "Plugin name -> (lambda (agent method subpath origin)) returning a reply body.")
(defvar *plugins* '() "(name status detail) of each plugin found at start, in load order.")
(defvar *plugin-menu* '() "(name label) of each plugin page offered in the operator's menu, in order.")

(defmacro define-plugin-route (name (agent method subpath origin) &body body)
  "Answer requests under /plugins/NAME/ for the signed-in operator. SUBPATH is
the rest of the path (\"\" for the plugin's root). BODY sets the content type
(plugin-reply and plugin-static help) and returns the body."
  `(setf (gethash ,name *plugin-routes*)
         (lambda (,agent ,method ,subpath ,origin)
           (declare (ignorable ,agent ,method ,subpath ,origin))
           ,@body)))

(defun plugin-menu-entry (name label)
  "Offer plugin NAME's page, /plugins/NAME/, in the operator's menu as LABEL."
  (setf *plugin-menu* (append (remove name *plugin-menu* :key #'first :test #'equal) (list (list name label))))
  name)

(defun plugin-menu ()
  "The menu entries whose plugin answers a route, as objects for the web menu."
  (loop for (name label) in *plugin-menu*
        when (gethash name *plugin-routes*)
          collect (obj "name" name "label" label "href" (format nil "/plugins/~a/" name))))

(defparameter +plugin-types+
  '(("html" . "text/html; charset=utf-8") ("js" . "text/javascript; charset=utf-8") ("mjs" . "text/javascript; charset=utf-8")
    ("css" . "text/css; charset=utf-8") ("json" . "application/json") ("png" . "image/png") ("jpg" . "image/jpeg")
    ("webp" . "image/webp") ("svg" . "image/svg+xml") ("wav" . "audio/wav") ("mp3" . "audio/mpeg")
    ("vrm" . "application/octet-stream") ("glb" . "model/gltf-binary") ("vrma" . "application/octet-stream")))

(defun plugin-reply (content-type body &key (status 200))
  "BODY (string or octets) with CONTENT-TYPE and STATUS. Browsers check back
before reusing it, so a reinstalled plugin is seen on the next load."
  (setf (hunchentoot:return-code*) status (hunchentoot:content-type*) content-type
        (hunchentoot:header-out "Cache-Control") "no-cache")
  body)

(defun plugin-static (root subpath &key (index "index.html"))
  "The file SUBPATH under directory ROOT, or a 404. Paths that climb out of
ROOT, or name hidden files, are refused."
  (let ((rel (if (or (equal subpath "") (uiop:string-suffix-p subpath "/")) (concatenate 'string subpath index) subpath)))
    (if (or (search ".." rel) (search "\\" rel) (uiop:string-prefix-p "/" rel)
            (some (lambda (part) (and (plusp (length part)) (char= #\. (char part 0)))) (uiop:split-string rel :separator "/")))
        (plugin-reply "text/plain" "No such file." :status 404)
        (let ((file (merge-pathnames rel (uiop:ensure-directory-pathname root))))
          (if (probe-file file)
              (plugin-reply (or (cdr (assoc (string-downcase (or (pathname-type file) "")) +plugin-types+ :test #'equal))
                                "application/octet-stream")
                            (with-open-file (in file :element-type '(unsigned-byte 8))
                              (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
                                (read-sequence bytes in)
                                bytes)))
              (plugin-reply "text/plain" "No such file." :status 404))))))

(defun plugin-route (agent method path)
  "Answer /plugins/<name>/... for the plugin that registered NAME."
  (let* ((rest (subseq path (length "/plugins/")))
         (slash (position #\/ rest))
         (name (subseq rest 0 (or slash (length rest))))
         (subpath (if slash (subseq rest (1+ slash)) ""))
         (handler (gethash name *plugin-routes*))
         (origin (%signed-in agent)))
    (cond ((null handler) (reply 404 (obj "error" "no such plugin")))
          ((null slash)                  ; /plugins/avatar -> /plugins/avatar/, so relative links work
           (setf (hunchentoot:header-out "Location") (concatenate 'string path "/"))
           (plugin-reply "text/plain" "" :status 302))
          ((null origin) (reply 401 (obj "error" "sign in first")))
          ((and (eq method :post) (not (same-origin-p agent))) (reply 403 (obj "error" (%refusal agent))))
          (t (handler-case (funcall handler agent method subpath origin)
               (error (e) (reply 500 (obj "error" (format nil "plugin ~a: ~a" name
                                                          (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m)))))))))))))

(defun plugins-directory ()
  (let ((env (uiop:getenv "PAI_PLUGINS")))
    (uiop:ensure-directory-pathname
     (if (and env (plusp (length env))) env (merge-pathnames "plugins/" (user-homedir-pathname))))))

(defun load-plugins (&optional (dir (plugins-directory)))
  "Load every plugin under DIR: each folder holding <folder>.asd is one.
Returns *plugins*. Nothing found is not an error, and no failure stops the agent."
  (setf *plugins* '() *plugin-menu* '())
  (dolist (folder (and (probe-file dir) (uiop:subdirectories dir)) (setf *plugins* (nreverse *plugins*)))
    (let* ((name (car (last (pathname-directory folder))))
           (asd (merge-pathnames (make-pathname :name name :type "asd") folder)))
      (when (probe-file asd)
        (handler-case
            (progn
              (pushnew folder asdf:*central-registry* :test #'equal)
              (handler-bind ((warning #'muffle-warning))
                (if (find-package '#:ql)
                    (uiop:symbol-call '#:ql '#:quickload name :silent t)
                    (asdf:load-system name)))
              (push (list name "loaded" (namestring folder)) *plugins*)
              (format t "~&Plugin ~a loaded from ~a~%" name (namestring folder)))
          (error (e)
            (push (list name "failed" (let ((m (princ-to-string e))) (subseq m 0 (min 300 (length m))))) *plugins*)
            (format t "~&Plugin ~a FAILED to load: ~a~%" name e)))))))
