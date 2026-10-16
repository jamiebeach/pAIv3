(in-package #:pai-v3.api)

;;; The terminal's Files view: the operator browses, reads, edits and uploads
;;; files in the agent's workspace (shell.workspace), in v2's routes and
;;; shapes. Signed in only. A path is relative to the workspace and must stay
;;; inside it, symbolic links included. Uploads never replace a file.

(define-setting "files.operator_writes"
  :type :boolean :default t :group "Self"
  :description "Whether the operator may save and upload files in the workspace from the Files view.")

(defparameter +file-preview-bytes+ 1000000 "Largest file the Files view shows as text.")
(defparameter +file-upload-bytes+ 20000000 "Largest upload.")

(define-condition file-refused (error)
  ((reason :initarg :reason :reader file-refused-reason))
  (:report (lambda (c s) (format s "~a" (file-refused-reason c)))))

(defun %file-refuse (fmt &rest args) (error 'file-refused :reason (apply #'format nil fmt args)))

(defun %workspace (kernel)
  (let ((dir (setting kernel "shell.workspace")))
    (unless (and (stringp dir) (plusp (length dir))) (%file-refuse "there is no workspace: shell.workspace is not set"))
    (ensure-directories-exist (uiop:ensure-directory-pathname dir))
    (truename (uiop:ensure-directory-pathname dir))))

(defun %clean-relative (path)
  "PATH's segments, refusing anything that could leave the workspace."
  (let ((path (or path "")))
    (when (or (find #\\ path) (find (code-char 0) path) (uiop:string-prefix-p "/" path))
      (%file-refuse "give a path inside the workspace"))
    (let ((parts (remove "" (uiop:split-string path :separator "/") :test #'string=)))
      (when (some (lambda (p) (member p '("." "..") :test #'string=)) parts)
        (%file-refuse "give a path inside the workspace"))
      parts)))

(defun %inside-p (root path)
  (uiop:string-prefix-p (namestring root) (namestring path)))

(defun %resolve (kernel path &key directory must-exist)
  "The pathname for workspace-relative PATH; its real location must be inside the workspace."
  (let* ((root (%workspace kernel))
         (parts (%clean-relative path))
         (target (if directory
                     (merge-pathnames (make-pathname :directory (cons :relative parts)) root)
                     (progn (unless parts (%file-refuse "name a file"))
                            (merge-pathnames (make-pathname :directory (cons :relative (butlast parts)))
                                             (merge-pathnames (car (last parts)) root)))))
         (real (probe-file target)))
    (when (and must-exist (null real)) (%file-refuse "no such ~:[file~;folder~]" directory))
    (when (and real (not (%inside-p root real))) (%file-refuse "that path leads outside the workspace"))
    (values (or real target) root)))

(defun %relative-name (root path)
  (subseq (namestring path) (length (namestring root))))

(defun files-list (kernel path)
  (multiple-value-bind (dir root) (%resolve kernel path :directory t :must-exist t)
    (obj "root" (namestring root) "path" (%relative-name root dir)
         "can_write" (if (setting kernel "files.operator_writes") t :false)
         "entries" (append (mapcar (lambda (d) (obj "name" (car (last (pathname-directory d))) "is_dir" t))
                                   (uiop:subdirectories dir))
                           (mapcar (lambda (f) (obj "name" (file-namestring f) "is_dir" :false
                                                    "size" (or (ignore-errors (with-open-file (s f :element-type '(unsigned-byte 8)) (file-length s))) 0)))
                                   (uiop:directory-files dir))))))

(defun %read-octets (file &optional limit)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((n (if limit (min limit (file-length s)) (file-length s)))
           (buf (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (values buf (file-length s)))))

(defun file-read (kernel path)
  (let ((file (%resolve kernel path :must-exist t)))
    (when (uiop:directory-pathname-p file) (%file-refuse "that is a folder"))
    (multiple-value-bind (octets size) (%read-octets file +file-preview-bytes+)
      (let ((text (and (not (find 0 octets))
                       (ignore-errors (babel:octets-to-string octets :encoding :utf-8 :errorp t)))))
        (obj "path" path "size" size "truncated" (if (> size +file-preview-bytes+) t :false)
             "editable" (if (and text (<= size +file-preview-bytes+)) t :false)
             "content" (or text "")
             "can_write" (if (setting kernel "files.operator_writes") t :false))))))

(defun %check-writable (kernel)
  (unless (setting kernel "files.operator_writes") (%file-refuse "saving and uploading are off (files.operator_writes)")))

(defun file-save (kernel path content)
  (%check-writable kernel)
  (unless (stringp content) (%file-refuse "give the content"))
  (let ((file (%resolve kernel path :must-exist t)))
    (when (uiop:directory-pathname-p file) (%file-refuse "that is a folder"))
    (with-open-file (s file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string content s))
    (obj "saved" path)))

(defun %safe-file-name (name)
  (let ((name (file-namestring (substitute #\/ #\\ (or name "")))))
    (when (or (zerop (length name)) (member name '("." "..") :test #'string=) (find (code-char 0) name)
              (> (length name) 200))
      (%file-refuse "that file name cannot be used"))
    name))

(defun file-upload (kernel dir source name)
  "Copy the uploaded temporary file SOURCE into workspace folder DIR as NAME, never over an existing file."
  (%check-writable kernel)
  (let* ((folder (%resolve kernel dir :directory t :must-exist t))
         (name (%safe-file-name name)))
    (let ((target (uiop:parse-native-namestring (concatenate 'string (namestring folder) name))))
      (when (probe-file target) (%file-refuse "~a already exists; rename it before uploading" name))
      (with-open-file (in source :element-type '(unsigned-byte 8))
        (when (> (file-length in) +file-upload-bytes+) (%file-refuse "the upload limit is 20 MB"))
        (with-open-file (out target :direction :output :if-exists nil :element-type '(unsigned-byte 8))
          (unless out (%file-refuse "~a already exists" name))
          (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8))))
      (obj "uploaded" (%relative-name (%workspace kernel) target)))))

(defun files-route (agent method path)
  "Answer /api/v2/files, /file, /download and /upload for a signed-in operator."
  (let ((k (agent-kernel agent))
        (param (lambda (n) (hunchentoot:get-parameter n))))
    (handler-case
        (cond ((and (eq method :get) (string= path "/api/v2/files"))
               (reply 200 (files-list k (funcall param "path"))))
              ((and (eq method :get) (string= path "/api/v2/file"))
               (reply 200 (file-read k (funcall param "path"))))
              ((and (eq method :get) (string= path "/api/v2/download"))
               (let ((file (%resolve k (funcall param "path") :must-exist t)))
                 (when (uiop:directory-pathname-p file) (%file-refuse "that is a folder"))
                 (setf (hunchentoot:content-type*) "application/octet-stream"
                       (hunchentoot:header-out "Content-Disposition")
                       (format nil "attachment; filename=\"~a\"" (remove #\" (file-namestring file))))
                 (%read-octets file)))
              ((and (eq method :post) (string= path "/api/v2/file"))
               (let ((body (body-json)))
                 (reply 200 (file-save k (obj-get body "path") (obj-get body "content")))))
              ((and (eq method :post) (string= path "/api/v2/upload"))
               (let ((part (hunchentoot:post-parameter "file")))
                 (unless (and (consp part) (first part)) (%file-refuse "choose a file to upload"))
                 (reply 200 (file-upload k (funcall param "path") (first part) (second part)))))
              (t (reply 404 (obj "error" "no such route"))))
      (file-refused (e) (reply 400 (obj "error" (princ-to-string e)))))))
