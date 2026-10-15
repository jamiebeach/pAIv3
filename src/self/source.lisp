(in-package #:pai-v3.self)

;;; Reading my own source (bead v3-jm9). A development agent runs from a copy
;;; of its source; this tool lists, reads and searches that copy, read-only
;;; and only inside it. Changes go back as proposed patches on a bead (the
;;; beads tool, action patch), never as edits here. Off unless dev.source_dir
;;; is set.

(define-setting "dev.source_dir"
  :type :string :default "" :group "Development"
  :description "Where this agent's source is, for the source tool, such as /home/pai/v3/. Empty: no source tool.")

(defparameter *source-read-lines* 400 "Most lines one read returns.")
(defparameter *source-search-hits* 60 "Most matches one search returns.")
(defparameter *source-types* '("lisp" "asd" "md" "html" "js" "sh" "py" "json" "txt"))

(defun %source-root (kernel)
  (let ((dir (setting kernel "dev.source_dir")))
    (and (plusp (length dir))
         (uiop:ensure-directory-pathname (uiop:parse-native-namestring dir)))))

(defun %source-path (root relative)
  "The file RELATIVE names inside ROOT, or NIL if it would leave ROOT."
  (let ((parts (uiop:split-string (string-trim "/" (or relative "")) :separator "/")))
    (unless (or (some (lambda (p) (member p '(".." ".") :test #'string=)) parts)
                (and relative (plusp (length relative)) (char= #\/ (char relative 0)))
                (find #\\ (or relative "")))
      (let ((path (merge-pathnames (format nil "~{~a~^/~}" parts) root)))
        (and (uiop:subpathp path root) path)))))

(defun %source-files (root)
  "Every source file under ROOT, relative, in name order."
  (let ((out '()))
    (uiop:collect-sub*directories
     root (constantly t) (constantly t)
     (lambda (dir)
       (dolist (f (uiop:directory-files dir))
         (when (member (pathname-type f) *source-types* :test #'equal)
           (push (enough-namestring f root) out)))))
    (sort out #'string<)))

(defun %source-list (root prefix)
  (let ((files (remove-if-not (lambda (f) (or (null prefix) (uiop:string-prefix-p prefix f)))
                              (%source-files root))))
    (if files (format nil "~{~a~^~%~}" files) "No files there.")))

(defun %source-read (root path from count)
  (let ((file (%source-path root path)))
    (cond ((null file) "That path is outside my source.")
          ((not (probe-file file)) (format nil "There is no ~a." path))
          (t (let* ((lines (uiop:read-file-lines file :external-format :utf-8))
                    (from (max 1 (or from 1)))
                    (count (max 1 (min *source-read-lines* (or count *source-read-lines*))))
                    (shown (subseq lines (min (1- from) (length lines))
                                   (min (length lines) (+ (1- from) count)))))
               (format nil "~a, lines ~d-~d of ~d:~%~{~a~%~}" path from (+ from (length shown) -1) (length lines)
                       (loop for line in shown for n from from collect (format nil "~5d  ~a" n line))))))))

(defparameter *source-min-search* 4 "Shortest search text; shorter matches too much.")

(defun %source-search (root text prefix)
  "Matches for TEXT. When there are many, the count per file comes first, so
the next search can be narrowed rather than paged through."
  (if (< (length text) *source-min-search*)
      (format nil "Search for at least ~d characters; ~s would match almost everything." *source-min-search* text)
      (let ((hits '()) (per-file '()) (count 0))
        (dolist (rel (%source-files root))
          (when (or (null prefix) (uiop:string-prefix-p prefix rel))
            (let ((here 0))
              (loop for line in (ignore-errors (uiop:read-file-lines (merge-pathnames rel root) :external-format :utf-8))
                    for n from 1
                    when (search text line :test #'char-equal)
                      do (incf count) (incf here)
                         (push (format nil "~a:~d: ~a" rel n (string-trim " " line)) hits))
              (when (plusp here) (push (cons rel here) per-file)))))
        (setf hits (nreverse hits))
        (cond ((zerop count) (format nil "Nothing in my source matches ~s." text))
              ((<= count *source-search-hits*) (format nil "~{~a~^~%~}" hits))
              (t (format nil "~d matches in ~d files; narrow with path or a longer text.~%~{~a~^~%~}~%~%First matches:~%~{~a~^~%~}"
                         count (length per-file)
                         (mapcar (lambda (f) (format nil "  ~a: ~d" (car f) (cdr f)))
                                 (subseq (sort per-file #'> :key #'cdr) 0 (min 15 (length per-file))))
                         (subseq hits 0 (min 20 (length hits)))))))))

(defun %source-outline (root path)
  "The definitions in PATH with their line numbers: a map of the file."
  (let ((file (%source-path root path)))
    (cond ((null file) "That path is outside my source.")
          ((not (probe-file file)) (format nil "There is no ~a." path))
          (t (let ((defs (loop for line in (uiop:read-file-lines file :external-format :utf-8)
                               for n from 1
                               when (or (uiop:string-prefix-p "(def" line) (uiop:string-prefix-p "(define-" line)
                                        (uiop:string-prefix-p ";;; " line))
                                 collect (format nil "~5d  ~a" n (if (> (length line) 110) (subseq line 0 110) line)))))
               (if defs (format nil "~a:~%~{~a~^~%~}" path defs) (format nil "~a has no top-level definitions." path)))))))

(define-tool "source"
  :description "Read my own source code, read-only. action: list (files; path narrows to a folder such as src/mind/),
outline (path: its definitions and section comments with line numbers; start here to find your way in a file),
read (path, optionally from line and lines, at most 400), search (text of 4 or more characters, case-insensitive;
path narrows). Outline before reading, and search for whole names rather than fragments.
To change my code, propose a patch on a bead with the beads tool (action patch); I never edit the source here."
  :summary "Read and search my own source code."
  :available '("operator" "coding-agent")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "list" "outline" "read" "search"))
                                     "path" (obj "type" "string")
                                     "text" (obj "type" "string")
                                     "from" (obj "type" "integer")
                                     "lines" (obj "type" "integer"))
                   "required" (list "action"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((root (%source-root kernel))
               (path (let ((p (obj-get args "path"))) (and (stringp p) (plusp (length p)) p))))
           (cond ((null root) "My source is not available here: dev.source_dir is not set.")
                 ((not (uiop:directory-exists-p root)) "My source folder is missing.")
                 (t (let ((action (obj-get args "action")))
                      (cond ((equal action "list") (%source-list root path))
                            ((equal action "outline")
                             (if path (%source-outline root path) "Give the path to outline."))
                            ((equal action "read")
                             (if path (%source-read root path (obj-get args "from") (obj-get args "lines"))
                                 "Give the path to read."))
                            ((equal action "search")
                             (let ((text (obj-get args "text")))
                               (if (and (stringp text) (plusp (length text)))
                                   (%source-search root text path)
                                   "Give the text to search for.")))
                            (t "action must be list, outline, read or search"))))))))
