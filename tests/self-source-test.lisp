(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.self)

;;; Reading my own source (bead v3-jm9): list, read and search inside the
;;; source copy only.

(defun source-tool (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "source" pai-v3.mind::*tools*)) k 1 (apply #'obj args)))

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defmacro with-source-copy ((k root) &body body)
  `(with-temp-kernel (,k)
     (let ((,root (uiop:ensure-directory-pathname
                   (format nil "/tmp/src-~d/" (random 1000000000)))))
       (ensure-directories-exist (merge-pathnames "src/mind/" ,root))
       (with-open-file (s (merge-pathnames "src/mind/thought.lisp" ,root) :direction :output)
         (format s "(in-package #:x)~%(defun think ()~%  :thinking)~%(defun answer () :answered)~%"))
       (with-open-file (s (merge-pathnames "pai-v3.asd" ,root) :direction :output)
         (format s "(defsystem x)~%"))
       (with-open-file (s (format nil "/tmp/secret-~d.txt" (random 1000)) :direction :output :if-exists :supersede)
         (format s "outside~%"))
       (unwind-protect
            (progn (change-setting ,k "dev.source_dir" (namestring ,root) :origin *operator*)
                   ,@body)
         (uiop:delete-directory-tree ,root :validate t)))))

(deftest the-source-tool-is-off-until-set
  (with-temp-kernel (k)
    (is (search "not set" (source-tool k "action" "list")))))

(deftest it-lists-reads-and-searches
  (with-source-copy (k root)
    (is (equal (format nil "pai-v3.asd~%src/mind/thought.lisp") (source-tool k "action" "list")))
    (is (equal "src/mind/thought.lisp" (source-tool k "action" "list" "path" "src/")))
    (let ((text (source-tool k "action" "read" "path" "src/mind/thought.lisp" "from" 2 "lines" 2)))
      (is (search "lines 2-3 of 4" text))
      (is (search "    2  (defun think ()" text))
      (is (null (search "answer" text)) "only the lines asked for"))
    (is (search "src/mind/thought.lisp:4: (defun answer () :answered)" (source-tool k "action" "search" "text" "ANSWERED")))
    (is (search "Nothing in my source" (source-tool k "action" "search" "text" "nowhere")))))

(deftest it-never-leaves-the-source-copy
  (with-source-copy (k root)
    (dolist (path (list "../secret.txt" "src/../../etc/passwd" "/etc/passwd"
                        (format nil "src~c..~cx" (code-char 92) (code-char 92)) "./pai-v3.asd"))
      (is (search "outside my source" (source-tool k "action" "read" "path" path)) path))))

(deftest only-operator-and-coding-agent-thoughts-hold-it
  (let ((tool (gethash "source" *tools*)))
    (is (member tool (tools-for (list :audience "operator"))))
    (is (member tool (tools-for (list :audience "coding-agent"))))
    (is (not (member tool (tools-for (list :audience "guest:alex")))))))

(deftest it-outlines-and-keeps-searches-useful
  (with-source-copy (k root)
    (let ((outline (source-tool k "action" "outline" "path" "src/mind/thought.lisp")))
      (is (search "    2  (defun think ()" outline))
      (is (search "    4  (defun answer ()" outline)))
    (is (search "at least 4 characters" (source-tool k "action" "search" "text" "req")) "fragments are refused")
    (let ((pai-v3.self::*source-search-hits* 1))
      (let ((many (source-tool k "action" "search" "text" "defun")))
        (is (search "2 matches in 1 files" many) many)
        (is (search "src/mind/thought.lisp: 2" many) "counts per file come first")))))

(deftest a-repeated-read-is-pointed-out
  (is (null (pai-v3.mind::%repeat-note "source" "{\"action\":\"read\",\"path\":\"a.lisp\",\"from\":1}"
                                       '(("source" "{\"action\":\"read\",\"path\":\"a.lisp\",\"from\":50}")))))
  (is (search "read number 3 of a.lisp"
              (pai-v3.mind::%repeat-note "source" "{\"action\":\"read\",\"path\":\"a.lisp\",\"from\":100}"
                                         '(("source" "{\"action\":\"read\",\"path\":\"a.lisp\",\"from\":1}")
                                           ("source" "{\"action\":\"read\",\"path\":\"a.lisp\",\"from\":50}"))))))

(run-suite "self-source")
