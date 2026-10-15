(in-package #:pai-v3.test)

;;; Every SELECT in src/ must be answered from an index, never by scanning a
;;; table (engineering spec §11). Runtime code only: maintenance is not
;;; loaded at runtime and pages through whole tables by design. Statements are collected by reading the
;;; source forms, then EXPLAIN QUERY PLAN runs against a freshly opened store.

(defun walk-strings (form fn)
  (cond ((stringp form) (funcall fn form))
        ((consp form)
         (walk-strings (car form) fn)
         (walk-strings (cdr form) fn))))

(defun source-selects ()
  (let ((found '()))
    (dolist (path (remove-if (lambda (p) (member "maintenance" (pathname-directory p) :test #'equal))
                           (directory #p"/v3/src/**/*.lisp")))
      (with-open-file (in path :external-format :utf-8)
        (let ((*package* (find-package '#:pai-v3.kernel))
              (*read-eval* nil))
          (loop for form = (read in nil in)
                until (eq form in)
                do (when (and (consp form) (eq (car form) 'in-package))
                     (setf *package* (find-package (second form))))
                   (walk-strings
                    form
                    (lambda (s)
                      (when (and (>= (length s) 6)
                                 (not (find #\~ s))
                                 (string-equal "SELECT" (string-left-trim " " s) :end2 6)
                                 (search " FROM " (string-upcase s)))
                        (push (cons (file-namestring path) s) found))))))))
    found))

(defun full-scan-p (detail)
  "A plan step that reads a whole table rather than an index range."
  (and (search "SCAN " detail)
       (not (search " USING " detail))
       ;; The schema table is read once at open to prove the key.
       (not (search "sqlite_master" detail))
       ;; A full-text MATCH is answered from the FTS index.
       (not (and (search "VIRTUAL TABLE INDEX" detail) (search ":M" detail)))))

(deftest every-select-uses-an-index
  (with-temp-kernel (k)
    ;; Tables of the other stores, so their statements are explained too.
    (sql-exec (kernel-db k) pai-v3.trust::+secrets-ddl+)
    (let ((selects (source-selects)))
      (is (> (length selects) 5) "statements were collected")
      (dolist (entry selects)
        (dolist (step (sql-rows (kernel-db k)
                                (concatenate 'string "EXPLAIN QUERY PLAN " (cdr entry))))
          (let ((detail (fourth step)))
            (is (not (full-scan-p detail))
                (format nil "~a: ~a -- ~a" (car entry) detail (cdr entry)))))))))

(run-suite "kernel-query-plan")
