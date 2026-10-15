;;; Minimal test harness. Each suite runs in a fresh SBCL process.
(defpackage #:pai-v3.test
  (:use #:cl #:pai-v3.kernel)
  (:export #:deftest #:is #:signals #:run-suite #:with-temp-kernel
           #:*test-key* #:*t* #:at-hour #:advance #:with-clock))
(in-package #:pai-v3.test)

(defvar *tests* '())
(defvar *failures* 0)
(defvar *checks* 0)

(defmacro deftest (name &body body)
  `(progn (setf *tests* (append (remove ',name *tests*) (list ',name)))
          (defun ,name () ,@body)))

(defmacro is (form &optional description)
  `(progn (incf *checks*)
          (unless ,form
            (incf *failures*)
            (format t "~&  FAIL: ~a~@[ (~a)~]~%" ',form ,description))))

(defmacro signals (condition &body body)
  `(progn (incf *checks*)
          (unless (handler-case (progn ,@body nil) (,condition () t))
            (incf *failures*)
            (format t "~&  FAIL: expected ~a from ~s~%" ',condition ',body))))

(defun temp-path ()
  (format nil "/tmp/pai-v3-test-~d-~d.sqlite3" (get-universal-time) (random 1000000)))

(defvar *test-key* (ironclad:random-data 32)
  "Every test store is encrypted, as every real store is (spec §17).")

(defmacro with-temp-kernel ((var &key (agent-id "test-agent") path (key '*test-key*)) &body body)
  (let ((p (gensym)))
    `(let* ((,p (or ,path (temp-path)))
            (,var (open-kernel ,p :agent-id ,agent-id :key ,key)))
       (unwind-protect (progn ,@body)
         (close-kernel ,var)))))

;;; Virtual clock for time-dependent tests.
(defvar *t* 0)
(defun at-hour (h) (setf *t* (+ (encode-universal-time 0 0 0 12 10 2026 0) (* h 3600))))
(defun advance (seconds) (incf *t* seconds))
(defmacro with-clock (&body body) `(let ((*clock* (lambda () *t*))) (at-hour 0) ,@body))

(defun run-suite (name)
  (format t "~&Suite ~a~%" name)
  (dolist (test *tests*)
    (let ((before *failures*))
      (handler-case (funcall test)
        (error (e) (incf *failures*)
          (format t "~&  ERROR in ~a: ~a~%" test e)))
      (format t "~&  ~:[ok  ~;FAIL~] ~(~a~)~%" (> *failures* before) test)))
  (format t "~&~a: ~d checks, ~d failures~%" name *checks* *failures*)
  (sb-ext:exit :code (if (zerop *failures*) 0 1)))
