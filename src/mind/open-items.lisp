(in-package #:pai-v3.mind)

;;; What is open about something (design note interaction-recall §5, §11).
;;; Concerns, the docket and schedules each keep their own open items; each
;;; registers one function answering "what is open that names any of these
;;; terms, for this audience?", so whoever asks never reads their tables.

(defvar *open-item-sources* '()
  "(name . function) pairs; function is (lambda (kernel terms audience)),
returning lines describing open items that mention any of TERMS.")

(defmacro define-open-items (name (kernel terms audience) &body body)
  "Register NAME's answer to what is open about TERMS for AUDIENCE."
  `(let ((fn (lambda (,kernel ,terms ,audience) (declare (ignorable ,kernel ,terms ,audience)) ,@body)))
     (setf *open-item-sources*
           (cons (cons ,name fn) (remove ,name *open-item-sources* :key #'car :test #'equal)))
     ,name))

(defun mentions-any-p (text terms)
  "Whether TEXT names any of TERMS as a whole word, ignoring case."
  (and (stringp text)
       (some (lambda (term)
               (and (stringp term) (plusp (length term))
                    (loop with n = (length term)
                          for at = (search term text :test #'char-equal)
                            then (search term text :test #'char-equal :start2 (1+ at))
                          while at
                          thereis (and (or (zerop at) (not (alphanumericp (char text (1- at)))))
                                       (or (= (+ at n) (length text))
                                           (not (alphanumericp (char text (+ at n)))))))))
             terms)))

(defun open-items-about (kernel terms audience &key (most 5))
  "Lines describing what is open about TERMS that AUDIENCE may know, at most MOST."
  (let ((lines (loop for (nil . fn) in (reverse *open-item-sources*)
                     append (ignore-errors (funcall fn kernel terms audience)))))
    (subseq lines 0 (min most (length lines)))))
