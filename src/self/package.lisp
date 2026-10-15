(defpackage #:pai-v3.self
  (:documentation "The agent's own package. lisp-eval reads here; what the
agent defines here are its skills, kept across restarts.")
  (:use #:cl #:pai-v3.kernel #:pai-v3.mind)
  (:export #:*kernel* #:*thought-id* #:lisp-eval #:restore-self #:live-changes))
