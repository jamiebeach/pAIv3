(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Context assembly latency gate (spec §5: p95 < 150 ms at 1M events,
;;; excluding the query embedding). Not part of the default run.
;;; Builds 1M events: 400k dialogue turns across audiences and channels,
;;; 100k thoughts with steps, and filler; then times 300 assemblies.

(define-event-type "gate.filler" :schema (("n" :integer)))

(defparameter *path* "/tmp/pai-v3-gate-context.sqlite3")

(defun build (k)
  (append! k "persona.profile.set" (obj "persona_id" "gate" "revision" 1
                                        "identity" (format nil "~{~a ~}" (make-list 200 :initial-element "identity"))
                                        "voice" "Plain."))
  (append! k "persona.instructions.set" (obj "revision" 1 "text" "Rules."))
  (loop with n = 0
        while (< (ledger-head k) 1000000)
        do (append-batch!
            k (loop repeat 2000
                    for i = (incf n)
                    collect (case (mod i 10)
                              ((0 1 2 3)
                               (list "turn.received"
                                     (obj "channel" (if (evenp i) "web" "telegram")
                                          "audience" (if (zerop (mod i 7)) "person:guest" "operator")
                                          "speaker" "Operator"
                                          "text" (format nil "turn ~d about the garden, the weather and plans" i)
                                          "origin" (obj "principal_type" "operator"))))
                              (4 (list "thought.requested"
                                       (obj "origin" "private" "reason" (format nil "musing ~d" i)
                                            "audience" "operator")))
                              (t (list "gate.filler" (obj "n" i))))))))

(defun percentile (values p)
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (min (1- (length sorted)) (floor (* p (length sorted)))) sorted)))

(deftest assembly-is-fast-at-a-million-events
  (ignore-errors (delete-file *path*))
  (let ((t0 (get-internal-real-time)))
    (let ((k (open-kernel *path* :agent-id "gate" :key *test-key*)))
      (build k)
      (close-kernel k))
    (format t "~&  built 1M in ~,1f s~%" (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
  (let ((k (open-kernel *path* :agent-id "gate" :key *test-key*))
        (times '())
        ;; The query embedding is excluded from this gate (a network call to
        ;; the embedding service); recall over the store is still measured.
        (*embedder* (lambda (texts) (mapcar (lambda (x) (declare (ignore x)) (make-list 768 :initial-element 0.5)) texts))))
    (unwind-protect
         (dotimes (i 300)
           (let ((start (get-internal-real-time)))
             (assemble-context k (if (evenp i) "operator-turn" "private-cognition")
                               (list :audience "operator" :task "What should I do next?"))
             (push (/ (* 1000.0 (- (get-internal-real-time) start)) internal-time-units-per-second)
                   times)))
      (close-kernel k))
    (format t "~&  assembly ms: p50 ~,1f  p95 ~,1f  max ~,1f~%"
            (percentile times 0.5) (percentile times 0.95) (reduce #'max times))
    (is (< (percentile times 0.95) 150) "p95 under 150 ms"))
  (ignore-errors (delete-file *path*)))

(run-suite "gate-context")
