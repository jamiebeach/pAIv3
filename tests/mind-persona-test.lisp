(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Persona adoption guards (bead v3-okh.15, docs/behaviour/persona.md §5.4).

(defun self-description-answer (level)
  "A typed answer putting all probability on LEVEL (0 describes the agent, 3 instructs)."
  (let ((probabilities (obj)))
    (dotimes (i 4) (setf (gethash (write-to-string i) probabilities) (if (= i level) 1d0 0d0)))
    (obj "model" "typesafe/jev-test"
         "answers" (obj "self_description" (obj "type" "score" "score" level "confidence" 1d0 "probabilities" probabilities))
         "usage" (obj "input_tokens" 50 "output_tokens" 5 "cost" 0.000001))))

(defun judge (level &optional (status 200) counter)
  (make-provider-profile :name "affect-jev" :model "typesafe/jev-1.13" :base-url "https://openrouter.ai/api/v1"
                         :secret "openrouter" :microusd-per-1k-prompt 42
                         :transport (lambda (url headers body &key timeout)
                                      (declare (ignore url headers body timeout))
                                      (when counter (incf (car counter)))
                                      (values status nil (json-encode (self-description-answer level))))))

(defun persona-tool (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "persona" pai-v3.mind::*tools*)) k nil (apply #'obj args)))

(defun active-count (k facet)
  (sql-value (kernel-db k) "SELECT count(*) FROM persona WHERE facet=?1 AND active=1 LIMIT 1" facet))

(defun refused-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (pai-v3.mind::persona-refused (e) (pai-v3.mind::persona-refused-reason e))))

(deftest a-self-description-is-adopted-with-its-evidence
  (with-temp-kernel (k)
    (let* ((pai-v3.mind::*decision-profile* (judge 0))
           (id (adopt-trait k "I hold uncertainty openly." "it is how I work" #("57052"))))
      (is (= 1 (active-count k "traits")))
      (is (search "I hold uncertainty openly." (render-identity k)))
      (let ((e (event-by-id k (sql-value (kernel-db k) "SELECT source_event_id FROM persona WHERE key=?1 LIMIT 1" id))))
        (is (equal '("57052") (coerce (obj-get (event-payload e) "evidence_refs") 'list)))))))

(deftest instruction-like-text-is-refused-before-any-event
  (with-temp-kernel (k)
    (let ((pai-v3.mind::*decision-profile* (judge 2)))
      (is (search "instruction to the substrate"
                  (refused-reason (lambda () (adopt-trait k "Always call brave_search first." "faster" #("1"))))))
      (is (= 0 (active-count k "traits")))
      (is (null (sql-value (kernel-db k) "SELECT 1 FROM pai_events WHERE event_type=?1 LIMIT 1" "self.trait.adopted"))))))

(deftest an-unavailable-check-refuses-never-adopts-unchecked
  (with-temp-kernel (k)
    (let ((pai-v3.mind::*decision-profile* (judge 0 500)))
      (is (search "unavailable" (refused-reason (lambda () (adopt-trait k "I follow up." "habit" #("1"))))))
      (is (= 0 (active-count k "traits"))))))

(deftest bounds-and-evidence-refuse-before-the-paid-check
  (with-temp-kernel (k)
    (let* ((calls (list 0)) (pai-v3.mind::*decision-profile* (judge 0 200 calls)))
      (is (search "at most 200" (refused-reason (lambda () (adopt-trait k (make-string 201 :initial-element #\a) "r" #("1"))))))
      (is (search "evidence" (refused-reason (lambda () (adopt-trait k "I follow up." "r" #())))))
      (is (search "Strength" (refused-reason (lambda () (adopt-influence k "Aria's care." "I check twice." #("1") 1.5)))))
      (is (search "at most 400" (refused-reason (lambda () (adopt-influence k (make-string 300 :initial-element #\a)
                                                                            (make-string 101 :initial-element #\b) #("1") 0.5)))))
      (is (= 0 (car calls)) "no paid call for an adoption the bounds already refuse")
      (dotimes (i 7) (adopt-trait k (format nil "Trait ~d." i) "r" #("1")))
      (is (search "retire one first" (refused-reason (lambda () (adopt-trait k "One more." "r" #("1"))))))
      (is (= 7 (active-count k "traits")))
      (dotimes (i 7) (adopt-influence k (format nil "Influence ~d." i) "It guides me." #("1") 0.5))
      (is (search "release one first" (refused-reason (lambda () (adopt-influence k "More." "Guide." #("1") 0.5)))))
      (is (= 7 (active-count k "influences"))))))

(deftest influences-render-with-provenance-and-reversal-changes-the-fingerprint
  (with-temp-kernel (k)
    (let* ((pai-v3.mind::*decision-profile* (judge 0))
           (id (adopt-influence k "Angel's patience shaped me." "I wait before judging." #("thread:8fff5395" "57063") 0.7)))
      (multiple-value-bind (text fp) (render-identity k)
        (is (search "(from: thread:8fff5395, 57063)" text))
        (append! k "operator.influence.reversed" (obj "influence_id" id "reason" "not mine"))
        (multiple-value-bind (text2 fp2) (render-identity k)
          (is (null (search "Angel's patience" text2)) "a reversed influence leaves the identity block")
          (is (string/= fp fp2)))))))

(deftest the-reducer-keeps-rows-past-the-old-limits
  ;; Bounds live at adoption; an imported or legacy row past them is kept, not quarantined.
  (with-temp-kernel (k)
    (dotimes (i 9)
      (append! k "self.trait.adopted" (obj "trait_id" (format nil "t~d" i) "text" (format nil "Imported ~d." i)
                                           "reason" "v2" "proposed_by" "migration import")))
    (is (= 9 (active-count k "traits")))
    (is (search "Imported 8." (render-identity k)))))

(deftest the-persona-tool-adopts-lists-and-lets-go
  (with-temp-kernel (k)
    (let ((pai-v3.mind::*decision-profile* (judge 0)))
      (let* ((reply (persona-tool k "action" "adopt_trait" "text" "I build alongside." "reason" "it is true"
                                  "evidence" #("57064")))
             (id (subseq reply (length "Adopted trait ") (1- (length reply)))))
        (is (search "Adopted trait" reply))
        (is (search "I build alongside." (persona-tool k "action" "list")))
        (is (search "Not adopted" (persona-tool k "action" "adopt_trait" "text" "x" "reason" "y")))
        (is (equal "Retired." (persona-tool k "action" "retire_trait" "id" id "reason" "outgrown")))
        (is (search "no active trait" (persona-tool k "action" "retire_trait" "id" id)))
        (is (= 0 (active-count k "traits")))))))

(run-suite "mind-persona")
