(in-package #:pai-v3.mind)

;;; Persona (docs/behaviour/persona.md; agent doc §12, §14 term 3).
;;; DRAFT ORDERING pending each agent's identity review (checklist A5): the
;;; order and grouping of the identity block live in *identity-order* so the
;;; review changes data, not code.

(defparameter *identity-order* '(:instructions :identity :voice :traits :influences :tone)
  "Order of facets in the rendered identity block. Under agent review.")

(defparameter *max-traits* 7)
(defparameter *max-trait-chars* 200)

(define-event-type "persona.profile.set"
  :schema (("persona_id" :string) ("revision" :integer) ("identity" :string) ("voice" :string)))
(define-event-type "persona.instructions.set"
  :schema (("revision" :integer) ("text" :string)))
(define-event-type "persona.tone.set"
  :schema (("text" :string)))
(define-event-type "self.trait.adopted"
  :schema (("trait_id" :string) ("text" :string) ("reason" :string) ("proposed_by" :string)))
(define-event-type "self.trait.retired"
  :schema (("trait_id" :string) ("reason" :string)))
(define-event-type "self.influence.adopted"
  :schema (("influence_id" :string) ("what_it_means" :string) ("behaviour_guidance" :string)
           ("source_refs" :array) ("strength" :number)))
(define-event-type "self.influence.released"
  :schema (("influence_id" :string) ("reason" :string)))
(define-event-type "operator.influence.reversed"
  :schema (("influence_id" :string) ("reason" :string)))

(defun %upsert-facet (db facet key text order source)
  (sql-exec db "INSERT INTO persona(facet,key,text,sort_key,source_event_id,active) VALUES(?1,?2,?3,?4,?5,1) ON CONFLICT(facet,key) DO UPDATE SET text=excluded.text,sort_key=excluded.sort_key,source_event_id=excluded.source_event_id,active=1"
            facet key text order source))

(defun %deactivate (db facet key)
  (sql-exec db "UPDATE persona SET active=0 WHERE facet=?1 AND key=?2" facet key))

(defun %reduce-persona (db e)
  (let ((p (event-payload e)) (type (event-type e)) (id (event-id e)))
    (flet ((f (k) (obj-get p k)))
      (cond
        ((string= type "persona.profile.set")
         (%upsert-facet db "identity" "main" (f "identity") 0 id)
         (%upsert-facet db "voice" "main" (f "voice") 0 id)
         (%upsert-facet db "persona_id" "main" (f "persona_id") 0 id))
        ((string= type "persona.instructions.set")
         (%upsert-facet db "instructions" "main" (f "text") 0 id))
        ((string= type "persona.tone.set")
         (%upsert-facet db "tone" "main" (f "text") 0 id))
        ((string= type "self.trait.adopted")
         (when (> (length (f "text")) *max-trait-chars*)
           (error "Trait text exceeds ~d characters" *max-trait-chars*))
         (when (>= (or (sql-value db "SELECT count(*) FROM persona WHERE facet=?1 AND active=1 LIMIT 1" "traits") 0)
                   *max-traits*)
           (error "Already ~d active traits" *max-traits*))
         (%upsert-facet db "traits" (f "trait_id")
                        (format nil "~a (adopted: ~a)" (f "text") (f "reason")) id id))
        ((string= type "self.trait.retired") (%deactivate db "traits" (f "trait_id")))
        ((string= type "self.influence.adopted")
         (%upsert-facet db "influences" (f "influence_id")
                        (format nil "~a ~a" (f "what_it_means") (f "behaviour_guidance"))
                        (- (round (* 1000 (f "strength")))) id))
        ((or (string= type "self.influence.released") (string= type "operator.influence.reversed"))
         (%deactivate db "influences" (f "influence_id")))))))

(define-projection "persona"
  :kind :inline
  :consumes ("persona.profile.set" "persona.instructions.set" "persona.tone.set"
             "self.trait.adopted" "self.trait.retired" "self.influence.adopted"
             "self.influence.released" "operator.influence.reversed")
  :tables ("CREATE TABLE IF NOT EXISTS persona (facet TEXT NOT NULL, key TEXT NOT NULL, text TEXT NOT NULL, sort_key INTEGER NOT NULL, source_event_id INTEGER NOT NULL, active INTEGER NOT NULL, PRIMARY KEY(facet,key))")
  :entity (lambda (e) (or (obj-get (event-payload e) "trait_id")
                          (obj-get (event-payload e) "influence_id") "persona"))
  :reduce #'%reduce-persona)

(defparameter +facet-headings+
  '(:instructions nil :identity nil :voice nil
    :traits "Traits I have adopted" :influences "What has shaped me" :tone "Tone"))

(defun render-identity (kernel &key (operator-facing t))
  "The identity block and its fingerprint. Reads at most a few dozen rows."
  (let ((sections '()))
    (dolist (facet *identity-order*)
      (unless (and (eq facet :tone) (not operator-facing))
        (let ((rows (sql-rows (kernel-db kernel)
                              "SELECT text FROM persona WHERE facet=?1 AND active=1 ORDER BY sort_key,key LIMIT 16"
                              (string-downcase (symbol-name facet))))
              (heading (getf +facet-headings+ facet)))
          (when rows
            (push (if heading
                      (format nil "## ~a~%~{- ~a~^~%~}" heading (mapcar #'first rows))
                      (format nil "~{~a~^~%~%~}" (mapcar #'first rows)))
                  sections)))))
    (let ((text (format nil "~{~a~^~%~%~}" (nreverse sections))))
      (values text (pai-v3.kernel::sha256-hex text)))))
