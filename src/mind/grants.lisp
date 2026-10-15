(in-package #:pai-v3.mind)

;;; Audiences, sensitivity and grants (spec §16.5; scenario 6).
;;;
;;; Everything the agent knows carries a sensitivity class. An audience may
;;; receive a class by default (the operator receives everything; peers
;;; receive "peers" and "public"; anyone else "public"), or because the
;;; operator granted that principal a topic. Only operator origin may grant
;;; or revoke, and every grant is an audited event.

(defparameter +sensitivities+ '("operator-only" "household" "peers" "public"))

(defun granted-class (topic) (format nil "granted:~a" topic))

(defun valid-sensitivity-p (class)
  (or (member class +sensitivities+ :test #'string=)
      (and (> (length class) 8) (string= "granted:" class :end2 8))))

(define-event-type "trust.granted"
  :schema (("principal" :string) ("topic" :string) ("origin" :object)))
(define-event-type "trust.revoked"
  :schema (("principal" :string) ("topic" :string) ("reason" :string) ("origin" :object)))

(define-projection "grants"
  :kind :inline :consumes ("trust.granted" "trust.revoked")
  :tables ("CREATE TABLE IF NOT EXISTS trust_grants (principal TEXT NOT NULL, topic TEXT NOT NULL, active INTEGER NOT NULL, changed_at TEXT NOT NULL, PRIMARY KEY(principal,topic))"
           "CREATE INDEX IF NOT EXISTS trust_grants_topic_idx ON trust_grants(active,topic)")
  :entity (lambda (e) (obj-get (event-payload e) "principal"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO trust_grants(principal,topic,active,changed_at) VALUES(?1,?2,?3,?4) ON CONFLICT(principal,topic) DO UPDATE SET active=excluded.active,changed_at=excluded.changed_at"
                        (obj-get p "principal") (obj-get p "topic")
                        (if (string= (event-type e) "trust.granted") 1 0) (event-at e)))))

(define-condition authority-refused (error)
  ((action :initarg :action :reader refused-action))
  (:report (lambda (c s) (format s "Only the operator may ~a." (refused-action c)))))

(defun %require-operator (origin action)
  (unless (and (hash-table-p origin) (equal "operator" (obj-get origin "principal_type")))
    (error 'authority-refused :action action)))

(defun grant-topic (kernel origin principal topic)
  "Let PRINCIPAL receive memories classed granted:TOPIC (or the household
class when TOPIC is \"household\"). ORIGIN must be a verified operator origin."
  (%require-operator origin "grant access")
  (append! kernel "trust.granted" (obj "principal" principal "topic" topic "origin" origin)))

(defun revoke-topic (kernel origin principal topic &optional (reason "revoked"))
  (%require-operator origin "revoke access")
  (append! kernel "trust.revoked" (obj "principal" principal "topic" topic "reason" reason "origin" origin)))

(defun %grants-of (kernel principal)
  (mapcar #'first (sql-rows (kernel-db kernel)
                            "SELECT topic FROM trust_grants WHERE principal=?1 AND active=1 LIMIT 64"
                            principal)))

(defun %all-granted-topics (kernel)
  (mapcar #'first (sql-rows (kernel-db kernel)
                            "SELECT DISTINCT topic FROM trust_grants WHERE active=?1 LIMIT 256" 1)))

(defun sensitivities-for (kernel audience)
  "Every sensitivity class AUDIENCE may receive."
  (cond ((equal audience "operator")
         (append +sensitivities+ (mapcar #'granted-class (%all-granted-topics kernel))))
        (t
         (let ((topics (%grants-of kernel audience)))
           (append (if (and (> (length audience) 5) (string= "peer:" audience :end2 5))
                       '("peers" "public")
                       '("public"))
                   (and (member "household" topics :test #'string=) '("household"))
                   (mapcar #'granted-class (remove "household" topics :test #'string=)))))))
