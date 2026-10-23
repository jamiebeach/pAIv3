(in-package #:pai-v3.kernel)

;;; Settings (operator decision 2026-10-12; bead v3-fu8.15). Everything the
;;; operator or the agent may tune is a declared setting kept in the store,
;;; never a command-line flag and never a code edit. Only what is needed
;;; before the store can open lives in the bootstrap file.
;;;
;;; A declaration gives the type, validation, default (from code, so a fresh
;;; agent works), description, group, and who may change it:
;;;   :operator              the operator only (the API also asks for a fresh
;;;                          passkey verification)
;;;   (:agent :min x :max y) the agent too, within bounds; the operator always
;;; A change is a setting.changed event (value, previous value, reason,
;;; origin); the settings projection holds current values. Readers ask for a
;;; setting when they use it, so a change applies at once, with no restart.

(defstruct setting-def
  name type default description group (changed-by :operator) min max one-of validate)

(defvar *settings* (make-hash-table :test 'equal) "Declared settings by name.")

(defmacro define-setting (name &key type default description group (changed-by :operator)
                                    min max one-of validate)
  "Declare setting NAME. DEFAULT is a form evaluated each time the setting
is read without a stored value."
  `(setf (gethash ,name *settings*)
         (make-setting-def :name ,name :type ,type :default (lambda () ,default)
                           :description ,description :group ,group :changed-by ',changed-by
                           :min ,min :max ,max :one-of ,one-of :validate ,validate)))

(define-condition setting-refused (error)
  ((name :initarg :name :reader setting-refused-name)
   (reason :initarg :reason :reader setting-refused-reason))
  (:report (lambda (c s) (format s "~a: ~a" (setting-refused-name c) (setting-refused-reason c)))))

(define-event-type "setting.changed"
  :schema (("name" :string) ("value" :any) ("previous" :any :optional)
           ("reason" :string) ("origin" :object)))

(define-projection "settings"
  :kind :inline
  :consumes ("setting.changed")
  :tables ("CREATE TABLE IF NOT EXISTS settings (name TEXT PRIMARY KEY, value_json TEXT NOT NULL, changed_at TEXT NOT NULL, event_id INTEGER NOT NULL)")
  :entity (lambda (e) (obj-get (event-payload e) "name"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO settings(name,value_json,changed_at,event_id) VALUES(?1,?2,?3,?4) ON CONFLICT(name) DO UPDATE SET value_json=excluded.value_json,changed_at=excluded.changed_at,event_id=excluded.event_id"
                        (obj-get p "name") (json-encode (obj-get p "value")) (event-at e) (event-id e)))))

(defun %def (name)
  (or (gethash name *settings*)
      (error 'setting-refused :name name :reason "there is no such setting")))

(defun %stored (kernel name)
  "The stored JSON text for NAME, or NIL."
  (sql-value (kernel-db kernel) "SELECT value_json FROM settings WHERE name=?1 LIMIT 1" name))

(defun setting (kernel name)
  "The current value of setting NAME: stored if changed, else its default."
  (let ((def (%def name)) (stored (%stored kernel name)))
    (if stored (json-parse stored) (funcall (setting-def-default def)))))

(defun %valid (def value)
  "NIL when VALUE suits DEF, else the reason it does not."
  (let ((type (setting-def-type def)))
    (cond ((not (case type
                  (:integer (integerp value))
                  (:number (realp value))
                  (:string (stringp value))
                  (:boolean (member value '(t :false)))
                  (:object (hash-table-p value))
                  (:array (listp value))
                  (t t)))
           (format nil "must be ~(~a~)" type))
          ((and (setting-def-min def) (realp value) (< value (setting-def-min def)))
           (format nil "must be at least ~a" (setting-def-min def)))
          ((and (setting-def-max def) (realp value) (> value (setting-def-max def)))
           (format nil "must be at most ~a" (setting-def-max def)))
          ((and (setting-def-one-of def) (not (member value (setting-def-one-of def) :test #'equal)))
           (format nil "must be one of ~{~a~^, ~}" (setting-def-one-of def)))
          ((setting-def-validate def) (funcall (setting-def-validate def) value)))))

(defun %may-change-p (def origin value)
  (let ((who (and (hash-table-p origin) (obj-get origin "principal_type")))
        (rule (setting-def-changed-by def)))
    (cond ((equal who "operator") t)
          ((and (equal who "self") (consp rule) (eq :agent (first rule)))
           (destructuring-bind (&key min max) (rest rule)
             (and (or (null min) (not (realp value)) (>= value min))
                  (or (null max) (not (realp value)) (<= value max))))))))

(defun change-setting (kernel name value &key origin (reason ""))
  "Change setting NAME to VALUE on behalf of ORIGIN. Signals SETTING-REFUSED
when the value is invalid or ORIGIN may not make this change."
  (let ((def (%def name)))
    (let ((invalid (%valid def value)))
      (when invalid (error 'setting-refused :name name :reason invalid)))
    (unless (%may-change-p def origin value)
      (error 'setting-refused :name name
                              :reason (if (consp (setting-def-changed-by def))
                                          "the agent may change this only within its bounds"
                                          "only the operator may change this")))
    (let ((payload (obj "name" name "previous" (setting kernel name) "reason" reason "origin" origin)))
      ;; Set apart so an empty list, which obj leaves out, is still a value.
      (setf (gethash "value" payload) value)
      (append! kernel "setting.changed" payload))
    value))

(defun reset-setting (kernel name &key origin (reason "back to the default"))
  "Return NAME to its default value."
  (change-setting kernel name (funcall (setting-def-default (%def name))) :origin origin :reason reason))

(defun settings-list (kernel)
  "Every declared setting with its value, default and rules, by group and name."
  (sort (loop for def being the hash-values of *settings*
              collect (obj "name" (setting-def-name def) "group" (or (setting-def-group def) "General")
                           "type" (string-downcase (symbol-name (or (setting-def-type def) :any)))
                           "value" (setting kernel (setting-def-name def))
                           "default" (funcall (setting-def-default def))
                           "changed" (if (%stored kernel (setting-def-name def)) t :false)
                           "description" (or (setting-def-description def) "")
                           "min" (or (setting-def-min def) :null) "max" (or (setting-def-max def) :null)
                           "one_of" (or (setting-def-one-of def) :null)
                           "changed_by" (if (consp (setting-def-changed-by def)) "agent-within-bounds" "operator")))
        #'string< :key (lambda (o) (format nil "~a/~a" (obj-get o "group") (obj-get o "name")))))

;;; Export and import: the whole configuration as a readable file, edited by
;;; hand or kept under version control, and applied back as ordinary
;;; validated changes. The store stays the source of truth.

(defun export-settings (kernel)
  "Every setting's current value, by name."
  (let ((out (obj)))
    (dolist (s (settings-list kernel) out)
      (setf (gethash (obj-get s "name") out) (obj-get s "value")))))

(defun import-settings (kernel values &key origin (reason "imported from a file") dry-run)
  "Apply VALUES (name -> value) that differ from the current ones. Every
entry is validated and permission-checked first; nothing is applied unless
all pass. Returns the changes as (name previous new)."
  (let ((changes '()))
    (maphash (lambda (name value)
               (let ((def (%def name)))
                 (let ((invalid (%valid def value)))
                   (when invalid (error 'setting-refused :name name :reason invalid)))
                 (unless (%may-change-p def origin value)
                   (error 'setting-refused :name name :reason "not permitted for this origin"))
                 (let ((current (setting kernel name)))
                   (unless (equal (json-encode current) (json-encode value))
                     (push (list name current value) changes)))))
             values)
    (unless dry-run
      (dolist (c changes)
        (change-setting kernel (first c) (third c) :origin origin :reason reason)))
    (sort changes #'string< :key #'first)))
