(in-package #:pai-v3.self)

;;; lisp-eval: the agent's hands on its own image (spec §16.4, §16.10).
;;;
;;; Forms are read in this package with *read-eval* off, run under a
;;; timeout with output capped, and recorded in full as thought steps. A
;;; definition is also recorded as a live change: a definition whose name
;;; lives in this package is a skill and is re-applied at boot; a
;;; redefinition of anything else is a repair, live until restart, after
;;; which it is marked lost-on-restart so the agent proposes it properly.
;;;
;;; The kernel and trust packages are locked while agent code runs, so
;;; redefining, rebinding or interning into them fails. That is a tripwire,
;;; not a sandbox: the boundaries are that only operator-trust thoughts have
;;; this tool, every form is audited, and the process holds no host
;;; credentials.

(defvar *kernel* nil "The agent's kernel, while its own code runs.")
(defvar *thought-id* nil "The thought running the code.")

(defparameter *eval-seconds* 10)

(define-setting "self.eval_seconds"
  :type :integer :min 1 :max 120 :group "Self" :default *eval-seconds*
  :description "How long lisp_eval may run before it is stopped.")
(defparameter *eval-output-chars* 8000)
(defparameter +protected-packages+ (quote ("PAI-V3.KERNEL" "PAI-V3.TRUST" "PAI-V3.HOST")))
(defparameter +tripwires+
  '("unlock-package" "without-package-locks" "disable-package-locks" "with-unlocked-packages"
    ;; Keys, secrets and the egress guard (spec §17). Agent code reaches the
    ;; trust and host packages only by naming them; the guard variable lives
    ;; in the kernel, which this package uses.
    "pai-v3.trust" "pai-v3.host" "egress-guard" "secret-cache" "secrets-db")
  "Text that only appears in code trying to step around the package locks.")

(defparameter +definers+
  '(defun defmacro defgeneric defmethod defvar defparameter defconstant defstruct defclass
    define-tool define-slot-source define-context-profile))

(define-event-type "self.change.applied"
  :schema (("name" :string) ("scope" :string)        ; skill | repair
           ("form" :string) ("digest" :string)
           ("thought_id" :integer :optional)))

(define-event-type "self.change.lost"
  :schema (("digest" :string) ("reason" :string)))

;;; A skill is never deleted, only archived: kept in the ledger, not applied at
;;; boot, and restorable (operator, 2026-11-05).
(define-event-type "self.change.archived"
  :schema (("digest" :string) ("reason" :string) ("thought_id" :integer :optional)))
(define-event-type "self.change.restored"
  :schema (("digest" :string) ("thought_id" :integer :optional)))

(define-projection "live_changes"
  :kind :inline
  :consumes ("self.change.applied" "self.change.lost" "self.change.archived" "self.change.restored")
  :tables ("CREATE TABLE IF NOT EXISTS live_changes (event_id INTEGER PRIMARY KEY, name TEXT NOT NULL, scope TEXT NOT NULL, form TEXT NOT NULL, digest TEXT NOT NULL, status TEXT NOT NULL, reason TEXT, thought_id INTEGER, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS live_changes_status_idx ON live_changes(status,event_id)"
           "CREATE INDEX IF NOT EXISTS live_changes_digest_idx ON live_changes(digest)"
           "CREATE INDEX IF NOT EXISTS live_changes_name_idx ON live_changes(name,status)")
  :entity (lambda (e) (obj-get (event-payload e) "digest"))
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (cond
                ((string= type "self.change.archived")
                 (sql-exec db "UPDATE live_changes SET status='archived',reason=?2 WHERE digest=?1 AND status='active'"
                           (obj-get p "digest") (obj-get p "reason")))
                ((string= type "self.change.restored")
                 (let ((name (sql-value db "SELECT name FROM live_changes WHERE digest=?1 LIMIT 1" (obj-get p "digest"))))
                   (sql-exec db "UPDATE live_changes SET status='superseded' WHERE name=?1 AND status='active'" name)
                   (sql-exec db "UPDATE live_changes SET status='active',reason=NULL WHERE digest=?1 AND status='archived'"
                             (obj-get p "digest"))))
                ((string= type "self.change.applied")
                 (progn
                    ;; A newer definition of the same name supersedes the older one.
                    (sql-exec db "UPDATE live_changes SET status='superseded' WHERE name=?1 AND status='active'"
                              (obj-get p "name"))
                    (sql-exec db "INSERT INTO live_changes(event_id,name,scope,form,digest,status,thought_id,at) VALUES(?1,?2,?3,?4,?5,'active',?6,?7)"
                              (event-id e) (obj-get p "name") (obj-get p "scope") (obj-get p "form")
                              (obj-get p "digest") (obj-get p "thought_id") (event-at e))))
                (t
                 (sql-exec db "UPDATE live_changes SET status='lost-on-restart',reason=?2 WHERE digest=?1 AND status='active'"
                           (obj-get p "digest") (obj-get p "reason")))))))

(defun live-changes (kernel &key (status "active") (limit 200))
  "Live changes with STATUS, oldest first, as (name scope form digest)."
  (sql-rows (kernel-db kernel)
            "SELECT name,scope,form,digest FROM live_changes WHERE status=?1 ORDER BY event_id LIMIT ?2"
            status limit))

;;; Reading

(defun %read-forms (text)
  "The forms in TEXT, each with its own source text."
  (let ((*package* (find-package '#:pai-v3.self))
        (*read-eval* nil)
        (forms '()) (start 0))
    (loop
      (multiple-value-bind (form end)
          (read-from-string text nil text :start start)
        (when (eq form text) (return (nreverse forms)))
        (push (cons form (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text start end))) forms)
        (setf start end)))))

(defun %definition (form)
  "The defined name and its scope when FORM is a definition, else NIL."
  (when (and (consp form) (member (first form) +definers+))
    (let ((name (second form)))
      (cond ((stringp name) (values name "skill"))   ; define-tool, profiles
            ((and (consp name) (eq 'setf (first name)) (symbolp (second name)))
             (values (format nil "(setf ~s)" (second name))
                     (if (eq (symbol-package (second name)) (find-package '#:pai-v3.self)) "skill" "repair")))
            ((symbolp name)
             (values (let ((*package* (find-package '#:pai-v3.self))) (prin1-to-string name))
                     (if (or (keywordp name) (eq (symbol-package name) (find-package '#:pai-v3.self)))
                         "skill" "repair")))))))

;;; Running

(defvar *restored-variables* '()
  "Protected variables the last agent code changed, and that were put back.")

(defun %protected-variables ()
  "Global variables homed in the protected packages, with their values."
  (loop for name in +protected-packages+
        for package = (find-package name)
        when package
          append (let ((vars '()))
                   (do-symbols (s package vars)
                     (when (and (eq (symbol-package s) package) (boundp s)
                                (not (constantp s)))
                       (push (cons s (symbol-value s)) vars))))))

(defmacro %with-protection (&body body)
  "Run BODY with the protected packages locked. Package locks stop
definitions and interning; a protected variable that BODY sets is put back
afterwards and reported in *RESTORED-VARIABLES*."
  `(let ((locked '()) (saved (%protected-variables)))
     (setf *restored-variables* '())
     (unwind-protect
          (progn (dolist (name +protected-packages+)
                   (let ((package (find-package name)))
                     (when (and package (not (sb-ext:package-locked-p package)))
                       (sb-ext:lock-package package)
                       (push package locked))))
                 ,@body)
       (mapc #'sb-ext:unlock-package locked)
       (loop for (s . value) in saved
             unless (eq value (symbol-value s))
               do (setf (symbol-value s) value)
                  (push s *restored-variables*)))))

(defun %cap (text)
  (if (> (length text) *eval-output-chars*)
      (format nil "~a~%[~d more characters not shown]" (subseq text 0 *eval-output-chars*)
              (- (length text) *eval-output-chars*))
      text))

(defun lisp-eval (kernel thought-id text)
  "Evaluate the forms in TEXT as the agent. Returns what they printed and
returned, or the error, as text."
  (let ((lower (string-downcase text)))
    (dolist (wire +tripwires+)
      (when (search wire lower)
        (return-from lisp-eval
          (format nil "Refused: ~a steps around the protection of the kernel and trust. Propose the change instead." wire)))))
  (let ((out (make-string-output-stream))
        (values-text '())
        (seconds (setting kernel "self.eval_seconds")))
    (handler-case
        (let ((*kernel* kernel) (*thought-id* thought-id) (*agent-defining* t)
              (*package* (find-package '#:pai-v3.self))
              (*print-length* 50) (*print-level* 6) (*print-circle* t))
          (let ((*standard-output* out) (*error-output* out) (*trace-output* out))
            (%with-protection
              (sb-ext:with-timeout seconds
                (dolist (pair (%read-forms text))
                  (destructuring-bind (form . source) pair
                    (setf values-text (mapcar #'prin1-to-string (multiple-value-list (eval form))))
                    (multiple-value-bind (name scope) (%definition form)
                      (when name
                        (append! kernel "self.change.applied"
                                 (obj "name" name "scope" scope "form" source
                                      "digest" (pai-v3.kernel::sha256-hex source)
                                      "thought_id" thought-id)))))))))
          (%cap (format nil "~@[Output:~%~a~%~]=> ~{~a~^, ~}~@[~%Refused and put back: ~{~s~^, ~} belong to the kernel or trust. Propose the change instead.~]"
                        (let ((o (get-output-stream-string out))) (and (plusp (length o)) o))
                        (or values-text '("(no values)"))
                        *restored-variables*)))
      (sb-ext:timeout ()
        (format nil "Stopped after ~d seconds.~@[ Output so far:~%~a~]" seconds
                (%cap (get-output-stream-string out))))
      (sb-ext:package-lock-violation (e)
        (format nil "Refused: ~a~%That belongs to the kernel or trust, which cannot be changed live. Propose the change instead." e))
      (error (e)
        (%cap (format nil "Error: ~a~@[~%Output:~%~a~]" e
                      (let ((o (get-output-stream-string out))) (and (plusp (length o)) o))))))))

(define-tool "lisp_eval"
  :description "Evaluate Common Lisp forms in your own running image, in the package PAI-V3.SELF, and see what they print and return. Use it to look at yourself (describe, apropos, documentation, your projections through *kernel*) and to repair or extend yourself. Functions you define in PAI-V3.SELF are your skills and survive restarts; redefining anything else is a live repair that lasts until restart, so propose it as well. The kernel and trust cannot be changed this way."
  :parameters (obj "type" "object"
                   "properties" (obj "forms" (obj "type" "string" "description" "One or more Lisp forms."))
                   "required" (list "forms"))
  :available :operator-trust
  :core '("introspection" "work")
  :run (lambda (kernel id args) (lisp-eval kernel id (obj-get args "forms" ""))))

;;; Boot

(defun %shadows-built-in-tool (form-text)
  "The name of a source-defined tool FORM-TEXT redefines, else NIL."
  (let ((form (ignore-errors (let ((*package* (find-package '#:pai-v3.self)) (*read-eval* nil))
                               (read-from-string form-text)))))
    (and (consp form) (eq (first form) 'define-tool) (stringp (second form))
         (gethash (second form) *tools*)
         (second form))))

(defun restore-self (kernel)
  "At boot: re-apply skills in the order they were made; mark repairs, and
skills that no longer load, lost-on-restart. Returns (restored lost)."
  (let ((restored 0) (lost 0))
    (loop for (name scope form digest) in (live-changes kernel :limit 1000)
          for shadowed = (%shadows-built-in-tool form)
          ;; A skill that redefines a tool the source defines would replace it at
          ;; every boot, so no source fix to that tool could ever arrive (Angel's
          ;; copy of mail, 2026-11-05). The source wins; the skill is archived,
          ;; where it can be read and restored.
          when shadowed
            do (append! kernel "self.change.archived"
                        (obj "digest" digest
                             "reason" (format nil "it redefines the built-in tool ~a, which the source keeps up to date; archived at boot so source fixes reach you" shadowed)))
               (incf lost)
          unless shadowed
          do (if (string= scope "repair")
                 (progn (append! kernel "self.change.lost"
                                 (obj "digest" digest "reason" "a repair lasts until restart; propose it to keep it"))
                        (incf lost))
                 (handler-case
                     (let ((*package* (find-package '#:pai-v3.self)) (*read-eval* nil)
                           (*agent-defining* t))
                       (%with-protection (eval (read-from-string form)))
                       (incf restored))
                   (error (e)
                     (append! kernel "self.change.lost"
                              (obj "digest" digest "reason" (format nil "~a no longer loads: ~a" name e)))
                     (incf lost)))))
    (list restored lost)))

;;; The agent's own skills: list them, archive one it no longer wants (never
;;; deleted), or restore an archived one.

(defun %skill-rows (kernel status)
  (sql-rows (kernel-db kernel)
            "SELECT name,digest,form,reason FROM live_changes WHERE scope='skill' AND status=?1 ORDER BY event_id LIMIT 300"
            status))

(define-tool "skills"
  :description "My own skills, the definitions I made with lisp_eval that come back at every restart. action: list (active and archived), archive (name, reason: set one aside; it is kept and can be restored, never deleted), restore (name: bring back an archived one). A skill that redefines a built-in tool is archived at restart so source fixes reach me."
  :summary "List, archive or restore my own skills."
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "list" "archive" "restore"))
                                     "name" (obj "type" "string") "reason" (obj "type" "string"))
                   "required" (list "action"))
  :available :operator-trust
  :run (lambda (kernel thought-id args)
         (let ((action (obj-get args "action" "list")) (name (obj-get args "name")))
           (flet ((find-skill (status)
                    (find-if (lambda (r) (string-equal (first r) (or name ""))) (%skill-rows kernel status))))
             (cond
               ((string= action "archive")
                (let ((row (find-skill "active")) (reason (obj-get args "reason")))
                  (cond ((null row) (format nil "No active skill named ~a." name))
                        ((not (and (stringp reason) (plusp (length (string-trim " " reason))))) "Give a reason.")
                        (t (append! kernel "self.change.archived"
                                    (obj "digest" (second row) "reason" (subseq reason 0 (min 300 (length reason)))
                                         "thought_id" thought-id))
                           (format nil "Archived ~a. It stays defined until restart, and can be restored." (first row))))))
               ((string= action "restore")
                (let ((row (find-skill "archived")))
                  (cond ((null row) (format nil "No archived skill named ~a." name))
                        ((%shadows-built-in-tool (third row))
                         (format nil "~a redefines a built-in tool; it would be archived again at restart." (first row)))
                        (t (append! kernel "self.change.restored" (obj "digest" (second row) "thought_id" thought-id))
                           (format nil "Restored ~a; it comes back at the next restart." (first row))))))
               (t (flet ((lines (status)
                           (mapcar (lambda (r) (format nil "  ~a~@[ (~a)~]: ~a" (first r) (fourth r)
                                                       (let ((f (substitute #\Space #\Newline (third r)))) (subseq f 0 (min 100 (length f))))))
                                   (%skill-rows kernel status))))
                    (format nil "Active skills:~%~:[  none~;~:*~{~a~^~%~}~]~%Archived:~%~:[  none~;~:*~{~a~^~%~}~]"
                            (lines "active") (lines "archived")))))))))
