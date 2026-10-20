(in-package #:pai-v3.mind)

;;; Tools (spec §16.10). A tool is a Lisp function with a docstring. What a
;;; thought may use is computed from its origin and audience; tools it may not
;;; use are never mentioned. Native schemas are sent only for the tools in
;;; hand (core for the origin, describe_tool, and any the thought has picked
;;; up); every other available tool is one line in a compact index. All
;;; output is in name order so the prefix stays byte-identical for caching.

(defstruct tool name summary doc parameters (available :any) core run publishes)

(defvar *tools* (make-hash-table :test 'equal))

(defun %first-sentence (text)
  (let ((end (or (position #\. text) (position #\Newline text))))
    (if end (subseq text 0 (1+ end)) text)))

(defmacro define-tool (name &key description summary parameters (available :any) core run publishes)
  "Declare a tool. DESCRIPTION is its documentation; SUMMARY (default: the
first sentence of DESCRIPTION) is its index line. PARAMETERS is a JSON-schema
object. AVAILABLE is :ANY, :OPERATOR-TRUST (only thoughts whose audience is
the operator), or a list of the audiences that may use it. CORE is T, or a list of origins, for which the tool is always
in hand. RUN is (lambda (kernel thought-id arguments-object)) returning a
string, or :PENDING when the result will arrive later. PUBLISHES marks tools
that publish to non-operator audiences; private reflection cannot use them."
  `(register-tool (make-tool :name ,name :doc ,description
                             :summary (or ,summary (%first-sentence ,description))
                             :parameters ,parameters :available ,available :core ,core :run ,run
                             :publishes ,publishes)))

(defvar *agent-defining* nil
  "True while the agent's own code (lisp-eval) runs. A tool the agent
defines for itself is always operator-trust and never core.")

(defun register-tool (tool)
  (when *agent-defining*
    (setf (tool-available tool) :operator-trust (tool-core tool) nil))
  (setf (gethash (tool-name tool) *tools*) tool))

(defun reflecting-p (thought)
  "Both ordinary introspection and fleet-digest reflection think privately."
  (member (getf thought :origin) '("introspection" "reflection") :test #'equal))

(defun private-initiative-p (thought)
  "Operator-context initiative must draft before speaking to a peer."
  (and (equal "operator" (getf thought :audience))
       (member (getf thought :origin)
               '("introspection" "reflection" "concern" "docket" "noticing" "unbidden" "private")
               :test #'equal)))

(defun tool-available-p (tool thought)
  ;; Private initiative thinks with operator context, so nothing it does may reach anyone
  ;; else directly: the tools that publish are not its to use (docs/reflect-then-speak.md).
  (and (not (and (tool-publishes tool) (private-initiative-p thought)))
   (let ((available (tool-available tool)))
    (if (listp available)
        (member (getf thought :audience) available :test #'equal)
        (ecase available
          (:any t)
          (:operator-trust (equal "operator" (getf thought :audience)))
          ;; The operator, and fleet peers (audience "peer:<id>").
          (:operator-and-peers (let ((a (getf thought :audience)))
                                 (or (equal "operator" a)
                                     (and (stringp a) (> (length a) 5) (string= "peer:" a :end2 5))))))))))

(defun tools-for (thought)
  "Tools THOUGHT may use, in name order."
  (sort (loop for tool being the hash-values of *tools*
              when (tool-available-p tool thought) collect tool)
        #'string< :key #'tool-name))

(defun %core-p (tool origin)
  (let ((core (tool-core tool)))
    (or (eq core t) (and (listp core) (member origin core :test #'string=)))))

(defun tools-in-hand (thought tools picked-up)
  "The subset of TOOLS whose schemas are sent: core for THOUGHT's origin, and
those named in PICKED-UP. describe_tool is in hand only if something is not."
  (let* ((hand (remove-if-not (lambda (tool)
                                (or (%core-p tool (getf thought :origin))
                                    (member (tool-name tool) picked-up :test #'string=)))
                              tools))
         (rest (set-difference tools hand)))
    (if rest
        hand
        (remove "describe_tool" hand :key #'tool-name :test #'string=))))

(defun tool-specs (tools)
  (loop for tool in tools
        collect (obj "type" "function"
                     "function" (obj "name" (tool-name tool)
                                     "description" (tool-summary tool)
                                     "parameters" (or (tool-parameters tool) (obj "type" "object"))))))

(defun tool-index-text (tools hand)
  "One line per available tool not in HAND, or NIL."
  (let ((others (set-difference tools hand)))
    (when others
      (format nil "## Tools~%Besides the tools you hold, you can use these; call describe_tool for how.~%~{- ~a~%~}"
              (mapcar (lambda (tool) (format nil "~a: ~a" (tool-name tool) (tool-summary tool)))
                      (sort (copy-list others) #'string< :key #'tool-name))))))

(define-tool "describe_tool"
  :description "Show how to use a tool, and keep it in hand for the rest of this thought."
  :parameters (obj "type" "object"
                   "properties" (obj "name" (obj "type" "string"))
                   "required" (list "name"))
  :core t
  :run (lambda (kernel id args)
         (let ((tool (gethash (obj-get args "name" "") *tools*)))
           (if (and tool (tool-available-p tool (thought-row kernel id)))
               (format nil "~a~%~%~a~%~%Parameters: ~a" (tool-name tool) (tool-doc tool)
                       (json-encode (or (tool-parameters tool) (obj "type" "object"))))
               (format nil "No tool named ~a." (obj-get args "name" ""))))))
