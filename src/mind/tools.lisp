(in-package #:pai-v3.mind)

;;; Tools (spec §16.10). A tool is a Lisp function with a docstring. What a
;;; thought may use is computed from its phase and private/recipient scope; tools it may not
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
object. AVAILABLE is :ANY, :OPERATOR-TRUST (private cognition, or communication to
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

(defvar *publication-recipient* nil
  "Dynamically bound by the checked dispatcher, never by model arguments.")

(defvar *communication-root-p* (lambda (kernel root) (declare (ignore kernel root)) nil)
  "Registered boundary: true only for an assigned draft review root.")
(defvar *communication-egress-guard* (lambda (kernel thought name args) (declare (ignore kernel thought name args)) nil)
  "Recipient/route/artifact guard; fails closed before a publishing tool runs.")

(defvar *communication-answer* (lambda (kernel thought text)
                               (declare (ignore kernel thought text))
                               (error "No communication sender registered"))
  "Registered communication boundary: validate a filter result and publish its assigned candidate.")
(defvar *communication-task* (lambda (kernel thought) (declare (ignore kernel thought)) nil)
  "Current candidate and recipient policy, independent of an older persisted prompt.")
(defvar *communication-input-images* (lambda (kernel thought) (declare (ignore kernel thought)) nil)
  "Attachments supplied with a communication candidate, not private resource permissions.")

(defun peer-audience-p (audience)
  (and (stringp audience) (> (length audience) 5) (string= "peer:" audience :end2 5)))

(defun private-initiative-p (thought)
  "Every stimulus is considered privately; communication is a separate phase."
  (and thought (not (getf thought :communicating))))

(defun thought-reading-audience (thought)
  "Private cognition reads as the pAI; communication uses recipient permissions."
  (if (private-initiative-p thought) "operator" (getf thought :audience)))

(defun tool-available-p (tool thought)
  (and (private-initiative-p thought)
       (not (tool-publishes tool))
       (let ((available (tool-available tool))
             (audience (thought-reading-audience thought)))
         (if (listp available)
             (member audience available :test #'equal)
             (ecase available
               (:any t)
               (:operator-trust (equal "operator" audience))
               (:operator-and-peers (or (equal "operator" audience) (peer-audience-p audience))))))))

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
