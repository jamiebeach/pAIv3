(in-package #:pai-v3.mind)

;;; Beadwork: the work ledger the agent's code is built from. The agent
;;; reaches it through a narrow bridge on the host (tools/beads-bridge.py),
;;; which may read beads and add comments and new beads, and nothing else.
;;; Off until beads.url is set; the bridge's token is the secret named beads,
;;; bound to the bridge's host.

(define-setting "beads.url"
  :type :string :default "" :group "Development"
  :description "Where the Beadwork bridge answers, such as http://host.docker.internal:8765/run. Empty: no beads tool.")

(defvar *beads-post*
  (lambda (url headers body)
    ;; The bridge is on the host, a private address the operator chose.
    (http-request :post url :headers headers :body body :read-timeout 60 :allow-private t))
  "(lambda (url headers body)) -> body, status. Replaced in tests.")

(defun beads (kernel args)
  "Run one bridge action and return its text."
  (let ((url (setting kernel "beads.url")))
    (if (equal url "")
        "Beadwork is not set up: the operator has not given me a bridge (beads.url)."
        (handler-case
            (multiple-value-bind (body status)
                (funcall *beads-post* url
                         (list (cons "Content-Type" "application/json")
                               (cons "Authorization" (list :secret "beads" "Bearer ~a")))
                         (json-encode args))
              (let ((answer (ignore-errors (json-parse body))))
                (cond ((null answer) (format nil "The Beadwork bridge answered ~d." status))
                      ((obj-get answer "error") (format nil "Beadwork refused: ~a" (obj-get answer "error")))
                      ((eq (obj-get answer "ok") :false) (format nil "bw failed: ~a" (obj-get answer "output")))
                      (t (let ((out (obj-get answer "output" "")))
                           (if (equal out "") "Done." out))))))
          (egress-refused (e) (egress-reason e))
          (error (e) (format nil "Could not reach the Beadwork bridge: ~a" e))))))

(define-tool "beads"
  :description "Read and add to the Beadwork ledger my code is built from. action:
ready (work that is unblocked), list (open beads; all: true includes closed; grep narrows),
show (one bead with its comments; give id), history (a bead's changes; give id),
comment (add text to bead id), create (a new bead: title, and optionally description,
parent, type task|bug|feature|chore and priority 0-4), ping (tell the coding agent that
builds me to look at bead id, with an optional short text), patch (propose a change to my
source for bead id: diff is a unified diff against paths as the source tool shows them, text
says why; the coding agent reviews and applies it, I never do).
When I find a problem in myself, I search for an existing bead first, then comment on it or
create one with the evidence, and ping the coding agent with its id. The coding agent answers
on the bead. I cannot close, delete or reopen beads; beads I create are labelled from-agent.
Creates, comments and pings are limited per day, so I raise what matters."
  :summary "Read and add to the Beadwork ledger my code is built from."
  ;; The coding agent's messages are answered on beads, so it may use them;
  ;; that is all it gains.
  :available '("operator" "coding-agent")
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string"
                                                   "enum" (list "ready" "list" "show" "history" "comment" "create" "ping" "patch"))
                                     "id" (obj "type" "string")
                                     "text" (obj "type" "string")
                                     "grep" (obj "type" "string")
                                     "all" (obj "type" "boolean")
                                     "title" (obj "type" "string")
                                     "description" (obj "type" "string")
                                     "parent" (obj "type" "string")
                                     "type" (obj "type" "string")
                                     "priority" (obj "type" "integer")
                                     "diff" (obj "type" "string"))
                   "required" (list "action"))
  :run (lambda (kernel id args) (declare (ignore id)) (beads kernel args)))
