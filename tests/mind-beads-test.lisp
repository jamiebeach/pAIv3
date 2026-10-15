(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; The beads tool: off until the operator gives a bridge, then a thin client
;;; that names its token and turns the bridge's answers into text.

(defvar *posted* '())
(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun bridge (answer &key (status 200))
  (lambda (url headers body)
    (push (list url headers (json-parse body)) *posted*)
    (values (json-encode answer) status)))

(deftest beads-are-off-until-a-bridge-is-set
  (with-temp-kernel (k)
    (let ((*posted* '()) (*beads-post* (bridge (obj "ok" t "output" "x"))))
      (is (search "not set up" (beads k (obj "action" "ready"))))
      (is (null *posted*) "nothing is sent"))))

(deftest beads-go-through-the-bridge
  (with-temp-kernel (k)
    (change-setting k "beads.url" "http://host.docker.internal:8765/run" :origin *operator*)
    (let ((*posted* '()) (*beads-post* (bridge (obj "ok" t "output" "v3-abc  open  Do the thing"))))
      (is (search "Do the thing" (beads k (obj "action" "list" "grep" "thing"))))
      (destructuring-bind (url headers body) (first *posted*)
        (is (equal "http://host.docker.internal:8765/run" url))
        (is (equal '(:secret "beads" "Bearer ~a") (cdr (assoc "Authorization" headers :test #'string=)))
            "the token is named, never held")
        (is (equal "thing" (obj-get body "grep")))))
    (let ((*beads-post* (bridge (obj "error" "action must be ready, list, show, history, comment or create") :status 400)))
      (is (search "Beadwork refused: action must be" (beads k (obj "action" "close" "id" "v3-abc")))))
    (let ((*beads-post* (bridge (obj "ok" :false "output" "no such issue"))))
      (is (search "bw failed: no such issue" (beads k (obj "action" "show" "id" "v3-zzz")))))
    (let ((*beads-post* (lambda (u h b) (declare (ignore u h b)) (error "connection refused"))))
      (is (search "Could not reach the Beadwork bridge" (beads k (obj "action" "ready")))))))

(deftest only-operator-thoughts-hold-beads
  ;; Private thinking runs with the operator as audience too, so it holds beads.
  (let ((tool (gethash "beads" *tools*)))
    (is (member tool (tools-for (list :audience "operator"))))
    (is (not (member tool (tools-for (list :audience "guest:alex")))))))

(deftest a-coding-agent-thought-out-of-calls-answers-on-its-bead
  (with-temp-kernel (k)
    (change-setting k "beads.url" "http://host.docker.internal:8765/run" :origin *operator*)
    (let* ((*posted* '()) (*beads-post* (bridge (obj "ok" t "output" "done")))
           (id (coding-agent-message k "Claude Code" "Please check the merge." :bead "v3-o1z")))
      (pai-v3.mind::coding-agent-wrap-up k (thought-row k id) "The merge is wired; I ran out of calls.")
      (let ((sent (reverse (mapcar #'third *posted*))))
        (is (equal '("comment" "ping") (mapcar (lambda (b) (obj-get b "action")) sent)))
        (is (equal "v3-o1z" (obj-get (first sent) "id")))
        (is (search "The merge is wired" (obj-get (first sent) "text")))))))

(run-suite "mind-beads")
