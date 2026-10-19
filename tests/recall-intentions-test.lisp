(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Prospective memory (design note interaction-recall §6): an intention
;;; comes back with what it is about, until it is settled.

(defun remember-to (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "remember_to" *tools*)) k nil (apply #'obj args)))

(deftest an-intention-comes-back-with-who-it-is-about
  (with-temp-kernel (k)
    (let ((nova (form-entity k :kind "person" :name "Nova" :lineage (list (lineage-hop (kernel-agent-id k) "formed")))))
      (let* ((said (remember-to k "action" "add" "about" "Nova" "do" "ask how the portrait came out"))
             (id (subseq said (1+ (search "(" said :from-end t)) (search ")" said :from-end t))))
        (is (search "I will remember" said))
        (is (search "ask how the portrait came out" (render-impression (impression k nova "operator")))
            "it comes back when she comes to mind")
        (is (search "ask how the portrait came out" (format nil "~{~a~}" (getf (impression k nova "peer:nova") :open)))
            "including in a thought she started")
        (is (search id (remember-to k "action" "list")))
        (remember-to k "action" "done" "id" id "note" "asked; she loves it")
        (is (null (getf (impression k nova "operator") :open)) "settled, it no longer comes back")
        (is (search "no open intention" (remember-to k "action" "done" "id" id)) "and cannot be settled twice"))
      (is (search "know no one" (remember-to k "action" "add" "about" "Zed" "do" "say hello")))
      (is (search "use the carry tool" (remember-to k "action" "add" "about" "Zed" "do" "say hello"))
          "an unknown subject is told what to do instead")
      (let ((said (remember-to k "action" "add" "do" "give her camera access" "id" "webcam-access")))
        (is (search "needs `about`" said) "a missing subject says so, not 'nothing called NIL'")
        (is (null (search "NIL" said)))
        (is (search "carry tool" said)))
      (is (search "needs `about`" (remember-to k "action" "add" "about" "  " "do" "say hello")) "nor is a blank one accepted")
      (append! k "fleet.peer.joined" (obj "peer_id" "77aa88bb-99" "name" "Echo" "address" "http://x" "how" "approved"))
      (is (search "I will remember" (remember-to k "action" "add" "about" "77aa88bb-99" "do" "send the map")))
      (is (search "send the map" (format nil "~{~a~}" (getf (impression k "h:77aa88bb-99" "operator") :open)))
          "something the graph does not hold can carry an intention by its identifier"))))

(run-suite "recall-intentions")
