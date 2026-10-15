(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Settings in the store (bead v3-fu8.15).

(define-setting "test.limit" :type :integer :min 1 :max 10 :default 3 :group "Test"
  :description "A test limit.")
(define-setting "test.tunable" :type :number :min 0 :max 1 :default 0.5 :group "Test"
  :changed-by (:agent :min 0.2 :max 0.8) :description "The agent may tune this.")

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defparameter *self* (obj "principal_type" "self"))
(defparameter *guest* (obj "principal_type" "person"))

(deftest defaults-then-changes-apply-at-once
  (with-temp-kernel (k)
    (is (= 3 (setting k "test.limit")) "a fresh agent works on code defaults")
    (change-setting k "test.limit" 7 :origin *operator* :reason "more room")
    (is (= 7 (setting k "test.limit")) "no restart needed")
    (let ((row (find "test.limit" (settings-list k) :key (lambda (s) (obj-get s "name")) :test #'equal)))
      (is (eq t (obj-get row "changed")))
      (is (= 3 (obj-get row "default"))))
    (signals setting-refused (change-setting k "test.limit" 11 :origin *operator*))
    (signals setting-refused (change-setting k "test.limit" "five" :origin *operator*))
    (signals setting-refused (change-setting k "no.such" 1 :origin *operator*))))

(deftest who-may-change-what
  (with-temp-kernel (k)
    (signals setting-refused (change-setting k "test.limit" 5 :origin *self*))
    (signals setting-refused (change-setting k "test.limit" 5 :origin *guest*))
    (signals setting-refused (change-setting k "test.tunable" 0.9 :origin *self*) )
    (change-setting k "test.tunable" 0.3 :origin *self* :reason "fewer false alarms")
    (is (= 0.3 (setting k "test.tunable")))
    (change-setting k "test.tunable" 0.95 :origin *operator*)
    (is (= 0.95 (setting k "test.tunable")) "the operator is not bound by the agent's bounds")))

(deftest changes-are-audited-events
  (with-temp-kernel (k)
    (change-setting k "test.limit" 4 :origin *operator* :reason "because")
    (let ((e (event-by-id k (ledger-head k))))
      (is (equal "setting.changed" (event-type e)))
      (is (= 3 (obj-get (event-payload e) "previous")))
      (is (equal "because" (obj-get (event-payload e) "reason"))))))

(deftest export-and-import-round-trip
  (with-temp-kernel (k)
    (let ((file (export-settings k)))
      (setf (gethash "test.limit" file) 9)
      (is (equal '(("test.limit" 3 9)) (import-settings k file :origin *operator* :dry-run t)))
      (is (= 3 (setting k "test.limit")) "a dry run changes nothing")
      (import-settings k file :origin *operator*)
      (is (= 9 (setting k "test.limit")))
      (setf (gethash "test.limit" file) 2 (gethash "test.tunable" file) 7)
      (signals setting-refused (import-settings k file :origin *operator*))
      (is (= 9 (setting k "test.limit")) "nothing is applied unless every entry is valid"))))

(deftest models-are-chosen-by-route
  (with-temp-kernel (k)
    (let ((*thought-provider* nil))
      (signals provider-failure (profile-for k "chat"))
      (change-setting k "llm.profiles"
                      (list (obj "name" "main" "base_url" "https://openrouter.ai/api/v1" "model" "big" "secret" "openrouter")
                            (obj "name" "calm" "base_url" "https://api.nous.example/v1" "model" "steady")
                            (obj "name" "local" "base_url" "http://127.0.0.1:8000/v1" "model" "small" "local" t))
                      :origin *operator*)
      (change-setting k "llm.routes"
                      (obj "chat" (list (obj "profile" "calm" "when" (obj "arousal" (obj ">" 0.7)))
                                        (obj "profile" "main"))
                           "thinking" (list (obj "profile" "local"))
                           "default" (list (obj "profile" "main")))
                      :origin *operator*)
      (is (equal "big" (pai-v3.mind::provider-profile-model (profile-for k "chat"))))
      (is (equal "steady" (pai-v3.mind::provider-profile-model (profile-for k "chat" (obj "arousal" 0.9))))
          "routing follows affect")
      (is (equal "big" (pai-v3.mind::provider-profile-model (profile-for k "chat" (obj "arousal" 0.2)))))
      (is (pai-v3.mind::provider-profile-local (profile-for k "thinking")))
      (is (equal "big" (pai-v3.mind::provider-profile-model (profile-for k "summary"))) "unrouted purposes use default")
      (is (equal "openrouter" (pai-v3.mind::provider-profile-secret (profile-for k "chat"))))
      (signals setting-refused (change-setting k "llm.profiles" (list (obj "name" "x")) :origin *operator*)))))

(run-suite "kernel-settings")
