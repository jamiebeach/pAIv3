(load "/v3/tests/support-webauthn.lisp")
(in-package #:pai-v3.test)

(defun hexo (s) (ironclad:hex-string-to-byte-array s))

(deftest es256-matches-rfc6979-known-answer
  ;; RFC 6979 A.2.5: P-256, SHA-256, message "sample".
  (let ((point (hexo (concatenate 'string "04"
                                  "60FED4BA255A9D31C961EB74C6356D68C049B8923B61FA6CE669622E60F29FB6"
                                  "7903FE1008B8BC99A41AE9E95628BC64F2F1B20C2D7E9F5177A3C294D4462299")))
        (sig (hexo (concatenate 'string
                                "EFD48B2AACB6A8FD1140DD9CD45E81D69D2C877B56AAF991C34D0EA84EAF3716"
                                "F7CB1C942D657C41D436C7A1B6E29F65F3E900DBB9AFF4064DC4AB2F843ACDA8"))))
    (is (es256-verify point (sha (utf8 "sample")) sig))
    (is (not (es256-verify point (sha (utf8 "samplf")) sig)))
    (is (equalp sig (der-to-raw-signature (raw-to-der sig))) "DER round trip")))

(deftest base64url-and-cbor-roundtrip
  (dolist (n '(0 1 2 3 4 31 32 33))
    (let ((o (ironclad:random-data n)))
      (is (equalp o (base64url-decode (base64url-encode o))))))
  (is (equal '(("a" . 1) (-2 . -7))
             (cbor-decode (cbor-map (cbor-text "a") (cbor-int 1) (cbor-int -2) (cbor-int -7))))))

(deftest enroll-login-and-attest-origin
  (with-clock
    (with-temp-kernel (k)
      (let* ((a (make-software-authenticator))
             (cred (register k a))
             (token (apply #'login k (assertion a :login)))
             (origin (session-origin k token)))
        (is (equal cred (base64url-encode (authn-cred-id a))))
        (is (equal "operator" (obj-get origin "principal_type")))
        (is (equal cred (obj-get origin "credential_id")))
        (is (null (session-origin k "made-up-token")) "unknown tokens carry no authority")
        (is (null (sql-value (kernel-db k)
                             "SELECT 1 FROM pai_events WHERE agent_id=?1 AND instr(event_json,?2)>0 LIMIT 1"
                             (kernel-agent-id k) token))
            "the token itself is never stored")))))

(deftest rejections
  (with-clock
    (with-temp-kernel (k)
      (let ((a (make-software-authenticator))
            (stranger (make-software-authenticator)))
        (register k a)
        (signals webauthn-error (register k stranger))
        (signals webauthn-error (apply #'login k (assertion a :login :tamper t)))
        (signals webauthn-error (apply #'login k (assertion a :login :flags #x01)))
        (let ((wrong (assertion a :login)))
          (setf (getf wrong :origin) "https://evil.example")
          (signals webauthn-error (apply #'login k wrong)))
        (let ((replay (assertion a :login)))
          (apply #'login k replay)
          (signals webauthn-error (apply #'login k replay)))
        (signals webauthn-error (apply #'login k (assertion stranger :login)))))))

(deftest sessions-expire-and-revocation-ends-them
  (with-clock
    (with-temp-kernel (k)
      (let* ((a (make-software-authenticator))
             (cred (register k a))
             (token (apply #'login k (assertion a :login))))
        (advance (* 29 86400))
        (multiple-value-bind (origin rotated) (session-origin k token)
          (is origin "within idle window")
          (is (and rotated (string/= rotated token)) "a session in use is rotated")
          (is (session-origin k token) "the old token still works for a moment")
          (advance 120)
          (is (null (session-origin k token)) "and then no longer")
          (is (null (nth-value 1 (session-origin k rotated))) "a fresh token is not rotated again")
          (setf token rotated))
        (advance (* 29 86400))
        (is (session-origin k token) "activity refreshed the idle window")
        (advance (* 31 86400))
        (is (null (session-origin k token)) "idle expiry")
        (let ((t2 (apply #'login k (assertion a :login))))
          (revoke-device k cred "lost phone")
          (is (null (session-origin k t2)) "revoking the device ends its sessions"))))))

(deftest recovery-codes-enrol-a-new-device-once
  (with-clock
    (with-temp-kernel (k)
      (let* ((phone (make-software-authenticator))
             (spare (make-software-authenticator))
             (cred (register k phone))
             (token (apply #'login k (assertion phone :login)))
             (codes (issue-recovery-codes k token)))
        (is (= 10 (length codes)))
        (is (= 10 (recovery-codes-left k)))
        (is (null (sql-value (kernel-db k) "SELECT 1 FROM pai_events WHERE agent_id=?1 AND instr(event_json,?2)>0 LIMIT 1"
                             (kernel-agent-id k) (first codes)))
            "codes are stored only as hashes")
        (revoke-device k cred "phone lost")
        (let ((grant (redeem-recovery-code k (string-downcase (first codes)))))
          (signals webauthn-error (redeem-recovery-code k (first codes)))
          (register k spare :authorized-by (list :recovery grant) :name "new phone")
          (is (apply #'login k (assertion spare :login)) "the new device signs in")
          (signals webauthn-error (register k (make-software-authenticator) :authorized-by (list :recovery grant)))
          (is (= 9 (recovery-codes-left k))))
        (let ((late (redeem-recovery-code k (second codes))))
          (advance 900)
          (signals webauthn-error (register k (make-software-authenticator) :authorized-by (list :recovery late)))
          "an unused grant expires")))))

(deftest recovery-is-rate-limited-and-needs-a-fresh-verification
  (with-clock
    (with-temp-kernel (k)
      (let* ((phone (make-software-authenticator))
             (cred (register k phone))
             (token (apply #'login k (assertion phone :login))))
        (declare (ignore cred))
        (advance 3600)
        (signals webauthn-error (issue-recovery-codes k token))
        (apply #'step-up k token (assertion phone :step-up))
        (let ((codes (issue-recovery-codes k token)))
          (dotimes (i 5) (signals webauthn-error (redeem-recovery-code k "AAAA-BBBB-CCCC")))
          (signals webauthn-error (redeem-recovery-code k (first codes)))
          (advance 3600)
          (is (redeem-recovery-code k (first codes)) "the limit lifts next hour")
          (let ((newer (issue-recovery-codes k (progn (apply #'step-up k token (assertion phone :step-up)) token))))
            (signals webauthn-error (redeem-recovery-code k (second codes)))
            (is (redeem-recovery-code k (first newer)) "a new set replaces the old one")))))))

(deftest step-up-gates-sensitive-actions
  (with-clock
    (with-temp-kernel (k)
      (let* ((a (make-software-authenticator))
             (b (make-software-authenticator))
             (token (progn (register k a) (apply #'login k (assertion a :login)))))
        (advance 600)
        (is (not (stepped-up-p k token)) "login ten minutes ago is not a step-up")
        (signals webauthn-error (register k b :authorized-by token))
        (apply #'step-up k token (assertion a :step-up))
        (is (stepped-up-p k token))
        (register k b :authorized-by token :name "laptop")
        (is (= 2 (length (active-devices k))))))))

(run-suite "trust-auth")
