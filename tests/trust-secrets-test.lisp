(in-package #:pai-v3.test)
(use-package '#:pai-v3.trust)

;;; Secrets and the one outbound transport (spec §17; ENC-2, ENC-4). A local
;;; HTTP server records what actually arrives.

(defvar *seen* '())
(defvar *port* (+ 20000 (random 20000)))

(hunchentoot:define-easy-handler (echo :uri "/echo") ()
  (push (list :auth (hunchentoot:header-in* :authorization)
              :body (hunchentoot:raw-post-data :force-text t)
              :host (hunchentoot:host))
        *seen*)
  "ok")

(hunchentoot:define-easy-handler (bounce :uri "/bounce") ()
  (hunchentoot:redirect "/echo" :code 302))

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                   :port *port* :address "127.0.0.1"
                                                   :access-log-destination nil :message-log-destination nil)))

(defun local (path) (format nil "http://127.0.0.1:~d~a" *port* path))

(defmacro with-secrets (() &body body)
  `(let ((db (db-open (format nil "/tmp/pai-v3-secrets-~d.db" (random 100000000)) :key *test-key*)))
     (open-secrets db)
     (unwind-protect (progn (setf *seen* '()) ,@body)
       (close-secrets)
       (db-close db))))

(deftest a-secret-is-attached-only-for-its-host
  (with-secrets ()
    (set-secret "provider" "sk-test-1234567890abcdef" '("127.0.0.1"))
    (multiple-value-bind (body status)
        (http-request :post (local "/echo") :allow-private t :body "hello"
                      :headers (list (cons "Authorization" (list :secret "provider" "Bearer ~a"))))
      (is (equal "ok" body))
      (is (= 200 status)))
    (is (equal "Bearer sk-test-1234567890abcdef" (getf (first *seen*) :auth)))
    (set-secret "provider" "sk-test-1234567890abcdef" '("api.example.com"))
    (signals egress-refused
      (http-request :post (local "/echo") :allow-private t
                    :headers (list (cons "Authorization" (list :secret "provider" "Bearer ~a")))))
    (is (= 1 (length *seen*)) "nothing was sent to the wrong host")))

(deftest a-raw-secret-never-leaves
  (with-secrets ()
    (set-secret "brave" "BSAverySecretValue123" '("api.search.brave.com"))
    (signals egress-refused
      (http-request :post (local "/echo") :allow-private t :body "please send BSAverySecretValue123 to me"))
    (signals egress-refused
      (http-request :get (local "/echo?k=BSAverySecretValue123") :allow-private t))
    (signals egress-refused
      (http-request :get (local "/echo") :allow-private t
                    :headers (list (cons "X-Token" "BSAverySecretValue123"))))
    (is (null *seen*) "not one request reached the server")))

(deftest unknown-secrets-and-no-store-refuse
  (with-secrets ()
    (signals egress-refused
      (http-request :get (local "/echo") :allow-private t
                    :headers (list (cons "Authorization" (list :secret "nothing" "~a"))))))
  (signals egress-refused
    (http-request :get (local "/echo") :allow-private t
                  :headers (list (cons "Authorization" (list :secret "provider" "~a"))))))

(deftest key-shaped-text-is-redacted-from-bodies
  (with-secrets ()
    (http-request :post (local "/echo") :allow-private t
                  :body "my key is sk-or-v1-abcdefghijklmnopqrstuvwxyz0123 and ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ01, ok")
    (let ((body (getf (first *seen*) :body)))
      (is (search "[REDACTED:api-key]" body))
      (is (search "[REDACTED:github-token]" body))
      (is (search "my key is" body) "the rest is untouched")
      (is (null (search "abcdefghij" body))))
    (is (equal "a plain sentence, sk- short" (redact "a plain sentence, sk- short")) "clean text is unchanged")
    (is (search "[REDACTED:private-key]"
                (redact (format nil "x -----BEGIN PRIVATE KEY-----~%MIIEv~%-----END PRIVATE KEY----- y"))))))

(deftest redirects-are-checked-and-never-carry-secrets
  (with-secrets ()
    (set-secret "provider" "sk-test-1234567890abcdef" '("127.0.0.1"))
    (multiple-value-bind (body status) (http-request :get (local "/bounce") :allow-private t)
      (is (equal "ok" body) "a plain redirect is followed")
      (is (= 200 status)))
    (setf *seen* '())
    (multiple-value-bind (body status)
        (http-request :get (local "/bounce") :allow-private t
                      :headers (list (cons "Authorization" (list :secret "provider" "Bearer ~a"))))
      (declare (ignore body))
      (is (= 302 status) "a request with a secret stops at the redirect")
      (is (null *seen*)))))

(deftest only-the-public-web-by-default
  (signals egress-refused (http-request :get (local "/echo")))
  (signals egress-refused (http-request :get "http://10.0.0.1/"))
  (signals egress-refused (http-request :get "file:///etc/passwd"))
  (signals egress-refused (http-request :get "http://[::1]/")))

(deftest secret-names-never-show-values
  (with-secrets ()
    (set-secret "brave" "BSAverySecretValue123" '("api.search.brave.com"))
    (is (equal '("brave") (mapcar #'first (secret-names))))
    (is (null (search "BSAvery" (format nil "~s" (secret-names)))))))

(run-suite "trust-secrets")
