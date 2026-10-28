(in-package #:pai-v3.kernel)

;;; The one outbound transport (spec §17; NFR ENC-2, ENC-4). Every HTTP
;;; request any module makes goes through HTTP-REQUEST; lint rejects other
;;; HTTP clients in src/. It:
;;;
;;; - allows only http and https, with verified TLS;
;;; - refuses hosts that resolve to loopback, private or link-local
;;;   addresses unless the caller passes :allow-private (configured local
;;;   services such as the embedding server, never model-chosen addresses);
;;; - follows redirects itself, re-checking every hop, and never follows one
;;;   on a request that carries a secret;
;;; - hands headers and body to *EGRESS-GUARD* (installed by the trust
;;;   module), which attaches secrets by name for their bound hosts, refuses
;;;   any request carrying a secret value anywhere else, and redacts
;;;   key-shaped text.
;;;
;;; A header value may name a secret instead of holding it:
;;;   ("Authorization" . (:secret "openrouter" "Bearer ~a"))

(define-condition egress-refused (error)
  ((reason :initarg :reason :reader egress-reason)
   ;; A host that could not be looked up may be found a moment later (a DNS
   ;; or network blip): refused for now, not refused by rule (bead v3-efb).
   (transient :initarg :transient :initform nil :reader egress-transient-p))
  (:report (lambda (c s) (write-string (egress-reason c) s))))

(defun %refuse (fmt &rest args) (error 'egress-refused :reason (apply #'format nil fmt args)))

(defvar *egress-guard*
  (lambda (url host headers body)
    (declare (ignore url host))
    (let ((named (find-if (lambda (h) (consp (cdr h))) headers)))
      (when named
        (%refuse "There is no secret named ~a: no secret store is open." (second (cdr named)))))
    (values headers body nil))
  "(lambda (url host headers body)) -> headers, body, secrets-attached-p.
Installed by the trust module; signals EGRESS-REFUSED.")

(defun %private-octets-p (octets)
  (let ((a (aref octets 0)) (b (aref octets 1)))
    (or (= a 0) (= a 10) (= a 127)
        (and (= a 169) (= b 254))
        (and (= a 172) (<= 16 b 31))
        (and (= a 192) (= b 168))
        (and (= a 100) (<= 64 b 127)))))

(defun check-outbound-url (url &key allow-private)
  "Signal EGRESS-REFUSED unless URL may be requested. Returns the host."
  (let* ((uri (handler-case (quri:uri url) (error () (%refuse "~a is not an address." url))))
         (scheme (quri:uri-scheme uri))
         (host (quri:uri-host uri)))
    (unless (member scheme '("http" "https") :test #'equalp)
      (%refuse "Only http and https can be requested, not ~a." (or scheme "that")))
    (unless (plusp (length host)) (%refuse "~a has no host." url))
    (unless allow-private
      (when (find #\: host)
        (%refuse "~a is on a private network; only the public web is reachable." host))
      (let ((addresses (handler-case
                           (sb-bsd-sockets:host-ent-addresses (sb-bsd-sockets:get-host-by-name host))
                         (error () (error 'egress-refused :transient t :reason (format nil "~a could not be found." host))))))
        (when (some #'%private-octets-p addresses)
          (%refuse "~a is on a private network; only the public web is reachable." host))))
    (string-downcase host)))

(defun http-request (method url &key headers body stream (connect-timeout 10) (read-timeout 60)
                                     (max-redirects 5) allow-private)
  "Make an outbound request. Returns (values body status response-headers
final-url); with STREAM, body is a stream the caller closes. Network
failures signal; HTTP error statuses are returned, not signalled."
  (loop repeat (1+ max-redirects)
        do (let ((host (check-outbound-url url :allow-private allow-private)))
             (multiple-value-bind (sent-headers sent-body secret-p)
                 (funcall *egress-guard* url host headers body)
               (multiple-value-bind (out status response-headers)
                   (handler-case
                       (dex:request url :method method :headers sent-headers :content sent-body
                                        :want-stream stream :force-string (not stream)
                                        :max-redirects 0 :insecure nil :keep-alive nil
                                        :connect-timeout connect-timeout :read-timeout read-timeout)
                     (dex:http-request-failed (e)
                       (values (dex:response-body e) (dex:response-status e) (dex:response-headers e))))
                 (let ((location (and (<= 300 status 399) response-headers
                                      (gethash "location" response-headers))))
                   (cond ((or (null location) secret-p)
                          ;; A request carrying a secret never follows a redirect.
                          (return (values out status response-headers url)))
                         (t (when (and stream (streamp out)) (close out))
                            (setf url (quri:render-uri (quri:merge-uris (quri:uri location) (quri:uri url)))
                                  method (if (= status 307) method :get)
                                  body (if (= status 307) body nil))))))))
        finally (%refuse "Too many redirects.")))
