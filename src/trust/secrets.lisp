(in-package #:pai-v3.trust)

;;; Secrets (spec §17; NFR ENC-4). Provider keys, tokens and peer keys live
;;; in the agent's encrypted secrets store, never in the ledger, derived
;;; stores, config, environment or model context. Each secret is bound to
;;; the hosts it may be sent to. Code asks for a secret by name in a request
;;; header; the egress guard fills it in only for a bound host, refuses any
;;; request in which a secret's value appears raw, and redacts key-shaped
;;; text from bodies (v2 pai-o6z).
;;;
;;; This store is in the agent's own process (operator decision 2026-10-12):
;;; code running in the image could still read a value from memory. The
;;; outbound checks close the realistic leak path, prompt injection asking
;;; the agent to send a key somewhere.

(defvar *secrets-db* nil "The open secrets store, or NIL.")

;;; Changes are audited in the ledger by name and hosts, never value (AUTH-9).
(define-event-type "secret.set"
  :schema (("name" :string) ("hosts" :array) ("origin" :object)))
(define-event-type "secret.removed"
  :schema (("name" :string) ("origin" :object)))
(defvar *secret-cache* '() "((name value hosts) ...) for the outbound scan.")
(defvar *secrets-lock* (bt:make-lock "secrets"))
(defvar *redactions* (make-hash-table :test 'equal) "Content-free counters by kind.")

(defparameter +secrets-ddl+
  "CREATE TABLE IF NOT EXISTS secrets (name TEXT PRIMARY KEY, value BLOB NOT NULL, hosts TEXT NOT NULL, updated_at TEXT NOT NULL)")

(defun %load-secrets ()
  (setf *secret-cache*
        (and *secrets-db*
             (mapcar (lambda (row)
                       (list (first row) (babel:octets-to-string (second row) :encoding :utf-8)
                             (%split-hosts (third row))))
                     (sql-rows *secrets-db* "SELECT name,value,hosts FROM secrets WHERE name>?1 ORDER BY name LIMIT 1000" "")))))

(defun %split-hosts (text)
  (loop with start = 0
        for end = (position #\, text :start start)
        for host = (string-trim " " (subseq text start (or end (length text))))
        when (plusp (length host)) collect (string-downcase host)
        while end do (setf start (1+ end))))

(defun open-secrets (db)
  "Install DB (the agent's opened secrets store) as the secret store."
  (sql-exec db +secrets-ddl+)
  (bt:with-lock-held (*secrets-lock*)
    (setf *secrets-db* db)
    (%load-secrets))
  (setf *egress-guard* #'egress-guard)
  db)

(defun close-secrets ()
  (bt:with-lock-held (*secrets-lock*)
    (setf *secrets-db* nil *secret-cache* '())))

(defun set-secret (name value hosts)
  "Store VALUE as secret NAME, sendable only to HOSTS (a list; \"*.x.com\"
matches subdomains). The caller records the audited event; this function
never sees the ledger."
  (unless *secrets-db* (error 'keyring-error :message "The secret store is not open."))
  (when (< (length value) 8) (error 'keyring-error :message "That is too short to be a secret."))
  (when (null hosts) (error 'keyring-error :message "Say which hosts this secret may be sent to."))
  (bt:with-lock-held (*secrets-lock*)
    (sql-exec *secrets-db*
              "INSERT INTO secrets(name,value,hosts,updated_at) VALUES(?1,?2,?3,?4) ON CONFLICT(name) DO UPDATE SET value=excluded.value,hosts=excluded.hosts,updated_at=excluded.updated_at"
              name (babel:string-to-octets value :encoding :utf-8)
              (format nil "~{~a~^,~}" (mapcar #'string-downcase hosts)) (now-iso8601))
    (%load-secrets))
  name)

(defun remove-secret (name)
  (when *secrets-db*
    (bt:with-lock-held (*secrets-lock*)
      (sql-exec *secrets-db* "DELETE FROM secrets WHERE name=?1" name)
      (%load-secrets)))
  name)

(defun secret-hmac (name message)
  "Hex HMAC-SHA256 of MESSAGE (a string) keyed by secret NAME, whose value is
a hex key: how fleet requests are signed and checked (v2's protocol), without
the key leaving this module. NIL when there is no such secret."
  (let ((entry (find name (bt:with-lock-held (*secrets-lock*) *secret-cache*) :key #'first :test #'string=)))
    (when entry
      (let ((hmac (ironclad:make-hmac (ironclad:hex-string-to-byte-array (second entry)) :sha256)))
        (ironclad:update-hmac hmac (babel:string-to-octets message :encoding :utf-8))
        (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))))

(defun secret-hmac-valid-p (name message signature-hex)
  "True when SIGNATURE-HEX is MESSAGE's HMAC under secret NAME, compared in
constant time. A malformed signature is simply not valid."
  (let ((expected (secret-hmac name message)))
    (and expected (stringp signature-hex)
         (handler-case (ironclad:constant-time-equal (ironclad:hex-string-to-byte-array expected)
                                                     (ironclad:hex-string-to-byte-array signature-hex))
           (error () nil)))))

(defun secret-names ()
  "Secrets as (name hosts updated-at); never values."
  (and *secrets-db*
       (mapcar (lambda (r) (list (first r) (%split-hosts (second r)) (third r)))
               (sql-rows *secrets-db* "SELECT name,hosts,updated_at FROM secrets WHERE name>?1 ORDER BY name LIMIT 1000" ""))))

;;; The guard

(defun %host-bound-p (host hosts)
  (some (lambda (pattern)
          (if (and (> (length pattern) 2) (string= "*." pattern :end2 2))
              (let ((suffix (subseq pattern 1)))
                (and (> (length host) (length suffix))
                     (string= suffix host :start2 (- (length host) (length suffix)))))
              (string= pattern host)))
        hosts))

(defun %body-string (body)
  (etypecase body
    (null "")
    (string body)
    ((vector (unsigned-byte 8)) (babel:octets-to-string body :encoding :utf-8 :errorp nil))))

;;; Pattern redaction: deterministic scanners for key-shaped tokens.

(defun %token-char-p (c) (or (alphanumericp c) (member c '(#\- #\_))))

(defparameter +key-prefixes+
  '(("sk-" . "api-key") ("ghp_" . "github-token") ("gho_" . "github-token") ("github_pat_" . "github-token")
    ("xoxb-" . "slack-token") ("xoxp-" . "slack-token") ("AKIA" . "aws-key") ("AIza" . "google-key")
    ("eyJ" . "jwt"))
  "Prefixes of key-shaped tokens; a match needs 20 more token characters.")

(defun redact (text)
  "TEXT with key-shaped tokens and private-key blocks replaced by
[REDACTED:kind]. Only the matched text changes."
  (let ((out (make-string-output-stream)) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((block-start (and (char= (char text i) #\-) (search "-----BEGIN " text :start2 i :end2 (min n (+ i 11))))))
               (cond
                 ((and block-start (= block-start i)
                       (search "PRIVATE KEY-----" text :start2 i :end2 (min n (+ i 80))))
                  (let ((end (search "-----END " text :start2 i)))
                    (write-string "[REDACTED:private-key]" out)
                    (incf (gethash "private-key" *redactions* 0))
                    (setf i (if end (let ((close (search "-----" text :start2 (+ end 9)))) (if close (+ close 5) n)) n))))
                 ((or (zerop i) (not (%token-char-p (char text (1- i)))))
                  (let* ((end (or (position-if-not (lambda (c) (or (%token-char-p c) (char= c #\.))) text :start i) n))
                         (token (subseq text i end))
                         (kind (cdr (find-if (lambda (p) (and (> (length token) (+ 20 (length (car p))))
                                                              (string= (car p) token :end2 (length (car p)))))
                                             +key-prefixes+))))
                    (if (and kind (or (string/= kind "jwt") (= 2 (count #\. token))))
                        (progn (format out "[REDACTED:~a]" kind)
                               (incf (gethash kind *redactions* 0)))
                        (write-string text out :start i :end (max (1+ i) end)))
                    (setf i (max (1+ i) end))))
                 (t (write-char (char text i) out) (incf i)))))
    (get-output-stream-string out)))

(defun egress-guard (url host headers body)
  "Attach named secrets bound to HOST; refuse raw secret values anywhere;
redact key-shaped text from the body."
  (let ((cache (bt:with-lock-held (*secrets-lock*) *secret-cache*))
        (attached nil))
    (flet ((raw-check (text where)
             (dolist (entry cache)
               (when (and (>= (length (second entry)) 8) (search (second entry) text))
                 (%refuse "The request ~a carries the secret ~a, which is never sent that way." where (first entry))))))
      (let ((out-headers
              (loop for (name . value) in headers
                    collect (cons name
                                  (if (and (consp value) (eq :secret (first value)))
                                      (let ((entry (find (second value) cache :key #'first :test #'string=)))
                                        (unless entry
                                          (%refuse "There is no secret named ~a." (second value)))
                                        (unless (%host-bound-p host (third entry))
                                          (%refuse "The secret ~a is not for ~a." (second value) host))
                                        (setf attached t)
                                        (format nil (or (third value) "~a") (second entry)))
                                      (progn (raw-check (princ-to-string value) (format nil "header ~a" name))
                                             value))))))
        (raw-check url "address")
        (let ((text (%body-string body)))
          (raw-check text "body")
          (values out-headers
                  (if (and (stringp body) (plusp (length body))) (redact body) body)
                  attached))))))

(defun %refuse (fmt &rest args)
  (error 'egress-refused :reason (apply #'format nil fmt args)))
