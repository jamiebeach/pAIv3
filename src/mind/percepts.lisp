(in-package #:pai-v3.mind)

;;; Recent percepts (engineering spec §16.1; bead v3-baw.1.1; design note
;;; docs/architecture/body-interfaces.md). What a receptor perceived lately,
;;; kept in one bounded table that is not the ledger: routine perceiving is
;;; never experience (agent document §13). A percept here is codes and
;;; numbers only. Whatever is said about it is composed on this side, so a
;;; receptor that reports from somewhere untrusted (a page) has no words to
;;; put into a context, a judgment or the reply check (spec §16.5).
;;;
;;; This is the first tenant of the general table: the avatar's outcomes and
;;; views (v3-baw.1.2, 1.6). Sight keeps its own path (api/perception.lisp)
;;; until it moves here.
;;;
;;; Bounds. At most +PERCEPTS-MAX-ROWS+ rows; a row is at most
;;; +PERCEPT-ROW-BYTES+ (six tokens of at most 64 characters, eight numbers);
;;; so the table's budget is 128 KB. A row expires by its receptor's
;;; retention, six hours at most. Pruning happens on write. Nothing ticks.

(defparameter +percepts-max-rows+ 256)
(defparameter +percept-row-bytes+ 512)
(defparameter +percept-retention-seconds+ (* 6 3600))
(defparameter +percept-numbers-max+ 8)
(defparameter +senses+
  '("language" "sight" "hearing" "attunement" "ambient" "time" "interoception" "proprioception")
  "The senses of the agent document §3. A new one is a reviewed code change.")

(define-projection "percepts"
  :kind :inline
  :derivation :scratch
  :consumes ()
  :tables ("CREATE TABLE IF NOT EXISTS percepts_recent (id INTEGER PRIMARY KEY AUTOINCREMENT, sense TEXT NOT NULL, receptor TEXT NOT NULL, kind TEXT NOT NULL, code TEXT NOT NULL, subject TEXT NOT NULL, salience REAL NOT NULL, repeats INTEGER NOT NULL, numbers TEXT NOT NULL, at TEXT NOT NULL, expires_at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS percepts_recent_same_idx ON percepts_recent(receptor,kind,code,at)"
           "CREATE INDEX IF NOT EXISTS percepts_recent_receptor_idx ON percepts_recent(receptor,id)"
           "CREATE INDEX IF NOT EXISTS percepts_recent_sense_idx ON percepts_recent(sense,id)"
           "CREATE INDEX IF NOT EXISTS percepts_recent_expiry_idx ON percepts_recent(expires_at)")
  :reduce (lambda (db e) (declare (ignore db e))))

;;; Receptors: who may emit, and exactly what

(defstruct receptor name sense kinds retention habituation)

(defvar *receptors* (make-hash-table :test 'equal))

(defun percept-token-p (x &optional (most 64))
  "True of a short machine token: lower-case letters, digits and . _ : -,
starting with a letter or digit. Nothing a sentence could be written in."
  (and (stringp x) (<= 1 (length x) most)
       (alphanumericp (char x 0))
       (every (lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9) (find c "._:-"))) x)))

(defmacro define-receptor (name &key (sense "proprioception") kinds
                                  (retention-seconds '+percept-retention-seconds+)
                                  (habituation-seconds 900))
  "Declare receptor NAME (a token) on SENSE. KINDS is a list of (kind code...):
the closed set of things it may report. Anything else it emits is dropped.
RETENTION-SECONDS is the longest one of its percepts is kept (six hours at
most); HABITUATION-SECONDS is how long a repeat of the same kind and code
counts against its salience. Redefining NAME replaces it."
  `(register-receptor ,name ,sense ',kinds ,retention-seconds ,habituation-seconds))

(defun register-receptor (name sense kinds retention habituation)
  (unless (percept-token-p name) (error "Receptor name ~s is not a token" name))
  (unless (member sense +senses+ :test #'equal) (error "~s is not a sense (agent document §3)" sense))
  (dolist (k kinds)
    (unless (and (consp k) (cdr k) (every #'percept-token-p k))
      (error "Receptor ~a: ~s is not (kind code...) in tokens" name k)))
  (setf (gethash name *receptors*)
        (make-receptor :name name :sense sense :kinds kinds
                       :retention (max 1 (min retention +percept-retention-seconds+))
                       :habituation (max 0 habituation)))
  name)

;;; Emitting

(defun %percept-numbers (numbers)
  "NUMBERS (an object of token -> real, or nil) as JSON text, or :REFUSED."
  (cond ((null numbers) "{}")
        ((not (hash-table-p numbers)) :refused)
        ((> (hash-table-count numbers) +percept-numbers-max+) :refused)
        (t (let ((out (obj)))
             (loop for k being the hash-keys of numbers using (hash-value v)
                   do (unless (and (percept-token-p k 32) (realp v) (< (abs v) 1d15))
                        (return-from %percept-numbers :refused))
                      (setf (gethash k out) (if (integerp v) v (coerce v 'double-float))))
             (json-encode out)))))

(defun emit-percept (kernel receptor kind code &key (subject "-") (salience 0.5d0) expires-in numbers)
  "Record that RECEPTOR perceived KIND with CODE about SUBJECT (a token, such
as the id of the act it is the outcome of; "-" when it is about nothing in
particular). SALIENCE is 0 to 1 before
habituation; EXPIRES-IN is seconds, at most the receptor's retention; NUMBERS
is an object of at most eight token -> number. Returns the percept's id, or
NIL when it was dropped: an undeclared receptor, kind or code, or anything
that is not a token or a number. Dropping is silent on purpose: what a view
reports is untrusted, and a refusal must not become a channel either.
Never a ledger event."
  (let* ((r (gethash receptor *receptors*))
         (codes (and r (cdr (assoc kind (receptor-kinds r) :test #'equal))))
         (numbers (%percept-numbers numbers)))
    (when (and r (member code codes :test #'equal)
               (or (equal subject "-") (percept-token-p subject)) (realp salience) (not (eq numbers :refused))
               (or (null expires-in) (realp expires-in)))
      (let* ((db (kernel-db kernel)) (at (now))
             (life (max 1 (min (or expires-in (receptor-retention r)) (receptor-retention r)))))
        (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
          (with-transaction (db)
            (sql-exec db "DELETE FROM percepts_recent WHERE expires_at<=?1" (iso8601 at))
            (let ((repeats (or (sql-value db "SELECT count(*) FROM percepts_recent WHERE receptor=?1 AND kind=?2 AND code=?3 AND at>?4 LIMIT 1"
                                          receptor kind code (iso8601 (- at (receptor-habituation r))))
                               0)))
              (sql-exec db "INSERT INTO percepts_recent(sense,receptor,kind,code,subject,salience,repeats,numbers,at,expires_at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
                        (receptor-sense r) receptor kind code subject
                        (coerce (max 0 (min 1 salience)) 'double-float) repeats numbers
                        (iso8601 at) (iso8601 (+ at (ceiling life)))))
            (let ((id (sql-value db "SELECT last_insert_rowid()")))
              (sql-exec db "DELETE FROM percepts_recent WHERE id<=?1" (- id +percepts-max-rows+))
              id)))))))

;;; Reading

(defun habituated-salience (salience repeats)
  "SALIENCE after REPEATS of the same kind and code lately: the curve noticing
change already uses for surprise (api/orienting.lisp)."
  (/ salience (1+ (* 0.5d0 repeats))))

(defun recent-percepts (kernel &key sense receptor kind code subject (min-salience 0) (limit 20))
  "Unexpired percepts of one RECEPTOR, or of one SENSE (proprioception when
neither is named), newest first, at most LIMIT (64 at most), narrowed by any
of KIND, CODE, SUBJECT, and to those whose habituated salience is at least
MIN-SALIENCE. Each is a plist: :id :sense :receptor
:kind :code :subject :salience (habituated) :raw :repeats :numbers (an
object) :at (universal time) :age (seconds)."
  (let ((at (now)))
    (loop for (id sn rc kd cd sb raw repeats numbers when) in
             (if receptor
                 (sql-rows (kernel-db kernel)
                           "SELECT id,sense,receptor,kind,code,subject,salience,repeats,numbers,at FROM percepts_recent WHERE receptor=?1 AND expires_at>?2 AND (?3 IS NULL OR sense=?3) AND (?4 IS NULL OR kind=?4) AND (?5 IS NULL OR code=?5) AND (?6 IS NULL OR subject=?6) ORDER BY id DESC LIMIT ?7"
                           receptor (iso8601 at) sense kind code subject +percepts-max-rows+)
                 (sql-rows (kernel-db kernel)
                           "SELECT id,sense,receptor,kind,code,subject,salience,repeats,numbers,at FROM percepts_recent WHERE sense=?1 AND expires_at>?2 AND (?3 IS NULL OR kind=?3) AND (?4 IS NULL OR code=?4) AND (?5 IS NULL OR subject=?5) ORDER BY id DESC LIMIT ?6"
                           (or sense "proprioception") (iso8601 at) kind code subject +percepts-max-rows+))
          for salience = (habituated-salience raw repeats)
          for time = (parse-iso8601 when)
          when (>= salience min-salience)
            collect (list :id id :sense sn :receptor rc :kind kd :code cd :subject sb
                          :salience salience :raw raw :repeats repeats
                          :numbers (or (ignore-errors (json-parse numbers)) (obj))
                          :at time :age (max 0 (- at time)))
            into found
          when (>= (length found) (max 1 (min limit 64))) do (loop-finish)
          finally (return found))))
