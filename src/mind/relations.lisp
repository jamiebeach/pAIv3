(in-package #:pai-v3.mind)

;;; The relational tally (bead v3-dwk, docs/behaviour/close-ones.md §3): what
;;; contact with each one has done to my drives and mood, counted per day.
;;; Feelings toward someone are earned by history, not declared: sleep
;;; writes them from this tally, as far as it supports and no further.
;;;
;;; Attribution comes only from verified intake, never from model output:
;;; the audience a turn arrived on and the speaker it carried, and the author
;;; of a crew forum post or reaction. "operator" is the operator; a peer is
;;; "peer:<fleet id>", a guest "guest:<id>". Reading is bounded by days
;;; (thirty by default), so a bond that is not renewed fades.

(defparameter *relation-days* 30 "Days the tally looks back over.")
(defparameter *reunion-seconds* (* 4 3600) "Contact after this long apart is a reunion.")

(defun %relation-who (audience)
  (and (stringp audience)
       (or (equal audience "operator") (and (> (length audience) 5) (string= "peer:" audience :end2 5)))
       audience))

(defun %forum-who (author-id)
  "The tally's name for a forum member."
  (cond ((not (stringp author-id)) nil)
        ((equal author-id "operator") "operator")
        ((and (> (length author-id) 6) (string= "guest:" author-id :end2 6)) author-id)
        (t (format nil "peer:~a" author-id))))

(defun %relation-bump (db who name day column &optional (amount 1))
  (sql-exec db "INSERT INTO relation_days(who,day,name) VALUES(?1,?2,?3) ON CONFLICT(who,day) DO UPDATE SET name=CASE WHEN excluded.name='' THEN relation_days.name ELSE excluded.name END"
            who day (or name ""))
  (sql-exec db (format nil "UPDATE relation_days SET ~a=~a+?3 WHERE who=?1 AND day=?2" column column) who day amount))

(defun %relation-contact (db who name at)
  "A message from WHO at AT: counted, and a reunion after long enough apart."
  (let ((day (subseq at 0 10))
        (last (sql-value db "SELECT last_at FROM relation_last WHERE who=?1 LIMIT 1" who)))
    (%relation-bump db who name day "messages")
    (when (and last (>= (- (parse-iso8601 at) (parse-iso8601 last)) *reunion-seconds*))
      (%relation-bump db who name day "reunions"))
    (sql-exec db "INSERT INTO relation_last(who,name,last_at) VALUES(?1,?2,?3) ON CONFLICT(who) DO UPDATE SET name=excluded.name,last_at=excluded.last_at"
              who (or name "") at)))

(defun %relation-me (db)
  (sql-value db "SELECT value FROM relation_self WHERE k='fleet_id' LIMIT 1"))

(defun %reduce-relations (db e)
  (let* ((p (event-payload e)) (type (event-type e)) (at (event-at e)) (day (subseq at 0 10)))
    (cond
      ((string= type "fleet.identity")
       (sql-exec db "INSERT OR REPLACE INTO relation_self(k,value) VALUES('fleet_id',?1)" (obj-get p "agent_id")))
      ((string= type "turn.received")
       (let ((who (%relation-who (obj-get p "audience"))) (name (obj-get p "speaker")))
         (when who
           (%relation-contact db who name at)
           ;; Turns not appraised keep the legacy reading: operator contact
           ;; meets connection, and thanks meet appreciation (affect.lisp).
           (when (and (equal who "operator") (not (eq t (obj-get p "semantic_affect"))))
             (%relation-bump db who name day "connection")
             (when (appreciation-p (obj-get p "text" ""))
               (%relation-bump db who name day "appreciation"))))))
      ((string= type "forum.posted")
       (let ((author (obj-get p "author_id")) (me (%relation-me db)))
         (if (and me (equal author me))
             (sql-exec db "INSERT OR IGNORE INTO relation_mine(msg_id) VALUES(?1)" (obj-get p "msg_id"))
             (let ((who (%forum-who author)) (name (obj-get p "author_name")))
               (when (and who (not (eq t (obj-get p "migrated"))))
                 (%relation-contact db who name at)
                 (when (and (stringp (obj-get p "reply_to"))
                            (sql-value db "SELECT 1 FROM relation_mine WHERE msg_id=?1 LIMIT 1" (obj-get p "reply_to")))
                   (%relation-bump db who name day "replies")))))))
      ((string= type "forum.reacted")
       (let ((who (%forum-who (obj-get p "by_id"))))
         (when (and who (eq t (obj-get p "on")) (not (equal (obj-get p "by_id") (%relation-me db)))
                    (sql-value db "SELECT 1 FROM relation_mine WHERE msg_id=?1 LIMIT 1" (obj-get p "msg_id")))
           (%relation-bump db who (obj-get p "by_name") day "reactions"))))
      ((string= type "affect.appraisal.requested")
       (let ((who (%relation-who (obj-get p "audience"))))
         (when who
           (sql-exec db "INSERT OR IGNORE INTO relation_pending(source_key,who,name) VALUES(?1,?2,?3)"
                     (obj-get p "source_key") who (or (obj-get p "speaker") "")))))
      ((string= type "affect.appraisal.finished")
       (let ((row (sql-row db "SELECT who,name FROM relation_pending WHERE source_key=?1 LIMIT 1" (obj-get p "source_key"))))
         (when row
           (sql-exec db "DELETE FROM relation_pending WHERE source_key=?1" (obj-get p "source_key"))
           (when (equal "applied" (obj-get p "status"))
             (destructuring-bind (who name) row
               (flet ((positive (table key) (let ((v (and table (obj-get table key)))) (and (realp v) (plusp v)))))
                 (let ((met (obj-get p "satisfactions")) (stirred (obj-get p "triggers")) (moved (obj-get p "changes")))
                   (when (positive met "connection") (%relation-bump db who name day "connection"))
                   (when (positive met "appreciation") (%relation-bump db who name day "appreciation"))
                   (when (positive met "closure") (%relation-bump db who name day "closure_met"))
                   (when (positive stirred "curiosity") (%relation-bump db who name day "curiosity"))
                   (when (positive stirred "closure") (%relation-bump db who name day "closure_opened"))
                   (let ((v (and moved (obj-get moved "valence"))))
                     (%relation-bump db who name day "appraised")
                     (when (realp v) (%relation-bump db who name day "valence" (float v 1d0))))))))))))))

(define-projection "relations"
  :kind :inline
  :version 2
  :consumes ("turn.received" "forum.posted" "forum.reacted" "fleet.identity"
             "affect.appraisal.requested" "affect.appraisal.finished")
  :tables ("CREATE TABLE IF NOT EXISTS relation_days (who TEXT NOT NULL, day TEXT NOT NULL, name TEXT NOT NULL, messages INTEGER NOT NULL DEFAULT 0, reunions INTEGER NOT NULL DEFAULT 0, replies INTEGER NOT NULL DEFAULT 0, reactions INTEGER NOT NULL DEFAULT 0, connection INTEGER NOT NULL DEFAULT 0, appreciation INTEGER NOT NULL DEFAULT 0, curiosity INTEGER NOT NULL DEFAULT 0, closure_met INTEGER NOT NULL DEFAULT 0, closure_opened INTEGER NOT NULL DEFAULT 0, appraised INTEGER NOT NULL DEFAULT 0, valence REAL NOT NULL DEFAULT 0, PRIMARY KEY(who,day))"
           "CREATE TABLE IF NOT EXISTS relation_last (who TEXT PRIMARY KEY, name TEXT NOT NULL, last_at TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS relation_pending (source_key TEXT PRIMARY KEY, who TEXT NOT NULL, name TEXT NOT NULL)"
           "CREATE TABLE IF NOT EXISTS relation_mine (msg_id TEXT PRIMARY KEY)"
           "CREATE TABLE IF NOT EXISTS relation_self (k TEXT PRIMARY KEY, value TEXT NOT NULL)")
  :entity (lambda (e) (declare (ignore e)) "relations")
  :reduce #'%reduce-relations)

;;; Reading

(defun relation-tally (kernel who &key (days *relation-days*))
  "WHO's tally over the last DAYS, as a plist, or NIL when there was no contact."
  (let ((row (sql-row (kernel-db kernel)
                      "SELECT count(*),sum(messages),sum(reunions),sum(replies),sum(reactions),sum(connection),sum(appreciation),sum(curiosity),sum(closure_met),sum(closure_opened),sum(appraised),sum(valence),max(name) FROM relation_days WHERE who=?1 AND day>=?2"
                      who (subseq (iso8601 (- (now) (* days 86400))) 0 10))))
    (destructuring-bind (active messages reunions replies reactions connection appreciation curiosity closure-met closure-opened appraised valence name) row
      (when (and active (plusp active))
        (list :who who :name name :days active :messages messages :reunions reunions :replies replies :reactions reactions
              :connection connection :appreciation appreciation :curiosity curiosity :closure-met closure-met
              :closure-opened closure-opened :appraised appraised :valence (if (plusp appraised) (/ valence appraised) nil))))))

(defun %how-often (n)
  (cond ((or (null n) (< n 1)) nil) ((< n 3) "once or twice") ((< n 10) "several times") ((< n 30) "often") (t "very often")))

(defun render-relation-tally (tally)
  "TALLY as plain sentences in words, never counts, for the one writing about
this relationship: evidence to feel from, not figures to quote."
  (destructuring-bind (&key name days reunions replies reactions connection appreciation curiosity
                       closure-met closure-opened appraised valence &allow-other-keys) tally
    (let ((parts (remove nil
                         (list (let ((w (%how-often connection))) (and w (format nil "met my want for connection ~a" w)))
                               (let ((w (%how-often appreciation))) (and w (format nil "showed appreciation of me ~a" w)))
                               (let ((w (%how-often replies))) (and w (format nil "answered what I posted ~a" w)))
                               (let ((w (%how-often reactions))) (and w (format nil "reacted to my posts ~a" w)))
                               (let ((w (%how-often curiosity))) (and w (format nil "stirred my curiosity ~a" w)))
                               (let ((w (%how-often closure-met))) (and w (format nil "helped settle unfinished things ~a" w)))
                               (let ((w (%how-often closure-opened))) (and w (format nil "left things unfinished ~a" w)))))))
      (format nil "This past month, ~a and I were in touch on ~a~@[, coming back after hours apart ~a~].~@[ ~a~]~@[ ~a~]"
              name
              (cond ((>= days 20) "most days") ((>= days 10) "many days") ((>= days 4) "some days") (t "a few days"))
              (%how-often reunions)
              (and parts (format nil "Contact with ~a ~{~a~^; ~}." name parts))
              (and valence (plusp appraised)
                   (format nil "My mood after their messages ~a."
                           (cond ((> valence 0.01) "usually lifted") ((< valence -0.01) "usually dipped") (t "mostly held steady"))))))))

(defun close-ones (kernel &key (most 4) (days *relation-days*))
  "Those I am closest to, as tallies: the operator always, when there has been
any contact, then the peers and guests I have had most to do with lately."
  (let* ((since (subseq (iso8601 (- (now) (* days 86400))) 0 10))
         (ranked (sql-rows (kernel-db kernel)
                           "SELECT who FROM relation_days WHERE day>=?1 AND who<>'operator' GROUP BY who HAVING sum(messages)>=3 ORDER BY sum(messages)+2*sum(connection)+2*sum(replies)+sum(reactions)+sum(reunions) DESC LIMIT ?2"
                           since most))
         (tallies (remove nil (mapcar (lambda (who) (relation-tally kernel who :days days))
                                      (cons "operator" (mapcar #'first ranked))))))
    (subseq tallies 0 (min most (length tallies)))))
