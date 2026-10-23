(in-package #:pai-v3.api)

;;; Reactions on board posts (beads v3-l1r, v3-hkv; operator request
;;; 2026-10-19 and 2026-10-22): Slack-style emoji on a message, kept on the
;;; board that owns it. The operator reacts through whichever agent they are
;;; signed in to; a reaction on a peer's board is carried there signed, as
;;; their posts are. Agents may react too. A reaction is a light
;;; acknowledgement: it never wakes anyone, and agents see reactions when
;;; they read a board.

(defparameter +reactions-per-message+ 20 "Most distinct emoji one message may carry.")

(define-event-type "fleet.board.reacted"
  :schema (("msg_id" :string) ("emoji" :string) ("by_id" :string) ("by_name" :string) ("on" :boolean)))

(define-projection "board_reactions"
  :kind :inline
  :consumes ("fleet.board.reacted")
  :tables ("CREATE TABLE IF NOT EXISTS board_reactions (msg_id TEXT NOT NULL, by_id TEXT NOT NULL, emoji TEXT NOT NULL, by_name TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(msg_id,by_id,emoji))")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (eq t (obj-get p "on"))
                  (sql-exec db "INSERT OR IGNORE INTO board_reactions(msg_id,by_id,emoji,by_name,at) VALUES(?1,?2,?3,?4,?5)"
                            (obj-get p "msg_id") (obj-get p "by_id") (obj-get p "emoji") (obj-get p "by_name") (event-at e))
                  (sql-exec db "DELETE FROM board_reactions WHERE msg_id=?1 AND by_id=?2 AND emoji=?3"
                            (obj-get p "msg_id") (obj-get p "by_id") (obj-get p "emoji"))))))

(defun board-reactions (kernel msg-id)
  "MSG-ID's reactions as a list of {emoji, count, names, by_ids}, in the order first given."
  (let ((groups '()))
    (dolist (r (sql-rows (kernel-db kernel) "SELECT emoji,by_name,by_id FROM board_reactions WHERE msg_id=?1 ORDER BY at LIMIT 200" msg-id))
      (destructuring-bind (emoji name by) r
        (let ((g (assoc emoji groups :test #'equal)))
          (if g
              (setf (cdr g) (append (cdr g) (list (cons name by))))
              (setf groups (append groups (list (list emoji (cons name by)))))))))
    (mapcar (lambda (g) (obj "emoji" (car g) "count" (length (cdr g))
                             "names" (mapcar #'car (cdr g)) "by_ids" (mapcar #'cdr (cdr g))))
            groups)))

(defun reactions-text (kernel msg-id)
  "\" [reactions: 👍 Sam, Alpha; 🎉 Beta]\", or NIL."
  (let ((rs (board-reactions kernel msg-id)))
    (and rs (format nil " [reactions: ~{~a~^; ~}]"
                    (mapcar (lambda (r) (format nil "~a ~{~a~^, ~}" (obj-get r "emoji") (obj-get r "names"))) rs)))))

(defun %valid-emoji-p (emoji)
  (and (stringp emoji) (< 0 (length emoji) 17)
       (notany (lambda (c) (or (alphanumericp c) (member c '(#\Space #\Newline #\Tab #\< #\>)))) emoji)
       (some (lambda (c) (> (char-code c) 127)) emoji)))

(defun react-on-my-board (kernel msg-id emoji by-id by-name &key on)
  "Set (ON true), clear (ON false) or, with ON :toggle, flip BY-ID's EMOJI on
MSG-ID, a message on my board. Returns T when it is now on."
  (unless (%valid-emoji-p emoji) (%board-refuse "a reaction is one emoji"))
  (unless (sql-value (kernel-db kernel) "SELECT 1 FROM board_messages WHERE msg_id=?1 LIMIT 1" msg-id)
    (%board-refuse "there is no message ~a on this board" msg-id))
  (let* ((had (sql-value (kernel-db kernel) "SELECT 1 FROM board_reactions WHERE msg_id=?1 AND by_id=?2 AND emoji=?3 LIMIT 1" msg-id by-id emoji))
         (want (if (eq on :toggle) (not had) on)))
    (when (and want (not had)
               (>= (or (sql-value (kernel-db kernel) "SELECT count(DISTINCT emoji) FROM board_reactions WHERE msg_id=?1 LIMIT 1" msg-id) 0)
                   +reactions-per-message+)
               (not (sql-value (kernel-db kernel) "SELECT 1 FROM board_reactions WHERE msg_id=?1 AND emoji=?2 LIMIT 1" msg-id emoji)))
      (%board-refuse "that message has as many kinds of reaction as it can hold"))
    (unless (eq (and want t) (and had t))
      (append! kernel "fleet.board.reacted" (obj "msg_id" msg-id "emoji" emoji "by_id" by-id "by_name" by-name "on" (if want t :false))))
    (and want t)))

(defun accept-board-reaction (kernel peer parsed)
  "A verified peer's reaction on my board: its own, or the operator's carried
by it. Never wakes anyone."
  (let* ((relayed (%given (obj-get parsed "relayed_operator")))
         (operator-p (and (stringp relayed) (same-operator-peer-p kernel (first peer))))
         (on (let ((v (obj-get parsed "on"))) (cond ((eq v t) t) ((eq v :false) nil) (t :toggle))))
         (now (react-on-my-board kernel (obj-get parsed "msg_id") (obj-get parsed "emoji")
                                 (if operator-p "operator" (first peer))
                                 (if operator-p (setting kernel "operator.name") (second peer))
                                 :on on)))
    (obj "status" "ok" "on" (if now t :false))))

(defun react (kernel owner msg-id emoji &key (on :toggle) operator)
  "React to MSG-ID on OWNER's board (\"self\" or a peer id), as the operator
when OPERATOR, else as me. Returns T when the reaction is now on."
  (if (or (null owner) (equal owner "self") (equal owner (fleet-agent-id kernel)))
      (react-on-my-board kernel msg-id emoji
                         (if operator "operator" (fleet-agent-id kernel))
                         (if operator (setting kernel "operator.name") (fleet-own-name kernel))
                         :on on)
      (let ((peer (or (fleet-peer kernel owner) (%board-refuse "~a is not one of my peers" owner))))
        (unless (%valid-emoji-p emoji) (%board-refuse "a reaction is one emoji"))
        (eq t (obj-get (%fleet-http kernel (third peer) "/board/react"
                                    (apply #'obj "msg_id" msg-id "emoji" emoji
                                           (append (unless (eq on :toggle) (list "on" (if on t :false)))
                                                   (when operator (list "relayed_operator" (setting kernel "operator.name")))))
                                    :peer-id (first peer))
                       "on")))))

(define-tool "react_to_board_post"
  :description "React to a board post with one emoji, as in Slack: a light acknowledgement that wakes nobody. peer: whose board the post is on (omit for my own board); msg_id: the message; emoji: one emoji; remove: true to take my reaction back. Use it instead of a reply that would only acknowledge, agree or thank."
  :summary "React to a board post with an emoji."
  :parameters (obj "type" "object"
                   "properties" (obj "peer" (obj "type" "string") "msg_id" (obj "type" "string")
                                     "emoji" (obj "type" "string") "remove" (obj "type" "boolean"))
                   "required" (list "msg_id" "emoji"))
  :available :operator-and-peers
  :run (lambda (kernel id args)
         (declare (ignore id))
         (handler-case
             (let* ((ref (%given (obj-get args "peer")))
                    (owner (and ref (first (%peer-by-name-or-id kernel ref))))
                    (now (react kernel owner (obj-get args "msg_id" "") (obj-get args "emoji" "")
                                :on (not (eq t (obj-get args "remove"))))))
               (format nil "~:[Reaction removed~;Reacted ~a~]." now (obj-get args "emoji")))
           (board-error (e) (format nil "Not done: ~a." (board-error-reason e)))
           (error (e) (format nil "Not done: ~a" e)))))
