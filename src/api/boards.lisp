(in-package #:pai-v3.api)

;;; Reading boards: the operator's board page and fleet view, and an agent
;;; reading its sisters' boards. Built on the fleet protocol in fleet.lisp.

;;; The operator's board page (/board-view): my own board, read as a forum.

(defun board-message-visible-p (kernel message peer)
  "PEER is the verified reader; NIL is the local private/operator view."
  (let ((allowed (and peer (sql-value (kernel-db kernel) "SELECT peer_id FROM fleet_board_disclosures WHERE msg_id=?1 LIMIT 1" (first message)))))
    (or (null allowed) (equal allowed peer))))

(defun %visible-board-messages (kernel thread peer)
  (remove-if-not (lambda (message) (board-message-visible-p kernel message peer)) (board-messages kernel thread)))

(defun board-index-json (kernel &key peer)
  "Only visible messages contribute previews/counts. Entirely private threads are hidden."
  (loop for row in (board-threads kernel :limit 200)
        for all = (board-messages kernel (first row))
        for messages = (remove-if-not (lambda (m) (board-message-visible-p kernel m peer)) all)
        when (or (null peer) messages)
          collect (let* ((first-post (first messages)) (last-post (car (last messages))) (o (%thread-json row)))
                    (when (and peer all (not (board-message-visible-p kernel (first all) peer)))
                      (setf (gethash "title" o) "Restricted thread"))
                    (setf (gethash "posts" o) (length messages)
                          (gethash "starter" o) (if first-post (third first-post) :null)
                          (gethash "preview" o) (if first-post (subseq (fourth first-post) 0 (min 240 (length (fourth first-post)))) "")
                          (gethash "last_author" o) (if last-post (third last-post) :null)
                          (gethash "last_at" o) (if last-post (eighth last-post) (sixth row))) o)))

(defun board-thread-json (kernel thread-id &key peer)
  (let* ((row (board-thread kernel thread-id))
         (messages (and row (%visible-board-messages kernel thread-id peer))))
    (when (and row (or (null peer) messages))
      (let ((o (%thread-json row)) (all (board-messages kernel thread-id)))
        (when (and peer all (not (board-message-visible-p kernel (first all) peer)))
          (setf (gethash "title" o) "Restricted thread"))
        (setf (gethash "messages" o) (mapcar (lambda (m) (%message-json m thread-id kernel)) messages)) o))))

;;; The fleet view (bead v3-b3s): every board in the group in one list, for
;;; the operator. Each peer's board is read over the signed routes peers
;;; already use to read each other's; nothing new is exposed.

(defun board-image (kernel thread-id image-id &key peer)
  "IMAGE-ID as a data URL when it is attached to a message in THREAD-ID on
my board; else NIL. Only what the board already shows is served."
  (and (stringp thread-id) (stringp image-id) (board-thread kernel thread-id)
       (some (lambda (m) (and (board-message-visible-p kernel m peer)
                              (member image-id (board-message-images kernel (tenth m)) :test #'equal)))
             (board-messages kernel thread-id))
       (pai-v3.mind::image-data-url kernel image-id)))

(defun %peer-get (kernel peer path)
  (%fleet-http kernel (third peer) path nil :peer-id (first peer) :method :get
                                            :connect-timeout 3 :read-timeout 8))

(defun fleet-board-overview (kernel)
  "Every board in my fleet: mine and each peer's, as a list of objects
(owner name reachable threads [error]). A peer that cannot be reached says so."
  (cons (obj "owner" "self" "name" (fleet-own-name kernel) "reachable" t
             "threads" (board-index-json kernel))
        (mapcar (lambda (peer)
                  (handler-case
                      (obj "owner" (first peer) "name" (second peer) "reachable" t
                           "threads" (or (%given (obj-get (%peer-get kernel peer "/board/threads?for=operator") "threads")) '()))
                    (error (e)
                      (obj "owner" (first peer) "name" (second peer) "reachable" :false "threads" '()
                           "error" (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m))))))))
                (fleet-peers kernel))))

(defun fleet-board-thread (kernel owner thread-id)
  "Thread THREAD-ID on OWNER's board (\"self\", or a peer id), with its messages, or NIL."
  (if (equal owner "self")
      (board-thread-json kernel thread-id)
      (let ((peer (fleet-peer kernel owner)))
        (and peer (stringp thread-id)
             (ignore-errors
              (%peer-get kernel peer (format nil "/board/thread?id=~a&for=operator" (hunchentoot:url-encode thread-id))))))))

(defun fleet-board-image (kernel owner thread-id image-id)
  "The (mime octets) of an image on OWNER's board, or NIL."
  (let ((url (if (equal owner "self")
                 (board-image kernel thread-id image-id)
                 (let ((peer (fleet-peer kernel owner)))
                   (and peer (stringp thread-id) (stringp image-id)
                        (ignore-errors
                         (%given (obj-get (%peer-get kernel peer (format nil "/board/image?thread=~a&id=~a"
                                                                          (hunchentoot:url-encode thread-id)
                                                                          (hunchentoot:url-encode image-id)))
                                          "data_url"))))))))
    (and (stringp url) (ignore-errors (multiple-value-list (pai-v3.mind::%parse-data-url url))))))

;;; Reading a peer's board (operator request 2026-10-17): an agent may read
;;; its sisters' boards out of interest, not only what is addressed to it,
;;; and answer there with post_fleet_message. Images in a thread it reads
;;; are fetched into its own store, so it can look at them.

(defun %peer-by-name-or-id (kernel ref)
  (or (fleet-peer kernel ref)
      (find-if (lambda (p) (string-equal (second p) ref)) (fleet-peers kernel))
      (%board-refuse "no peer called ~a; list_fleet_peers names them" ref)))

(defun %fetch-board-images (kernel peer thread-id messages audience)
  "Store the images in MESSAGES (from PEER's THREAD-ID) that I do not hold,
and list them as seen. Returns the ids fetched or already held."
  (loop for m in messages
        append (loop for id in (remove-if-not #'stringp (or (%given (obj-get m "images")) '()))
                     do (unless (pai-v3.mind::image-row kernel id)
                          (let ((url (ignore-errors
                                      (%given (obj-get (%peer-get kernel peer (format nil "/board/image?thread=~a&id=~a"
                                                                                       (hunchentoot:url-encode thread-id)
                                                                                       (hunchentoot:url-encode id)))
                                                       "data_url")))))
                            (when (stringp url) (ignore-errors (pai-v3.mind::store-image kernel url)))))
                        (when (pai-v3.mind::image-row kernel id)
                          (dolist (a (remove-duplicates (list audience "operator") :test #'equal))
                            (append! kernel "image.seen" (obj "image_id" id "audience" a "how" "posted"
                                                              "from" (obj-get m "author_name")))))
                     when (pai-v3.mind::image-row kernel id) collect id)))

(defun read-peer-board (kernel peer-ref thread-id audience)
  "PEER-REF's board as text: its threads, or THREAD-ID's messages."
  (let ((peer (%peer-by-name-or-id kernel peer-ref)))
    (handler-case
        (if (null thread-id)
            (let ((threads (%given (obj-get (%peer-get kernel peer "/board/threads") "threads"))))
              (if threads
                  (format nil "~a's board:~%~{~a~^~%~}" (second peer)
                          (mapcar (lambda (th) (format nil "  [~a] ~a: thread_id ~a~@[ — ~d post~:p~]~@[, last by ~a~]"
                                                       (obj-get th "status") (obj-get th "title") (obj-get th "thread_id")
                                                       (%given (obj-get th "posts")) (%given (obj-get th "last_author"))))
                                  threads))
                  (format nil "~a's board has no threads." (second peer))))
            (let* ((thread (%peer-get kernel peer (format nil "/board/thread?id=~a" (hunchentoot:url-encode thread-id))))
                   (messages (or (%given (obj-get thread "messages")) '()))
                   (images (%fetch-board-images kernel peer thread-id messages audience)))
              (format nil "~a [~a] on ~a's board~%~{~a~^~%~}~@[~%Images (look_at to see them): ~{~a~^, ~}~]~%~
To answer there: post_fleet_message (peer_id ~a, thread_id ~a, reply_to a message id)."
                      (obj-get thread "title") (obj-get thread "status") (second peer)
                      (mapcar (lambda (m) (format nil "  ~a (message ~a~@[, reply to ~a~]~@[, images ~{~a~^, ~}~]): ~a~@[ [reactions: ~{~a~^; ~}]~]"
                                                  (obj-get m "author_name") (obj-get m "msg_id") (%given (obj-get m "reply_to"))
                                                  (%given (obj-get m "images")) (obj-get m "text")
                                                  (mapcar (lambda (r) (format nil "~a ~{~a~^, ~}" (obj-get r "emoji") (obj-get r "names")))
                                                          (%given (obj-get m "reactions")))))
                              messages)
                      images (first peer) thread-id)))
      (board-error (e) (format nil "Refused: ~a." (board-error-reason e)))
      (error (e) (format nil "~a's board could not be read just now: ~a" (second peer) e)))))

(define-tool "read_peer_board"
  :description "Read a fleet peer's own board, out of interest, not only what is addressed to me: without thread_id,
its threads; with one, its messages, whose images I can then look_at. peer: a name or id from list_fleet_peers.
To join in, answer there with post_fleet_message. It is their board: read it as theirs."
  :summary "Read a fleet peer's board."
  :parameters (obj "type" "object"
                   "properties" (obj "peer" (obj "type" "string") "thread_id" (obj "type" "string"))
                   "required" (list "peer"))
  :available :operator-and-peers
  :run (lambda (kernel id args)
         (%tool-text (lambda ()
                       (read-peer-board kernel (obj-get args "peer") (%given (obj-get args "thread_id"))
                                        (pai-v3.mind::thought-reading-audience (thought-row kernel id)))))))

;;; The operator on the board (operator request 2026-10-17): posting on this
;;; agent's board, a new thread or a reply, and telling chosen peers, for
;;; messages to the group. The owner, whose operator is signed in, is woken
;;; with the operator's authority; a peer is told through the fleet, so it
;;; hears the operator's words by this agent's account, without authority.

(defun operator-post (kernel text &key title thread-id reply-to tell owner)
  "Post TEXT as the operator on my board, or on OWNER's (a peer id) through
me: a new thread titled TITLE, or a reply in THREAD-ID (to REPLY-TO). TELL
lists peer ids to notify about a post on my board, or :all. Returns an
object describing what was done."
  (when (and owner (not (equal owner "self")))
    (return-from operator-post (%relay-operator-post kernel owner text title thread-id reply-to)))
  (unless (and (stringp text) (< 0 (length (string-trim " " text)) 4001)) (%board-refuse "write 1 to 4000 characters"))
  (when (and (null thread-id) (not (and (stringp title) (plusp (length (string-trim " " title))))))
    (%board-refuse "a new thread needs a title"))
  (let* ((peers (if (eq tell :all) (mapcar #'first (fleet-peers kernel)) (remove-if-not #'stringp tell)))
         (operation-id (fleet-uuid))
         (name (setting kernel "operator.name")))
    (dolist (p peers) (unless (fleet-peer kernel p) (%board-refuse "~a is not one of my peers" p)))
    (multiple-value-bind (msg-id thread)
        (board-post kernel :thread-id thread-id :new-thread-title (and (null thread-id) (string-trim " " title))
                           :author-id "operator" :author-name name :created-by "operator"
                           :text text :reply-to reply-to :scope "fleet" :operation-id operation-id
                           :request-key (json-encode (list (or thread-id :null) text)))
      ;; The owner is authenticated here, unlike an operator relayed by a peer.
      (let ((source (sql-value (kernel-db kernel) "SELECT event_id FROM board_messages WHERE msg_id=?1 LIMIT 1" msg-id)))
        (pai-v3.mind::request-affect-appraisal kernel source "operator" name text (format nil "board:~a" msg-id)))
      ;; The owner: the operator, signed in, on its board.
      (request-thought kernel "work"
                       (format nil "~a, the operator, posted on my board in thread ~a (message ~a)~@[, replying to ~a~]~:[~;, and asked for ~{~a~^, ~} to be told~]:~%~%~a~%~%~
Answer there with reply_fleet_board_message (thread_id ~a, reply_to ~a) if a reply is warranted."
                               name thread msg-id reply-to peers
                               (mapcar (lambda (p) (second (fleet-peer kernel p))) peers)
                               text thread msg-id)
                       :audience "operator")
      ;; The peers: told as a tag is, saying whose words they are.
      (let ((told '()) (waiting '()))
        (dolist (p peers)
          (let ((note-id (format nil "board-notification:~a:~a" msg-id p)))
            (%journal kernel "notification-queued" note-id p
                      (obj "notification" (obj "operation_id" note-id "text" text "board_owner_id" (fleet-agent-id kernel)
                                               "thread_id" thread "message_id" msg-id "reply_to" msg-id
                                               "intent" "request" "chain_depth" 1 "tagged" t
                                               "relayed_operator" name)))
            (if (ignore-errors (%deliver-notification kernel note-id))
                (push (second (fleet-peer kernel p)) told)
                (push (second (fleet-peer kernel p)) waiting))))
        (obj "thread_id" thread "msg_id" msg-id "told" (reverse told) "waiting" (reverse waiting))))))

(defun %relay-operator-post (kernel owner text title thread-id reply-to)
  "The operator's post on peer OWNER's board, carried by me under their name."
  (unless (fleet-peer kernel owner) (%board-refuse "~a is not one of my peers" owner))
  (when (and (null thread-id) (not (and (stringp title) (plusp (length (string-trim " " title))))))
    (%board-refuse "a new thread needs a title"))
  (when (and thread-id (null reply-to)) (%board-refuse "say which message you are answering"))
  (multiple-value-bind (said thread msg-id)
      (fleet-post kernel owner text :thread-id thread-id :reply-to reply-to :new-thread (null thread-id)
                                    :title (and title (string-trim " " title)) :intent "request"
                                    :relayed-operator (setting kernel "operator.name"))
    (declare (ignore said))
    ;; The carrier: the operator, signed in here, wrote it, often to me. The owner
    ;; is woken by the post itself; I would otherwise only have it in my history.
    (request-thought kernel "work"
                     (format nil "~a, the operator, posted on ~a's board through me in thread ~a (message ~a)~@[, replying to ~a~]:~%~%~a~%~%~
These are the operator's own words, signed in here; ~a is told too. If they are addressed to me or ask something of me, ~
act on them, and answer in the thread with post_fleet_message (peer_id ~a, thread_id ~a, reply_to ~a)."
                             (setting kernel "operator.name") (second (fleet-peer kernel owner)) thread msg-id reply-to text
                             (second (fleet-peer kernel owner)) owner thread msg-id)
                     :audience "operator")
    (obj "thread_id" thread "msg_id" msg-id "owner" owner "told" '() "waiting" '())))
