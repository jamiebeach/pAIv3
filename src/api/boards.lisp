(in-package #:pai-v3.api)

;;; Reading boards: the operator's board page and fleet view, and an agent
;;; reading its sisters' boards. Built on the fleet protocol in fleet.lisp.

;;; The operator's board page (/board-view): my own board, read as a forum.

(defun board-index-json (kernel)
  "Threads, latest activity first, each with its opening post, post count and last poster."
  (mapcar (lambda (row)
            (let* ((messages (board-messages kernel (first row)))
                   (first-post (first messages))
                   (last-post (car (last messages)))
                   (o (%thread-json row)))
              (setf (gethash "posts" o) (length messages)
                    (gethash "starter" o) (if first-post (third first-post) :null)
                    (gethash "preview" o) (if first-post (subseq (fourth first-post) 0 (min 240 (length (fourth first-post)))) "")
                    (gethash "last_author" o) (if last-post (third last-post) :null)
                    (gethash "last_at" o) (if last-post (eighth last-post) (sixth row)))
              o))
          (board-threads kernel :limit 200)))

(defun board-thread-json (kernel thread-id)
  "One thread with all its posts, or NIL."
  (let ((row (board-thread kernel thread-id)))
    (and row
         (let ((o (%thread-json row)))
           (setf (gethash "messages" o)
                 (mapcar (lambda (m) (%message-json m thread-id kernel)) (board-messages kernel thread-id)))
           o))))

;;; The fleet view (bead v3-b3s): every board in the group in one list, for
;;; the operator. Each peer's board is read over the signed routes peers
;;; already use to read each other's; nothing new is exposed.

(defun board-image (kernel thread-id image-id)
  "IMAGE-ID as a data URL when it is attached to a message in THREAD-ID on
my board; else NIL. Only what the board already shows is served."
  (and (stringp thread-id) (stringp image-id) (board-thread kernel thread-id)
       (some (lambda (m) (member image-id (board-message-images kernel (tenth m)) :test #'equal))
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
                           "threads" (or (%given (obj-get (%peer-get kernel peer "/board/threads") "threads")) '()))
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
              (%peer-get kernel peer (format nil "/board/thread?id=~a" (hunchentoot:url-encode thread-id))))))))

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
                      (mapcar (lambda (m) (format nil "  ~a (message ~a~@[, reply to ~a~]~@[, images ~{~a~^, ~}~]): ~a"
                                                  (obj-get m "author_name") (obj-get m "msg_id") (%given (obj-get m "reply_to"))
                                                  (%given (obj-get m "images")) (obj-get m "text")))
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
                                        (getf (thought-row kernel id) :audience))))))
