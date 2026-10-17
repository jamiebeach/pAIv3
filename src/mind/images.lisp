(in-package #:pai-v3.mind)

;;; Images the operator shows the agent (operator request 2026-10-15: let the
;;; agents see before they move). An image sent with a turn is kept in the
;;; agent's encrypted blob store, named by its content, and the turn records
;;; only its id. When a thought answers that turn and its model can see, the
;;; image goes with the turn; when it cannot, the agent is told an image was
;;; there, so it never pretends to have seen it.

(defvar *image-store* nil
  "The open database images are kept in: the agent's blob store, set when the
agent is opened. NIL: the ledger's own database (tests).")

(defparameter +image-types+ '("image/png" "image/jpeg" "image/webp" "image/gif"))
(defparameter *image-max-bytes* (* 6 1024 1024))
(defparameter *images-per-turn* 4)

(defparameter +images-ddl+
  "CREATE TABLE IF NOT EXISTS images (id TEXT PRIMARY KEY, mime TEXT NOT NULL, bytes BLOB NOT NULL, at TEXT NOT NULL)")

(defun %image-db (kernel)
  (let ((db (or *image-store* (kernel-db kernel))))
    (sql-exec db +images-ddl+)
    db))

(defun %parse-data-url (url)
  "(values mime octets) of a base64 image data URL, or signals."
  (let* ((comma (and (stringp url) (position #\, url)))
         (head (and comma (subseq url 0 comma)))
         (mime (and head (uiop:string-prefix-p "data:" head) (search ";base64" head)
                    (string-downcase (subseq head 5 (search ";base64" head))))))
    (unless (member mime +image-types+ :test #'string=)
      (error "An image must be a PNG, JPEG, WebP or GIF data URL."))
    (let ((octets (base64url-decode (substitute #\- #\+ (substitute #\_ #\/ (string-right-trim "=" (subseq url (1+ comma))))))))
      (when (> (length octets) *image-max-bytes*)
        (error "That image is larger than ~d MB." (floor *image-max-bytes* (* 1024 1024))))
      (values mime octets))))

(defun store-image (kernel data-url)
  "Keep the image in DATA-URL. Returns its id."
  (multiple-value-bind (mime octets) (%parse-data-url data-url)
    (let ((id (subseq (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 octets)) 0 32)))
      (sql-exec (%image-db kernel) "INSERT OR IGNORE INTO images(id,mime,bytes,at) VALUES(?1,?2,?3,?4)"
                id mime octets (now-iso8601))
      id)))

(defun image-row (kernel id)
  "(mime octets) of image ID, or NIL."
  (and (stringp id) (every (lambda (c) (digit-char-p c 16)) id)
       (sql-row (%image-db kernel) "SELECT mime,bytes FROM images WHERE id=?1 LIMIT 1" id)))

(defun image-data-url (kernel id)
  (let ((row (image-row kernel id)))
    (and row (format nil "data:~a;base64,~a" (first row)
                     (let ((b (substitute #\+ #\- (substitute #\/ #\_ (base64url-encode (second row))))))
                       (concatenate 'string b (make-string (mod (- (length b)) 4) :initial-element #\=)))))))

(defun turn-images (kernel thought)
  "The image ids sent with what THOUGHT answers: the operator's turn, or a
peer's board message or notification."
  (let* ((source (getf thought :source-event-id))
         (event (and source (ignore-errors (event-by-id kernel source))))
         (p (and event (event-payload event))))
    (and event
         (let ((ids (cond ((member (event-type event) '("turn.received" "fleet.board.posted") :test #'string=)
                           (obj-get p "images"))
                          ((string= (event-type event) "fleet.op") (obj-get (obj-get p "body") "images")))))
           (and (listp ids) (remove-if-not #'stringp ids))))))

(defun with-images (kernel messages ids profile)
  "MESSAGES with the images IDS added to the last (user) message when PROFILE
can see, else with a note that they were not seen."
  (let* ((last (car (last messages)))
         (text (obj-get last "content" ""))
         (content
           (if (provider-profile-vision profile)
               (cons (obj "type" "text" "text" text)
                     (loop for id in ids
                           for url = (image-data-url kernel id)
                           when url collect (obj "type" "image_url" "image_url" (obj "url" url))))
               (format nil "~a~%~%(The operator attached ~d image~:p. The model I am thinking with now cannot see images, so I have not seen ~:*~[them~;it~:;them~]; say so rather than guess.)"
                       text (length ids)))))
    (append (butlast messages) (list (obj "role" (obj-get last "role" "user") "content" content)))))
