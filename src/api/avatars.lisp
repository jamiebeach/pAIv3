(in-package #:pai-v3.api)

;;; Avatars on the wire. The pages ask /api/avatar?who=self|operator|<peer id>;
;;; the answer is the picture, or 404 and the page keeps its first-letter
;;; circle. A peer's picture is fetched from it over the signed fleet
;;; route and kept in memory for a few minutes, so a board with many posts
;;; asks it once. The operator chooses from the Avatars page: a picture is
;;; resized in the browser to a small square, stored like any image, and its
;;; id set as the setting.

(defparameter *peer-avatar-seconds* 300 "How long a peer's picture is kept before it is asked again.")
(defparameter *peer-avatar-none-seconds* 20
  "How long \"it has none\" (or \"it could not be reached\") is believed: briefly, so a picture it
chooses soon after a page was loaded shows at the next one.")
(defvar *peer-avatars* (make-hash-table :test 'equal)
  "(my id . peer id) -> (fetched-at mime octets); mime and octets are NIL when it had none.")
(defvar *peer-avatars-lock* (bt:make-lock "peer avatars"))

(defun own-avatar (kernel who)
  "(values mime octets image-id) of my avatar (WHO :self) or the operator's (:operator), or NIL."
  (let* ((id (pai-v3.mind:avatar-image-id kernel who))
         (row (and id (pai-v3.mind::image-row kernel id))))
    (and row (values (first row) (second row) id))))

(defun %fetch-peer-avatar (kernel peer-id)
  "(mime octets) of a peer's avatar from it, or NIL."
  (let ((peer (fleet-peer kernel peer-id)))
    (and peer
         (let ((url (ignore-errors (%given (obj-get (%peer-get kernel peer "/fleet/avatar") "data_url")))))
           (and (stringp url) (ignore-errors (multiple-value-list (pai-v3.mind::%parse-data-url url))))))))

(defun peer-avatar (kernel peer-id)
  "(mime octets) of PEER-ID's avatar, remembered for *peer-avatar-seconds* (a miss only for *peer-avatar-none-seconds*); NIL when it has none or cannot be reached."
  (let ((hit (bt:with-lock-held (*peer-avatars-lock*) (gethash (cons (kernel-agent-id kernel) peer-id) *peer-avatars*))))
    (if (and hit (< (- (now) (first hit)) (if (second hit) *peer-avatar-seconds* *peer-avatar-none-seconds*)))
        (and (second hit) (list (second hit) (third hit)))
        (let ((fresh (%fetch-peer-avatar kernel peer-id)))
          (bt:with-lock-held (*peer-avatars-lock*)
            (setf (gethash (cons (kernel-agent-id kernel) peer-id) *peer-avatars*)
                  (list (now) (first fresh) (second fresh))))
          fresh))))

(defun avatar-for (kernel who)
  "(values mime octets etag) for the page's WHO, or NIL."
  (cond ((equal who "self") (multiple-value-bind (mime octets id) (own-avatar kernel :self) (and mime (values mime octets id))))
        ((equal who "operator") (multiple-value-bind (mime octets id) (own-avatar kernel :operator) (and mime (values mime octets id))))
        ((and (stringp who) (equal who (fleet-agent-id kernel))) (avatar-for kernel "self"))
        ((stringp who) (let ((image (peer-avatar kernel who))) (and image (values (first image) (second image) nil))))))

(defun avatar-summary (kernel)
  "What the pages need to decide how to draw: whether I and the operator have pictures, and who my peers are."
  (obj "self" (or (pai-v3.mind:avatar-image-id kernel :self) :null)
       "operator" (or (pai-v3.mind:avatar-image-id kernel :operator) :null)
       "self_id" (fleet-agent-id kernel) "name" (fleet-own-name kernel) "operator_name" (setting kernel "operator.name")
       "peers" (mapcar (lambda (p) (obj "id" (first p) "name" (second p))) (fleet-peers kernel))))

(defun avatar-choices (kernel &optional (limit 30))
  "The pictures this agent holds that the operator has met, newest first, as objects: what to choose an avatar from."
  (mapcar (lambda (r) (obj "id" (first r) "how" (second r) "source" (or (third r) :null) "at" (fourth r)))
          (sql-rows (kernel-db kernel)
                    "SELECT image_id,how,source,at FROM seen_images WHERE audience=?1 AND at>=?2 ORDER BY at DESC LIMIT ?3"
                    "operator" "2000-01-01T00:00:00Z" limit)))

(defun choose-avatar (kernel who data-url origin &key image-id)
  "Make a picture WHO's avatar (\"self\" or \"operator\"): the one in DATA-URL, stored now, or the stored image
IMAGE-ID. Neither clears it."
  (let ((name (cond ((equal who "self") "avatar.image") ((equal who "operator") "operator.avatar")
                    (t (error "who is self or operator")))))
    (cond ((and (stringp image-id) (plusp (length image-id)))
           (unless (pai-v3.mind::image-row kernel image-id) (error "no such picture is held"))
           (change-setting kernel name image-id :origin origin :reason "avatar picked on the Avatars page"))
          ((or (null data-url) (equal data-url ""))
           (change-setting kernel name "" :origin origin :reason "avatar cleared on the Avatars page"))
          (t (change-setting kernel name (pai-v3.mind::store-image kernel data-url) :origin origin
                                          :reason "avatar chosen on the Avatars page")))))

(defun avatar-route (agent method path origin)
  "GET /api/avatars (summary), GET /api/avatar?who= (a picture), POST /api/avatar {who, data_url} (choose)."
  (let ((k (agent-kernel agent)))
    (handler-case
        (cond
          ((and (eq method :get) (string= path "/api/avatars")) (reply 200 (avatar-summary k)))
          ((and (eq method :get) (string= path "/api/avatar/choices")) (reply 200 (obj "images" (avatar-choices k))))
          ((and (eq method :get) (string= path "/api/avatar") (hunchentoot:get-parameter "image"))
           ;; A stored picture, for choosing among them: named by its content, so kept a day.
           (let ((row (pai-v3.mind::image-row k (hunchentoot:get-parameter "image"))))
             (if row
                 (progn (setf (hunchentoot:content-type*) (first row)
                              (hunchentoot:header-out "Cache-Control") "private, max-age=86400")
                        (second row))
                 (reply 404 (obj "error" "no such picture")))))
          ((and (eq method :get) (string= path "/api/avatar"))
           (multiple-value-bind (mime octets etag) (avatar-for k (hunchentoot:get-parameter "who"))
             (cond ((null mime) (reply 404 (obj "error" "no avatar")))
                   ((and etag (equal (hunchentoot:header-in* :if-none-match) (format nil "\"~a\"" etag)))
                    (setf (hunchentoot:return-code*) 304) "")
                   (t (setf (hunchentoot:content-type*) mime
                            (hunchentoot:header-out "Cache-Control") "private, max-age=300")
                      (when etag (setf (hunchentoot:header-out "ETag") (format nil "\"~a\"" etag)))
                      octets))))
          ((and (eq method :post) (string= path "/api/avatar"))
           (let ((b (body-json)))
             (choose-avatar k (obj-get b "who") (%given (obj-get b "data_url")) origin :image-id (%given (obj-get b "image_id")))
             (reply 200 (avatar-summary k))))
          (t (reply 404 (obj "error" "no such route"))))
      (setting-refused (e) (reply 400 (obj "error" (princ-to-string e))))
      (error (e) (reply 400 (obj "error" (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m))))))))))
