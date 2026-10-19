(in-package #:pai-v3.api)

;;; Avatars on the wire. The pages ask /api/avatar?who=self|operator|<peer id>;
;;; the answer is the picture, or 404 and the page keeps its first-letter
;;; circle. A sister's picture is fetched from her over the signed fleet
;;; route and kept in memory for a few minutes, so a board with many posts
;;; asks her once. The operator chooses from the Avatars page: a picture is
;;; resized in the browser to a small square, stored like any image, and its
;;; id set as the setting.

(defparameter *peer-avatar-minutes* 10)
(defvar *peer-avatars* (make-hash-table :test 'equal) "peer id -> (fetched-at mime octets) or (fetched-at nil nil)")
(defvar *peer-avatars-lock* (bt:make-lock "peer avatars"))

(defun own-avatar (kernel who)
  "(values mime octets image-id) of my avatar (WHO :self) or the operator's (:operator), or NIL."
  (let* ((id (pai-v3.mind:avatar-image-id kernel who))
         (row (and id (pai-v3.mind::image-row kernel id))))
    (and row (values (first row) (second row) id))))

(defun %fetch-peer-avatar (kernel peer-id)
  "(mime octets) of a sister's avatar from her, or NIL."
  (let ((peer (fleet-peer kernel peer-id)))
    (and peer
         (let ((url (ignore-errors (%given (obj-get (%peer-get kernel peer "/fleet/avatar") "data_url")))))
           (and (stringp url) (ignore-errors (multiple-value-list (pai-v3.mind::%parse-data-url url))))))))

(defun peer-avatar (kernel peer-id)
  "(mime octets) of PEER-ID's avatar, remembered for *peer-avatar-minutes*; NIL when she has none or cannot be reached."
  (let ((hit (bt:with-lock-held (*peer-avatars-lock*) (gethash (cons (kernel-agent-id kernel) peer-id) *peer-avatars*))))
    (if (and hit (< (- (now) (first hit)) (* 60 *peer-avatar-minutes*)))
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
  "What the pages need to decide how to draw: whether I and the operator have pictures, and who my sisters are."
  (obj "self" (or (pai-v3.mind:avatar-image-id kernel :self) :null)
       "operator" (or (pai-v3.mind:avatar-image-id kernel :operator) :null)
       "self_id" (fleet-agent-id kernel) "name" (fleet-own-name kernel) "operator_name" (setting kernel "operator.name")
       "peers" (mapcar (lambda (p) (obj "id" (first p) "name" (second p))) (fleet-peers kernel))))

(defun choose-avatar (kernel who data-url origin)
  "Store the picture in DATA-URL and make it WHO's avatar (\"self\" or \"operator\"); an empty DATA-URL clears it."
  (let ((name (cond ((equal who "self") "avatar.image") ((equal who "operator") "operator.avatar")
                    (t (error "who is self or operator")))))
    (if (or (null data-url) (equal data-url ""))
        (change-setting kernel name "" :origin origin :reason "avatar cleared on the Avatars page")
        (change-setting kernel name (pai-v3.mind::store-image kernel data-url) :origin origin
                                    :reason "avatar chosen on the Avatars page"))))

(defun avatar-route (agent method path origin)
  "GET /api/avatars (summary), GET /api/avatar?who= (a picture), POST /api/avatar {who, data_url} (choose)."
  (let ((k (agent-kernel agent)))
    (handler-case
        (cond
          ((and (eq method :get) (string= path "/api/avatars")) (reply 200 (avatar-summary k)))
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
             (choose-avatar k (obj-get b "who") (%given (obj-get b "data_url")) origin)
             (reply 200 (avatar-summary k))))
          (t (reply 404 (obj "error" "no such route"))))
      (setting-refused (e) (reply 400 (obj "error" (princ-to-string e))))
      (error (e) (reply 400 (obj "error" (let ((m (princ-to-string e))) (subseq m 0 (min 200 (length m))))))))))
