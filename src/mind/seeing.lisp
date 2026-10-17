(in-package #:pai-v3.mind)

;;; Seeing again (operator request 2026-10-17). An image is shown inline only
;;; with the turn it came in; after that the agent keeps, for a while, a list
;;; of what it has seen lately (each image's id and a one-line caption), and
;;; looks again with look_at when it needs to: "now find the pickle" in the
;;; picture from two turns ago. Captions are written once per image: an
;;; image the agent made is described by its own prompt; any other is
;;; captioned in the background by a model that can see.

(define-setting "images.recent_minutes"
  :type :integer :min 1 :max 1440 :default 30 :group "Images"
  :description "How long an image stays in the list of what I have seen lately.")

(define-event-type "image.seen"
  :schema (("image_id" :string) ("audience" :string) ("how" :string) ("from" :string :optional)
           ("caption" :string :optional)))
(define-event-type "image.captioned"
  :schema (("image_id" :string) ("caption" :string)))
(define-event-type "image.looked"
  :schema (("image_id" :string) ("thought_id" :integer)))

(defun %seen (db image audience how from caption at)
  (sql-exec db "INSERT INTO seen_images(image_id,audience,how,source,at) VALUES(?1,?2,?3,?4,?5) ON CONFLICT(image_id,audience) DO UPDATE SET at=excluded.at"
            image audience how from at)
  (sql-exec db "INSERT INTO image_captions(image_id,caption) VALUES(?1,?2) ON CONFLICT(image_id) DO NOTHING"
            image (or caption ""))
  (when (and caption (plusp (length caption)))
    (sql-exec db "UPDATE image_captions SET caption=?2 WHERE image_id=?1 AND caption=''" image caption)))

(define-projection "seeing"
  :kind :inline
  :consumes ("turn.received" "image.generated" "fleet.board.posted" "fleet.op" "image.seen" "image.captioned" "image.looked")
  :tables ("CREATE TABLE IF NOT EXISTS seen_images (image_id TEXT NOT NULL, audience TEXT NOT NULL, how TEXT NOT NULL, source TEXT, at TEXT NOT NULL, PRIMARY KEY(image_id,audience))"
           "CREATE INDEX IF NOT EXISTS seen_images_recent_idx ON seen_images(audience,at)"
           "CREATE TABLE IF NOT EXISTS image_captions (image_id TEXT PRIMARY KEY, caption TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS image_captions_pending_idx ON image_captions(caption,image_id)"
           "CREATE TABLE IF NOT EXISTS looked_images (thought_id INTEGER NOT NULL, image_id TEXT NOT NULL, PRIMARY KEY(thought_id,image_id))")
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)) (at (event-at e)))
              (flet ((ids (v) (and (listp v) (remove-if-not #'stringp v))))
                (cond ((string= type "turn.received")
                       (dolist (id (ids (obj-get p "images")))
                         (%seen db id (obj-get p "audience") "sent" (obj-get p "speaker") nil at)))
                      ((string= type "image.generated")
                       (%seen db (obj-get p "image_id") "operator" "made" nil (obj-get p "prompt") at))
                      ((string= type "fleet.board.posted")
                       (dolist (id (ids (obj-get p "images")))
                         (%seen db id (format nil "peer:~a" (obj-get p "author_id")) "posted" (obj-get p "author_name") nil at)
                         (%seen db id "operator" "posted" (obj-get p "author_name") nil at)))
                      ((string= type "fleet.op")
                       (when (equal "notification-received" (obj-get p "kind"))
                         (dolist (id (ids (obj-get (obj-get p "body") "images")))
                           (%seen db id (format nil "peer:~a" (obj-get p "peer_id")) "posted" nil nil at)
                           (%seen db id "operator" "posted" nil nil at))))
                      ((string= type "image.seen")
                       (%seen db (obj-get p "image_id") (obj-get p "audience") (obj-get p "how") (obj-get p "from")
                              (obj-get p "caption") at))
                      ((string= type "image.captioned")
                       (sql-exec db "INSERT INTO image_captions(image_id,caption) VALUES(?1,?2) ON CONFLICT(image_id) DO UPDATE SET caption=excluded.caption"
                                 (obj-get p "image_id") (obj-get p "caption")))
                      ((string= type "image.looked")
                       (sql-exec db "INSERT OR IGNORE INTO looked_images(thought_id,image_id) VALUES(?1,?2)"
                                 (obj-get p "thought_id") (obj-get p "image_id"))))))))

(defun image-caption (kernel image-id)
  (let ((c (sql-value (kernel-db kernel) "SELECT caption FROM image_captions WHERE image_id=?1 LIMIT 1" image-id)))
    (and (stringp c) (plusp (length c)) c)))

(defun recently-seen (kernel audience &key (most 5))
  "Images AUDIENCE's thoughts have met lately, newest first, as (image-id how source at)."
  (sql-rows (kernel-db kernel)
            "SELECT image_id,how,source,at FROM seen_images WHERE audience=?1 AND at>=?2 ORDER BY at DESC LIMIT ?3"
            audience (iso8601 (- (now) (* 60 (setting kernel "images.recent_minutes")))) most))

;;; Captions, once per image, by a model that can see

(defun caption-image (kernel work)
  (let* ((id (getf work :entity))
         (profile (profile-for kernel "vision")))
    (unless (image-row kernel id) (error 'handler-failure :class :source :reason "no such image"))
    (unless (provider-profile-vision profile)
      (error 'handler-failure :class :source :reason "no model that can see is routed for vision"))
    (let* ((reply (complete kernel profile
                            (with-images kernel (list (obj "role" "user"
                                                           "content" "Describe this image in one sentence, plainly: what it shows, so it can be found again later."))
                                         (list id) profile)
                            :work-id (getf work :id) :lane "vision"))
           (text (string-trim '(#\Space #\Newline) (or (obj-get reply "content") ""))))
      (when (plusp (length text))
        (append! kernel "image.captioned" (obj "image_id" id "caption" (subseq text 0 (min 300 (length text))))))
      nil)))

(define-handler "image.caption" :run #'caption-image :paid t :estimate-microusd 2000)

(defun request-captions (kernel &key (batch 5))
  "Ask for a caption for images seen but not yet described."
  (dolist (row (sql-rows (kernel-db kernel) "SELECT image_id FROM image_captions WHERE caption=?1 LIMIT ?2" "" batch))
    (unless (work-pending-p kernel "image.caption" (first row))
      (request-work kernel "image.caption" (first row) (obj) :priority :maintenance))))

(pushnew 'request-captions *periodic-hooks*)

;;; What a thought sees: its turn's images, and what it chose to look at

(defun thought-images (kernel thought)
  "The image ids shown to THOUGHT's model: those that came with what it
answers, and those it asked to look at again."
  (remove-duplicates
   (append (turn-images kernel thought)
           (mapcar #'first (sql-rows (kernel-db kernel) "SELECT image_id FROM looked_images WHERE thought_id=?1 LIMIT 8"
                                     (getf thought :id))))
   :test #'equal :from-end t))

(define-slot-source :seen (kernel request max)
  (let ((rows (recently-seen kernel (getf request :audience))))
    (when rows
      (let ((text (format nil "~{~a~^~%~}"
                          (loop for (id how source at) in rows
                                collect (format nil "- ~a: ~a (~a~@[ by ~a~], ~a). look_at to see it again."
                                                id (or (image-caption kernel id) "not yet described")
                                                (cond ((equal how "made") "I made it") ((equal how "sent") "sent")
                                                      (t "posted"))
                                                (and (not (equal how "made")) source)
                                                (or (ignore-errors (local-stamp kernel at)) at))))))
        (list (list text (estimate-tokens text)))))))

(define-tool "look_at"
  :description "Look again at stored images: they are shown to me at my next step, as if just sent. images: their
ids (from the list of images I have seen lately, a picture's entry in my knowledge, or a board post). Use it to
answer more about a picture from earlier in the conversation."
  :summary "Look again at stored images."
  :available :operator-and-peers
  :parameters (obj "type" "object"
                   "properties" (obj "images" (obj "type" "array" "items" (obj "type" "string")))
                   "required" (list "images"))
  :run (lambda (kernel thought-id args)
         (let* ((ids (remove-if-not #'stringp (or (obj-get args "images") '())))
                (audience (getf (thought-row kernel thought-id) :audience))
                (mine (remove-if-not (lambda (id) (and (image-row kernel id)
                                                       ;; Another audience sees only images it has met itself.
                                                       (or (equal audience "operator")
                                                           (sql-value (kernel-db kernel) "SELECT 1 FROM seen_images WHERE image_id=?1 AND audience=?2 LIMIT 1"
                                                                      id audience))))
                                     ids)))
           (dolist (id (subseq mine 0 (min 4 (length mine))))
             (append! kernel "image.looked" (obj "image_id" id "thought_id" thought-id)))
           (cond ((null ids) "Name the images to look at.")
                 ((null mine) "None of those is an image I can look at.")
                 (t (format nil "Looking at ~{~a~^, ~}; ~:[it is~;they are~] in front of me at my next step."
                            (subseq mine 0 (min 4 (length mine))) (rest mine)))))))
