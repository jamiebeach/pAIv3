(in-package #:pai-v3.mind)

;;; Avatars (operator request 2026-10-19, raised by the agents on the board):
;;; each agent has a picture of herself, and the operator has one, shown on
;;; the board and in the terminal in place of the first-letter circle.
;;;
;;; A picture is an image already in the agent's blob store, named by its
;;; content id (the same ids the image tools and the board use). An agent
;;; chooses her own with set_avatar from an image she holds; the operator may
;;; choose either from the Avatars page. Both are ordinary settings, so every
;;; change is in the ledger with who made it.

(defun %avatar-id-problem (v)
  (unless (or (equal v "") (and (stringp v) (= 32 (length v)) (every (lambda (c) (digit-char-p c 16)) v)))
    "an avatar is the 32-digit id of an image I hold, or empty for none"))

(define-setting "avatar.image"
  :type :string :default "" :group "Avatars" :changed-by (:agent)
  :validate #'%avatar-id-problem
  :description "The id of the image that is this agent's avatar on the board and in the terminal; empty for the first-letter circle. The agent may choose her own.")

(define-setting "operator.avatar"
  :type :string :default "" :group "Avatars"
  :validate #'%avatar-id-problem
  :description "The id of the image that is the operator's avatar; empty for the first-letter circle.")

(defun avatar-image-id (kernel who)
  "The image id of WHO's avatar (:self or :operator), or NIL when none is set or its image is gone."
  (let ((id (setting kernel (if (eq who :operator) "operator.avatar" "avatar.image"))))
    (and (stringp id) (plusp (length id)) (image-row kernel id) id)))

(define-tool "set_avatar"
  :description "Choose the picture that is you on the board and in the terminal. image_id is the id of an image you hold: one you generated with the image tool, or one you have been shown and looked at (look_at lists those you have seen lately). Give an empty image_id to go back to the first-letter circle. The operator can also change it."
  :summary "Choose your own avatar from an image you hold."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "image_id" (obj "type" "string" "description" "an image id, or empty to clear"))
                   "required" (list "image_id"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((image (string-trim " " (or (obj-get args "image_id") ""))))
           (handler-case
               (cond ((zerop (length image))
                      (change-setting kernel "avatar.image" "" :origin (obj "principal_type" "self") :reason "cleared by me")
                      "Back to the first-letter circle.")
                     ((not (image-row kernel image))
                      "Not done: I do not hold an image with that id. Generate one, or use an id from look_at.")
                     (t (change-setting kernel "avatar.image" image :origin (obj "principal_type" "self") :reason "chosen by me")
                        "That is my avatar now; it shows on the board and in the terminal."))
             (setting-refused (e) (format nil "Not done: ~a" (setting-refused-reason e)))
             (error (e) (format nil "Not done: ~a" e))))))
