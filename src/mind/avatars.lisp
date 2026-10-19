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

(defun avatar-candidates (kernel audience)
  "The pictures the agent may be offered as her avatar, newest first, as (id how at caption): those she made, and, for the
operator's own thoughts, those the operator sent. Pictures others posted are not offered."
  (let ((hows (if (equal audience "operator") '("made" "sent") '("made"))))
    (loop for how in hows
          append (loop for (id at) in (sql-rows (kernel-db kernel)
                                                "SELECT image_id,at FROM seen_images WHERE audience=?1 AND how=?2 AND at>=?3 ORDER BY at DESC LIMIT ?4"
                                                "operator" how "2000-01-01T00:00:00Z" 8)
                       collect (list id how at (or (sql-value (kernel-db kernel) "SELECT caption FROM image_captions WHERE image_id=?1 LIMIT 1" id) ""))))))

(define-tool "set_avatar"
  :description "Choose the picture that is you on the board and in the terminal. Call it with no image_id to list the pictures you have made (each with its id and a caption when there is one); then call it again with that image_id. image_id is the id of an image you hold. Give an empty image_id to go back to the first-letter circle. The operator can also change it. Looking for your picture with search_experience will not find it: that searches what was said, not pictures."
  :summary "Choose your own avatar from an image you hold."
  ;; The operator's thoughts and fleet peers' (replying on a board): her own picture is hers to
  ;; choose, from an image she already holds. Not the mail digest, which runs on outside content.
  :available :operator-and-peers
  :core '("responding" "introspection" "work")
  :parameters (obj "type" "object"
                   "properties" (obj "image_id" (obj "type" "string" "description" "an image id, or empty to clear"))
                   "required" '())
  :run (lambda (kernel id args)
         (let ((image (string-trim " " (or (obj-get args "image_id") ""))))
           (handler-case
               (cond ((null (obj-get args "image_id"))
                      (let ((found (avatar-candidates kernel (getf (thought-row kernel id) :audience))))
                        (if found
                            (format nil "Pictures you can choose from, newest first:~%~{~a~^~%~}~%Call set_avatar again with the image_id you want."
                                    (mapcar (lambda (c) (format nil "- ~a (~a, ~a)~@[ ~a~]" (first c) (second c)
                                                                (ignore-errors (local-stamp kernel (third c)))
                                                                (and (plusp (length (fourth c))) (subseq (fourth c) 0 (min 140 (length (fourth c)))))))
                                            found))
                            "You have made no pictures I can offer. Generate one with the image tool, then call this again.")))
                     ((zerop (length image))
                      (change-setting kernel "avatar.image" "" :origin (obj "principal_type" "self") :reason "cleared by me")
                      "Back to the first-letter circle.")
                     ((not (image-row kernel image))
                      "Not done: I do not hold an image with that id. Generate one, or use an id from look_at.")
                     (t (change-setting kernel "avatar.image" image :origin (obj "principal_type" "self") :reason "chosen by me")
                        "That is my avatar now; it shows on the board and in the terminal."))
             (setting-refused (e) (format nil "Not done: ~a" (setting-refused-reason e)))
             (error (e) (format nil "Not done: ~a" e))))))
