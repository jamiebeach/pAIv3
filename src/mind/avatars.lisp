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

(defun avatar-provenance (kernel)
  "(values when by) of the last change to my avatar: its time (ISO) and \"me\" or \"the operator\"; NIL when never set."
  (let ((row (sql-row (kernel-db kernel) "SELECT changed_at,event_id FROM settings WHERE name=?1 LIMIT 1" "avatar.image")))
    (when row
      (let ((event (ignore-errors (event-by-id kernel (second row)))))
        (values (first row)
                (if (and event (equal "operator" (obj-get (obj-get (event-payload event) "origin") "principal_type")))
                    "the operator" "me"))))))

(define-slot-source :myself (kernel request max)
  ;; What I may always know about myself, whoever I am thinking for: whether I have chosen a face,
  ;; and whether I have been making pictures. Nothing private: no content, only the fact.
  (let* ((lines '())
         (id (avatar-image-id kernel :self)))
    (multiple-value-bind (at by) (avatar-provenance kernel)
      (push (if (and id at)
                (format nil "I have an avatar (chosen ~a, by ~a). I do not need to make or choose another unless I mean to change it."
                        (or (ignore-errors (local-stamp kernel at)) at) by)
                "I have not chosen an avatar yet.")
            lines))
    (let ((made (or (sql-value (kernel-db kernel) "SELECT count(*) FROM seen_images WHERE audience=?1 AND how=?2 AND at>=?3 LIMIT 1"
                               "operator" "made" (iso8601 (- (now) (* 3 86400))))
                    0)))
      (when (plusp made)
        (push (format nil "I made ~d picture~:p in the last three days." made) lines)))
    (loop for text in (nreverse lines)
          for tokens = (estimate-tokens text)
          while (<= tokens max)
          collect (list text tokens))))

(define-tool "set_avatar"
  :description "Choose the picture that is you on the board and in the terminal. Call it with no image_id to list the pictures you have made (each with its id and a caption when there is one); then call it again with that image_id. image_id is the id of an image you hold. Give an empty image_id to go back to the first-letter circle. If you already have an avatar it is not replaced unless you pass replace true, so you do not overwrite a choice by accident. The operator can also change it. Looking for your picture with search_experience will not find it: that searches what was said, not pictures."
  :summary "Choose your own avatar from an image you hold."
  ;; The operator's thoughts and fleet peers' (replying on a board): her own picture is hers to
  ;; choose, from an image she already holds. Not the mail digest, which runs on outside content.
  :available :operator-and-peers
  :core '("responding" "introspection" "work")
  :parameters (obj "type" "object"
                   "properties" (obj "image_id" (obj "type" "string" "description" "an image id, or empty to clear")
                                     "replace" (obj "type" "boolean" "description" "true to replace an avatar you already have"))
                   "required" '())
  :run (lambda (kernel id args)
         (let ((image (string-trim " " (or (obj-get args "image_id") ""))))
           (handler-case
               (cond ((null (obj-get args "image_id"))
                      (let ((found (avatar-candidates kernel (getf (thought-row kernel id) :audience))))
                        (if found
                            (format nil "~aPictures you can choose from, newest first:~%~{~a~^~%~}~%Call set_avatar again with the image_id you want."
                                    (if (avatar-image-id kernel :self)
                                        (multiple-value-bind (at by) (avatar-provenance kernel)
                                          (format nil "Your current avatar was chosen ~a by ~a.~%"
                                                  (or (ignore-errors (local-stamp kernel at)) at) by))
                                        "")
                                    (mapcar (lambda (c) (format nil "- ~a (~a, ~a)~@[ ~a~]" (first c) (second c)
                                                                (ignore-errors (local-stamp kernel (third c)))
                                                                (and (plusp (length (fourth c))) (subseq (fourth c) 0 (min 140 (length (fourth c)))))))
                                            found))
                            "You have made no pictures I can offer. Generate one with the image tool, then call this again.")))
                     ((zerop (length image))
                      (change-setting kernel "avatar.image" "" :origin (obj "principal_type" "self") :reason "cleared by me")
                      "Back to the first-letter circle.")
                     ((not (image-row kernel image))
                      (format nil "Not done: ~a is not an image I hold.~:[~; A memory id (m-...) or fact id (f-...) is not a picture's.~] Call set_avatar with no image_id to list the pictures I made, or generate one."
                              image (and (> (length image) 2) (member (char image 0) '(#\m #\f)) (char= (char image 1) #\-))))
                     ((and (avatar-image-id kernel :self) (not (equal image (avatar-image-id kernel :self)))
                           (not (eq t (obj-get args "replace"))))
                      (multiple-value-bind (at by) (avatar-provenance kernel)
                        (format nil "Not done: I already have an avatar, chosen ~a by ~a. If I really mean to change it, call set_avatar again with replace true."
                                (or (ignore-errors (local-stamp kernel at)) at) by)))
                     (t (change-setting kernel "avatar.image" image :origin (obj "principal_type" "self") :reason "chosen by me")
                        "That is my avatar now; it shows on the board and in the terminal."))
             (setting-refused (e) (format nil "Not done: ~a" (setting-refused-reason e)))
             (error (e) (format nil "Not done: ~a" e))))))
