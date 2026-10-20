(in-package #:pai-v3.api)

;;; Carried items, the api side (src/mind/carried.lisp is the store and the
;;; tool): looking at what has come due, and the page where the operator sees
;;; and changes what is carried.
;;;
;;; An item whose time has come is claimed (marked considered) before its
;;; thought is requested, so a crash loses the thought and never repeats it.
;;; The thought is the operator's own and a noticing one: it can notify, open
;;; docket work, carry the item on to a later time, or close it. Quiet hours
;;; hold due items back; they are looked at once the hours end.

(defun %consideration-prompt (kernel item)
  (declare (ignore kernel))
  (pai-v3.mind::carried-consideration-prompt item))

(defun consider-carried (kernel)
  "Look at items whose time has come. Returns how many were taken up."
  (unless (%quiet-now-p kernel (notify-policy kernel))
    (let ((due (carried-due kernel :limit 3)))
      (dolist (item due (length due))
        (append! kernel "carried.considered" (obj "id" (obj-get item "id")))
        (request-thought kernel "noticing" (%consideration-prompt kernel item) :audience "operator")))))

(pushnew 'consider-carried *periodic-hooks*)

;;; The page

(defun carried-route (agent method path origin)
  "GET /api/carried: what is carried, and what was closed lately. POST: add,
revise or close, as the operator."
  (declare (ignore origin))
  (let ((k (agent-kernel agent)))
    (handler-case
        (cond
          ((and (eq method :post) (string= path "/api/carried"))
           (let* ((b (body-json)) (action (obj-get b "action")))
             (flet ((got (key) (%given (obj-get b key))))
               (multiple-value-bind (id why-not)
                   (cond ((equal action "add")
                          (carry-add k :what (got "what") :why (got "why") :condition (got "condition")
                                       :consider-at (got "consider_at") :due-at (got "due_at") :by "operator"
                                       :source "added by the operator"))
                         ((equal action "revise")
                          (carry-revise k (or (got "id") "") :what (got "what") :why (got "why") :condition (got "condition")
                                                             :consider-at (got "consider_at") :due-at (got "due_at") :note (got "note")))
                         ((equal action "close") (carry-close k (or (got "id") "") (got "state") (got "note")))
                         (t (values nil "action is add, revise or close")))
                 (if (and id (or (equal action "add") (null why-not)))
                     (reply 200 (obj "ok" t "id" id))
                     (reply 400 (obj "error" (or why-not "not done"))))))))
          ((string= path "/api/carried")
           (reply 200 (obj "items" (carried-items k :limit 60)
                           "closed" (carried-items k :state "done" :limit 10)
                           "dropped" (carried-items k :state "dropped" :limit 10)
                           "zone" (setting k "time.zone"))))
          (t (reply 404 (obj "error" "no such route"))))
      (error (e) (reply 400 (obj "error" (princ-to-string e)))))))
