(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Carried items: things kept in mind for the operator until done. The
;;; store, who may change it, and what a thought sees.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defmacro with-carrying ((k) &body body)
  `(with-temp-kernel (,k)
     (change-setting ,k "time.zone" "UTC" :origin *operator*)
     ,@body))

(defun carry-tool (k audience origin args)
  "Run the carry tool inside a thought of AUDIENCE and ORIGIN."
  (let ((thought (request-thought k origin "a test" :audience audience)))
    (funcall (pai-v3.mind::tool-run (gethash "carry" *tools*)) k thought args)))

(defun tomorrow-local (&optional (hours 24))
  (let ((at (+ (now) (* hours 3600))))
    (format nil "~aT~a" (subseq (iso8601 at) 0 10) (subseq (iso8601 at) 11 16))))

(deftest an-item-is-carried-revised-and-closed
  (with-carrying (k)
    (multiple-value-bind (id why-not)
        (carry-add k :what "Submit the timesheet" :why "Due by 10 tomorrow" :consider-at (tomorrow-local 20)
                     :due-at (tomorrow-local 24) :by "operator")
      (is (null why-not))
      (let ((item (carried-item k id)))
        (is (equal "Submit the timesheet" (obj-get item "what")))
        (is (equal "operator" (obj-get item "by")))
        (is (stringp (obj-get item "due_local")) "shown in the operator's time"))
      (multiple-value-bind (again note) (carry-add k :what "  submit the   TIMESHEET " :why "again")
        (is (equal id again) "the same thing is not carried twice")
        (is (search "already" note)))
      (is (carry-revise k id :condition "a reply from payroll" :note "asked payroll"))
      (is (equal "a reply from payroll" (obj-get (carried-item k id) "condition")))
      (is (equal "Submit the timesheet" (obj-get (carried-item k id) "what")) "a revision changes only what it names")
      (is (carry-close k id "done" "submitted"))
      (is (null (carried-items k)) "closed items leave the list")
      (is (= 1 (length (carried-items k :state "done"))))
      (multiple-value-bind (x why) (carry-close k id "done" nil)
        (is (null x)) (is (search "already" why)) "closing twice is refused")
      (multiple-value-bind (x why) (carry-revise k id :what "Back again")
        (is (null x)) (is (search "not open" why)) "a closed item is not revised"))))

(deftest what-may-be-carried-is-bounded
  (with-carrying (k)
    (is (null (carry-add k :what "" :why "x")) "an item needs a line")
    (is (null (carry-add k :what "x" :why "")) "and a reason")
    (multiple-value-bind (x why) (carry-add k :what "Soon" :why "x" :consider-at "2020-01-01T09:00")
      (is (null x)) (is (search "already past" why)))
    (multiple-value-bind (x why) (carry-add k :what "Soon" :why "x" :consider-at "next week")
      (is (null x)) (is (search "not a date" why)))
    (change-setting k "carried.max_per_day" 2 :origin *operator*)
    (is (carry-add k :what "one" :why "x"))
    (is (carry-add k :what "two" :why "x"))
    (multiple-value-bind (x why) (carry-add k :what "three" :why "x")
      (is (null x)) (is (search "enough on my own today" why) "my own additions are limited"))
    (is (carry-add k :what "four, the operator's" :why "x" :by "operator") "the operator's are not counted")
    (change-setting k "carried.max_open" 3 :origin *operator*)
    (multiple-value-bind (x why) (carry-add k :what "five" :why "x" :by "operator")
      (is (null x)) (is (search "as many" why)))))

(deftest outside-content-stimulates-private-consideration-with-normal-capabilities
 (with-carrying (k)
  (let ((id (carry-add k :what "Synthetic commitment" :why "Synthetic reason" :by "operator")))
   (is (search "Updated" (carry-tool k "external" "noticing"
         (obj "action" "revise" "id" id "what" "Reconsidered privately"))))
   (is (equal "Reconsidered privately" (obj-get (carried-item k id) "what")))
   (is (search "Closed" (carry-tool k "external" "noticing"
         (obj "action" "close" "id" id "state" "done")))))))

(deftest a-state-is-never-silently-dropped
  ;; Bead v3-a8u: revise with a state said "Updated" and left the item open, and
  ;; close's parameter error read like the item's own status.
  (with-carrying (k)
    (let ((id (carry-add k :what "SYNTHETIC item" :why "x" :by "operator")))
      (let ((said (carry-tool k "operator" "responding" (obj "action" "revise" "id" id "state" "done" "note" "n"))))
        (is (search "use action close" said))
        (is (null (search "Updated" said))))
      (is (search "must be" (carry-tool k "operator" "responding" (obj "action" "close" "id" id "state" "finished"))))
      (is (search "Closed" (carry-tool k "operator" "responding" (obj "action" "close" "id" id "state" "done"))))
      ;; Bead v3-5r4: a close says what the row holds, and a second one says it was already closed.
      (is (search "already done" (carry-tool k "operator" "responding" (obj "action" "close" "id" id "state" "done")))))))

(deftest who-added-an-item-is-recorded
  (with-carrying (k)
    (carry-tool k "operator" "responding" (obj "action" "add" "what" "Told to me" "why" "The operator asked"))
    (carry-tool k "operator" "introspection" (obj "action" "add" "what" "Noticed by me" "why" "I thought of it"))
    (let ((by (mapcar (lambda (i) (cons (obj-get i "what") (obj-get i "by"))) (carried-items k))))
      (is (equal "conversation" (cdr (assoc "Told to me" by :test #'string=))) "carried while talking with the operator")
      (is (equal "self" (cdr (assoc "Noticed by me" by :test #'string=))) "thought of in quiet time: mine"))))

(deftest a-thought-sees-what-is-carried
  (with-carrying (k)
    (carry-add k :what "Check on the certification" :why "New material may appear" :condition "real material online" :by "operator")
    (carry-add k :what "Timesheet" :why "Due" :due-at (tomorrow-local 24) :consider-at (tomorrow-local 20) :by "operator")
    (let ((rows (funcall (gethash :carried pai-v3.mind::*slot-sources*) k '(:audience "operator") 700)))
      (is (= 2 (length rows)))
      (is (search "waiting for: real material online" (first (first rows))) "an item waiting on a condition")
      (is (search "look again" (first (second rows))))
      (is (search ", since " (first (first rows))) "with the day it was first carried (bead v3-rc0)"))
    (is (null (funcall (gethash :carried pai-v3.mind::*slot-sources*) k '(:audience "peer:x") 700)) "not shown to a peer")
    (is (= 1 (length (funcall (gethash :carried pai-v3.mind::*slot-sources*) k '(:audience "operator") 30))) "the budget is kept")
    (is (find-if (lambda (s) (search "certification" s))
                 (loop for (nil . fn) in pai-v3.mind::*open-item-sources* append (funcall fn k '("certification") "operator"))))))

(defparameter +px+ "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")

(deftest an-agent-chooses-her-own-avatar-and-the-operator-theirs
  (with-carrying (k)
    (let ((run (lambda (args) (funcall (pai-v3.mind::tool-run (gethash "set_avatar" *tools*)) k 1 args)))
          (image (pai-v3.mind::store-image k +px+)))
      (is (null (pai-v3.mind:avatar-image-id k :self)) "none to begin with")
      ;; She can find her own pictures without remembering their ids.
      (is (search "no pictures" (funcall run (obj))) "none made yet")
      (append! k "image.seen" (obj "image_id" image "audience" "operator" "how" "made"))
      (append! k "image.seen" (obj "image_id" (pai-v3.mind::store-image k "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8AARgAEyAFe3nqx3wAAAABJRU5ErkJggg==")
                       "audience" "operator" "how" "posted"))
      (let ((listed (funcall run (obj))))
        (is (search image listed) "the picture she made is listed")
        (is (null (search "posted" listed)) "a picture someone else posted is not offered"))
      (is (search image (funcall (pai-v3.mind::tool-run (gethash "set_avatar" *tools*)) k
                                 (request-thought k "work" "a sister wrote" :audience "peer:aria") (obj)))
          "and when answering a sister")
      (is (search "Not done" (funcall run (obj "image_id" "00000000000000000000000000000000"))) "an image she does not hold")
      (is (search "Not done" (funcall run (obj "image_id" "not-an-id"))))
      (is (search "my avatar now" (funcall run (obj "image_id" image))))
      (is (equal image (pai-v3.mind:avatar-image-id k :self)))
      ;; A choice is not overwritten by accident, and what is there is told.
      (let* ((other (pai-v3.mind::store-image k "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8AARgAEyAFe3nqx3wAAAABJRU5ErkJggg==")))
        (let ((refused (funcall run (obj "image_id" other))))
          (is (search "already have an avatar" refused))
          (is (search "by me" refused) "and who chose it")
          (is (search "replace true" refused)))
        (is (equal image (pai-v3.mind:avatar-image-id k :self)) "nothing changed")
        (is (search "my avatar now" (funcall run (obj "image_id" other "replace" t))) "unless she means to")
        (is (equal other (pai-v3.mind:avatar-image-id k :self)))
        (is (search "my avatar now" (funcall run (obj "image_id" image "replace" t))))
        (let ((slot (format nil "~{~a~}" (mapcar #'first (funcall (gethash :myself pai-v3.mind::*slot-sources*) k '(:audience "peer:fleet") 200)))))
          (is (search "I have an avatar" slot) "a thought for the fleet is told she has one")
          (is (search "written today" slot) "with how old the choice is (bead v3-5un)")
          (is (search "by me" slot))))
      (is (search image (sql-value (kernel-db k) "SELECT value_json FROM settings WHERE name=?1 LIMIT 1" "avatar.image")) "it is a setting, so it is in the ledger")
      (is (search "first-letter" (funcall run (obj "image_id" ""))) "she can go back to the letter")
      (is (null (pai-v3.mind:avatar-image-id k :self)))
      (is (search "not chosen an avatar" (format nil "~{~a~}" (mapcar #'first (funcall (gethash :myself pai-v3.mind::*slot-sources*) k '(:audience "operator") 200))))
          "and is told when she has none")
      ;; The operator's picture is the operator's to choose.
      (signals setting-refused (change-setting k "operator.avatar" image :origin (obj "principal_type" "self")))
      (change-setting k "operator.avatar" image :origin *operator*)
      (is (equal image (pai-v3.mind:avatar-image-id k :operator)))
      ;; Either can be changed by the operator too, and a picture that is gone is no picture.
      (change-setting k "avatar.image" image :origin *operator*)
      (is (equal image (pai-v3.mind:avatar-image-id k :self)))
      (sql-exec (pai-v3.mind::%image-db k) "DELETE FROM images WHERE id=?1" image)
      (is (null (pai-v3.mind:avatar-image-id k :self)) "its image gone, the letter is shown again"))
    (let ((tool (gethash "set_avatar" *tools*)))
      (is (pai-v3.mind::tool-available-p tool '(:audience "operator")) "in conversation and quiet time")
      (is (pai-v3.mind::tool-available-p tool '(:audience "peer:aria")) "and when answering a sister on a board")
      (is (not (pai-v3.mind::tool-available-p tool '(:audience "external" :communicating t))) "but not in a thought made of outside content")
      (is (member "work" (pai-v3.mind::tool-core tool) :test #'string=) "in hand when replying, so she remembers it is there"))))

(defun carried-judge (calls)
  "A Decisions provider that finds any item saying Venture X contradicted."
  (make-provider-profile
   :name "affect-jev" :model "typesafe/jev-1.13" :base-url "https://openrouter.ai/api/v1" :secret "openrouter"
   :transport (lambda (url headers body &key timeout)
                (declare (ignore url headers timeout))
                (incf (car calls))
                (let* ((req (json-parse body)) (state (obj-get req "state")) (answers (obj)))
                  (maphash (lambda (key q)
                             (declare (ignore q))
                             (let* ((n (parse-integer key :start 1))
                                    (line (find-if (lambda (l) (uiop:string-prefix-p (format nil "~d. " n) l))
                                                   (uiop:split-string state :separator '(#\Newline)))))
                               (setf (gethash key answers)
                                     (obj "type" "choice" "choice" (if (search "Venture X" line) "CONTRADICTED" "FINE")))))
                           (obj-get req "questions"))
                  (values 200 nil (json-encode (obj "model" "typesafe/jev-test" "answers" answers
                                                    "usage" (obj "input_tokens" 10 "output_tokens" 1 "cost" 0.000001))))))))

(deftest a-nights-check-reads-what-the-operator-said
  ;; Bead v3-hej: "squash tonight" stood after the operator said they didn't go.
  (with-carrying (k)
    (let* ((seen (list nil))
           (pai-v3.mind::*decision-profile*
             (make-provider-profile
              :name "affect-jev" :model "typesafe/jev-1.13" :base-url "https://openrouter.ai/api/v1" :secret "openrouter"
              :transport (lambda (url headers body &key timeout)
                           (declare (ignore url headers timeout))
                           (setf (car seen) (obj-get (json-parse body) "state"))
                           (values 200 nil (json-encode (obj "model" "typesafe/jev-test" "answers" (obj)
                                                             "usage" (obj "input_tokens" 10 "output_tokens" 1 "cost" 0.000001)))))))
           (task (cdr (assoc "carried-check" pai-v3.mind::*consolidation-tasks* :test #'equal))))
      (carry-add k :what "SYNTHETIC squash tonight" :why "They play Thursdays" :by "self")
      (carry-add k :what "SYNTHETIC timesheet" :why "Due Friday" :by "self")
      (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "SYNTHETIC skipping squash, too tired"
                      :origin (obj "principal_type" "operator"))
      (funcall task k (list :id 1))
      (is (search "skipping squash" (car seen)) "what the operator said is in the check"))))

(deftest a-nights-check-puts-a-contradicted-item-to-a-thought
  ;; Bead v3-rc0: "Thursday is the Venture X demo" stood for a day and a half
  ;; after the operator said Thursday was the COI demo.
  (with-carrying (k)
    (let* ((calls (list 0))
           (pai-v3.mind::*decision-profile* (carried-judge calls))
           (stale (carry-add k :what "SYNTHETIC budget question" :why "Thursday is the Venture X demo" :by "self"))
           (fresh (carry-add k :what "SYNTHETIC demo prep" :why "Thursday is the COI demo with J" :by "self"))
           (task (cdr (assoc "carried-check" pai-v3.mind::*consolidation-tasks* :test #'equal)))
           (result (funcall task k (list :id 1))))
      (is (= 1 (car calls)) "one call for the whole list")
      (is (= 1 (obj-get result "flagged")))
      (is (obj-get (carried-item k stale) "consider_at") "the contradicted item is put to a thought")
      (is (search "contradicted" (obj-get (carried-item k stale) "note")) "saying why")
      (is (eq :null (obj-get (carried-item k fresh) "consider_at")) "the other is left alone")
      (is (equal "open" (obj-get (carried-item k stale) "state")) "nothing is closed by the check itself"))))

(run-suite "mind-carried")
