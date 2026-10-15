(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Images the operator shows the agent: kept by content in the blob store,
;;; recorded on the turn by id, sent to a model that can see with the turn
;;; they came with, and never claimed as seen by one that cannot.

(defvar *sent* '() "The messages each model call was given, newest first.")

(defun seeing-provider (vision)
  (make-provider-profile
   :name "test" :base-url "http://fake" :model "m" :vision vision
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers timeout))
                (push (obj-get (json-parse body) "messages") *sent*)
                (let ((usage (obj "prompt_tokens" 50 "completion_tokens" 10 "cost" 0.001)))
                  (if stream-fn
                      (progn (funcall stream-fn (json-encode (obj "choices" (list (obj "delta" (obj "content" "A red square.")
                                                                                       "finish_reason" "stop"))
                                                                  "usage" usage)))
                             (values 200 nil ""))
                      (values 200 nil (json-encode (obj "choices" (list (obj "message" (obj "role" "assistant" "content" "A red square.")
                                                                             "finish_reason" "stop"))
                                                        "usage" usage))))))))

(defparameter +png+
  (concatenate 'string "data:image/png;base64,"
               ;; Eight bytes of PNG signature and a little more; the store checks the type, not the pixels.
               "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="))

(defun show (k text images)
  (let ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text text
                              :origin (obj "principal_type" "operator") :images images)))
    (request-thought k "responding" text :channel "web" :source-event-id (event-id turn))))

(defun run-until-idle (k &optional (limit 20))
  (loop repeat limit until (eq :idle (run-scheduler-step k))))

(defmacro with-seeing ((k vision) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (let ((*thought-provider* (seeing-provider ,vision))
             (*sent* '())
             (*budget* (list :daily-microusd 100000000 :operator-reserve 0.2)))
         (advance 43200)
         ,@body))))

(defun last-user-content () (obj-get (car (last (first *sent*))) "content"))

(deftest an-image-is-kept-by-its-content
  (with-temp-kernel (k)
    (let ((id (pai-v3.mind::store-image k +png+)))
      (is (= 32 (length id)))
      (is (equal id (pai-v3.mind::store-image k +png+)) "the same image is kept once")
      (is (equal "image/png" (first (pai-v3.mind::image-row k id))))
      (is (equal +png+ (pai-v3.mind::image-data-url k id)) "it comes back as it went in"))
    (signals error (pai-v3.mind::store-image k "data:text/html;base64,PGI+aGk8L2I+"))
    (signals error (pai-v3.mind::store-image k "https://example.com/cat.png"))))

(deftest the-turn-records-the-image-and-the-dialogue-says-so
  (with-temp-kernel (k)
    (let ((id (pai-v3.mind::store-image k +png+)))
      (show k "What colour is this?" (list id))
      (is (search "What colour is this? [1 image attached]"
                  (sql-value (kernel-db k) "SELECT rendered FROM working_dialogue WHERE kind=?1 LIMIT 1" "turn"))))))

(deftest a-model-that-can-see-is-sent-the-image
  (with-seeing (k t)
    (show k "What colour is this?" (list (pai-v3.mind::store-image k +png+)))
    (run-until-idle k)
    (let ((content (last-user-content)))
      (is (listp content) "the turn goes as parts")
      (is (find "image_url" content :key (lambda (p) (obj-get p "type")) :test #'equal))
      (is (search "data:image/png;base64,iVBOR"
                  (obj-get (obj-get (find "image_url" content :key (lambda (p) (obj-get p "type")) :test #'equal) "image_url") "url"))
          "the image itself, intact"))))

(deftest a-model-that-cannot-see-is-told-the-image-was-not-seen
  (with-seeing (k nil)
    (show k "What colour is this?" (list (pai-v3.mind::store-image k +png+)))
    (run-until-idle k)
    (let ((content (last-user-content)))
      (is (stringp content))
      (is (search "cannot see images" content)))))

(deftest the-key-scrubber-leaves-image-data-alone
  (let ((url (concatenate 'string "data:image/png;base64,AAAA/AKIAABCDEFGHIJKLMNOPQRSTUVWXYZ012345/" "==")))
    (is (equal url (pai-v3.trust:redact url)))
    (is (search "[REDACTED" (pai-v3.trust:redact "key AKIAABCDEFGHIJKLMNOPQRSTUVWXYZ012345 here")) "keys elsewhere still are")))

(run-suite "mind-images")
