(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Making images: one Runware task per call, the key named and never seen,
;;; the image kept with the others, and spending capped by day.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defvar *sent* '())

;; A 1x1 PNG.
(defparameter +png-b64+ "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")

(defun runware (&key (cost 0.004) (status 200) answer)
  (lambda (url headers body)
    (push (list url headers (json-parse body)) *sent*)
    (values (json-encode (or answer (obj "data" (list (obj "taskType" "imageInference" "imageBase64Data" +png-b64+ "cost" cost)))))
            status)))

(deftest an-image-is-made-kept-and-shown
  (with-temp-kernel (k)
    (let* ((*sent* '()) (pai-v3.mind::*image-post* (runware))
           (said (pai-v3.mind::generate-image k nil "a lighthouse at dusk, watercolor" :size "portrait")))
      (is (search "Made image" said))
      (let* ((sent (first *sent*)) (task (first (third sent))))
        (is (equal "https://api.runware.ai/v1" (first sent)))
        (is (equal '(:secret "runware" "Bearer ~a") (cdr (assoc "Authorization" (second sent) :test #'equal)))
            "the key is named for the egress guard, never written into the request")
        (is (equal "runware:z-image@turbo" (obj-get task "model")))
        (is (equal '(832 1216) (list (obj-get task "width") (obj-get task "height")))))
      (let ((id (subseq said (+ 11 (search "Made image " said)) (+ 11 32 (search "Made image " said)))))
        (is (equal "image/png" (first (pai-v3.mind::image-row k id))) "kept with the operator's images")
        (is (search (format nil "](/api/v2/images/~a)" id) said) "and the reply can show it"))
      (is (< 0.0039 (pai-v3.mind::images-spent-today k) 0.0041)))))

(deftest image-making-stops-at-the-daily-budget
  (with-temp-kernel (k)
    (change-setting k "images.generation_daily_usd" 0.01d0 :origin *operator*)
    (let ((*sent* '()) (pai-v3.mind::*image-post* (runware :cost 0.006)))
      (is (search "Made image" (pai-v3.mind::generate-image k nil "first")))
      (is (search "Made image" (pai-v3.mind::generate-image k nil "second")))
      (is (search "budget" (pai-v3.mind::generate-image k nil "third")) "spent past the limit, the next waits")
      (is (= 2 (length *sent*)) "and nothing more is sent"))))

(deftest a-refused-image-says-why-and-costs-nothing
  (with-temp-kernel (k)
    (let ((*sent* '())
          (pai-v3.mind::*image-post* (runware :status 400 :answer (obj "errors" (list (obj "message" "Invalid model"))))))
      (is (search "Invalid model" (pai-v3.mind::generate-image k nil "anything")))
      (is (zerop (pai-v3.mind::images-spent-today k))))
    (let ((*sent* '())
          (pai-v3.mind::*image-post* (runware :status 400 :answer (obj "errors" (list (obj "message" "External inference tasks require a paid invoice or at least $5 credit. https://my.runware.ai/wallet"))))))
      (let ((said (pai-v3.mind::generate-image k nil "anything")))
        (is (search "no credit" said) "a known cause is said plainly, with what to do (bead v3-q78)")
        (is (not (search "https://" said)) "not the provider's raw text")))
    (let ((*sent* '()) (pai-v3.mind::*image-post* (runware :status 401 :answer (obj "errors" (list (obj "message" "bad key"))))))
      (is (search "check the runware secret" (pai-v3.mind::generate-image k nil "anything"))))
    (let ((*sent* '()) (pai-v3.mind::*image-post* (runware)))
      (is (search "size is one of" (pai-v3.mind::generate-image k nil "anything" :size "huge")))
      (is (search "3 to 2000" (pai-v3.mind::generate-image k nil "")))
      (is (null *sent*) "bad requests are not sent"))))

(run-suite "mind-imagegen")
