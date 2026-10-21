(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Seeing again: what was shown stays listed for a while, captioned, and
;;; the agent can look at it again with look_at.

(defparameter +shown-png+
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")

(defun seeing-provider (text)
  (make-provider-profile
   :name "eyes" :base-url "http://fake" :model "m" :vision t
   :transport (lambda (url headers body &key stream-fn timeout)
                (declare (ignore url headers body stream-fn timeout))
                (values 200 nil
                        (json-encode (obj "choices" (list (obj "message" (obj "role" "assistant" "content" text)
                                                               "finish_reason" "stop"))
                                          "usage" (obj "prompt_tokens" 10 "completion_tokens" 5)))))))

(defun seen-slot (k audience)
  (format nil "~{~a~}" (mapcar #'first (funcall (gethash :seen pai-v3.mind::*slot-sources*) k (list :audience audience) 400))))

(deftest a-picture-stays-in-view-captioned
  (with-temp-kernel (k)
    (let ((id (pai-v3.mind::store-image k +shown-png+)))
      (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text "Find the hammer."
                      :images (list id) :origin (obj "principal_type" "operator"))
      (is (search id (seen-slot k "operator")) "a picture just sent is listed")
      (is (search "not yet described" (seen-slot k "operator")))
      (is (string= "" (seen-slot k "peer:someone")) "only for the audience that saw it")
      (pai-v3.mind::request-captions k)
      (is (work-pending-p k "image.caption" id) "captioned once, in the background")
      (let ((*thought-provider* (seeing-provider "A busy cartoon market scene with tools and food.")))
        (pai-v3.mind::caption-image k (list :entity id :id nil)))
      (is (search "busy cartoon market" (seen-slot k "operator")) "with its caption")
      (pai-v3.mind::request-captions k)
      (is (not (work-pending-p k "image.caption" (format nil "~a-x" id))) "nothing more to caption"))))

(deftest a-made-image-is-described-by-its-prompt
  (with-temp-kernel (k)
    (let ((id (pai-v3.mind::store-image k +shown-png+)))
      (append! k "image.generated" (obj "image_id" id "prompt" "a lighthouse at dusk" "model" "m"
                                        "width" 1 "height" 1 "cost_usd" 0.001d0))
      (is (search "a lighthouse at dusk (I made it" (seen-slot k "operator")))
      (pai-v3.mind::request-captions k)
      (is (not (work-pending-p k "image.caption" id)) "no model is asked"))))

(deftest a-picture-sent-to-a-sister-is-one-she-has-met
  (with-temp-kernel (k)
    (let ((id (pai-v3.mind::store-image k +shown-png+)))
      (append! k "image.generated" (obj "image_id" id "prompt" "a calm portrait in dark clothes" "model" "m" "width" 1 "height" 1 "cost_usd" 0.001d0))
      (is (null (search "calm portrait" (seen-slot k "peer:sister1"))) "a picture made for the operator is not yet in view for a sister")
      (append! k "fleet.op" (obj "kind" "publication-intent" "operation_id" "op-1" "peer_id" "sister1"
                                 "body" (obj "request" (obj "text" "Here is mine." "image_ids" (list id)))))
      (is (search "calm portrait" (seen-slot k "peer:sister1")) "once posted to her, a thought answering her sees it, with its caption")
      (is (null (search "calm portrait" (seen-slot k "peer:sister2"))) "and only the sister it was sent to")
      (is (sql-value (kernel-db k) "SELECT 1 FROM seen_images WHERE image_id=?1 AND audience=?2 AND how=?3 LIMIT 1" id "peer:sister1" "shared")))))

(deftest looking-again-puts-the-picture-in-view
  (with-temp-kernel (k)
    (let* ((id (pai-v3.mind::store-image k +shown-png+))
           (thought (request-thought k "work" "find the pickle" :audience "operator"))
           (peer-thought (request-thought k "work" "look" :audience "peer:someone"))
           (look (pai-v3.mind::tool-run (gethash "look_at" *tools*))))
      (is (search "in front of me" (funcall look k thought (obj "images" (list id)))))
      (is (equal (list id) (pai-v3.mind::thought-images k (thought-row k thought))) "the next step sees it")
      (is (search "in front of me" (funcall look k peer-thought (obj "images" (list id))))
          "a peer-triggered private thought can inspect its own stored picture")
      (let ((external (request-thought k "work" "untrusted source" :audience "external")))
        (is (search "Looking at" (funcall look k external (obj "images" (list id))))
            "an external stimulus is considered by the same private thinker"))
      (let ((sent (pai-v3.mind::with-images k (list (obj "role" "user" "content" "find it")
                                                    (obj "role" "tool" "tool_call_id" "c" "content" "Looking."))
                                             (list id) (seeing-provider "x"))))
        (is (equal "tool" (obj-get (second sent) "role")) "a tool result keeps its text")
        (is (equal "user" (obj-get (third sent) "role")) "the picture comes in a message of its own")
        (is (consp (obj-get (third sent) "content")))))))

(run-suite "mind-seeing")
