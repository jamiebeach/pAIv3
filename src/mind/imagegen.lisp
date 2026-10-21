(in-package #:pai-v3.mind)

;;; Making images (bead v3-6nl; operator decision 2026-10-17: Runware, with
;;; Z-Image Turbo). The agent describes an image; the provider makes it; it
;;; is kept with the images the operator sends, in the encrypted blob store,
;;; and recorded as an event with its prompt and cost, so what the agent has
;;; made can be listed and its spending capped by day. The provider's key is
;;; the secret named runware, bound to its host: the agent never sees it.

(define-setting "images.generation_url"
  :type :string :default "https://api.runware.ai/v1" :group "Images"
  :description "The Runware API endpoint images are made through. Empty: no image making.")
(define-setting "images.generation_model"
  :type :string :default "runware:z-image@turbo" :group "Images"
  :description "The model images are made with, by its Runware identifier.")
(define-setting "images.reference_model"
  :type :string :default "bytedance:seedream@5.0-flash" :group "Images"
  :description "The model used when an image starts from reference images (a consistent portrait, the same character in a new scene): one that keeps identity from references, by its Runware identifier.")
(define-setting "images.generation_daily_usd"
  :type :number :min 0 :max 50 :default 1.0d0 :group "Images"
  :description "Most US dollars of image making a day (UTC). At the limit, image making waits for the next day.")

(define-event-type "image.generated"
  :schema (("image_id" :string) ("prompt" :string) ("model" :string)
           ("width" :integer) ("height" :integer) ("cost_usd" :number) ("thought_id" :integer :optional)
           ("references" :array :optional)))

(define-projection "generated_images"
  :kind :inline
  :consumes ("image.generated")
  :tables ("CREATE TABLE IF NOT EXISTS generated_images (event_id INTEGER PRIMARY KEY, image_id TEXT NOT NULL, prompt TEXT NOT NULL, model TEXT NOT NULL, cost_usd REAL NOT NULL, day TEXT NOT NULL, at TEXT NOT NULL, thought_id INTEGER)"
           "CREATE INDEX IF NOT EXISTS generated_images_day_idx ON generated_images(day)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO generated_images(event_id,image_id,prompt,model,cost_usd,day,at,thought_id) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                        (event-id e) (obj-get p "image_id") (obj-get p "prompt") (obj-get p "model")
                        (obj-get p "cost_usd") (subseq (event-at e) 0 10) (event-at e) (obj-get p "thought_id")))))

(defparameter +image-sizes+
  '(("square" 1024 1024) ("portrait" 832 1216) ("landscape" 1216 832))
  "Shapes an image may take, in pixels; multiples of 64, as the models want.")

(defparameter +reference-sizes+
  '(("square" 2048 2048) ("portrait" 1664 2496) ("landscape" 2496 1664))
  "The same shapes for the reference model, which wants at least 3.69 megapixels.")

(defparameter *references-most* 4 "Most reference images one image may start from.")

(defun images-spent-today (kernel)
  (or (sql-value (kernel-db kernel) "SELECT sum(cost_usd) FROM generated_images WHERE day=?1 LIMIT 1"
                 (subseq (now-iso8601) 0 10))
      0))

(defun %uuid-v4 ()
  (let ((b (ironclad:random-data 16)))
    (setf (aref b 6) (logior #x40 (logand (aref b 6) #x0f)) (aref b 8) (logior #x80 (logand (aref b 8) #x3f)))
    (let ((h (ironclad:byte-array-to-hex-string b)))
      (format nil "~a-~a-~a-~a-~a" (subseq h 0 8) (subseq h 8 12) (subseq h 12 16) (subseq h 16 20) (subseq h 20)))))

;; Errors the operator can act on, said as cause and action (bead v3-q78);
;; anything else keeps the provider's own message.
(defun %image-refusal (status message)
  (let ((m (string-downcase (or message ""))))
    (cond ((member status '(401 403))
           "The image was not made: the provider refused the key. The operator should check the runware secret.")
          ((or (eql status 402) (search "credit" m) (search "invoice" m) (search "balance" m))
           "The image was not made: the provider account has no credit. The operator needs to add funds to the Runware account.")
          ((eql status 429)
           "The image was not made: the provider is limiting requests. Try again in a minute.")
          ((and (integerp status) (>= status 500))
           (format nil "The image was not made: the provider failed (HTTP ~a). Try again later." status))
          (t (format nil "The image was not made (HTTP ~a): ~a" status message)))))

(defvar *image-post*
  (lambda (url headers body)
    (http-request :post url :headers headers :body body :read-timeout 120))
  "(lambda (url headers body)) -> body, status. Replaced in tests.")

(defun %reference-allowed-p (kernel thought-id image-id)
  "Whether a thought may use IMAGE-ID as a reference: any, for the operator's thoughts; for a sister's, only a
picture her audience has met or one the agent made, so a picture the operator showed in private cannot leak into one
made for a sister."
  (let ((audience (thought-reading-audience (thought-row kernel thought-id))))
    (or (null audience) (equal audience "operator")
        (sql-value (kernel-db kernel) "SELECT 1 FROM seen_images WHERE image_id=?1 AND audience=?2 LIMIT 1" image-id audience)
        (sql-value (kernel-db kernel) "SELECT 1 FROM seen_images WHERE image_id=?1 AND audience=?2 AND how=?3 LIMIT 1" image-id "operator" "made"))))

(defun generate-image (kernel thought-id prompt &key (size "square") references depicts)
  "Make an image of PROMPT. With REFERENCES (stored image ids), the reference
model makes it, keeping who or what they show. Returns the text the agent
sees."
  (let* ((url (setting kernel "images.generation_url"))
         (model (setting kernel (if references "images.reference_model" "images.generation_model")))
         (shape (assoc size (if references +reference-sizes+ +image-sizes+) :test #'equal))
         (output (if references "JPG" "PNG"))
         (limit (setting kernel "images.generation_daily_usd")))
    (cond ((equal url "") "Image making is off: images.generation_url is not set.")
          ((not (and (stringp prompt) (<= 3 (length (string-trim " " prompt)) 2000)))
           "Describe the image in 3 to 2000 characters.")
          ((null shape) (format nil "size is one of ~{~a~^, ~}." (mapcar #'first +image-sizes+)))
          ((> (length references) *references-most*)
           (format nil "At most ~d reference images." *references-most*))
          ((find-if-not (lambda (r) (image-row kernel r)) references)
           (format nil "There is no stored image ~a to start from."
                   (find-if-not (lambda (r) (image-row kernel r)) references)))
          ((>= (images-spent-today kernel) limit)
           (format nil "Today's image budget (US$~,2f) is spent; image making resumes tomorrow (UTC)." limit))
          (t
           (multiple-value-bind (body status)
               (funcall *image-post* url
                        (list (cons "Content-Type" "application/json")
                              (cons "Authorization" (list :secret "runware" "Bearer ~a")))
                        (json-encode (list (let ((task (obj "taskType" "imageInference"
                                                            "taskUUID" (%uuid-v4)
                                                            "positivePrompt" prompt "model" model
                                                            "width" (second shape) "height" (third shape)
                                                            "numberResults" 1 "outputType" "base64Data"
                                                            "outputFormat" output "includeCost" t)))
                                             (when references
                                               (setf (gethash "inputs" task)
                                                     (obj "referenceImages" (mapcar (lambda (r) (image-data-url kernel r)) references))))
                                             task))))
             (let* ((answer (ignore-errors (json-parse body)))
                    (result (and (hash-table-p answer) (first (obj-get answer "data"))))
                    (data (and result (obj-get result "imageBase64Data"))))
               (if (not (and (eql status 200) (stringp data)))
                   (%image-refusal status
                                   (let ((e (and (hash-table-p answer) (first (obj-get answer "errors")))))
                                     (or (and e (obj-get e "message"))
                                         (subseq (or body "") 0 (min 300 (length (or body "")))))))
                   (let ((id (store-image kernel (format nil "data:image/~:[png~;jpeg~];base64,~a" references data)))
                         (cost (let ((c (obj-get result "cost"))) (if (realp c) (float c 1d0) 0d0))))
                     (append! kernel "image.generated"
                              (obj "image_id" id "prompt" prompt "model" model "references" references
                                   "width" (second shape) "height" (third shape) "cost_usd" cost
                                   "thought_id" thought-id))
                     (let ((shows (%note-made-depictions kernel id depicts references thought-id)))
                       (format nil "Made image ~a (US$~,4f).~@[ It shows ~{~a~^, ~}.~] To show it, put this in your reply on a line of its own:~%![~a](/api/v2/images/~a)"
                               id cost shows (substitute #\Space #\] (subseq prompt 0 (min 80 (length prompt)))) id))))))))))

(defun %note-made-depictions (kernel image-id depicts references thought-id)
  "Link a made image to who it shows: those named in DEPICTS, and those its
REFERENCES show. Returns their names; a name that resolves to nothing is skipped."
  (let ((names '()))
    (dolist (ref depicts)
      (handler-case (setf names (append names (note-depictions kernel image-id (list ref) :how "made"
                                                                                       :source-event-id thought-id)))
        (error () nil)))
    (dolist (entity (depicted-in kernel references))
      (handler-case (progn (note-depiction kernel image-id entity :how "shown" :source-event-id thought-id)
                           (pushnew (third (graph-entity kernel entity)) names :test #'equal))
        (error () nil)))
    names))

(define-tool "generate_image"
  :description "Make an image from a description, with the image model the operator chose. prompt: what the image shows,
concretely (subject, setting, style, light, composition); size: square, portrait or landscape. The image is kept, and
the result says how to show it in a reply. Each image costs money and there is a daily budget, so make one when it is
wanted, not several to choose from. references: ids of up to 4 stored images (ones I made, or the operator sent)
showing who or what should appear again, for a consistent portrait or the same character in a new scene; a model
that keeps identity from references then makes it. Describe the new scene in the prompt. depicts: names or
entity ids of who or what the image shows, so I can find it again as a picture of them (whoever the references
show is linked anyway)."
  :summary "Make an image from a description."
  ;; Almost always at hand: the operator's thoughts and a sister's (answering her on a board),
  ;; but not a thought made of outside content (the mail digest). Spend is capped by
  ;; images.generation_daily_usd, and a sister's thought may use as references only pictures
  ;; she has met or the agent made.
  :available :operator-and-peers
  :core t
  :parameters (obj "type" "object"
                   "properties" (obj "prompt" (obj "type" "string")
                                     "size" (obj "type" "string" "enum" (mapcar #'first +image-sizes+))
                                     "references" (obj "type" "array" "items" (obj "type" "string"))
                                     "depicts" (obj "type" "array" "items" (obj "type" "string")))
                   "required" (list "prompt"))
  :run (lambda (kernel thought-id args)
         (generate-image kernel thought-id (obj-get args "prompt") :size (or (obj-get args "size") "square")
                         :references (let ((r (obj-get args "references")))
                                       (and (consp r)
                                            (remove-if-not (lambda (id) (and (stringp id) (%reference-allowed-p kernel thought-id id))) r)))
                         :depicts (let ((d (obj-get args "depicts"))) (and (consp d) (remove-if-not #'stringp d))))))
