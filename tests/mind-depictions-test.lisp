(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.recall)

;;; Pictures in the graph (bead v3-3za): a fact can cite the picture it was
;;; read from, appearance has its own relation, and which images show whom
;;; is a lookup, carried into recall.

(defparameter +a-png+
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
(defparameter +b-png+
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

(defun me-formed (k) (list (lineage-hop (kernel-agent-id k) "formed")))

(deftest a-fact-can-cite-the-picture-it-was-read-from
  (with-temp-kernel (k)
    (let* ((nova (form-entity k :kind "person" :name "Nova" :lineage (me-formed k)))
           (look (form-entity k :kind "attribute_value" :name "blonde, blue eyes, freckles" :lineage (me-formed k)))
           (photo (pai-v3.mind::store-image k +a-png+)))
      (is (admit-fact k :subject nova :predicate "has_appearance" :object look
                        :text "Nova has blonde hair, blue eyes and freckles."
                        :evidence (list (obj "kind" "image" "quote" "blonde, blue eyes, freckles"
                                             "source_ref" (format nil "image:~a" photo)))
                        :lineage (me-formed k))
          "appearance has its own relation, and a picture is evidence")
      (is (search "stored image"
                  (handler-case (progn (admit-fact k :subject nova :predicate "has_appearance" :object look :text "x"
                                                     :evidence (list (obj "kind" "image" "quote" "x" "source_ref" "image:0000"))
                                                     :lineage (me-formed k))
                                       "admitted")
                    (graph-refused (e) (princ-to-string e))))
          "a picture that is not stored cannot be cited"))))

(deftest which-pictures-show-whom-is-a-lookup
  (with-temp-kernel (k)
    (let* ((nova (form-entity k :kind "person" :name "Nova" :lineage (me-formed k)))
           (twin (form-entity k :kind "person" :name "Novah" :lineage (me-formed k)))
           (photo (pai-v3.mind::store-image k +a-png+))
           (tool (pai-v3.mind::tool-run (gethash "note_picture" *tools*))))
      (is (search "shows Nova" (funcall tool k nil (obj "image" photo "shows" (list "Nova")))))
      (is (search "Not noted" (funcall tool k nil (obj "image" photo "shows" (list "Nobody")))) "an unknown name is refused")
      (is (equal (list photo) (pictures-of k nova)))
      (is (equal (list nova) (depicted-in k (list photo))))
      (is (search (format nil "Pictures: ~a" photo)
                  (funcall (pai-v3.mind::tool-run (gethash "knowledge" *tools*)) k nil (obj "action" "entity" "id" nova)))
          "the entity card shows them")
      (is (search photo (render-impression (impression k nova "operator"))) "and so does recall")
      (is (null (getf (impression k nova "peer:someone") :pictures)) "the operator's pictures are not a peer's")
      (merge-entities k twin nova "the same person")
      (pai-v3.mind::note-depiction k photo twin)
      (is (equal (list photo) (pictures-of k nova)) "pictures follow a merge"))))

(deftest a-made-image-knows-who-it-shows
  (with-temp-kernel (k)
    (let* ((nova (form-entity k :kind "person" :name "Nova" :lineage (me-formed k)))
           (calls 0)
           (pai-v3.mind::*image-post*
             (lambda (url headers body)
               (declare (ignore url headers body))
               (incf calls)
               (values (json-encode (obj "data" (list (obj "imageBase64Data"
                                                           (subseq (if (= calls 1) +a-png+ +b-png+) (length "data:image/png;base64,"))
                                                           "cost" 0.001))))
                       200))))
      (let* ((first-said (pai-v3.mind::generate-image k nil "a portrait of Nova" :depicts (list "Nova")))
             (portrait (subseq first-said 11 43)))
        (is (search "It shows Nova" first-said))
        (is (equal (list portrait) (pictures-of k nova)))
        (let* ((second-said (pai-v3.mind::generate-image k nil "the same woman by the sea" :references (list portrait)))
               (scene (subseq second-said 11 43)))
          (is (search "It shows Nova" second-said) "who the reference shows is linked anyway")
          (is (member scene (pictures-of k nova) :test #'equal)))))))

(run-suite "mind-depictions")
