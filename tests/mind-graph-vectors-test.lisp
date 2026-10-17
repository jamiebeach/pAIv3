(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Fact embeddings (design note interaction-recall §7a): each live fact's
;;; statement is embedded, found by meaning, and gone once retracted.

(defun words-vector (text)
  (let ((v (make-list 768 :initial-element 0.0)))
    (loop with start = 0
          for end = (position-if-not #'alpha-char-p text :start start)
          for word = (string-downcase (subseq text start (or end (length text))))
          when (plusp (length word)) do (incf (nth (mod (sxhash word) 768) v) 1.0)
          while end do (setf start (1+ end)))
    v))

(defun formed (k) (list (lineage-hop (kernel-agent-id k) "formed")))
(defun said (text) (list (obj "kind" "original-utterance" "quote" text)))

(deftest facts-are-embedded-and-found-by-meaning
  (with-temp-kernel (k)
    (let ((*embedder* (lambda (texts) (mapcar #'words-vector texts)))
          (pai-v3.mind::*embedding-cache* '()))
      (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
            pai-v3.mind::*embedder-resting-until* 0)
      (remhash k pai-v3.mind::*embedding-backfill-at*)
      (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (formed k)))
             (lee (form-entity k :kind "person" :name "Lee" :lineage (formed k)))
             (lake (form-entity k :kind "place" :name "the lake" :lineage (formed k)))
             (sister (admit-fact k :subject lee :predicate "sibling_of" :object sam :text "Lee is Sam's sister."
                                   :evidence (said "my sister Lee") :lineage (formed k)))
             (likes (admit-fact k :subject sam :predicate "likes" :object lake :text "Sam loves swimming in the lake."
                                  :evidence (said "I love the lake") :lineage (formed k))))
        (is (search "sister" (pai-v3.mind::fact-statement k sister)) "the statement as the agent reads it")
        (is (= 2 (pai-v3.mind::request-missing-embeddings k)) "both facts are queued")
        (loop repeat 200 until (eq :idle (run-scheduler-step k)))
        (let ((near (pai-v3.mind::facts-near k (embed "writing to my sister Lee"))))
          (is (equal sister (first (first near))) "the nearest in meaning comes first")
          (is (search "lake" (third (second near)))))
        (append! k "graph.fact.retracted" (obj "fact_id" likes "reason" "test"))
        (is (null (find likes (pai-v3.mind::facts-near k (embed "swimming in the lake")) :key #'first :test #'equal))
            "a retracted fact no longer comes to mind")
        (is (zerop (pai-v3.mind::request-missing-embeddings k :every 0)) "and is not embedded again")))))

(deftest formation-by-meaning-supports-and-suggests
  (with-temp-kernel (k)
    (let ((*embedder* (lambda (texts) (mapcar #'words-vector texts)))
          (pai-v3.mind::*embedding-cache* '()))
      (setf *budget* (list :daily-microusd 100000000 :operator-reserve 0.2)
            pai-v3.mind::*embedder-resting-until* 0)
      (remhash k pai-v3.mind::*embedding-backfill-at*)
      (let* ((sam (form-entity k :kind "person" :name "Sam" :lineage (formed k)))
             (samuel (form-entity k :kind "person" :name "Samuel" :lineage (formed k)))
             (lee (form-entity k :kind "person" :name "Lee" :lineage (formed k)))
             (lisbon (form-entity k :kind "place" :name "Lisbon" :lineage (formed k)))
             (sister (admit-fact k :subject lee :predicate "sibling_of" :object sam :text "Lee is Sam's sister."
                                   :evidence (said "my sister Lee") :lineage (formed k))))
        (admit-fact k :subject lee :predicate "related_to" :object sam :text "Lee is Sam's older sister."
                      :evidence (said "Lee and Sam") :lineage (formed k))
        (admit-fact k :subject sam :predicate "lives_at" :object lisbon :text "Sam lives in Lisbon by the river."
                      :evidence (said "I live in Lisbon") :lineage (formed k))
        (admit-fact k :subject samuel :predicate "lives_at" :object lisbon :text "Samuel lives in Lisbon by the river."
                      :evidence (said "he lives in Lisbon") :lineage (formed k))
        (pai-v3.mind::request-missing-embeddings k :every 0)
        (loop repeat 200 until (eq :idle (run-scheduler-step k)))
        (is (equal sister (pai-v3.mind::near-duplicate-fact k "Lee is Sam's sister!" lee sam "assertion" "positive"))
            "the same thing in other words supports the fact already there")
        (is (null (pai-v3.mind::near-duplicate-fact k "Lee is Sam's sister." lee sam "assertion" "negative"))
            "never across polarity: a denial is not support")
        (is (null (pai-v3.mind::near-duplicate-fact k "Lee is Sam's sister." sam lee "assertion" "positive"))
            "nor between other ends")
        (let ((text (format nil "~{~a~%~}" (pai-v3.mind::graph-suggestions k))))
          (is (search "perhaps predicate sibling_of" text) "a vague relation that reads like a precise one")
          (is (or (search "Sam and Samuel may be one person" text) (search "Samuel and Sam may be one person" text))
              "entities whose facts read alike"))))))

(run-suite "mind-graph-vectors")
