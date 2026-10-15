(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Exact recall latency at 20k memories of 768 dimensions (about twice the
;;; larger agent's memory). Not part of the default run.

(defun random-vector () (loop repeat 768 collect (- (random 2.0) 1.0)))

(deftest exact-recall-at-twenty-thousand-memories
  (with-temp-kernel (k)
    (let ((t0 (get-internal-real-time)))
      (loop for batch below 40
            do (append-batch!
                k (loop for i below 500
                        for id = (format nil "m~d-~d" batch i)
                        append (list (list "memory.formed"
                                           (obj "memory_id" id "kind" "episode"
                                                "content" (format nil "memory ~a with some ordinary words" id)
                                                "sensitivity" (if (zerop (mod i 5)) "public" "operator-only")
                                                "owner" "operator" "strength" 0.5))
                                     (list "memory.embedded"
                                           (obj "memory_id" id "model" "nomic-embed-text"
                                                "vector" (base64url-encode (encode-vector (random-vector)))))))))
      (format t "~&  built 20k memories in ~,1f s~%" (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
    (let ((times '()) (heap-before (progn (sb-ext:gc :full t) (sb-kernel:dynamic-usage))))
      (dotimes (i 50)
        (let ((q (encode-vector (random-vector)))
              (start (get-internal-real-time)))
          (recall k q "operator" :k 12)
          (push (/ (* 1000.0 (- (get-internal-real-time) start)) internal-time-units-per-second) times)))
      (sb-ext:gc :full t)
      (let ((sorted (sort times #'<)))
        (format t "~&  recall ms: p50 ~,1f  p95 ~,1f  max ~,1f; heap growth ~,1f MB~%"
                (nth 25 sorted) (nth 47 sorted) (car (last sorted))
                (/ (- (sb-kernel:dynamic-usage) heap-before) 1048576.0))
        (is (< (nth 47 sorted) 150) "p95 recall under 150 ms")))
    ;; The half-precision shortlist must not change what is recalled: compare
    ;; with an exhaustive exact cosine scan over the original vectors.
    (let ((same 0))
      (dotimes (i 20)
        (let* ((q (encode-vector (random-vector)))
               (fast (mapcar #'first (recall k q "operator" :k 12 :strength-weight 0)))
               (exact (mapcar #'first (sql-rows (kernel-db k)
                                                "SELECT id FROM memory_nodes ORDER BY pai_cosine_distance(retrieval_embedding,?1) LIMIT 12" q))))
          (when (equal fast exact) (incf same))))
      (format t "~&  identical to exhaustive exact top-12 in ~d of 20 queries~%" same)
      (is (= 20 same) "the shortlist recalls exactly what a full exact scan recalls"))))

(run-suite "gate-memory")
