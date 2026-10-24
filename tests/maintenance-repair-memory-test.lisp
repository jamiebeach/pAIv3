(handler-bind ((warning #'muffle-warning))
  (ql:quickload "pai-v3-maintenance" :silent t))

(in-package #:pai-v3.test)
(use-package '#:pai-v3.maintenance)

;;; Memories lost to repeated ids (bead v3-clp.2.1).

(defun formed (k id content)
  (append! k "memory.formed" (obj "memory_id" id "kind" "fact" "content" content "sensitivity" "operator-only"
                                  "owner" "operator" "strength" 0.5)))

(defun node-contents (k)
  (mapcar #'first (sql-rows (kernel-db k) "SELECT content FROM memory_nodes ORDER BY content LIMIT 50")))

(deftest new-memory-ids-do-not-repeat-when-random-starts-over
  ;; Every process used to start RANDOM from the same state.
  (with-temp-kernel (k)
    (let* ((state (make-random-state nil))
           (first-id (let ((*random-state* (make-random-state state)))
                       (pai-v3.mind:form-memory k :kind "fact" :content "SYNTHETIC one")))
           (second-id (let ((*random-state* (make-random-state state)))
                        (pai-v3.mind:form-memory k :kind "fact" :content "SYNTHETIC two"))))
      (is (string/= first-id second-id))
      (is (equal '("SYNTHETIC one" "SYNTHETIC two") (node-contents k))))))

(deftest a-memory-lost-to-a-repeated-id-is-formed-again-once
  (with-temp-kernel (k)
    (formed k "m-SAME" "SYNTHETIC first")
    (formed k "m-SAME" "SYNTHETIC lost")
    (formed k "m-SAME" "SYNTHETIC first")
    (is (equal '("SYNTHETIC first") (node-contents k)) "the repeats were quarantined")
    (is (= 2 (sql-value (kernel-db k) "SELECT count(*) FROM v3_quarantine WHERE projection=?1 LIMIT 1" "memory")))
    (multiple-value-bind (reformed held other) (reform-collided-memories k)
      (is (= 1 reformed) "the lost content comes back")
      (is (= 1 held) "a repeat of content already held is not formed twice")
      (is (= 0 other)))
    (is (equal '("SYNTHETIC first" "SYNTHETIC lost") (node-contents k)))
    (is (= 0 (sql-value (kernel-db k) "SELECT count(*) FROM v3_quarantine WHERE projection=?1 LIMIT 1" "memory")))
    (is (= 0 (sql-value (kernel-db k) "SELECT count(*) FROM v3_degraded WHERE projection=?1 LIMIT 1" "memory")))
    (is (equal '(0 0 0) (multiple-value-list (reform-collided-memories k))) "running it again changes nothing")))

(run-suite "maintenance-repair-memory")
