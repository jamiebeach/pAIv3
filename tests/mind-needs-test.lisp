(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; What I understand the operator needs (2026-11-07): my own view, my own words,
;;; held to the operator's own messages, which are quoted from the record.

(defparameter *needs-op* (obj "principal_type" "operator"))

(deftest a-need-rests-on-the-operators-own-words-and-shows-them
  (with-clock
    (with-temp-kernel (k)
      (let* ((said (event-id (receive-turn k :channel "web" :audience "operator" :speaker "Sam"
                                             :text "SYNTHETIC no work stuff on weekends please" :origin *needs-op*)))
             (peer (event-id (append! k "web.searched" (obj "query" "SYNTHETIC" "status" 200))))
             (run (pai-v3.mind::tool-run (gethash "operator_needs" pai-v3.mind::*tools*))))
        (is (search "name the operator's messages" (funcall run k nil (obj "action" "add" "text" "SYNTHETIC weekends free"))))
        (is (search "is not something the operator said" (funcall run k nil (obj "action" "add" "text" "SYNTHETIC weekends free" "record_ids" (list peer))))
            "only their own words count as evidence")
        (let* ((kept (funcall run k nil (obj "action" "add" "text" "SYNTHETIC keep weekends free of work asks" "record_ids" (list said))))
               (id (subseq kept 5 (1- (length kept))))
               (slot (format nil "~{~a~%~}" (pai-v3.mind::%fill k :needs (list :audience "operator") 400))))
          (is (search "Kept n-" kept))
          (is (search "SYNTHETIC keep weekends free of work asks" slot) "in my own words")
          (is (search "\"SYNTHETIC no work stuff on weekends please\"" slot) "their words, quoted by the system")
          (is (search "from 1 message of the operator's, the latest written today" slot) "with how old they are")
          (is (search "check again by" slot))
          (is (null (pai-v3.mind::%fill k :needs (list :audience "peer:nova") 400)) "no other agent or peer sees it")
          (advance (* 31 86400))
          (is (search "due to be checked again" (format nil "~{~a~}" (pai-v3.mind::%fill k :needs (list :audience "operator") 400)))
              "and it comes due")
          (is (search "Removed" (funcall run k nil (obj "action" "remove" "id" id))))
          (is (null (pai-v3.mind::%fill k :needs (list :audience "operator") 400)) "removed, it is gone"))))))

(run-suite "mind-needs")
