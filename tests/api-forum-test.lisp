(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; The crew forum: one forum copied to every agent of the operator. Posts,
;;; reactions and thread changes are applied where they are made and sent to
;;; each crew member; nobody relays. Private threads reach only their viewers.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defvar *next-forum-test-port* (+ 41000 (random 1500)))

(defun forum-address (port) (format nil "http://127.0.0.1:~d" port))

(defmacro with-crew ((a b &key (crew t)) &body body)
  "Two agents joined as a fleet, and (when CREW) marked as sharing an operator."
  `(let ((secrets (db-open (format nil "/tmp/pai-v3-forum-secrets-~d.db" (random 100000000)) :key *test-key*))
         (pa (incf *next-forum-test-port* 2)))
     (pai-v3.trust:open-secrets secrets)
     (unwind-protect
          (with-temp-kernel (,a :agent-id "beta")
            (with-temp-kernel (,b :agent-id "alpha")
              (loop for (k name port) in (list (list ,a "Beta" pa) (list ,b "Alpha" (1+ pa)))
                    do (change-setting k "fleet.enabled" t :origin *operator*)
                       (change-setting k "fleet.own_name" name :origin *operator*)
                       (change-setting k "fleet.own_address" (forum-address port) :origin *operator*))
              (let ((agent-a (start-agent ,a :port pa :address "127.0.0.1"))
                    (agent-b (start-agent ,b :port (1+ pa) :address "127.0.0.1")))
                (unwind-protect
                     (progn
                       (let* ((said (pai-v3.api::fleet-request ,a (forum-address (1+ pa))))
                              (code (subseq said (+ 13 (search "Your code is " said)) (+ 19 (search "Your code is " said)))))
                         (pai-v3.api::fleet-approve ,b (pai-v3.api::fleet-agent-id ,a) code))
                       (when ,crew
                         (change-setting ,a "fleet.same_operator_peers" (list "Alpha") :origin *operator*)
                         (change-setting ,b "fleet.same_operator_peers" (list "Beta") :origin *operator*))
                       ,@body)
                  (stop-agent agent-a)
                  (stop-agent agent-b)))))
       (pai-v3.trust:close-secrets)
       (db-close secrets))))

(defun texts (k thread-id)
  (mapcar (lambda (m) (obj-get m "text")) (obj-get (pai-v3.api::forum-thread-json k thread-id) "messages")))

(defun woken (k)
  (sql-rows (kernel-db k) "SELECT id,audience,reason FROM thoughts WHERE origin=?1 ORDER BY id LIMIT 50" "work"))

(deftest an-operator-post-reaches-every-sister-and-wakes-each-once
  (with-crew (a b)
    (let* ((r (pai-v3.api::forum-post a :title "SYNTHETIC plans" :text "SYNTHETIC what shall we build?"
                                        :author-id "operator" :author-name "Sam"))
           (thread (obj-get r "thread_id")))
      (is (equal '("SYNTHETIC what shall we build?") (texts b thread)) "the sister holds her own copy")
      (is (equal "SYNTHETIC plans" (obj-get (pai-v3.api::forum-thread-json b thread) "title")))
      (is (= 1 (length (woken a))) "the agent signed in to is woken")
      (is (equal "operator" (second (first (woken a)))) "with the operator's authority")
      (is (= 1 (length (woken b))) "the sister is woken too, without anyone relaying")
      (is (search "signed in to Beta" (third (first (woken b)))))
      (is (null (pai-v3.api::board-threads b)) "nothing on the old boards"))))

(deftest reactions-and-thread-changes-travel-and-wake-nobody
  (with-crew (a b)
    (let* ((thread (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC t" :text "SYNTHETIC one" :intent "fyi"
                                                      :author-id (pai-v3.api::fleet-agent-id a) :author-name "Beta")
                            "thread_id"))
           (msg (obj-get (first (obj-get (pai-v3.api::forum-thread-json b thread) "messages")) "msg_id"))
           (before (length (woken a))))
      (is (= 0 (length (woken b))) "an fyi wakes nobody")
      (is (eq t (pai-v3.api::forum-react b msg "👍" (pai-v3.api::fleet-agent-id b) "Alpha")))
      (is (eq t (pai-v3.api::forum-react a msg "👍" "operator" "Sam")))
      (is (= 2 (obj-get (first (obj-get (first (obj-get (pai-v3.api::forum-thread-json a thread) "messages")) "reactions")) "count"))
          "both reactions, on both copies")
      (is (= 2 (obj-get (first (obj-get (first (obj-get (pai-v3.api::forum-thread-json b thread) "messages")) "reactions")) "count")))
      (is (null (pai-v3.api::forum-react b msg "👍" (pai-v3.api::fleet-agent-id b) "Alpha")) "a toggle takes it back")
      (is (= 1 (obj-get (first (obj-get (first (obj-get (pai-v3.api::forum-thread-json a thread) "messages")) "reactions")) "count")))
      (is (= before (length (woken a))) "no reaction woke anyone")
      (pai-v3.api::forum-set-thread b thread (pai-v3.api::fleet-agent-id b) "Alpha" :status "resolved")
      (is (equal "resolved" (obj-get (pai-v3.api::forum-thread-json a thread) "status")) "any crew member may resolve it"))))

(deftest a-private-thread-reaches-only-its-viewers-and-opening-it-sends-its-history
  (with-crew (a b)
    (let* ((me (pai-v3.api::fleet-agent-id a))
           (thread (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC secret plan" :text "SYNTHETIC just us"
                                                      :author-id "operator" :author-name "Sam" :access "private" :viewers '())
                            "thread_id")))
      (is (null (pai-v3.api::forum-thread-json b thread :member (pai-v3.api::fleet-agent-id b))) "the other sister never got it")
      (is (equal '("SYNTHETIC just us") (texts a thread)))
      (pai-v3.api::forum-set-thread a thread "operator" "Sam" :access "crew")
      (is (equal '("SYNTHETIC just us") (texts b thread)) "opened to the crew, its history follows")
      (pai-v3.api::forum-set-thread a thread me "Beta" :access "private" :viewers '())
      (is (null (texts b thread)) "made private again, the sister drops what she held")
      (is (null (search "SYNTHETIC secret plan" (pai-v3.api::forum-text b nil))) "and her list no longer shows it")
      (is (null (search "SYNTHETIC just us" (pai-v3.api::forum-text b thread)))))))

(deftest someone-left-out-of-a-post-is-not-woken
  (with-crew (a b)
    (pai-v3.api::forum-post a :title "SYNTHETIC q" :text "SYNTHETIC for Beta only, really"
                              :author-id "operator" :author-name "Sam" :quiet-for (list "Alpha"))
    (is (= 0 (length (woken b))) "she has the post, unwoken")
    (is (= 1 (length (pai-v3.api::forum-threads b))))))

(deftest only-the-crew-shares-the-forum
  (with-crew (a b :crew nil)
    (pai-v3.api::forum-post a :title "SYNTHETIC t" :text "SYNTHETIC hello" :author-id "operator" :author-name "Sam")
    (is (null (pai-v3.api::forum-threads b)) "a peer of another operator gets nothing")
    (is (null (sql-value (kernel-db a) "SELECT 1 FROM fleet_ops WHERE kind=?1 LIMIT 1" "forum-queued")) "nothing was even queued")))

(deftest a-sister-speaks-through-a-draft-and-the-crew-review
  (with-crew (a b)
    (let* ((thread (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC ideas" :text "SYNTHETIC any ideas?"
                                                      :author-id "operator" :author-name "Sam")
                            "thread_id"))
           (asked (obj-get (first (obj-get (pai-v3.api::forum-thread-json b thread) "messages")) "msg_id"))
           (reflection (request-thought b "introspection" "SYNTHETIC reflection"))
           (draft (pai-v3.mind::tool-run (gethash "draft_forum_post" pai-v3.mind::*tools*))))
      (is (search "needs a title" (funcall draft b reflection (obj "text" "SYNTHETIC orphan"))))
      (is (search "left for the crew forum"
                  (funcall draft b reflection (obj "text" "SYNTHETIC a garden" "thread_id" thread "reply_to" asked "why" "PRIVATE-REASON"))))
      (append! b "thought.ended" (obj "thought_id" reflection "outcome" "concluded" "summary" "Done"))
      (is (= 1 (pai-v3.api::speak-drafts b)))
      (let* ((review (first (first (sql-rows (kernel-db b) "SELECT id FROM thoughts WHERE audience=?1 ORDER BY id DESC LIMIT 1" "peer:crew"))))
             (row (thought-row b review)))
        (is (search "light crew review" (getf row :reason)))
        (is (null (search "PRIVATE-REASON" (getf row :reason))) "never the reflection's reasons")
        (funcall pai-v3.mind::*communication-answer* b row
                 "{\"decision\":\"send\",\"text\":\"SYNTHETIC a garden\",\"images\":[],\"note\":\"fine\"}")
        (is (equal '("SYNTHETIC any ideas?" "SYNTHETIC a garden") (texts a thread)) "it reached the other sister")
        (is (equal "sent" (sql-value (kernel-db b) "SELECT fate FROM fleet_drafts WHERE speaking_thought_id=?1 LIMIT 1" review)))))))

(deftest old-boards-are-brought-into-the-forum-once
  (with-crew (a b)
    (pai-v3.api::operator-post a "SYNTHETIC old post" :title "SYNTHETIC old thread")
    (let ((old (first (first (pai-v3.api::board-threads a)))))
      (is (pai-v3.api::migrate-boards-to-forum a))
      (is (null (pai-v3.api::migrate-boards-to-forum a)) "once")
      (is (equal '("SYNTHETIC old post") (texts b old)) "the sister has the history too")
      (is (equal '("SYNTHETIC old post") (texts a old))))))

(deftest the-agent-reads-the-forum-and-the-old-tools-are-retired
  (with-crew (a b)
    (let ((thread (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC read me" :text "SYNTHETIC hello crew"
                                                     :author-id "operator" :author-name "Sam")
                           "thread_id"))
          (row (thought-row b (request-thought b "work" "SYNTHETIC" :audience "operator"))))
      (is (search "SYNTHETIC read me" (pai-v3.api::forum-text b nil)))
      (is (search "SYNTHETIC hello crew" (pai-v3.api::forum-text b thread)))
      (is (pai-v3.mind::tool-available-p (gethash "read_forum" *tools*) row))
      (is (pai-v3.mind::tool-available-p (gethash "draft_forum_post" *tools*) row))
      (is (not (pai-v3.mind::tool-available-p (gethash "draft_fleet_message" *tools*) row)) "the old way is no longer offered")
      (is (not (pai-v3.mind::tool-available-p (gethash "read_peer_board" *tools*) row))))))

(run-suite "api-forum")
