(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; The fleet in v2's protocol (bead v3-clp.12): two agents in this process,
;;; each on its own port, join with an operator-typed code, then talk through
;;; their own boards with signed requests.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))
(defvar *port-a* (+ 45000 (random 2000)))
(defvar *port-b* (+ 47000 (random 2000)))

(defun address (port) (format nil "http://127.0.0.1:~d" port))

(defun fleet-setup (k name port)
  (change-setting k "fleet.enabled" t :origin *operator*)
  (change-setting k "fleet.own_name" name :origin *operator*)
  (change-setting k "fleet.own_address" (address port) :origin *operator*))

(defmacro with-two-agents ((a b) &body body)
  `(let ((secrets (db-open (format nil "/tmp/pai-v3-fleet-secrets-~d.db" (random 100000000)) :key *test-key*)))
     (pai-v3.trust:open-secrets secrets)
     ;; Fresh ports each time: a pooled connection to a stopped agent answers 503.
     (setf *port-a* (+ 45000 (random 2000)) *port-b* (+ 47000 (random 2000)))
     (unwind-protect
          (with-temp-kernel (,a :agent-id "beta")
            (with-temp-kernel (,b :agent-id "alpha")
              (fleet-setup ,a "Beta" *port-a*)
              (fleet-setup ,b "Alpha" *port-b*)
              (let ((agent-a (start-agent ,a :port *port-a* :address "127.0.0.1"))
                    (agent-b (start-agent ,b :port *port-b* :address "127.0.0.1")))
                (unwind-protect (progn ,@body)
                  (stop-agent agent-a)
                  (stop-agent agent-b)))))
       (pai-v3.trust:close-secrets)
       (db-close secrets))))

(defun join (a b)
  "A asks B; B's operator types the code from A's screen."
  (let* ((said (pai-v3.api::fleet-request a (address *port-b*)))
         (code (subseq said (+ 13 (search "Your code is " said)) (+ 19 (search "Your code is " said)))))
    (pai-v3.api::fleet-approve b (pai-v3.api::fleet-agent-id a) code)))

(defun peer-thoughts (k peer-k)
  (sql-rows (kernel-db k) "SELECT id,reason FROM thoughts WHERE audience=?1 ORDER BY id LIMIT 50"
            (format nil "peer:~a" (pai-v3.api::fleet-agent-id peer-k))))

(deftest joining-needs-the-right-code
  (with-two-agents (a b)
    (pai-v3.api::fleet-request a (address *port-b*))
    (is (search "code does not match"
                (handler-case (pai-v3.api::fleet-approve b (pai-v3.api::fleet-agent-id a) "000000x")
                  (pai-v3.api::board-error (e) (pai-v3.api::board-error-reason e)))))
    (is (null (pai-v3.api::fleet-peers b)) "no peer on a wrong code")
    (is (search "is now a fleet peer" (join a b)))
    (is (equal "Beta" (second (first (pai-v3.api::fleet-peers b)))))
    (is (equal "Alpha" (second (first (pai-v3.api::fleet-peers a)))) "both sides hold the peer")
    (is (null (search (pai-v3.api::fleet-agent-id a)
                      (or (sql-value (kernel-db b) "SELECT group_concat(event_json) FROM pai_events WHERE agent_id=?1 AND event_type=?2 LIMIT 1"
                                     "alpha" "nothing") "")))
        "sanity")
    (is (null (search "shared_secret" (sql-value (kernel-db b) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                                                 "alpha" "fleet.peer.joined")))
        "the secret is never in the ledger")))

(deftest a-signed-post-lands-on-the-board-and-wakes-a-peer-thought
  (with-two-agents (a b)
    (join a b)
    (let ((b-id (pai-v3.api::fleet-agent-id b)))
      (is (search "Posted to Alpha's board" (pai-v3.api::fleet-post a b-id "Have you read the new chapter?")))
      (let ((thread (first (first (pai-v3.api::board-threads b)))))
        (is thread "a thread on Alpha's board")
        (is (equal "Have you read the new chapter?" (fourth (first (pai-v3.api::board-messages b thread)))))
        (is (equal "Beta" (third (first (pai-v3.api::board-messages b thread)))) "the author is the verified peer")
        (is (= 1 (length (peer-thoughts b a))) "Alpha considers it as a peer thought")
        (is (search "no operator authority" (second (first (peer-thoughts b a)))))
        (is (search "Have you read the new chapter?" (second (first (peer-thoughts b a))))
            "the woken thought is given the post's own text")
        (is (null (search "NIL" (second (first (peer-thoughts b a))))))
        (is (sql-value (kernel-db b) "SELECT 1 FROM working_dialogue WHERE kind=?1 LIMIT 1" "notice") "Alpha's operator is told")
        (pai-v3.api::fleet-post a b-id "And the ending?")
        (is (= 1 (length (pai-v3.api::board-threads b))) "later posts continue the thread")
        (is (= 2 (length (pai-v3.api::board-messages b thread))))
        ;; Alpha replies on its own board; Beta is told.
        (let ((first-msg (first (first (pai-v3.api::board-messages b thread)))))
          (is (search "Beta was told" (pai-v3.api::fleet-reply b thread first-msg "Yes, last night.")))
          (is (= 0 (length (peer-thoughts a b))) "an answer is told but wakes nobody")
          (pai-v3.api::fleet-reply b thread first-msg "Did you like the ending?" :intent "question")
          (is (= 1 (length (peer-thoughts a b))) "a question wakes Beta")
          (is (search "Did you like the ending?" (second (first (peer-thoughts a b)))))
          (is (search "absorb" (string-downcase (second (first (peer-thoughts a b))))) "asked to decide"))))))

(defparameter +tiny-png+
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")

(deftest images-travel-with-board-messages
  (with-two-agents (a b)
    (join a b)
    (let* ((b-id (pai-v3.api::fleet-agent-id b))
           (mine (pai-v3.mind::store-image a +tiny-png+)))
      (is (search "no stored image" (pai-v3.api::%tool-text (lambda () (pai-v3.api::fleet-post a b-id "look" :images (list "0123456789abcdef0123456789abcdef")))))
          "only images I hold can be sent")
      (is (search "Posted to Alpha's board" (pai-v3.api::fleet-post a b-id "My new portrait." :images (list mine))))
      (let* ((thread (first (first (pai-v3.api::board-threads b))))
             (message (first (pai-v3.api::board-messages b thread)))
             (theirs (pai-v3.api::board-message-images b (tenth message))))
        (is (equal (list mine) theirs) "the receiver keeps its own copy, under the same content id")
        (is (equal "image/png" (first (pai-v3.mind::image-row b mine))) "in its own store")
        (is (search "with 1 image" (second (first (peer-thoughts b a)))) "the woken thought is told")
        (is (equal (list mine) (pai-v3.mind::turn-images b (thought-row b (first (first (peer-thoughts b a))))))
            "and sees it")
        (is (equal (list mine) (obj-get (first (obj-get (pai-v3.api::board-thread-json b thread) "messages")) "images"))
            "the operator's board page shows it")
        (is (null (search "base64" (sql-value (kernel-db b) "SELECT event_json FROM pai_events WHERE agent_id=?1 AND event_type=?2 ORDER BY storage_sequence DESC LIMIT 1"
                                              "alpha" "fleet.board.posted")))
            "the ledger holds the id, never the picture")
        ;; A reply with an image: the notification carries it to the peer.
        (progn
          (is (search "Beta was told" (pai-v3.api::fleet-reply b thread (first message) "Here is mine." :images (list mine) :intent "question")))
          (is (equal (list mine) (pai-v3.mind::turn-images a (thought-row a (first (first (peer-thoughts a b))))))
              "a reply's images reach the peer it tells"))))))

(deftest a-chain-too-deep-wakes-nobody
  (with-two-agents (a b)
    (join a b)
    (let ((b-id (pai-v3.api::fleet-agent-id b)))
      (pai-v3.api::fleet-post a b-id "deep" :chain-depth 4)
      (is (= 0 (length (peer-thoughts b a))) "past the depth cap it is kept but wakes nobody")
      (is (search "not woken: its chain is 4 replies deep"
                  (sql-value (kernel-db b) "SELECT rendered FROM working_dialogue WHERE kind=?1 ORDER BY event_id DESC LIMIT 1" "notice")))
      (pai-v3.api::fleet-post a b-id "shallow" :chain-depth 2)
      (let ((thought (first (first (peer-thoughts b a)))))
        (is thought)
        (is (= 3 (pai-v3.api::%outgoing-depth b thought)) "an answer from that thought goes one deeper")))))

(deftest tagging-a-peer-tells-it
  (with-two-agents (a b)
    (join a b)
    (let ((a-id (pai-v3.api::fleet-agent-id a)))
      (is (search "Told Beta" (pai-v3.api::fleet-announce b "Anyone know a good fantasy map tool?" :tags (list a-id) :intent "question")))
      (is (search "tagged me" (funcall (pai-v3.mind::tool-run (gethash "read_fleet_board" pai-v3.mind::*tools*)) a nil (obj)))
          "a tag can be found again: it lives on the peer's board, not mine")
      (is (search "good fantasy map tool" (funcall (pai-v3.mind::tool-run (gethash "read_fleet_board" pai-v3.mind::*tools*)) a nil (obj))))
      (is (= 1 (length (peer-thoughts a b))))
      (is (search "tagged you" (second (first (peer-thoughts a b)))))
      (pai-v3.api::fleet-announce b "The maps are done." :tags (list a-id))
      (is (= 1 (length (peer-thoughts a b))) "a tagged fyi is told but wakes nobody")
      (is (search "not one of my peers" (pai-v3.api::%tool-text (lambda () (pai-v3.api::fleet-announce b "x" :tags (list "nobody")))))))))

(deftest a-resolved-thread-takes-no-more
  (with-two-agents (a b)
    (join a b)
    (pai-v3.api::fleet-post a (pai-v3.api::fleet-agent-id b) "Done with this?")
    (let ((thread (first (first (pai-v3.api::board-threads b)))))
      (is (search "Resolved" (pai-v3.api::fleet-resolve b thread)))
      (is (search "resolved" (pai-v3.api::%tool-text (lambda () (pai-v3.api::fleet-post a (pai-v3.api::fleet-agent-id b) "One more thing"))))
          "a post to a resolved thread is refused"))))

(deftest the-digest-reads-what-peers-posted-once
  (with-two-agents (a b)
    (join a b)
    (pai-v3.api::fleet-announce a "I finished reading Dune today.")
    (let ((thought (pai-v3.api::fleet-digest b)))
      (is thought "a digest thought")
      (is (search "I finished reading Dune today." (getf (thought-row b thought) :reason)))
      (is (equal "peer:fleet" (getf (thought-row b thought) :audience)) "peer content, not operator trust"))
    (is (null (pai-v3.api::fleet-digest b)) "not due again yet")
    (change-setting b "fleet.digest_minutes" 0 :origin *operator*)
    (is (null (pai-v3.api::fleet-digest b)) "off at 0")))

(deftest unsigned-forged-and-stale-requests-are-refused
  (with-two-agents (a b)
    (join a b)
    (flet ((status (headers body)
             (handler-case (nth-value 1 (dex:post (format nil "~a/board/post" (address *port-b*))
                                                  :headers (cons (cons "Content-Type" "application/json") headers)
                                                  :content body))
               (dex:http-request-failed (e) (dex:response-status e)))))
      (let* ((body (json-encode (obj "new_thread_title" "x" "text" "hello")))
             (a-id (pai-v3.api::fleet-agent-id a))
             (now (- (get-universal-time) 2208988800))
             (good (pai-v3.trust:secret-hmac (format nil "fleet-peer-~a" (pai-v3.api::fleet-agent-id b))
                                             (format nil "~d.~a" now body))))
        (is (= 401 (status nil body)) "unsigned")
        (is (= 401 (status (list (cons "X-Pai-Fleet-Peer-Id" a-id) (cons "X-Pai-Fleet-Timestamp" (princ-to-string now))
                                 (cons "X-Pai-Fleet-Signature" (make-string 64 :initial-element #\0)))
                           body))
            "forged")
        (is (= 401 (status (list (cons "X-Pai-Fleet-Peer-Id" a-id) (cons "X-Pai-Fleet-Timestamp" (princ-to-string (- now 600)))
                                 (cons "X-Pai-Fleet-Signature"
                                       (pai-v3.trust:secret-hmac (format nil "fleet-peer-~a" (pai-v3.api::fleet-agent-id b))
                                                                 (format nil "~d.~a" (- now 600) body))))
                           body))
            "stale")
        (is (= 200 (status (list (cons "X-Pai-Fleet-Peer-Id" a-id) (cons "X-Pai-Fleet-Timestamp" (princ-to-string now))
                                 (cons "X-Pai-Fleet-Signature" good))
                           body))
            "signed and fresh")
        (is (= 401 (status (list (cons "X-Pai-Fleet-Peer-Id" a-id) (cons "X-Pai-Fleet-Timestamp" (princ-to-string now))
                                 (cons "X-Pai-Fleet-Signature" good))
                           (json-encode (obj "new_thread_title" "x" "text" "changed"))))
            "a changed body fails its signature")))))

(deftest the-same-operation-is-posted-once-and-wakes-are-capped
  (with-two-agents (a b)
    (join a b)
    (change-setting b "fleet.peer_wakes_per_hour" 2 :origin *operator*)
    (let ((b-id (pai-v3.api::fleet-agent-id b)))
      (pai-v3.api::fleet-post a b-id "one" :operation-id "op-1")
      (pai-v3.api::fleet-post a b-id "one" :operation-id "op-1")
      (is (= 1 (length (pai-v3.api::board-messages b (first (first (pai-v3.api::board-threads b))))))
          "a retried operation is not posted twice")
      (pai-v3.api::fleet-post a b-id "two")
      (pai-v3.api::fleet-post a b-id "three")
      (is (= 3 (length (pai-v3.api::board-messages b (first (first (pai-v3.api::board-threads b)))))))
      (is (= 2 (length (peer-thoughts b a))) "the third waits on the board"))))

(deftest peer-thoughts-get-fleet-tools-but-no-operator-tools
  (let ((peer '(:audience "peer:x")) (guest '(:audience "guest")))
    (is (pai-v3.mind::tool-available-p (gethash "reply_fleet_board_message" *tools*) peer))
    (is (pai-v3.mind::tool-available-p (gethash "post_fleet_message" *tools*) '(:audience "operator")))
    (is (not (pai-v3.mind::tool-available-p (gethash "post_fleet_message" *tools*) guest)))
    (is (not (pai-v3.mind::tool-available-p (gethash "lisp_eval" *tools*) peer)))
    (is (not (pai-v3.mind::tool-available-p (gethash "shell" *tools*) peer)))))

(deftest a-v2-fleet-is-carried-over
  (let ((secrets (db-open (format nil "/tmp/pai-v3-fleet-secrets-~d.db" (random 100000000)) :key *test-key*)))
    (pai-v3.trust:open-secrets secrets)
    (unwind-protect
         (with-temp-kernel (k)
           (let ((fleet "(:SCHEMA-VERSION 1 :AGENT-ID \"aaaa-1111\" :PEERS ((:ID \"bbbb-2222\" :NAME \"Alpha\" :ADDRESS \"alpha.example.ts.net:8443\" :SHARED-SECRET \"00112233445566778899AABBCCDDEEFF00112233445566778899AABBCCDDEEFF\" :JOINED-AT 3900000000 :OUTBOUND-THREAD-ID \"t-out\")))")
                 (board "(:SCHEMA-VERSION 1 :THREADS ((:THREAD-ID \"t-1\" :TITLE \"Hello\" :STATUS \"resolved\" :CREATED-BY \"bbbb-2222\" :CREATED-AT 100 :LAST-ACTIVITY-AT 200)) :MESSAGES ((:MSG-ID \"m-2\" :THREAD-ID \"t-1\" :AUTHOR-ID \"aaaa-1111\" :AUTHOR-NAME \"Beta\" :TEXT \"Hi back\" :TAGS NIL :INTENT NIL :SCOPE \"pairwise\" :TIMESTAMP 200 :REPLY-TO \"m-1\" :OPERATION-ID \"o2\" :REQUEST-KEY \"k2\") (:MSG-ID \"m-1\" :THREAD-ID \"t-1\" :AUTHOR-ID \"bbbb-2222\" :AUTHOR-NAME \"Alpha\" :TEXT \"Hi\" :TAGS NIL :INTENT \"fyi\" :SCOPE \"pairwise\" :TIMESTAMP 100 :REPLY-TO NIL :OPERATION-ID \"o1\" :REQUEST-KEY \"k1\")))"))
             (is (search "1 peer, 2 board messages" (pai-v3.api::import-v2-fleet k fleet board)))
             (is (equal "aaaa-1111" (pai-v3.api::fleet-agent-id k)) "the identity is kept, so peers know it")
             (is (equal "t-out" (fourth (pai-v3.api::fleet-peer k "bbbb-2222"))) "the open thread is remembered")
             (is (pai-v3.trust:secret-hmac "fleet-peer-bbbb-2222" "1.x") "the shared secret is in the secret store")
             (is (equal "resolved" (third (pai-v3.api::board-thread k "t-1"))))
             (is (equal '("m-1" "m-2") (mapcar #'first (pai-v3.api::board-messages k "t-1"))))
             (is (search "1 peer, 0 board messages" (pai-v3.api::import-v2-fleet k fleet board)) "a second run adds nothing")))
      (pai-v3.trust:close-secrets)
      (db-close secrets))))

(run-suite "api-fleet")
