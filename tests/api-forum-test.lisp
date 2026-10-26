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
      (is (equal '("SYNTHETIC old post") (texts a old)))))
  (with-crew (a b)
    ;; An old board holds a sister's words too; they come along.
    (let* ((b-id (pai-v3.api::fleet-agent-id b)))
      (pai-v3.api::board-post a :new-thread-title "SYNTHETIC hers" :author-id b-id :author-name "Alpha"
                                :text "SYNTHETIC written by her" :operation-id "op-h" :request-key "h")
      (pai-v3.api::migrate-boards-to-forum a)
      (pai-v3.api::flush-forum a)
      (is (member "SYNTHETIC written by her"
                  (loop for th in (pai-v3.api::forum-threads b) append (texts b (first th))) :test #'equal)
          "a sister's own old words reach her copy")
      (is (= 0 (sql-value (kernel-db a) "SELECT count(*) FROM fleet_ops q WHERE q.kind=?1 AND NOT EXISTS (SELECT 1 FROM fleet_ops d WHERE d.kind=?2 AND d.operation_id=q.operation_id) LIMIT 1"
                          "forum-queued" "forum-delivered"))
          "nothing is left waiting"))))

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

(deftest a-draft-left-the-old-way-for-a-migrated-thread-is-said-in-the-forum
  (with-crew (a b)
    (pai-v3.api::operator-post b "SYNTHETIC old question" :title "SYNTHETIC old thread")
    (let* ((old (first (first (pai-v3.api::board-threads b))))
           (parent (first (first (pai-v3.api::board-messages b old))))
           (reflection (request-thought b "introspection" "SYNTHETIC reflection")))
      ;; The draft is made the old way, before the boards are brought in.
      (funcall (pai-v3.mind::tool-run (gethash "draft_fleet_message" pai-v3.mind::*tools*)) b reflection
               (obj "peer_id" (pai-v3.api::fleet-agent-id a) "text" "SYNTHETIC late answer" "thread_id" old "reply_to" parent))
      (pai-v3.api::migrate-boards-to-forum b)
      (append! b "thought.ended" (obj "thought_id" reflection "outcome" "concluded" "summary" "Done"))
      (pai-v3.api::speak-drafts b)
      (let* ((review (first (first (sql-rows (kernel-db b) "SELECT speaking_thought_id FROM fleet_drafts WHERE text=?1 LIMIT 1" "SYNTHETIC late answer")))))
        (funcall pai-v3.mind::*communication-answer* b (thought-row b review)
                 "{\"decision\":\"send\",\"text\":\"SYNTHETIC late answer\",\"images\":[],\"note\":\"fine\"}")
        (is (member "SYNTHETIC late answer" (texts a old) :test #'equal) "it reached the forum, and the sister's copy")))))

;;; Guests (guests.lisp): outside the crew, by key, only in threads open to guests.

(defun guest-judge (level)
  "A Decisions provider answering the guest post check with all weight on LEVEL (2: a secret)."
  (make-provider-profile :name "affect-jev" :model "typesafe/jev-1.13" :base-url "https://openrouter.ai/api/v1"
                         :secret "openrouter" :microusd-per-1k-prompt 42
                         :transport (lambda (url headers body &key timeout)
                                      (declare (ignore url headers body timeout))
                                      (let ((p (obj)))
                                        (dotimes (i 3) (setf (gethash (write-to-string i) p) (if (= i level) 1d0 0d0)))
                                        (values 200 nil (json-encode (obj "model" "typesafe/jev-test"
                                                                          "answers" (obj "secrets" (obj "type" "score" "score" level "confidence" 1d0 "probabilities" p))
                                                                          "usage" (obj "input_tokens" 9 "output_tokens" 1 "cost" 0.000001))))))))

(defmacro with-judge ((level) &body body)
  "The guest post check answered at LEVEL, for every thread: the agent answers requests on its own."
  `(let ((before pai-v3.mind::*decision-profile*))
     (setf pai-v3.mind::*decision-profile* (guest-judge ,level))
     (unwind-protect (progn ,@body) (setf pai-v3.mind::*decision-profile* before))))

(defun guest-call (port key method path &optional body)
  "(values status parsed-body) of a guest request to the agent on PORT."
  (handler-case
      (let ((url (format nil "~a~a" (forum-address port) path))
            (headers (append (and key (list (cons "Authorization" (format nil "Bearer ~a" key))))
                             (list (cons "Content-Type" "application/json")))))
        (multiple-value-bind (text status)
            (if (eq method :get)
                (dex:get url :headers headers)
                (dex:post url :headers headers :content (json-encode (or body (obj)))))
          (values status (json-parse text))))
    (dex:http-request-failed (e) (values (dex:response-status e) (ignore-errors (json-parse (dex:response-body e)))))))

(deftest a-guest-sees-and-speaks-only-in-threads-open-to-guests
  (with-crew (a b)
    (with-judge (0)
    (let* ((key (pai-v3.api::issue-guest-key a "Claude Code" *operator*))
           (port (parse-integer (subseq (setting a "fleet.own_address") 17)))
           (crew (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC crew only" :text "SYNTHETIC inside"
                                                    :author-id "operator" :author-name "Sam") "thread_id"))
           (open (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC with guests" :text "SYNTHETIC welcome"
                                                    :author-id "operator" :author-name "Sam" :access "guests") "thread_id")))
      (is (= 401 (guest-call port "pai-guest-wrong" :get "/v1/guest/forum/threads")) "a key is needed")
      (multiple-value-bind (status body) (guest-call port key :get "/v1/guest/forum/threads")
        (is (= 200 status))
        (is (equal (list open) (mapcar (lambda (th) (obj-get th "thread_id")) (obj-get body "threads")))
            "only the thread open to guests")
        (is (equal "guest:claude-code" (obj-get body "id"))))
      (is (= 404 (guest-call port key :get (format nil "/v1/guest/forum/thread?id=~a" crew))))
      (is (= 400 (guest-call port key :post "/v1/guest/forum/post" (obj "thread_id" crew "text" "SYNTHETIC sneaking in"))))
      (multiple-value-bind (status body) (guest-call port key :post "/v1/guest/forum/post"
                                                     (obj "thread_id" open "text" "SYNTHETIC hello from a guest" "intent" "question"))
        (is (= 200 status))
        (is (obj-get body "msg_id")))
      (is (member "SYNTHETIC hello from a guest" (texts b open) :test #'equal) "carried to the rest of the crew")
      (let ((woke (find-if (lambda (w) (search "SYNTHETIC hello from a guest" (third w))) (woken b))))
        (is woke "a question from a guest wakes the crew")
        (is (equal "peer:guest:claude-code" (second woke)) "as the guest, never as a crew peer")
        (is (search "a guest from outside the crew" (third woke))))
      ;; A thread the guest starts is open to guests, whatever it asks for.
      (multiple-value-bind (status body) (guest-call port key :post "/v1/guest/forum/post"
                                                     (obj "title" "SYNTHETIC from the guest" "text" "SYNTHETIC a new thread" "access" "crew"))
        (is (= 200 status))
        (is (equal "guests" (obj-get (pai-v3.api::forum-thread-json b (obj-get body "thread_id")) "access"))))))))

(deftest a-peer-cannot-pass-off-guest-words-outside-guest-threads
  (with-crew (a b)
    (let ((crew (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC crew only" :text "SYNTHETIC inside"
                                                   :author-id "operator" :author-name "Sam") "thread_id")))
      (is (handler-case
              (progn (pai-v3.api::accept-forum-event
                      b (list (pai-v3.api::fleet-agent-id a))
                      (obj "type" "forum.posted"
                           "payload" (obj "msg_id" "m-x" "thread_id" crew "author_id" "guest:mallory" "author_name" "Mallory"
                                          "text" "SYNTHETIC not allowed" "ts" 1)))
                     nil)
            (pai-v3.api::board-error () t))))))

(deftest a-guest-post-with-a-secret-or-past-the-hour-is-refused
  (with-crew (a b)
    (let* ((key (pai-v3.api::issue-guest-key a "Codex" *operator*))
           (port (parse-integer (subseq (setting a "fleet.own_address") 17)))
           (open (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC with guests" :text "SYNTHETIC welcome"
                                                    :author-id "operator" :author-name "Sam" :access "guests") "thread_id")))
      (with-judge (2)
        (multiple-value-bind (status body) (guest-call port key :post "/v1/guest/forum/post"
                                                       (obj "thread_id" open "text" "SYNTHETIC key sk-FAKE"))
          (is (= 400 status))
          (is (search "credential or secret" (obj-get body "error"))))
        (is (= 1 (length (texts a open))) "nothing posted"))
      (with-judge (0)
        (change-setting a "forum.guest_posts_per_hour" 2 :origin *operator*)
        (dotimes (i 2) (guest-call port key :post "/v1/guest/forum/post" (obj "thread_id" open "text" (format nil "SYNTHETIC ~d" i))))
        (multiple-value-bind (status body) (guest-call port key :post "/v1/guest/forum/post" (obj "thread_id" open "text" "SYNTHETIC more"))
          (is (= 400 status))
          (is (search "at most 2 posts an hour" (obj-get body "error")))))
      (pai-v3.api::revoke-guest a "Codex" *operator*)
      (is (= 404 (guest-call port key :get "/v1/guest/forum/threads")) "no guests left: no route"))))

(deftest a-relayed-thread-is-marked-for-the-whole-crew-and-cleared-as-easily
  ;; Bead v3-fy1: a guest thread whose copies leave the household says so wherever it is read or written to.
  (with-crew (a b)
    (let ((open (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC weather" :text "SYNTHETIC sunny"
                                                    :author-id "operator" :author-name "Sam" :access "guests") "thread_id"))
          (crew (obj-get (pai-v3.api::forum-post a :title "SYNTHETIC crew" :text "SYNTHETIC inside"
                                                    :author-id "operator" :author-name "Sam") "thread_id")))
      (is (handler-case (progn (pai-v3.api::forum-set-thread a open (pai-v3.api::fleet-agent-id a) "Beta" :relayed t) nil)
            (pai-v3.api::board-error () t)) "an agent cannot mark it")
      (is (handler-case (progn (pai-v3.api::forum-set-thread a crew "operator" "Sam" :relayed t) nil)
            (pai-v3.api::board-error () t)) "only a guest thread can be relayed")
      (pai-v3.api::forum-set-thread a open "operator" "Sam" :relayed t)
      (is (pai-v3.api::thread-relayed-p b open) "the mark reaches the rest of the crew")
      (is (search "RELAYED outside the household" (pai-v3.api::forum-text b open)) "reading the thread shows it")
      (is (search "RELAYED outside the household" (pai-v3.api::forum-text b nil)) "and the list of threads")
      (is (eq t (obj-get (pai-v3.api::forum-thread-json b open :member "guest:muse") "relayed")) "the guest API says so")
      (is (search "Hold THIS post to that" (pai-v3.api::%crew-speaking-prompt b "d-x" open nil "answer" "SYNTHETIC"))
          "the review of a post to it judges the post as held outside")
      (is (null (search "RELAYED" (pai-v3.api::%crew-speaking-prompt b "d-y" crew nil "answer" "SYNTHETIC"))))
      (pai-v3.api::forum-set-thread a open "operator" "Sam" :relayed :false)
      (is (not (pai-v3.api::thread-relayed-p b open)) "cleared as easily, for everyone")
      ;; A relayed guest's own thread is marked from the start.
      (change-setting a "forum.relay_guests" (list "Muse") :origin *operator*)
      (let ((pai-v3.mind::*decision-profile* (guest-judge 0)))
        (let ((mine (obj-get (pai-v3.api::guest-post a "guest:muse" "Muse" :title "SYNTHETIC from Muse" :text "SYNTHETIC hi") "thread_id")))
          (is (pai-v3.api::thread-relayed-p a mine))
          (is (pai-v3.api::thread-relayed-p b mine) "everywhere"))))))

(run-suite "api-forum")
