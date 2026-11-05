(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Stored reminders and carried items against what happened since (beads
;;; v3-hej, v3-hej.1): the operator's forum posts reach the conversation with
;;; them, a reminder or carried item that comes up says when it was set and
;;; what the operator has said since, an old one-off message is checked before
;;; it is sent, and the clock names the coming days.

(defparameter *premise-op* (obj "principal_type" "operator"))
(defun premise-utc (y mo d h mi) (encode-universal-time 0 mi h d mo y 0))
(defmacro premise-at ((u) &body body) `(let ((*clock* (lambda () ,u))) ,@body))

(defun operator-says (k text)
  (receive-turn k :channel "web" :audience "operator" :speaker "Sam" :text text
                  :origin (obj "principal_type" "operator")))

(defun forum-ts () (- (now) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun operator-posts (k text)
  (append! k "forum.posted" (obj "msg_id" (format nil "m-~a" (random 1000000)) "thread_id" "t1" "title" "Evening plans"
                                 "author_id" "operator" "author_name" "Sam" "text" text "ts" (forum-ts))))

(deftest the-operators-forum-threads-are-in-context-but-not-in-the-chat
  (with-temp-kernel (k)
    (operator-says k "Heading out")
    (append! k "forum.posted" (obj "msg_id" "m-peer" "thread_id" "t1" "title" "Evening plans" "author_id" "nova-id"
                                   "author_name" "Nova" "text" "Good luck at squash tonight!" "ts" (forum-ts)))
    (operator-posts k "Stuck in traffic, no squash tonight")
    (append! k "forum.posted" (obj "msg_id" "m-other" "thread_id" "t2" "title" "Peers only" "author_id" "iris-id"
                                   "author_name" "Iris" "text" "a thread the operator is not in" "ts" (forum-ts)))
    (is (not (find-if (lambda (r) (search "no squash" (dialogue-row-text r))) (recent-dialogue k "operator")))
        "the chat transcript is the conversation with me, not the forum")
    (let ((forum (format nil "~{~a~%~}" (funcall (gethash :forum pai-v3.mind::*slot-sources*) k '(:audience "operator") 900))))
      (is (search "Evening plans" forum) "a thread the operator posted in is in context")
      (is (search "Nova: Good luck at squash tonight!" forum) "with what they were answering")
      (is (search "Sam: Stuck in traffic, no squash tonight" forum))
      (is (< (search "Nova:" forum) (search "Sam:" forum)) "in the order it was said")
      (is (not (search "not in" forum)) "threads the operator is not in stay out"))
    (is (null (funcall (gethash :forum pai-v3.mind::*slot-sources*) k '(:audience "peer:nova") 900))
        "and only for the operator")))

(deftest a-carried-item-that-comes-up-is-checked-against-what-was-said-since
  (with-temp-kernel (k)
    (premise-at ((premise-utc 2026 10 29 10 0))
      (carry-add k :what "Ask how squash went (Thu 2026-10-29)" :why "They were looking forward to it" :by "agent"))
    (premise-at ((premise-utc 2026 10 29 22 0))
      (operator-posts k "Stuck in traffic, no squash tonight"))
    (premise-at ((premise-utc 2026 10 30 1 0))
      (let ((prompt (pai-v3.mind::carried-consideration-prompt (first (carried-items k :limit 5)) k)))
        (is (search "This item was set" prompt) "it says when it was set")
        (is (search "What the operator has said since" prompt))
        (is (search "no squash tonight" prompt) "including what they said on the forum")
        (is (search "rather than repeat it" prompt) "and to revise, move or close it rather than repeat it")))))

(deftest an-old-one-off-message-is-checked-before-it-is-sent
  (with-temp-kernel (k)
    (premise-at ((premise-utc 2026 10 29 10 0))
      (add-schedule k "Scope day tomorrow: nothing after midnight" :at "2026-10-29T20:00" :mode "notify"))
    (premise-at ((premise-utc 2026 10 29 15 0))
      (operator-says k "The scope is Fri 2026-11-06, not this Friday"))
    (premise-at ((premise-utc 2026 10 30 0 5))
      (is (= 1 (run-due-schedules k)))
      (is (not (find-if (lambda (r) (search "Scope day tomorrow" (dialogue-row-text r)))
                        (recent-dialogue k "operator" :token-budget 4000)))
          "the old text is not sent as it stood")
      (let ((reason (sql-value (kernel-db k) "SELECT reason FROM thoughts WHERE origin=?1 ORDER BY id DESC LIMIT 1" "noticing")))
        (is (search "only if it still holds" reason) "a thought checks it first")
        (is (search "Fri 2026-11-06" reason) "with what was said since in hand")))
    (premise-at ((premise-utc 2026 10 30 9 0))
      (add-schedule k "Bins out" :in 600 :mode "notify"))
    (premise-at ((premise-utc 2026 10 30 9 11))
      (is (= 1 (run-due-schedules k)))
      (is (find-if (lambda (r) (search "Bins out" (dialogue-row-text r))) (recent-dialogue k "operator" :token-budget 4000))
          "a fresh one is sent as written"))))

(deftest the-clock-names-the-coming-days
  (with-temp-kernel (k)
    (change-setting k "time.zone" "America/Toronto" :origin *premise-op*)
    (premise-at ((premise-utc 2026 10 30 16 0))     ; Friday noon in Toronto
      (let ((text (first (first (funcall (gethash :clock pai-v3.mind::*slot-sources*) k nil 160)))))
        (is (search "It is Friday" text))
        (is (search "tomorrow Sat Oct 31" text))
        (is (search "Fri Nov 6" text) "next Friday is a lookup, not arithmetic")))))

(run-suite "mind-premise")
