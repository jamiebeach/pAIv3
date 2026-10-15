(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defun operator-origin ()
  (obj "principal_type" "operator" "auth_method" "passkey" "credential_id" "dev-1"))

(deftest turns-and-replies-share-one-conversation-across-channels
  (with-temp-kernel (k)
    (let ((turn (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                                :text "Hi Alpha" :origin (operator-origin))))
      (deliver-reply k :channel "web" :audience "operator" :text "Good morning!"
                       :in-reply-to (event-id turn))
      (receive-turn k :channel "telegram" :audience "operator" :speaker "Operator"
                      :text "On my phone now" :origin (operator-origin))
      (let ((rows (recent-dialogue k "operator")))
        (is (= 3 (length rows)))
        (is (search "web] Operator: Hi Alpha" (dialogue-row-text (first rows))))
        (is (search "] Me: Good morning!" (dialogue-row-text (second rows))))
        (is (string= "telegram" (dialogue-row-channel (third rows))) "oldest first, all channels")))))

(deftest audience-isolates-conversations
  (with-temp-kernel (k)
    (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                    :text "My account balance is private" :origin (operator-origin))
    (receive-turn k :channel "chat" :audience "person:guest" :speaker "Guest"
                    :text "Hello" :origin (obj "principal_type" "person"))
    (is (= 1 (length (recent-dialogue k "person:guest"))))
    (is (notany (lambda (r) (search "balance" (dialogue-row-text r)))
                (recent-dialogue k "person:guest"))
        "operator dialogue never appears in another audience's view")))

(deftest token-budget-keeps-the-most-recent
  (with-temp-kernel (k)
    (dotimes (i 50)
      (receive-turn k :channel "web" :audience "operator" :speaker "Operator"
                      :text (format nil "message number ~d with some ordinary words" i)
                      :origin (operator-origin)))
    (multiple-value-bind (rows used) (recent-dialogue k "operator" :token-budget 100)
      (is (<= used 100))
      (is (< (length rows) 50))
      (is (search "number 49" (dialogue-row-text (car (last rows)))) "newest is kept"))))

(deftest interrupted-reply-is-kept-and-marked
  (with-temp-kernel (k)
    (deliver-reply k :channel "voice" :audience "operator" :text "Let me tell you abo"
                     :interrupted t)
    (is (search "[interrupted]" (dialogue-row-text (first (recent-dialogue k "operator")))))))

(run-suite "mind-dialogue")
