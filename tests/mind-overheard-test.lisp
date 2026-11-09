(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Overheard (2026-11-09): a meeting heard through the avatar, recorded as the
;;; operator's words, is marked as what it was.

(defparameter *ovh-voice* (obj "principal_type" "operator" "mode" "voice"))
(defparameter *ovh-typed* (obj "principal_type" "operator"))
(defun ovh-utc (y mo d h mi) (encode-universal-time 0 mi h d mo y 0))
(defmacro ovh-at ((u) &body body) `(let ((pai-v3.mind::*clock* (lambda () ,u))) ,@body))

(deftest a-span-of-spoken-turns-is-marked-overheard-and-read-so-everywhere
  (with-temp-kernel (k)
    (change-setting k "time.zone" "America/New_York" :origin *ovh-typed*)
    (let (meeting typed after)
      (ovh-at ((ovh-utc 2026 11 9 19 0))       ; 14:00 in New York
        (setf meeting (event-id (receive-turn k :channel "web" :audience "operator" :speaker "Sam"
                                                :text "SYNTHETIC we will pay the monthly fee" :origin *ovh-voice*))))
      (ovh-at ((ovh-utc 2026 11 9 19 30))
        (setf typed (event-id (receive-turn k :channel "web" :audience "operator" :speaker "Sam"
                                              :text "SYNTHETIC typed during the meeting" :origin *ovh-typed*))))
      (ovh-at ((ovh-utc 2026 11 9 21 30))      ; 16:30, after the span
        (setf after (event-id (receive-turn k :channel "web" :audience "operator" :speaker "Sam"
                                              :text "SYNTHETIC hey can you hear me" :origin *ovh-voice*)))
        (let ((run (pai-v3.mind::tool-run (gethash "mark_overheard" pai-v3.mind::*tools*))))
          (is (search "Marked 1 spoken turn" (funcall run k nil (obj "from" "2026-11-09T13:55" "to" "2026-11-09T16:15" "note" "a meeting"))))
          (is (search "No spoken turns" (funcall run k nil (obj "from" "2026-11-09T13:55" "to" "2026-11-09T16:15" "note" "again")))
              "marking twice marks nothing more"))
        (is (pai-v3.mind::overheard-p k meeting))
        (is (not (pai-v3.mind::overheard-p k typed)) "typed words stay the operator's")
        (is (not (pai-v3.mind::overheard-p k after)) "outside the span stays theirs")
        (let ((lines (mapcar #'dialogue-row-text (recent-dialogue k "operator" :token-budget 4000))))
          (is (some (lambda (l) (search "Overheard (not said to me): SYNTHETIC we will pay the monthly fee" l)) lines)
              "the conversation reads it as overheard")
          (is (some (lambda (l) (search "Sam: SYNTHETIC typed during the meeting" l)) lines)))
        (is (search "Overheard (not said to me): SYNTHETIC we will pay"
                    (sql-value (kernel-db k) "SELECT text FROM experience_fts WHERE event_id=?1 LIMIT 1" meeting))
            "search finds it as overheard")
        (is (null (pai-v3.mind::%operator-words k meeting)) "not grounds for anything about the operator")
        (is (pai-v3.mind::%operator-words k typed))))))

(run-suite "mind-overheard")
