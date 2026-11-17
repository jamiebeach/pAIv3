(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

;;; Notes to my next thought (continuity piece 2, first part; forum thread
;;; "Having a body", 2026-11-17).

(defun %leave (k thought text)
  (funcall (pai-v3.mind::tool-run (gethash "leave_note" pai-v3.mind::*tools*)) k thought (obj "text" text)))

(defun %left-for (k audience thought)
  (first (first (funcall (gethash :left-note pai-v3.mind::*slot-sources*) k
                         (list :audience audience :thought-id thought) 200))))

(deftest a-note-reaches-the-next-thought-of-its-audience
  (with-clock
    (with-temp-kernel (k)
      (let ((first (pai-v3.mind:request-thought k "work" "a question" :audience "operator"))
            (next (progn (advance 60) (pai-v3.mind:request-thought k "work" "the next one" :audience "operator")))
            (peer (pai-v3.mind:request-thought k "work" "a crew post" :audience "peer:crew")))
        (is (search "Left for my next thought" (%leave k first "Still open: whether the moth question was answered.")))
        (is (search "whether the moth question was answered" (or (%left-for k "operator" next) ""))
            "the next thought with the same audience gets it")
        (is (null (%left-for k "operator" first)) "a thought is not shown its own note")
        (is (null (%left-for k "peer:crew" peer)) "a thought with another audience never sees it")))))

(deftest a-later-note-replaces-and-old-notes-fade
  (with-clock
    (with-temp-kernel (k)
      (let ((a (pai-v3.mind:request-thought k "work" "one" :audience "operator"))
            (b (pai-v3.mind:request-thought k "work" "two" :audience "operator"))
            (c (pai-v3.mind:request-thought k "work" "three" :audience "operator")))
        (%leave k a "first note")
        (advance 60)
        (%leave k b "second note")
        (is (search "second note" (%left-for k "operator" c)) "the newest note is the one shown")
        (advance (* 25 3600))
        (is (null (%left-for k "operator" c)) "a note older than a day is no longer shown")))))

(deftest an-empty-note-is-not-left
  (with-temp-kernel (k)
    (let ((a (pai-v3.mind:request-thought k "work" "one" :audience "operator")))
      (is (search "empty" (%leave k a "   ")))
      (is (null (pai-v3.mind::latest-note k "operator"))))))

(run-suite "mind-notes")
