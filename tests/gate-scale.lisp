(in-package #:pai-v3.test)

;;; Phase 1 scale gate (REL-2, BUD-4). Not part of the default run: it builds
;;; a synthetic ledger of 1M events. Run with tools/run-tests.sh tests/gate-scale.lisp
;;;
;;; Measures, at 100k and at 1M events: time to open the store, heap after a
;;; fixed workload and a full GC, and process RSS. Memory must not grow with
;;; history; opening must be fast and read no history.

(define-event-type "gate.note" :schema (("thread" :string) ("text" :string)))
(define-event-type "gate.other" :schema (("n" :integer)))

(define-projection "gate.threads" :kind :inline :consumes ("gate.note")
  :tables ("CREATE TABLE IF NOT EXISTS g_threads (thread TEXT PRIMARY KEY, notes INTEGER NOT NULL)")
  :reduce (lambda (db e)
            (sql-exec db "INSERT INTO g_threads(thread,notes) VALUES(?1,1) ON CONFLICT(thread) DO UPDATE SET notes=notes+1"
                      (obj-get (event-payload e) "thread"))))

(define-projection "gate.count" :kind :async :consumes ("gate.other")
  :tables ("CREATE TABLE IF NOT EXISTS g_count (k INTEGER PRIMARY KEY CHECK(k=1), n INTEGER NOT NULL)")
  :reduce (lambda (db e) (declare (ignore e))
            (sql-exec db "INSERT INTO g_count(k,n) VALUES(1,1) ON CONFLICT(k) DO UPDATE SET n=n+1")))

(defparameter *path* "/tmp/pai-v3-gate.sqlite3")

(defun rss-mb ()
  (with-open-file (in "/proc/self/status")
    (loop for line = (read-line in nil) while line
          when (and (> (length line) 6) (string= "VmRSS:" line :end2 6))
            return (round (parse-integer line :start 6 :junk-allowed t) 1024))))

(defun heap-mb ()
  (sb-ext:gc :full t)
  (round (sb-kernel:dynamic-usage) (* 1024 1024)))

(defun grow-to (target)
  (let ((k (open-kernel *path* :agent-id "gate" :key *test-key*)))
    (unwind-protect
         (loop with batch = 2000
               for head = (ledger-head k)
               while (< head target)
               do (append-batch!
                   k (loop for i below (min batch (- target head))
                           for n = (+ head i)
                           collect (if (zerop (mod n 4))
                                       (list "gate.other" (obj "n" n))
                                       (list "gate.note" (obj "thread" (format nil "t~d" (mod n 500))
                                                              "text" "synthetic note text of ordinary length")))))
                  (loop repeat 4 while (plusp (step-async-projection k "gate.count" :max-events 1000))))
      (close-kernel k))))

(defun measure ()
  "Open, run a fixed workload, measure. Returns (open-seconds heap-mb rss-mb)."
  (let* ((start (get-internal-real-time))
         (k (open-kernel *path* :agent-id "gate" :key *test-key*))
         (open-s (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))
    (unwind-protect
         (progn
           (dotimes (i 1000)
             (append! k "gate.note" (obj "thread" "w" "text" "workload")))
           (dotimes (i 1000)
             (event-by-id k (1+ (random (ledger-head k)))))
           (dotimes (i 20) (step-async-projection k "gate.count" :max-events 100))
           (list open-s (heap-mb) (rss-mb)))
      (close-kernel k))))

(deftest memory-is-flat-and-startup-is-fast
  (ignore-errors (delete-file *path*))
  (let ((t0 (get-internal-real-time)))
    (grow-to 100000)
    (format t "~&  built 100k in ~,1f s~%" (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
  (destructuring-bind (open-a heap-a rss-a) (measure)
    (format t "~&  100k: open ~,3f s, heap ~d MB, rss ~d MB~%" open-a heap-a rss-a)
    (let ((t0 (get-internal-real-time)))
      (grow-to 1000000)
      (format t "~&  built 1M in ~,1f s~%" (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
    (destructuring-bind (open-b heap-b rss-b) (measure)
      (format t "~&  1M:   open ~,3f s, heap ~d MB, rss ~d MB~%" open-b heap-b rss-b)
      (is (< open-b 30) "REL-2: open under 30 s at 1M events")
      (is (< rss-b 512) "BUD-4: RSS under 512 MB")
      (is (<= heap-b (+ (* heap-a 1.05) 4)) "BUD-4: heap flat from 100k to 1M events")))
  (ignore-errors (delete-file *path*)))

(run-suite "gate-scale")
