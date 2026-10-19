(in-package #:pai-v3.api)

;;; The watchdog. Twice in one afternoon an agent stopped answering with its
;;; process alive and idle: every request waiting inside the database, nothing
;;; written, no thread of its own doing anything. A restart cured it, but
;;; nobody knew until a person noticed, and the cause was not visible. The
;;; watchdog makes both better:
;;;
;;; - It checks, from inside, what a person would notice from outside: that
;;;   the agent answers its own state route, and that the database will take
;;;   a write lock. Each check runs in a thread of its own with a deadline, so
;;;   a stuck check cannot stick the watchdog.
;;; - When both have failed for watchdog.stall_seconds, it writes every
;;;   thread's backtrace to a file beside the ledger, leaves a note for the
;;;   next start, and exits without unwinding (a wedged process cannot be
;;;   trusted to unwind). The container restarts it.
;;; - The next start tells the operator what happened and where the evidence
;;;   is, and records it in the ledger.
;;;
;;; Guards against making things worse: nothing is done in the first minutes
;;; after a start (a replay or rebuild can legitimately be slow), and at most
;;; three restarts an hour, after which it only records and says so.

(define-setting "watchdog.enabled"
  :type :boolean :default t :group "Health"
  :description "Restart this agent, keeping a record of what every thread was doing, when it has stopped answering for watchdog.stall_seconds.")
(define-setting "watchdog.stall_seconds"
  :type :integer :min 30 :max 3600 :default 120 :group "Health"
  :description "How long the agent may fail to answer its own checks before the watchdog restarts it.")
(define-setting "watchdog.interval_seconds"
  :type :integer :min 5 :max 300 :default 15 :group "Health"
  :description "How often the watchdog checks.")

(define-event-type "watchdog.recovered"
  :schema (("stalled_seconds" :integer) ("stalled_at" :string) ("dump_file" :string)))

(defparameter *watchdog-grace-seconds* 180 "Nothing is done this soon after a start.")
(defparameter *watchdog-restarts-per-hour* 3)
(defparameter *watchdog-keep-dumps* 5)

(defparameter *watchdog-dump-wait* 3 "Seconds allowed for the other threads to report.")
(defvar *watchdog-last* nil "(when healthy unhealthy-since checks) of the latest check, for the dev peek.")
(defvar *watchdog-exit* (lambda () (sb-ext:exit :code 3 :abort t))
  "How the watchdog ends a wedged process. Replaced in tests.")
(defvar *watchdog-timing* nil
  "Tests only: (interval stall grace) in seconds, in place of the settings.")
(defvar *watchdog-probes* nil
  "Tests only: a function of the kernel and port returning true when healthy.")

;;; The decision, apart from everything that can go wrong around it

(defun watchdog-step (since healthy now stall)
  "SINCE is NIL while healthy, else when the failures began. Returns the new
SINCE and what to do: :ok, :unhealthy (keep watching) or :restart."
  (cond (healthy (values nil :ok))
        ((null since) (values now :unhealthy))
        ((>= (- now since) stall) (values since :restart))
        (t (values since :unhealthy))))

;;; Checks

(defun %within (seconds fn)
  "FN's value from a thread of its own, or :stuck if it has not finished in
SECONDS. A stuck thread is left where it is."
  (let ((thread (sb-thread:make-thread (lambda () (handler-case (funcall fn) (error () :error)))
                                       :name "pai-v3 watchdog probe")))
    (sb-thread:join-thread thread :default :stuck :timeout seconds)))

(defun %db-writable-p (kernel &optional (seconds 20))
  "Whether the database takes a write lock within SECONDS, as any change to the ledger must."
  (eq :ok (%within seconds (lambda ()
                             (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
                               (with-transaction ((kernel-db kernel)) nil))
                             :ok))))

(defun %healthy-p (kernel port)
  "Whether the agent answers its own state route and the database takes a write lock."
  (if *watchdog-probes*
      (funcall *watchdog-probes* kernel port)
      (and (eq :ok (%within 20 (lambda ()
                                 (multiple-value-bind (body status)
                                     (http-request :get (format nil "http://127.0.0.1:~d/v1/auth/state" port)
                                                   :allow-private t :connect-timeout 5 :read-timeout 20 :max-redirects 0)
                                   (declare (ignore body))
                                   (if (eql status 200) :ok :bad)))))
           (%db-writable-p kernel))))

;;; The record

(defun %state-dir (kernel)
  (uiop:pathname-directory-pathname (pai-v3.kernel::db-path (kernel-db kernel))))

(defun %utc-stamp (&optional (at (now)))
  (multiple-value-bind (s mi h d mo y) (decode-universal-time at 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" y mo d h mi s)))

(defun %write-thread-dump (dir)
  "Every thread's backtrace into a new file in DIR; the oldest beyond
*watchdog-keep-dumps* are removed. Returns the file's name."
  (let* ((name (format nil "watchdog-~a.txt" (%utc-stamp)))
         (path (merge-pathnames name dir))
         (lock (bt:make-lock "watchdog dump")))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "=== watchdog thread dump at ~a: ~d threads~%" (now-iso8601) (length (sb-thread:list-all-threads)))
      (dolist (thread (sb-thread:list-all-threads))
        (flet ((dump ()
                 (bt:with-lock-held (lock)
                   (format out "~&--- ~a~%" (sb-thread:thread-name thread))
                   (ignore-errors (sb-debug:print-backtrace :count 30 :stream out))
                   (finish-output out))))
          (if (eq thread sb-thread:*current-thread*)
              (dump)
              (ignore-errors (sb-thread:interrupt-thread thread #'dump)))))
      ;; Interrupts are delivered asynchronously; give them a moment.
      (sleep *watchdog-dump-wait*)
      (finish-output out))
    (let ((old (sort (directory (merge-pathnames "watchdog-*.txt" dir)) #'string> :key #'namestring)))
      (dolist (file (nthcdr *watchdog-keep-dumps* old)) (ignore-errors (delete-file file))))
    name))

(defun %exits-file (dir) (merge-pathnames "watchdog.exits" dir))
(defun %pending-file (dir) (merge-pathnames "watchdog.pending" dir))

(defun %recent-exits (dir)
  "How many restarts the watchdog has made in the last hour."
  (let ((file (%exits-file dir)) (cutoff (- (now) 3600)))
    (if (probe-file file)
        (with-open-file (in file :external-format :utf-8)
          (loop for line = (read-line in nil) while line
                count (let ((at (ignore-errors (parse-integer line)))) (and at (> at cutoff)))))
        0)))

(defun %note-exit (dir)
  (with-open-file (out (%exits-file dir) :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~d~%" (now))))

(defun %leave-note (dir stalled at dump)
  (with-open-file (out (%pending-file dir) :direction :output :if-exists :supersede :external-format :utf-8)
    (format out "~d~%~a~%~a~%" stalled at dump)))

(defun %announce-recovery (kernel)
  "On a start after a watchdog restart: record it and tell the operator where the evidence is."
  (let ((file (%pending-file (%state-dir kernel))))
    (when (probe-file file)
      (destructuring-bind (&optional stalled at dump)
          (with-open-file (in file :external-format :utf-8) (loop for line = (read-line in nil) while line collect line))
        (let ((seconds (or (ignore-errors (parse-integer stalled)) 0)))
          (append! kernel "watchdog.recovered" (obj "stalled_seconds" seconds "stalled_at" (or at "") "dump_file" (or dump "")))
          (let ((event (post-notice kernel :channel "notify" :audience "operator" :importance "high"
                                           :text (format nil "I stopped answering for about ~d seconds at ~a and was restarted by my watchdog. What every thread was doing is saved as ~a beside my ledger, for finding the cause."
                                                         seconds (or at "an unknown time") (or dump "(no file)")))))
            (ignore-errors (notify kernel :topic "system-alert" :importance 3 :origin "runtime"
                                          :summary "I was unresponsive and restarted myself; details are in the terminal."
                                          :message-event-id (event-id event)
                                          :deep-link (format nil "/terminal#m=~d" (event-id event))))))
        (ignore-errors (delete-file file))))))

;;; The loop

(defun %watchdog-timing (kernel)
  (if *watchdog-timing*
      (values-list *watchdog-timing*)
      (values (setting kernel "watchdog.interval_seconds") (setting kernel "watchdog.stall_seconds")
              *watchdog-grace-seconds*)))

(defun %restart-wedged (kernel stalled at)
  (let ((dir (%state-dir kernel)))
    (if (>= (%recent-exits dir) *watchdog-restarts-per-hour*)
        (format *error-output* "~&watchdog: stalled ~d seconds, but already restarted ~d times this hour; not again.~%"
                stalled *watchdog-restarts-per-hour*)
        (let ((dump (ignore-errors (%write-thread-dump dir))))
          (%note-exit dir)
          (ignore-errors (%leave-note dir stalled at (or dump "")))
          (format *error-output* "~&watchdog: no answer for ~d seconds; restarting. Threads saved as ~a.~%" stalled dump)
          (finish-output *error-output*)
          (funcall *watchdog-exit*)))))

(defun start-watchdog (agent port)
  "Watch AGENT, serving on PORT, from a thread of its own."
  (let ((kernel (agent-kernel agent)) (started (get-universal-time)))
    (ignore-errors (%announce-recovery kernel))
    (bt:make-thread
     (lambda ()
       (let ((since nil) (since-iso nil))
         (loop while (agent-running agent)
               do (multiple-value-bind (interval stall grace) (ignore-errors (%watchdog-timing kernel))
                    (setf interval (or interval 15) stall (or stall 120) grace (or grace 180))
                    (sleep interval)
                    (when (ignore-errors (setting kernel "watchdog.enabled"))
                      (let* ((now (get-universal-time)) (healthy (%healthy-p kernel port)))
                        (multiple-value-bind (next action) (watchdog-step since healthy now stall)
                          (when (and next (null since)) (setf since-iso (now-iso8601)))
                          (setf since next
                                *watchdog-last* (list (now-iso8601) (if healthy t :false) (or since-iso :null)
                                                      (1+ (or (fourth *watchdog-last*) 0))))
                          (when (and (eq action :restart) (> (- now started) grace))
                            (%restart-wedged kernel (- now since) since-iso)))))))))
     :name "pai-v3 watchdog")))
