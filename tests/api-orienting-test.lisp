(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Noticing change (bead v3-wsd). Fixes and calendar events are what the
;;; phone and the calendar poll would supply; the geocoder answers from a table.

(defparameter *ori-operator* (obj "principal_type" "operator"))

(defmacro with-orienting ((k &key (senses ''("location" "calendar"))) &body body)
  `(with-clock
     (with-temp-kernel (,k)
       (at-hour 15)
       (clrhash pai-v3.api::*location-pending*)
       (change-setting ,k "time.zone" "America/New_York" :origin *ori-operator*)
       (change-setting ,k "senses.attend" ,senses :origin *ori-operator*)
       (change-setting ,k "location.places" (list (obj "name" "Mom's" "lat" 43.0 "lon" -80.0 "radius_m" 200)) :origin *ori-operator*)
       (let ((pai-v3.api::*geocoded* nil)
             (pai-v3.mind::*http-get* (lambda (url headers) (declare (ignore url headers))
                                        (values "{\"address\":{\"road\":\"King Street\",\"city\":\"Toronto\"}}" 200 "application/json" nil))))
         ,@body))))

(defun fix (k lat lon &optional speed)
  (pai-v3.api::record-location k lat lon :speed-kmh speed :fixed-at (now-iso8601)))

(defun noticings (k)
  (mapcar #'second (sql-rows (kernel-db k) "SELECT id,reason FROM thoughts WHERE origin=?1 ORDER BY id LIMIT 50" "noticing")))

(deftest a-place-counts-once-it-holds-and-a-red-light-starts-nothing
  (with-orienting (k)
    (fix k 43.7d0 -79.4d0)                         ; where things stand: no thought
    (is (null (noticings k)))
    (fix k 43.71d0 -79.4d0 80) (advance 60)        ; driving: two fixes in a row
    (fix k 43.72d0 -79.4d0 80)
    (is (= 1 (length (noticings k))) "started driving")
    (is (search "started driving" (first (noticings k))))
    (advance 60) (fix k 43.73d0 -79.4d0 0)          ; a red light
    (advance 60) (fix k 43.74d0 -79.4d0 70)
    (is (= 1 (length (noticings k))) "a stop shorter than the dwell is nothing")
    (advance 60) (fix k 43.0001d0 -80.0d0 0)        ; arrives at Mom's and stays
    (advance 200) (fix k 43.0001d0 -80.0d0 0)
    (is (= 2 (length (noticings k))))
    (let ((r (second (noticings k))))
      (is (search "stopped driving and is at Mom's" r))
      (is (search "Say it as an assumption" r))
      (is (search "what I chose not to do and why" r)))))

(deftest a-watched-for-change-stands-out-and-repeats-surprise-less
  (with-orienting (k)
    (let ((watch (pai-v3.mind::tool-run (gethash "watch_for" pai-v3.mind::*tools*))))
      (is (search "Watching for" (funcall watch k nil (obj "action" "set" "what" "arriving at Mom's" "why" "pin it"))))
      (change-setting k "orienting.threshold" 1.5d0 :origin *ori-operator*)   ; only a watched change can reach it
      ;; Fixes ten minutes apart: no speed is worked out from a jump between them.
      (fix k 43.7d0 -79.4d0)
      (advance 700) (fix k 43.0001d0 -80.0d0) (advance 200) (fix k 43.0001d0 -80.0d0)
      (is (= 1 (length (noticings k))) "the watch lifts it over the threshold")
      (is (search "I was watching for this: arriving at Mom's (pin it)" (first (noticings k))))
      (change-setting k "orienting.threshold" 0.45d0 :origin *ori-operator*)
      (let ((first-surprise (sql-value (kernel-db k) "SELECT max(event_id) FROM sense_changes LIMIT 1")))
        (declare (ignore first-surprise))
        (dotimes (i 3)
          (advance 700) (fix k 43.7d0 -79.4d0) (advance 200) (fix k 43.7d0 -79.4d0)
          (advance 700) (fix k 43.0001d0 -80.0d0) (advance 200) (fix k 43.0001d0 -80.0d0))
        (is (search "seen 3 times in the last month"
                    (car (last (noticings k)))) "habituation is stated")))))

(deftest a-sense-not-attended-notices-nothing
  (with-orienting (k :senses '())
    (fix k 43.7d0 -79.4d0) (advance 60) (fix k 43.71d0 -79.4d0 80) (advance 60) (fix k 43.72d0 -79.4d0 80)
    (is (null (noticings k)))
    (is (= 0 (sql-value (kernel-db k) "SELECT count(*) FROM sense_changes LIMIT 1")))))

(deftest a-near-calendar-change-is-noticed-and-a-far-one-is-not
  (with-orienting (k)
    (pai-v3.api::sense-calendar-change k "new" "Dentist" (iso8601 (+ (now) 7200)))
    (pai-v3.api::sense-calendar-change k "new" "Next week" (iso8601 (+ (now) (* 7 86400))))
    (is (= 1 (length (noticings k))))
    (is (search "a new event in the next few hours: Dentist" (first (noticings k))))))

(run-suite "api-orienting")
