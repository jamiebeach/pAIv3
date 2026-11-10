(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Where the operator is (beads v3-mre, v3-8d8). No phone, no geocoder: fixes
;;; are what the app would send, and the geocoder answers from a table.

(defparameter *loc-operator* (obj "principal_type" "operator"))
(defun loc-utc (h mi) (encode-universal-time 0 mi h 10 11 2026 0))
(defmacro loc-at ((u) &body body) `(let ((pai-v3.mind::*clock* (lambda () ,u))) ,@body))

(defmacro with-geocoder ((calls) &body body)
  `(let* ((,calls 0)
          (pai-v3.api::*geocoded* nil)
          (pai-v3.mind::*http-get*
            (lambda (url headers)
              (declare (ignore headers))
              (incf ,calls)
              (values (if (search "lat=43.7" url)
                          "{\"address\":{\"road\":\"Highway 401\",\"city\":\"Toronto\"}}"
                          "{\"address\":{\"road\":\"King Street West\",\"city\":\"Toronto\"}}")
                      200 "application/json" nil))))
     ,@body))

(deftest a-named-place-is-called-by-its-name-and-not-geocoded
  (with-geocoder (calls)
    (with-temp-kernel (k)
      (change-setting k "time.zone" "America/New_York" :origin *loc-operator*)
      (change-setting k "location.places" (list (obj "name" "Home" "lat" 43.5 "lon" -79.6 "radius_m" 200)) :origin *loc-operator*)
      (loc-at ((loc-utc 12 0))
        (pai-v3.api::record-location k 43.5005d0 -79.6d0 :fixed-at (now-iso8601))
        (is (= 0 calls))
        (let ((lines (pai-v3.api::location-present-lines k)))
          (is (search "Where the operator is, from their phone at 07:00: at Home (since 07:00)." (first lines))))))))

(deftest a-fast-fix-is-driving-near-a-geocoded-street-and-ages-into-where-they-were
  (with-geocoder (calls)
    (with-temp-kernel (k)
      (change-setting k "time.zone" "America/New_York" :origin *loc-operator*)
      (loc-at ((loc-utc 12 57))
        (pai-v3.api::record-location k 43.7d0 -79.4d0 :speed-kmh 96 :fixed-at (now-iso8601))
        (is (search "driving at about 96 km/h, near Highway 401, Toronto (since 07:57)"
                    (first (pai-v3.api::location-present-lines k))))
        (is (pai-v3.api::operator-driving-p k))
        (is (search "The operator is driving" (pai-v3.api::driving-voice-note k))))
      (loc-at ((loc-utc 12 58))
        ;; No speed from the phone: worked out from the last fix, 1.5 km in a minute.
        (let ((p (pai-v3.api::record-location k 43.7135d0 -79.4d0 :fixed-at (now-iso8601))))
          (is (eq t (obj-get p "driving")))
          (is (= 2 calls) "each new street is asked once")))
      (loc-at ((loc-utc 14 30))
        (is (not (pai-v3.api::operator-driving-p k)) "an old fix is no longer driving")
        (is (null (pai-v3.api::driving-voice-note k)))
        (is (search "Where the operator was last, from their phone at 07:58 (92 min ago; they may have moved)"
                    (first (pai-v3.api::location-present-lines k)))))
      (is (= 2 (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE event_type='operator.location'"))
          "every fix is kept"))))

(run-suite "api-location")
