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
              (values (if (search "lat=10.7" url)
                          "{\"address\":{\"road\":\"Highway 401\",\"city\":\"Toronto\"}}"
                          "{\"address\":{\"road\":\"King Street West\",\"city\":\"Toronto\"}}")
                      200 "application/json" nil))))
     ,@body))

(deftest a-named-place-is-called-by-its-name-and-not-geocoded
  (with-geocoder (calls)
    (with-temp-kernel (k)
      (change-setting k "time.zone" "America/New_York" :origin *loc-operator*)
      (change-setting k "location.places" (list (obj "name" "Home" "lat" 10.5 "lon" 20.6 "radius_m" 200)) :origin *loc-operator*)
      (loc-at ((loc-utc 12 0))
        (pai-v3.api::record-location k 10.5005d0 20.6d0 :fixed-at (now-iso8601))
        (is (= 0 calls))
        (let ((lines (pai-v3.api::location-present-lines k)))
          (is (search "Where the operator is, from their phone at 07:00: at Home (since 07:00)." (first lines))))))))

(deftest a-fast-fix-is-driving-near-a-geocoded-street-and-ages-into-where-they-were
  (with-geocoder (calls)
    (with-temp-kernel (k)
      (change-setting k "time.zone" "America/New_York" :origin *loc-operator*)
      (loc-at ((loc-utc 12 57))
        (pai-v3.api::record-location k 10.7d0 20.4d0 :speed-kmh 96 :fixed-at (now-iso8601))
        (is (search "driving at about 96 km/h, near Highway 401, Toronto (since 07:57)"
                    (first (pai-v3.api::location-present-lines k))))
        (is (pai-v3.api::operator-driving-p k))
        (is (search "The operator is driving" (pai-v3.api::driving-voice-note k))))
      (loc-at ((loc-utc 12 58))
        ;; No speed from the phone: worked out from the last fix, 1.5 km in a minute.
        (let ((p (pai-v3.api::record-location k 10.7135d0 20.4d0 :fixed-at (now-iso8601))))
          (is (eq t (obj-get p "driving")))
          (is (= 2 calls) "each new street is asked once")))
      (loc-at ((loc-utc 14 30))
        (is (not (pai-v3.api::operator-driving-p k)) "an old fix is no longer driving")
        (is (null (pai-v3.api::driving-voice-note k)))
        (is (search "Where the operator was last, from their phone at 07:58 (92 min ago; they may have moved)"
                    (first (pai-v3.api::location-present-lines k)))))
      (is (= 2 (sql-value (kernel-db k) "SELECT count(*) FROM pai_events WHERE event_type='operator.location'"))
          "every fix is kept"))))

(deftest find-places-searches-around-the-last-fix-nearest-first
  (with-temp-kernel (k)
    (let* ((urls '())
           (pai-v3.api::*osm-last-call* 0)
           (pai-v3.mind::*http-get*
             (lambda (url headers)
               (declare (ignore headers))
               (push url urls)
               (values (if (search "/search" url)
                           "[{\"lat\":\"10.71\",\"lon\":\"20.4\",\"name\":\"Far Burger\",\"display_name\":\"Far Burger, 2 Main St\"},{\"lat\":\"10.701\",\"lon\":\"20.4\",\"name\":\"Near Burger\",\"display_name\":\"Near Burger, 1 Main St\"}]"
                           "{\"address\":{\"road\":\"Main St\",\"city\":\"Toronto\"}}")
                       200 "application/json" nil))))
      (is (search "no location for the operator" (pai-v3.api::find-places k "burger")))
      (pai-v3.api::record-location k 10.7d0 20.4d0 :fixed-at (now-iso8601))
      (let ((text (pai-v3.api::find-places k "burger" :radius-km 3)))
        (is (search "\"burger\" within 3 km of Main St, Toronto" text))
        (is (< (search "Near Burger" text) (search "Far Burger" text)) "nearest first")
        (is (search "0.1 km: Near Burger" text)))
      (is (search "bounded=1" (first urls)) "the search is kept to the area"))))

(deftest a-pin-names-where-the-phone-is-and-gives-directions
  (with-geocoder (calls)
    (with-temp-kernel (k)
      (let ((pin (pai-v3.mind::tool-run (gethash "pin_place" pai-v3.mind::*tools*)))
            (mine (pai-v3.mind::tool-run (gethash "my_places" pai-v3.mind::*tools*))))
        (loc-at ((loc-utc 15 0))
          (is (search "no location" (funcall pin k nil (obj "name" "where we parked"))))
          (pai-v3.api::record-location k 10.6d0 20.5d0 :fixed-at (now-iso8601)))
        (loc-at ((loc-utc 15 30))
          (is (search "30 minutes old" (funcall pin k nil (obj "name" "where we parked"))) "a stale fix is not pinned")
          (pai-v3.api::record-location k 10.6d0 20.5d0 :fixed-at (now-iso8601))
          (is (search "destination=10.600000,20.500000" (funcall pin k nil (obj "name" "where we parked" "note" "level 2"))))
          (let ((p (pai-v3.api::record-location k 10.6003d0 20.5d0 :fixed-at (now-iso8601))))
            (is (equal "where we parked" (obj-get p "place")) "a pin names a fix inside it"))
          (is (search "where we parked (pinned" (funcall mine k nil (obj))))
          (is (search "level 2" (funcall mine k nil (obj))))
          (is (search "Forgot" (funcall pin k nil (obj "name" "Where We Parked" "forget" t))))
          (is (search "No pinned" (funcall mine k nil (obj)))))))))

(run-suite "api-location")
