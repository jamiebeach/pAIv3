(in-package #:pai-v3.api)

;;; Where the operator is (beads v3-mre, v3-8d8; operator, 2026-11-10: on a
;;; drive he asked for the brief twice and was asked follow-ups he could not
;;; answer at 100 km/h). The phone sends a fix when the app opens or is shown
;;; again, and every minute or so while it stays open. Each fix is kept whole
;;; in the ledger (the operator chose full history); the newest is what the
;;; agent sees, named: a place the operator named, else the street and town a
;;; reverse geocoder gives, and driving when the speed says so.

(define-setting "location.enabled"
  :type :boolean :default t :group "Location"
  :description "Accept location fixes from the operator's phone. Off: fixes are refused and the agent sees no location.")
(define-setting "location.places"
  :type :array :default '() :group "Location"
  :description "Places the operator has named: each {name, lat, lon, radius_m}. A fix inside one is called by that name, and is not geocoded.")
(define-setting "location.geocode"
  :type :boolean :default t :group "Location"
  :description "Name a fix outside the named places by asking a reverse geocoder for its street and town. This sends the coordinates to that service.")
(define-setting "location.geocode_url"
  :type :string :default "https://nominatim.openstreetmap.org/reverse" :group "Location"
  :description "The reverse geocoder, Nominatim's API: lat and lon are added to it.")
(define-setting "location.driving_kmh"
  :type :integer :min 5 :max 200 :default 25 :group "Location"
  :description "At or above this speed the operator counts as driving.")
(define-setting "location.fresh_minutes"
  :type :integer :min 1 :max 1440 :default 20 :group "Location"
  :description "A fix younger than this is where the operator is; an older one is only where they were last.")

(define-event-type "operator.location"
  :schema (("lat" :number) ("lon" :number) ("accuracy_m" :number :optional) ("speed_kmh" :number :optional)
           ("fixed_at" :string) ("place" :string) ("named" :boolean) ("driving" :boolean)))

(define-projection "operator_location"
  :kind :inline
  :consumes ("operator.location")
  :tables ("CREATE TABLE IF NOT EXISTS operator_location (id INTEGER PRIMARY KEY CHECK (id=1), lat REAL NOT NULL, lon REAL NOT NULL, speed_kmh REAL, fixed_at TEXT NOT NULL, place TEXT NOT NULL, named INTEGER NOT NULL, driving INTEGER NOT NULL, place_since TEXT NOT NULL)")
  :entity (lambda (e) (declare (ignore e)) "operator")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              ;; place_since: when the operator came to this place, or started driving.
              (sql-exec db "INSERT INTO operator_location(id,lat,lon,speed_kmh,fixed_at,place,named,driving,place_since) VALUES(1,?1,?2,?3,?4,?5,?6,?7,?4) ON CONFLICT(id) DO UPDATE SET lat=excluded.lat,lon=excluded.lon,speed_kmh=excluded.speed_kmh,fixed_at=excluded.fixed_at,place=excluded.place,named=excluded.named,driving=excluded.driving,place_since=CASE WHEN operator_location.driving=excluded.driving AND (excluded.driving=1 OR operator_location.place=excluded.place) THEN operator_location.place_since ELSE excluded.fixed_at END"
                        (obj-get p "lat") (obj-get p "lon") (obj-get p "speed_kmh") (obj-get p "fixed_at")
                        (obj-get p "place") (if (eq t (obj-get p "named")) 1 0) (if (eq t (obj-get p "driving")) 1 0)))))

(defun %metres-between (lat1 lon1 lat2 lon2)
  (let* ((r 6371000d0) (rad (/ pi 180))
         (dlat (* rad (- lat2 lat1))) (dlon (* rad (- lon2 lon1)))
         (a (+ (expt (sin (/ dlat 2)) 2) (* (cos (* rad lat1)) (cos (* rad lat2)) (expt (sin (/ dlon 2)) 2)))))
    (* 2 r (asin (min 1d0 (sqrt a))))))

(defun %named-place (kernel lat lon)
  (loop for p in (setting kernel "location.places")
        for plat = (obj-get p "lat") for plon = (obj-get p "lon")
        when (and (realp plat) (realp plon)
                  (<= (%metres-between lat lon plat plon) (let ((r (obj-get p "radius_m"))) (if (realp r) r 150))))
          return (obj-get p "name")))

(defvar *geocoded* nil "The last geocoded fix: (lat lon name), so a fix nearby is not asked again.")

(defun %geocode (kernel lat lon)
  "The street and town at LAT, LON, or NIL. Nearby fixes reuse the last answer."
  (destructuring-bind (&optional glat glon name) *geocoded*
    (if (and name (< (%metres-between lat lon glat glon) 250))
        name
        (handler-case
            (multiple-value-bind (body status)
                (funcall pai-v3.mind::*http-get*
                         (format nil "~a?format=jsonv2&zoom=17&lat=~,5f&lon=~,5f" (setting kernel "location.geocode_url") lat lon)
                         (list (cons "Accept" "application/json") (cons "User-Agent" "pAI-v3 (personal assistant; one user)")))
              (when (= status 200)
                (let* ((a (obj-get (json-parse body) "address" (obj)))
                       (street (or (obj-get a "road") (obj-get a "neighbourhood") (obj-get a "suburb")))
                       (town (or (obj-get a "city") (obj-get a "town") (obj-get a "village") (obj-get a "municipality")))
                       (name (format nil "~{~a~^, ~}" (remove nil (list street town)))))
                  (when (plusp (length name))
                    (setf *geocoded* (list lat lon name))
                    name))))
          (error () nil)))))

(defun record-location (kernel lat lon &key accuracy speed-kmh fixed-at)
  "Name the fix and record it. Returns the event's payload."
  ;; No speed from the phone: from the last fix, if it is recent and far enough.
  (let ((last (sql-rows (kernel-db kernel) "SELECT lat,lon,fixed_at FROM operator_location WHERE id=1")))
    (when (and (not (realp speed-kmh)) last fixed-at)
      (destructuring-bind (plat plon pat) (first last)
        (let ((secs (- (parse-iso8601 fixed-at) (parse-iso8601 pat))))
          (when (< 20 secs 600)
            (setf speed-kmh (* 3.6 (/ (%metres-between lat lon plat plon) secs))))))))
  (let* ((driving (and (realp speed-kmh) (>= speed-kmh (setting kernel "location.driving_kmh"))))
         (named (%named-place kernel lat lon))
         (place (or named
                    (and (setting kernel "location.geocode") (%geocode kernel lat lon))
                    "somewhere I have no name for"))
         (payload (obj "lat" lat "lon" lon "fixed_at" (or fixed-at (now-iso8601))
                       "place" place "named" (if named t :false) "driving" (if driving t :false))))
    (when (realp accuracy) (setf (gethash "accuracy_m" payload) accuracy))
    (when (realp speed-kmh) (setf (gethash "speed_kmh" payload) speed-kmh))
    (append! kernel "operator.location" payload)
    payload))

(defun %location-row (kernel)
  (first (sql-rows (kernel-db kernel) "SELECT place,named,driving,speed_kmh,fixed_at,place_since FROM operator_location WHERE id=1")))

(defun %minutes-since (iso)
  (floor (- (parse-iso8601 (now-iso8601)) (parse-iso8601 iso)) 60))

(defun operator-driving-p (kernel &optional (within 5))
  "True when the newest fix, younger than WITHIN minutes, says driving."
  (let ((row (%location-row kernel)))
    (and row (= 1 (third row)) (< (%minutes-since (fifth row)) within))))

;;; What the agent sees: one line in "The operator's day". A fresh fix is where
;;; the operator is; an older one is only where they were, with its age, so the
;;; agent weighs it rather than a judgment deciding whether to show it.
(defun location-present-lines (kernel)
  (let ((row (%location-row kernel)))
    (when (and row (setting kernel "location.enabled"))
      (destructuring-bind (place named driving speed fixed since) row
        (let* ((zone (%zone-of kernel))
               (clock (lambda (iso) (local-time:format-timestring nil (local-time:universal-to-timestamp (parse-iso8601 iso))
                                                                  :format '((:hour 2) #\: (:min 2)) :timezone zone)))
               (age (%minutes-since fixed))
               (where (cond ((= 1 driving) (format nil "driving~@[ at about ~d km/h~], near ~a (since ~a)"
                                                   (and (realp speed) (round speed)) place (funcall clock since)))
                            ((= 1 named) (format nil "at ~a (since ~a)" place (funcall clock since)))
                            (t (format nil "near ~a (since ~a)" place (funcall clock since))))))
          (list (if (< age (setting kernel "location.fresh_minutes"))
                    (format nil "Where the operator is, from their phone at ~a: ~a." (funcall clock fixed) where)
                    (format nil "Where the operator was last, from their phone at ~a (~a ago; they may have moved): ~a."
                            (funcall clock fixed) (if (< age 120) (format nil "~d min" age) (format nil "~d h" (floor age 60))) where))))))))

(pushnew 'location-present-lines *present-facets*)

;; Driving, a spoken reply has to be taken in by ear alone.
(defun driving-voice-note (kernel)
  (and (operator-driving-p kernel)
       "The operator is driving: they can listen but not read or look. Give what they asked for first, in a sentence or two; ask a question only if you cannot go on without the answer."))

(pushnew 'driving-voice-note pai-v3.mind::*voice-turn-notes*)
