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
         (named (or (%named-place kernel lat lon) (%pinned-place kernel lat lon)))
         (place (or named
                    (and (setting kernel "location.geocode") (%geocode kernel lat lon))
                    "somewhere I have no name for"))
         (payload (obj "lat" lat "lon" lon "fixed_at" (or fixed-at (now-iso8601))
                       "place" place "named" (if named t :false) "driving" (if driving t :false))))
    (when (realp accuracy) (setf (gethash "accuracy_m" payload) accuracy))
    (when (realp speed-kmh) (setf (gethash "speed_kmh" payload) speed-kmh))
    (append! kernel "operator.location" payload)
    (ignore-errors (sense-location-fix kernel payload))   ; orienting.lisp: a change may earn a thought
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

;;; Places near the operator, or near a place named (bead v3-dzz; operator,
;;; 2026-11-10: "only when there is a need to"). A tool, so nothing is searched
;;; unless a thought asks; OpenStreetMap's search, the same service that names
;;; a fix, asked at most once a second as its policy requires.

(define-setting "location.search_url"
  :type :string :default "https://nominatim.openstreetmap.org/search" :group "Location"
  :description "Nominatim's search, for finding places by kind or name near a point. Used only when the agent calls find_places.")

(defvar *osm-last-call* 0 "Internal real time of the last call to OpenStreetMap, to keep one a second.")
(defvar *osm-lock* (bt:make-lock "osm"))

(defun %osm-search (kernel params)
  "Nominatim search with PARAMS (an alist), as parsed JSON rows, or NIL."
  (bt:with-lock-held (*osm-lock*)
    (let ((wait (- 1.1 (/ (- (get-internal-real-time) *osm-last-call*) internal-time-units-per-second))))
      (when (plusp wait) (sleep wait)))
    (setf *osm-last-call* (get-internal-real-time)))
  (multiple-value-bind (body status)
      (funcall pai-v3.mind::*http-get*
               (format nil "~a?format=jsonv2&~{~a~^&~}" (setting kernel "location.search_url")
                       (mapcar (lambda (p) (format nil "~a=~a" (car p) (quri:url-encode (princ-to-string (cdr p))))) params))
               (list (cons "Accept" "application/json") (cons "User-Agent" "pAI-v3 (personal assistant; one user)")))
    (and (= status 200) (let ((rows (json-parse body))) (and (listp rows) rows)))))

(defun %num (x) (if (stringp x) (let ((*read-default-float-format* 'double-float)) (ignore-errors (float (read-from-string x) 1d0))) x))

(defun find-places (kernel what &key near (radius-km 5))
  "Up to eight places matching WHAT within RADIUS-KM of NEAR (a place name), or of
the operator's last fix when NEAR is empty. Returns a text for the thought."
  (let* ((radius (max 0.5 (min 50 (or radius-km 5))))
         (centre (if (and near (plusp (length near)) (not (string-equal near "here")))
                     (let ((hit (first (%osm-search kernel (list (cons "q" near) (cons "limit" 1))))))
                       (and hit (list (%num (obj-get hit "lat")) (%num (obj-get hit "lon")) (obj-get hit "display_name"))))
                     (let ((row (first (sql-rows (kernel-db kernel) "SELECT lat,lon,place FROM operator_location WHERE id=1"))))
                       (and row (list (first row) (second row) (third row)))))))
    (if (null centre)
        (if (and near (plusp (length near)))
            (format nil "I could not find ~s on the map." near)
            "I have no location for the operator yet, so there is no 'here'; name a place instead.")
        (destructuring-bind (lat lon label) centre
          (let* ((dlat (/ radius 111d0)) (dlon (/ radius (* 111d0 (max 0.1d0 (cos (* lat (/ pi 180)))))))
                 (rows (%osm-search kernel (list (cons "q" what) (cons "limit" 8) (cons "bounded" 1)
                                                 (cons "viewbox" (format nil "~,5f,~,5f,~,5f,~,5f" (- lon dlon) (+ lat dlat) (+ lon dlon) (- lat dlat))))))
                 (found (sort (loop for r in rows
                                    for rlat = (%num (obj-get r "lat")) for rlon = (%num (obj-get r "lon"))
                                    when (and (realp rlat) (realp rlon))
                                      collect (list (/ (%metres-between lat lon rlat rlon) 1000) (obj-get r "name" "") (obj-get r "display_name" "")))
                              #'< :key #'first)))
            (if (null found)
                (format nil "Nothing on OpenStreetMap matching ~s within ~d km of ~a. Its names for kinds of place are plain words (restaurant, cafe, fuel, pharmacy); try one, a wider radius, or a brand name." what (round radius) label)
                (format nil "OpenStreetMap, ~s within ~d km of ~a (nearest first; its data can be out of date):~{~%- ~a~}"
                        what (round radius) label
                        (mapcar (lambda (f) (format nil "~,1f km: ~a~@[ — ~a~]" (first f)
                                                    (if (plusp (length (second f))) (second f) (third f))
                                                    (and (plusp (length (second f))) (third f))))
                                found))))))))

(define-tool "find_places"
  :description "Find places on OpenStreetMap by kind or name (\"burger\", \"gas station\", \"pharmacy\", a business name) near the operator's last known location, or near a place you name (a town, an address, an exit). Use it only when a question needs it, such as somewhere to eat on a drive; it is not for checking where the operator is. To look along a route, call it near a few towns on the way. Results are nearest first, with distance."
  :summary "Find places by kind or name near the operator or a named place."
  :available '("operator")
  :parameters (obj "type" "object"
                   "properties" (obj "what" (obj "type" "string") "near" (obj "type" "string") "radius_km" (obj "type" "number"))
                   "required" (list "what"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (handler-case (find-places kernel (obj-get args "what" "") :near (obj-get args "near") :radius-km (obj-get args "radius_km"))
           (error (e) (format nil "The map search failed: ~a" e)))))

;;; Places the agent pins when told (operator, 2026-11-10: "hey, I'm home";
;;; "remember where we parked"; later, "bring up the map of where we parked").
;;; A pin is where the phone's newest fix is, under a name; a pin by the same
;;; name moves. Pins name fixes just as the operator's own places do, and each
;;; comes with a map link that opens directions on the phone.

(define-event-type "place.pinned"
  :schema (("name" :string) ("lat" :number) ("lon" :number) ("radius_m" :number) ("note" :string :optional) ("pinned" :boolean)))

(define-projection "pinned_places"
  :kind :inline
  :consumes ("place.pinned")
  :tables ("CREATE TABLE IF NOT EXISTS pinned_places (key TEXT PRIMARY KEY, name TEXT NOT NULL, lat REAL NOT NULL, lon REAL NOT NULL, radius_m REAL NOT NULL, note TEXT, at TEXT NOT NULL)")
  :entity (lambda (e) (string-downcase (obj-get (event-payload e) "name")))
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (eq t (obj-get p "pinned"))
                  (sql-exec db "INSERT OR REPLACE INTO pinned_places(key,name,lat,lon,radius_m,note,at) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                            (string-downcase (obj-get p "name")) (obj-get p "name") (obj-get p "lat") (obj-get p "lon")
                            (obj-get p "radius_m") (%given (obj-get p "note")) (event-at e))
                  (sql-exec db "DELETE FROM pinned_places WHERE key=?1" (string-downcase (obj-get p "name")))))))

(defun %pinned-place (kernel lat lon)
  (loop for (name plat plon radius) in (sql-rows (kernel-db kernel) "SELECT name,lat,lon,radius_m FROM pinned_places WHERE key>=''")
        when (<= (%metres-between lat lon plat plon) radius) return name))

(defun map-link (lat lon)
  "Directions to LAT, LON: on a phone, the link opens its maps app."
  (format nil "https://www.google.com/maps/dir/?api=1&destination=~,6f,~,6f" lat lon))

(defun pin-place (kernel name &key note)
  (let ((row (first (sql-rows (kernel-db kernel) "SELECT lat,lon,fixed_at FROM operator_location WHERE id=1"))))
    (cond ((zerop (length name)) "Not pinned: give the place a name.")
          ((null row) "Not pinned: I have no location from the operator's phone yet. Ask them to open the app with location allowed.")
          ((> (%minutes-since (third row)) 10)
           (format nil "Not pinned: the newest fix from the operator's phone is ~d minutes old, so it may not be where they are now. Ask them to open the app (it sends a fresh fix), then pin again." (%minutes-since (third row))))
          (t (destructuring-bind (lat lon at) row
               (declare (ignore at))
               (let ((p (obj "name" name "lat" lat "lon" lon "radius_m" 120 "pinned" t)))
                 (when (and note (plusp (length note))) (setf (gethash "note" p) note))
                 (append! kernel "place.pinned" p))
               (format nil "Pinned ~s where the operator's phone is now. Directions there: ~a" name (map-link lat lon)))))))

(define-tool "pin_place"
  :description "Remember where the operator is now, under a name: when they say \"I'm home\", \"remember where we parked\", \"pin this spot\". Uses the newest fix from their phone, and refuses if it is stale. Pinning a name again moves it. With forget true, removes the pin. Returns a map link with directions."
  :summary "Pin where the operator is now under a name, or forget a pin."
  :available '("operator")
  :core '("responding")
  :parameters (obj "type" "object"
                   "properties" (obj "name" (obj "type" "string") "note" (obj "type" "string") "forget" (obj "type" "boolean"))
                   "required" (list "name"))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let ((name (string-trim " " (obj-get args "name" ""))))
           (if (eq t (obj-get args "forget"))
               (if (sql-value (kernel-db kernel) "SELECT 1 FROM pinned_places WHERE key=?1" (string-downcase name))
                   (let ((row (first (sql-rows (kernel-db kernel) "SELECT name,lat,lon,radius_m FROM pinned_places WHERE key=?1" (string-downcase name)))))
                     (append! kernel "place.pinned" (obj "name" (first row) "lat" (second row) "lon" (third row) "radius_m" (fourth row) "pinned" :false))
                     (format nil "Forgot the pin ~s." (first row)))
                   (format nil "There is no pin named ~s." name))
               (pin-place kernel name :note (obj-get args "note"))))))

(define-tool "my_places"
  :description "The places I have pinned and the operator's named places, each with how far it is from the operator now and a map link with directions (on a phone it opens the maps app). Use it for \"where did we park?\" or \"take me home\": give the link."
  :summary "List pinned and named places with distance and directions links."
  :available '("operator")
  :parameters (obj "type" "object" "properties" (obj))
  :run (lambda (kernel id args)
         (declare (ignore id args))
         (let* ((here (first (sql-rows (kernel-db kernel) "SELECT lat,lon FROM operator_location WHERE id=1")))
                (rows (append (loop for (name lat lon note at) in (sql-rows (kernel-db kernel) "SELECT name,lat,lon,note,at FROM pinned_places WHERE key>='' ORDER BY at DESC")
                                    collect (list name lat lon (format nil "pinned ~a~@[, ~a~]" (pai-v3.mind::local-stamp kernel at) note)))
                              (loop for p in (setting kernel "location.places")
                                    when (and (realp (obj-get p "lat")) (realp (obj-get p "lon")))
                                      collect (list (obj-get p "name") (obj-get p "lat") (obj-get p "lon") "named by the operator")))))
           (if (null rows)
               "No pinned or named places yet."
               (format nil "~{~a~^~%~}"
                       (mapcar (lambda (r)
                                 (destructuring-bind (name lat lon how) r
                                   (format nil "- ~a (~a)~@[, ~,1f km away~]: ~a" name how
                                           (and here (/ (%metres-between (first here) (second here) lat lon) 1000))
                                           (map-link lat lon))))
                               rows))))))
