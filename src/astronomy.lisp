(in-package #:datetime-protocol)

;;;; Solar / lunar astronomy for civil calendars.
;;;;
;;;; Independent reimplementation of Jean Meeus formulae (Astronomical
;;;; Algorithms, 2nd ed.): solar longitude (ch. 25), equation of time,
;;;; new moons (ch. 49), sunrise/sunset via solar declination + hour angle.
;;;;
;;;; Moments are Rata Die + fraction (UT). RD 1 = proleptic Gregorian
;;;; 0001-01-01; J2000.0 noon UT = 730120.5.
;;;;
;;;; Location matters:
;;;;   • Civil date of an equinox / solar term = UT moment → standard zone
;;;;     of a reference location (Tokyo, Beijing, …).
;;;;   • Chinese lunisolar month boundaries use Beijing midnight.
;;;;   • Festivals tied to sunrise/sunset / Jewish dusk / Hindu “at sunrise”
;;;;     require latitude and longitude (see ASTRO-LOCATION, SUNRISE, SUNSET).

(defconstant +j2000-moment+ 730120.5d0
  "RD moment of J2000.0 (2000-01-01 12:00 UT).")

(defconstant +mean-tropical-year+ 365.242189d0)
(defconstant +mean-synodic-month+ 29.530588861d0)

(defun %deg-sin (deg) (sin (* deg (/ pi 180d0))))
(defun %deg-cos (deg) (cos (* deg (/ pi 180d0))))
(defun %deg-tan (deg) (tan (* deg (/ pi 180d0))))
(defun %deg-asin (x) (* (asin (max -1d0 (min 1d0 x))) (/ 180d0 pi)))
(defun %deg-acos (x) (* (acos (max -1d0 (min 1d0 x))) (/ 180d0 pi)))
(defun %deg-mod (x) (mod x 360d0))

(defun %poly (x coeffs)
  "Evaluate polynomial COEFFS (lowest degree first) at X."
  (loop for c in (reverse coeffs)
        for acc = c then (+ c (* acc x))
        finally (return acc)))

(defun %angle (d m s)
  (+ d (/ m 60d0) (/ s 3600d0)))

;;; --- Geographic locus -----------------------------------------------------

(defstruct (astro-location (:constructor make-astro-location)
                           (:conc-name location-))
  "Observer locus for zone conversion and sunrise/sunset.
LATITUDE / LONGITUDE in degrees (North / East positive).
ELEVATION in metres. ZONE is the standard offset from UT as a fraction of a
day (e.g. 9/24 for JST). DST is intentionally omitted — ritual and statutory
calendars cite a fixed reference zone."
  (latitude 0d0 :type double-float)
  (longitude 0d0 :type double-float)
  (elevation 0d0 :type double-float)
  (zone 0d0 :type double-float)
  (name nil :type (or null string)))

(defun location (latitude longitude &key (elevation 0d0) (zone 0d0) name)
  "Build an ASTRO-LOCATION. ZONE may be hours (if |ZONE|≤14) or a day-fraction."
  (make-astro-location
   :latitude (float latitude 0d0)
   :longitude (float longitude 0d0)
   :elevation (float elevation 0d0)
   :zone (let ((z (float zone 0d0)))
           (if (<= (abs z) 14d0) (/ z 24d0) z))
   :name name))

(defparameter +beijing+
  (location (%angle 39 55 0) (%angle 116 25 0)
            :elevation 43.5d0 :zone 8 :name "Beijing")
  "Beijing — Chinese civil lunisolar / 放假办法 solar terms (post-1929 CST).")

(defparameter +tokyo+
  (location 35.6895d0 139.6917d0 :elevation 40d0 :zone 9 :name "Tokyo")
  "Tokyo — Japan 春分日/秋分日 civil date (JST).")

(defparameter +delhi+
  (location 28.6139d0 77.2090d0 :elevation 216d0 :zone 5.5d0 :name "New Delhi")
  "New Delhi — India gazetted / sunrise-bound Hindu festivals (IST).")

(defparameter +ujjain+
  (location 23.1765d0 75.7885d0 :elevation 491d0 :zone 5.5d0 :name "Ujjain")
  "Ujjain — traditional Hindu astronomical meridian.")

(defparameter +jerusalem+
  (location 31.778d0 35.235d0 :elevation 754d0 :zone 2 :name "Jerusalem")
  "Jerusalem — Jewish day boundary at sunset (standard offset; ignore DST).")

(defparameter +mecca+
  (location (%angle 21 25 24) (%angle 39 49 24)
            :elevation 298d0 :zone 3 :name "Mecca")
  "Mecca — Islamic qibla / prayer-time reference.")

(defun standard-from-universal (moment loc)
  (+ moment (location-zone loc)))

(defun universal-from-standard (moment loc)
  (- moment (location-zone loc)))

(defun zone-from-longitude (longitude-degrees)
  (/ longitude-degrees 360d0))

(defun local-from-universal (moment loc)
  (+ moment (zone-from-longitude (location-longitude loc))))

(defun universal-from-local (moment loc)
  (- moment (zone-from-longitude (location-longitude loc))))

(defun civil-date-at (moment loc)
  "Gregorian DATE containing UT MOMENT in LOC's standard zone."
  (date-from-rd (floor (standard-from-universal moment loc))))

;;; --- Time scales / solar geometry -----------------------------------------

(defun julian-centuries (moment)
  "Julian centuries since J2000.0 for RD MOMENT (UT)."
  (/ (- moment +j2000-moment+) 36525d0))

(defun ephemeris-correction (moment)
  "ΔT ≈ TT−UT in days. Simplified Meeus/NASA polynomials for civil use."
  (let* ((year (date-year (date-from-rd (floor moment))))
         (y (- year 2000d0)))
    (cond
      ((<= 2005 year 2050)
       (/ (%poly y '(62.92d0 0.32217d0 0.005589d0)) 86400d0))
      ((<= 1986 year 2004)
       (/ (%poly y '(63.86d0 0.3345d0 -0.060374d0 0.0017275d0
                     0.000651814d0 0.00002373599d0))
          86400d0))
      ((<= 1900 year 1985)
       (let ((c (/ (- (fixed-from-date +gregorian+ year 7 1)
                      (fixed-from-date +gregorian+ 1900 1 1))
                   36525d0)))
         (%poly c '(-0.00002d0 0.000297d0 0.025184d0 -0.181133d0
                    0.553040d0 -0.861938d0 0.677066d0 -0.212591d0))))
      (t
       (/ (+ -20d0 (* 32d0 (expt (/ (- year 1820d0) 100d0) 2))) 86400d0)))))

(defun dynamical-from-universal (moment)
  (+ moment (ephemeris-correction moment)))

(defun universal-from-dynamical (moment)
  (- moment (ephemeris-correction moment)))

(defun obliquity (moment)
  "Mean obliquity of the ecliptic (degrees) at UT MOMENT."
  (let ((c (julian-centuries (dynamical-from-universal moment))))
    (+ (%angle 23 26 21.448d0)
       (%poly c (list 0d0
                      (/ -46.8150d0 3600d0)
                      (/ -0.00059d0 3600d0)
                      (/ 0.001813d0 3600d0))))))

(defun solar-longitude (moment)
  "Apparent geocentric ecliptic longitude of the Sun at UT MOMENT (degrees)."
  (let* ((tee (dynamical-from-universal moment))
         (c (julian-centuries tee))
         (l0 (%deg-mod (%poly c '(280.46646d0 36000.76983d0 0.0003032d0))))
         (m (%deg-mod (%poly c '(357.52911d0 35999.05029d0 -0.0001537d0))))
         (mr (* m (/ pi 180d0)))
         (c-sun (+ (* (%poly c '(1.914602d0 -0.004817d0 -0.000014d0)) (sin mr))
                   (* (%poly c '(0.019993d0 -0.000101d0)) (sin (* 2 mr)))
                   (* 0.000289d0 (sin (* 3 mr)))))
         (true-long (%deg-mod (+ l0 c-sun)))
         (omega (%deg-mod (- 125.04d0 (* 1934.136d0 c)))))
    (%deg-mod (+ true-long -0.00569d0 (* -0.00478d0 (%deg-sin omega))))))

(defun solar-declination (moment)
  "Geocentric declination of the Sun at UT MOMENT (degrees)."
  (let* ((eps (obliquity moment))
         (lam (solar-longitude moment)))
    (%deg-asin (* (%deg-sin eps) (%deg-sin lam)))))

(defun equation-of-time (moment)
  "Equation of time (fraction of a day) at UT MOMENT. Meeus AA ch. 28."
  (let* ((c (julian-centuries moment))
         (lambda (%poly c '(280.46645d0 36000.76983d0 0.0003032d0)))
         (anomaly (%poly c '(357.52910d0 35999.05030d0 -0.0001559d0 -0.00000048d0)))
         (ecc (%poly c '(0.016708617d0 -0.000042037d0 -0.0000001236d0)))
         (eps (obliquity moment))
         (y (expt (%deg-tan (/ eps 2d0)) 2))
         (equation
          (* (/ 1d0 (* 2d0 pi))
             (+ (* y (%deg-sin (* 2 lambda)))
                (* -2d0 ecc (%deg-sin anomaly))
                (* 4d0 ecc y (%deg-sin anomaly) (%deg-cos (* 2 lambda)))
                (* -0.5d0 y y (%deg-sin (* 4 lambda)))
                (* -1.25d0 ecc ecc (%deg-sin (* 2 anomaly)))))))
    (* (signum equation) (min (abs equation) 0.5d0))))

(defun solar-longitude-after (target moment)
  "Next UT moment at or after MOMENT when solar longitude equals TARGET degrees."
  (let* ((rate (/ +mean-tropical-year+ 360d0))
         (tau (+ moment (* rate (%deg-mod (- target (solar-longitude moment))))))
         (lo (max moment (- tau 5d0)))
         (hi (+ tau 5d0)))
    (loop repeat 48
          for mid = (/ (+ lo hi) 2d0)
          for delta = (%deg-mod (- (solar-longitude mid) target))
          do (if (< delta 180d0)
                 (setf hi mid)
                 (setf lo mid))
          finally (return mid))))

(defun estimate-prior-solar-longitude (target moment)
  (let ((rate (/ +mean-tropical-year+ 360d0)))
    (- moment (* rate (%deg-mod (- (solar-longitude moment) target))))))

(defun solar-longitude-date (longitude year &key (location +tokyo+))
  "Gregorian DATE at LOCATION on which the Sun reaches LONGITUDE degrees in YEAR."
  (let* ((start (float (fixed-from-date +gregorian+ year 1 1) 0d0))
         (tee (solar-longitude-after longitude start)))
    (civil-date-at tee location)))

(defun spring-equinox-date (year &key (location +tokyo+))
  "Civil date of the March equinox (λ=0°) in YEAR at LOCATION (default Tokyo/JST)."
  (solar-longitude-date 0d0 year :location location))

(defun autumn-equinox-date (year &key (location +tokyo+))
  "Civil date of the September equinox (λ=180°) in YEAR at LOCATION."
  (solar-longitude-date 180d0 year :location location))

(defun qingming-date (year &key (location +beijing+))
  "清明 — Sun at 15° (Chinese minor solar term) civil date at LOCATION."
  (solar-longitude-date 15d0 year :location location))

;;; --- Sunrise / sunset (need lat/lon) ---------------------------------------

(defun refraction-depression (moment loc)
  "Solar depression (degrees) for upper-limb rise/set: ~0.833° at sea level
plus geometric dip ≈ 0.0347√h (h in metres)."
  (declare (ignore moment))
  (let ((h (max 0d0 (location-elevation loc))))
    (+ 0.8333d0 (* 0.0347d0 (sqrt h)))))

(defun %sunrise-sunset-offset-days (moment loc &optional (depression nil))
  "Signed day-fraction from apparent noon to rise (−) or set (+), or NIL if
the Sun does not cross the horizon (polar day/night)."
  (let* ((phi (location-latitude loc))
         (delta (solar-declination moment))
         (alpha (or depression (refraction-depression moment loc)))
         ;; cos H0 = (−sin α − sin φ sin δ) / (cos φ cos δ)
         (num (- (%deg-sin (- alpha))
                 (* (%deg-sin phi) (%deg-sin delta))))
         (den (* (%deg-cos phi) (%deg-cos delta))))
    (when (and (/= den 0d0) (<= (abs (/ num den)) 1d0))
      (/ (%deg-acos (/ num den)) 360d0))))

(defun midday-ut (rd loc)
  "UT moment of apparent solar noon on civil day RD at LOC."
  (let ((local-noon (+ (float rd 0d0) 0.5d0)))
    ;; Iterate once with equation of time.
    (let* ((ut0 (universal-from-local local-noon loc))
           (e (equation-of-time ut0)))
      (universal-from-local (- local-noon e) loc))))

(defun sunrise (rd loc)
  "Standard-time moment of sunrise on civil day RD at LOC, or NIL if none."
  (let* ((noon (midday-ut rd loc))
         (off (%sunrise-sunset-offset-days noon loc)))
    (when off
      (standard-from-universal (- noon off) loc))))

(defun sunset (rd loc)
  "Standard-time moment of sunset on civil day RD at LOC, or NIL if none."
  (let* ((noon (midday-ut rd loc))
         (off (%sunrise-sunset-offset-days noon loc)))
    (when off
      (standard-from-universal (+ noon off) loc))))

(defun dawn (rd loc &optional (depression 6d0))
  "Standard-time civil/nautical/astronomical dawn (DEPRESSION degrees below
horizon; default 6° = civil). NIL if the Sun does not reach that altitude."
  (let* ((noon (midday-ut rd loc))
         (off (%sunrise-sunset-offset-days noon loc depression)))
    (when off
      (standard-from-universal (- noon off) loc))))

(defun dusk (rd loc &optional (depression 6d0))
  "Standard-time dusk analog of DAWN."
  (let* ((noon (midday-ut rd loc))
         (off (%sunrise-sunset-offset-days noon loc depression)))
    (when off
      (standard-from-universal (+ noon off) loc))))

;;; --- Jewish / Muslim ritual solar times (lat/lon required) -----------------
;;;;
;;;; Jewish calendar days begin at sunset. Melacha (creative work) on Shabbat
;;;; and yom tov is forbidden from sunset at the start until nightfall (tzeit
;;;; hakochavim) at the end — opinions differ on the depression angle.
;;;;
;;;; Islamic fasting (ṣawm) runs from Fajr (dawn, sun below horizon by a
;;;; convention-dependent angle) until Maghrib (sunset). Eating, drinking,
;;;; and marital relations are prohibited in that interval. Prayer times
;;;; likewise depend on the observer's latitude/longitude.

(defparameter +jewish-dusk-vilna+ (%angle 4 40 0)
  "Vilna Gaon nightfall ≈ 4°40′ solar depression.")

(defparameter +jewish-dusk-8.5+ 8.5d0
  "Common 8.5° nightfall (≈ 3 medium stars / some poskim).")

(defparameter +islamic-fajr-angle-mwl+ 18d0
  "Muslim World League Fajr angle (also common for Isha).")

(defparameter +islamic-fajr-angle-egypt+ 19.5d0)
(defparameter +islamic-fajr-angle-isna+ 15d0
  "ISNA Fajr/Isha angle.")

(defun jewish-sunset (rd &key (location +jerusalem+))
  "Standard-time sunset on civil RD at LOCATION — start of the next Jewish
day / onset of Shabbat or yom tov (candle-lighting is earlier by custom)."
  (sunset rd location))

(defun jewish-nightfall (rd &key (location +jerusalem+)
                              (depression +jewish-dusk-vilna+))
  "Standard-time nightfall (tzeit) on civil RD — end of Shabbat/yom tov when
melacha becomes permitted again. DEPRESSION selects the opinion
(+JEWISH-DUSK-VILNA+, +JEWISH-DUSK-8.5+, …)."
  (dusk rd location depression))

(defun jewish-day-begins (rd &key (location +jerusalem+))
  "UT moment when the Jewish day following civil RD begins (sunset)."
  (let ((ss (jewish-sunset rd :location location)))
    (when ss (universal-from-standard ss location))))

(defun jewish-shabbat-interval (friday-rd &key (location +jerusalem+)
                                           (nightfall-depression +jewish-dusk-vilna+))
  "Return (values START-UT END-UT) for Shabbat beginning at sunset on
FRIDAY-RD and ending at nightfall on Saturday (FRIDAY-RD+1). Melacha is
forbidden on [START, END]."
  (let* ((start (jewish-day-begins friday-rd :location location))
         (end-std (jewish-nightfall (1+ friday-rd)
                                    :location location
                                    :depression nightfall-depression))
         (end (when end-std (universal-from-standard end-std location))))
    (values start end)))

(defun jewish-melacha-forbidden-p (ut-moment friday-rd
                                   &key (location +jerusalem+)
                                     (nightfall-depression +jewish-dusk-vilna+))
  "True when UT-MOMENT falls inside Shabbat for the week of FRIDAY-RD."
  (multiple-value-bind (start end)
      (jewish-shabbat-interval friday-rd
                               :location location
                               :nightfall-depression nightfall-depression)
    (and start end (<= start ut-moment end))))

(defun islamic-maghrib (rd &key (location +mecca+))
  "Maghrib — sunset at LOCATION. Ends the daily fast; evening prayer."
  (sunset rd location))

(defun islamic-fajr (rd &key (location +mecca+)
                          (angle +islamic-fajr-angle-mwl+))
  "Fajr — dawn when the Sun is ANGLE degrees below the horizon. Starts the
daily fast. ANGLE defaults to MWL 18°; use +ISLAMIC-FAJR-ANGLE-ISNA+ (15°)
or +ISLAMIC-FAJR-ANGLE-EGYPT+ as required by local convention."
  (dawn rd location angle))

(defun islamic-isha (rd &key (location +mecca+)
                          (angle +islamic-fajr-angle-mwl+))
  "Isha — nightfall when the Sun is ANGLE degrees below the horizon."
  (dusk rd location angle))

(defun islamic-fasting-interval (rd &key (location +mecca+)
                                      (fajr-angle +islamic-fajr-angle-mwl+))
  "Return (values FAJR-UT MAGHRIB-UT) for civil RD at LOCATION. During Ramadan,
eating/drinking/marital relations are forbidden on [FAJR, MAGHRIB]."
  (let* ((f-std (islamic-fajr rd :location location :angle fajr-angle))
         (m-std (islamic-maghrib rd :location location))
         (f (when f-std (universal-from-standard f-std location)))
         (m (when m-std (universal-from-standard m-std location))))
    (values f m)))

(defun islamic-fasting-p (ut-moment rd
                          &key (location +mecca+)
                            (fajr-angle +islamic-fajr-angle-mwl+))
  "True when UT-MOMENT is inside the fasting window of civil RD."
  (multiple-value-bind (fajr maghrib)
      (islamic-fasting-interval rd :location location :fajr-angle fajr-angle)
    (and fajr maghrib (<= fajr ut-moment maghrib))))

;;; --- New moons (Meeus ch. 49) ----------------------------------------------

(defun nth-new-moon (n)
  "UT moment of the N-th new moon (n≈24724 near J2000). Meeus ch. 49."
  (let* ((n0 24724)
         (k (- n n0))
         (c (/ k 1236.85d0))
         (approx (+ +j2000-moment+
                    (%poly c (list 5.09766d0
                                   (* +mean-synodic-month+ 1236.85d0)
                                   0.00015437d0
                                   -0.000000150d0
                                   0.00000000073d0))))
         (cap-e (%poly c '(1d0 -0.002516d0 -0.0000074d0)))
         (solar-anomaly (%deg-mod (%poly c (list 2.5534d0
                                                 (* 1236.85d0 29.10535670d0)
                                                 -0.0000014d0
                                                 -0.00000011d0))))
         (lunar-anomaly (%deg-mod (%poly c (list 201.5643d0
                                                 (* 385.81693528d0 1236.85d0)
                                                 0.0107582d0
                                                 0.00001238d0
                                                 -0.000000058d0))))
         (moon-arg (%deg-mod (%poly c (list 160.7108d0
                                            (* 390.67050284d0 1236.85d0)
                                            -0.0016118d0
                                            -0.00000227d0
                                            0.000000011d0))))
         (cap-omega (%deg-mod (%poly c (list 124.7746d0
                                             (* -1.56375588d0 1236.85d0)
                                             0.0020672d0
                                             0.00000215d0))))
         (sine-coeff
          #(-0.40720d0 0.17241d0 0.01608d0 0.01039d0 0.00739d0 -0.00514d0
            0.00208d0 -0.00111d0 -0.00057d0 0.00056d0 -0.00042d0 0.00042d0
            0.00038d0 -0.00024d0 -0.00007d0 0.00004d0 0.00004d0 0.00003d0
            0.00003d0 -0.00003d0 0.00003d0 -0.00002d0 -0.00002d0 0.00002d0))
         (e-factor #(0 1 0 0 1 1 2 0 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0))
         (solar-coeff #(0 1 0 0 -1 1 2 0 0 1 0 1 1 -1 2 0 3 1 0 1 -1 -1 1 0))
         (lunar-coeff #(1 0 2 0 1 1 0 1 1 2 3 0 0 2 1 2 0 1 2 1 1 1 3 4))
         (moon-coeff #(0 0 0 2 0 0 0 -2 2 0 0 2 -2 0 0 -2 0 -2 2 2 2 -2 0 0))
         (correction
          (+ (* -0.00017d0 (%deg-sin cap-omega))
             (loop for i from 0 below (length sine-coeff)
                   sum (* (aref sine-coeff i)
                          (expt cap-e (aref e-factor i))
                          (%deg-sin (+ (* (aref solar-coeff i) solar-anomaly)
                                       (* (aref lunar-coeff i) lunar-anomaly)
                                       (* (aref moon-coeff i) moon-arg)))))))
         (extra (* 0.000325d0
                   (%deg-sin (%poly c '(299.77d0 132.8475848d0 -0.009173d0)))))
         (add-const #(251.88d0 251.83d0 349.42d0 84.66d0 141.74d0 207.14d0
                      154.84d0 34.52d0 207.19d0 291.34d0 161.72d0 239.56d0 331.55d0))
         (add-coeff #(0.016321d0 26.651886d0 36.412478d0 18.206239d0 53.303771d0
                      2.453732d0 7.306860d0 27.261239d0 0.121824d0 1.844379d0
                      24.198154d0 25.513099d0 3.592518d0))
         (add-factor #(0.000165d0 0.000164d0 0.000126d0 0.000110d0 0.000062d0
                       0.000060d0 0.000056d0 0.000047d0 0.000042d0 0.000040d0
                       0.000037d0 0.000035d0 0.000023d0))
         (additional
          (loop for i from 0 below (length add-const)
                sum (* (aref add-factor i)
                       (%deg-sin (+ (aref add-const i) (* (aref add-coeff i) k)))))))
    (universal-from-dynamical (+ approx correction extra additional))))

(defun new-moon-at-or-after (moment)
  "First new moon at or after MOMENT (UT)."
  (let* ((t0 (nth-new-moon 0))
         (n (round (/ (- moment t0) +mean-synodic-month+))))
    (loop for k from (- n 1)
          for tee = (nth-new-moon k)
          when (>= tee moment) return tee)))

(defun new-moon-before (moment)
  "Last new moon strictly before MOMENT (UT)."
  (let* ((t0 (nth-new-moon 0))
         (n (round (/ (- moment t0) +mean-synodic-month+))))
    (loop for k from (+ n 1) downto (- n 2)
          for tee = (nth-new-moon k)
          when (< tee moment) return tee)))
