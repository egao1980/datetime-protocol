(in-package #:datetime-protocol)

;;;; Solar Hijri (Persian) calendar, astronomical form: the official civil
;;;; calendar of Iran (and Afghanistan). 1 Farvardin is the day on which the
;;;; Sun reaches longitude 0° (the March equinox) before true (apparent) noon
;;;; at the reference meridian; when the equinox falls after that noon the year
;;;; begins the next day. Months 1–6 have 31 days, 7–11 have 30, and Esfand has
;;;; 29 or 30, whatever is left before the next 1 Farvardin.
;;;;
;;;; Reference: Reingold & Dershowitz, Calendrical Calculations (3rd ed.,
;;;; §15.2, "astronomical Persian calendar"), which takes the apparent noon of
;;;; Tehran. The Calendar Center states the rule as noon Iran Standard Time
;;;; (12:00 UTC+3:30, the 52.5° E meridian); both give the same 1 Farvardin in
;;;; every year 1206–1498. tests/persian-gold-test.lisp checks every one of
;;;; them against the leap-year table of the Calendar Center of the Institute
;;;; of Geophysics, University of Tehran (data/tests/persian-gold.sexp,
;;;; scripts/generate_persian_gold.py).

(defparameter +tehran+
  (location 35.68d0 51.42d0 :elevation 1100d0 :zone 3.5d0 :name "Tehran")
  "Tehran — Iran Standard Time (UTC+3:30); the meridian of the Solar Hijri
calendar's noon rule.")

(defparameter *persian-reference-location* +tehran+
  "The location whose apparent noon decides 1 Farvardin.")

(defconstant +persian-epoch-rd+ 226896
  "RD of 1 Farvardin AP 1 = Julian 622-03-19.")

(defun %persian-noon (rd)
  (midday-ut rd *persian-reference-location*))

(defun persian-new-year-on-or-before (rd)
  "RD of the 1 Farvardin on or before day RD."
  ;; ESTIMATE-PRIOR-SOLAR-LONGITUDE uses the mean rate, which can land up to
  ;; about four days after the equinox (the Sun is slower near aphelion), so
  ;; the search starts a week early: days before the equinox have a longitude
  ;; near 360°, never ≤ 2°.
  (let ((approx (estimate-prior-solar-longitude 0d0 (%persian-noon rd))))
    (loop for day from (cl:- (floor approx) 7)
          when (cl:<= (solar-longitude (%persian-noon day)) 2d0)
            return day)))

(defun persian-leap-year-p (year)
  "True when Esfand of Solar Hijri YEAR has 30 days."
  (cl:= 366 (cl:- (fixed-from-persian (cl:1+ year) 1 1) (fixed-from-persian year 1 1))))

(defun persian-month-length (year month)
  (check-type month (integer 1 12))
  (cond ((cl:<= month 6) 31)
        ((cl:<= month 11) 30)
        ((persian-leap-year-p year) 30)
        (t 29)))

(defun fixed-from-persian (year month day)
  "RD of Solar Hijri YEAR MONTH DAY (astronomical rule; YEAR ≥ 1)."
  (check-type year (integer 1))
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (let ((new-year (persian-new-year-on-or-before
                   (cl:+ +persian-epoch-rd+ 180
                         (floor (cl:* +mean-tropical-year+ (cl:1- year)))))))
    (cl:+ new-year -1
          (if (cl:<= month 7)
              (cl:* 31 (cl:1- month))
              (cl:+ (cl:* 30 (cl:1- month)) 6))
          day)))

(defun persian-from-fixed (rd)
  "Solar Hijri (values YEAR MONTH DAY) of day RD."
  (let* ((new-year (persian-new-year-on-or-before rd))
         (year (cl:1+ (round (cl:- new-year +persian-epoch-rd+) +mean-tropical-year+)))
         (day-of-year (cl:1+ (cl:- rd (fixed-from-persian year 1 1))))
         (month (if (cl:<= day-of-year 186)
                    (ceiling day-of-year 31)
                    (ceiling (cl:- day-of-year 6) 30)))
         (day (cl:1+ (cl:- rd (fixed-from-persian year month 1)))))
    (values year month day)))

(defun persian-new-year-date (g-year)
  "DATE of Nowruz, 1 Farvardin, in Gregorian G-YEAR."
  (date-from-rd (persian-new-year-on-or-before (fixed-from-date +gregorian+ g-year 4 30))))

(defun persian-dates-in-gregorian-year (g-year month day)
  "Every DATE in Gregorian G-YEAR that is Solar Hijri MONTH DAY (0–2 of them;
one except for some days of Dey)."
  (let ((start (fixed-from-date +gregorian+ g-year 1 1))
        (end (fixed-from-date +gregorian+ g-year 12 31)))
    (loop for y from (cl:- g-year 622) to (cl:- g-year 621)
          for rd = (and (cl:<= day (persian-month-length y month))
                        (fixed-from-persian y month day))
          when (and rd (cl:<= start rd end))
            collect (date-from-rd rd))))
