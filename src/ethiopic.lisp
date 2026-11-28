(in-package #:datetime-protocol)

;;;; Ethiopian (Ethiopic) calendar: twelve months of 30 days (Meskerem …
;;;; Nehase) and Pagume of 5 days, 6 in a year Y with Y mod 4 = 3. The leap
;;;; day therefore closes the Ethiopian year that ends in the September
;;;; before a Gregorian leap year: 1 Meskerem (Enkutatash) is 12 September
;;;; in that Gregorian year and 11 September otherwise (from 1900 to 2099).
;;;;
;;;; Reference: Reingold & Dershowitz, Calendrical Calculations, ch. 4
;;;; (epoch 1 Meskerem 1 EC = Julian 8-08-29, RD 2796). tests/ethiopic-gold-test.lisp
;;;; checks every 1 Meskerem 1900–2100 EC (data/tests/ethiopic-gold.sexp,
;;;; scripts/generate_ethiopic_gold.py).

(defconstant +ethiopic-epoch-rd+ 2796
  "RD of 1 Meskerem 1 EC = Julian 8-08-29.")

(defun ethiopic-leap-year-p (year)
  "True when Pagume of Ethiopian YEAR has 6 days."
  (cl:= (mod year 4) 3))

(defun ethiopic-month-length (year month)
  (check-type month (integer 1 13))
  (cond ((cl:< month 13) 30)
        ((ethiopic-leap-year-p year) 6)
        (t 5)))

(defun fixed-from-ethiopic (year month day)
  "RD of Ethiopian YEAR MONTH DAY (month 13 is Pagume)."
  (check-type month (integer 1 13))
  (check-type day (integer 1 30))
  (cl:+ +ethiopic-epoch-rd+ -1 (cl:* 365 (cl:1- year)) (floor year 4)
        (cl:* 30 (cl:1- month)) day))

(defun ethiopic-from-fixed (rd)
  "Ethiopian (values YEAR MONTH DAY) of day RD."
  (let* ((year (floor (cl:+ (cl:* 4 (cl:- rd +ethiopic-epoch-rd+)) 1463) 1461))
         (month (cl:1+ (floor (cl:- rd (fixed-from-ethiopic year 1 1)) 30)))
         (day (cl:- rd -1 (fixed-from-ethiopic year month 1))))
    (values year month day)))

(defun ethiopic-dates-in-gregorian-year (g-year month day)
  "Every DATE in Gregorian G-YEAR that is Ethiopian MONTH DAY (one, except
for days that do not exist in a common year, e.g. Pagume 6)."
  (let ((start (fixed-from-date +gregorian+ g-year 1 1))
        (end (fixed-from-date +gregorian+ g-year 12 31)))
    (loop for y from (cl:- g-year 9) to (cl:- g-year 7)
          for rd = (and (cl:<= day (ethiopic-month-length y month))
                        (fixed-from-ethiopic y month day))
          when (and rd (cl:<= start rd end))
            collect (date-from-rd rd))))

(defun ethiopic-new-year-date (g-year)
  "1 Meskerem (Enkutatash) in Gregorian G-YEAR, as a DATE."
  (first (ethiopic-dates-in-gregorian-year g-year 1 1)))
