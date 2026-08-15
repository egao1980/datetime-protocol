(in-package #:datetime-protocol/tests)

;;;; Golden values cross-checked against Python's datetime.date.toordinal()/
;;;; isocalendar() (proleptic Gregorian ordinal 1 = 0001-01-01, matching this
;;;; protocol's Rata Die numbering exactly).

(deftest gregorian-epoch-anchors
  (ok (= 1 (date-rd (make-date 1 1 1))))
  (ok (= 719163 (date-rd (make-date 1970 1 1))))
  (ok (= 738886 (date-rd (make-date 2024 1 1))))
  (ok (= 738945 (date-rd (make-date 2024 2 29))))
  (ok (= 730120 (date-rd (make-date 2000 1 1)))))

(deftest gregorian-roundtrip
  (dolist (rd (list 1 2 365 366 719163 738886 738945 730120 1 999999 -1 -365 0))
    (multiple-value-bind (y m d) (date-from-fixed +gregorian+ rd)
      (ok (= rd (fixed-from-date +gregorian+ y m d))
          (format nil "roundtrip rd=~a" rd)))))

(deftest gregorian-leap-years
  (ok (date-leap-year-p (make-date 2000 1 1)))
  (ok (date-leap-year-p (make-date 2024 1 1)))
  (ng (date-leap-year-p (make-date 1900 1 1)))
  (ng (date-leap-year-p (make-date 2023 1 1)))
  (ok (= 29 (days-in-gregorian-month 2024 2)))
  (ok (= 28 (days-in-gregorian-month 1900 2)))
  (ok (= 28 (days-in-gregorian-month 2023 2))))

(deftest gregorian-weekday
  ;; 1970-01-01 is a Thursday (Python weekday()==3, Mon=0); this protocol's
  ;; DATE-DAY-OF-WEEK is Mon=1..Sun=7, so Thursday == 4.
  (ok (= 4 (date-day-of-week (make-date 1970 1 1))))
  (ok (= 1 (date-day-of-week (make-date 1 1 1)))))

(deftest julian-reform-anchor
  ;; Julian 1582-10-04 was immediately followed by Gregorian 1582-10-15.
  (ok (= 1 (- (fixed-from-date +gregorian+ 1582 10 15)
              (fixed-from-date +julian+ 1582 10 4)))))

(deftest julian-modern-offset
  ;; In the modern era (1900-2099), Julian date + 13 days == Gregorian date
  ;; for the same physical day.
  (ok (equal '(2024 1 14) (multiple-value-list (date-from-fixed +gregorian+ (fixed-from-date +julian+ 2024 1 1))))))

(deftest iso-week-golden-values
  ;; 2024-12-31 -> ISO 2025-W01-2 (Tuesday)
  (ok (equal '(2025 1 2) (multiple-value-list (date-from-fixed +iso-week+ (date-rd (make-date 2024 12 31))))))
  ;; 2016-01-01 -> ISO 2015-W53-5 (Friday)
  (ok (equal '(2015 53 5) (multiple-value-list (date-from-fixed +iso-week+ (date-rd (make-date 2016 1 1))))))
  ;; 2016-01-04 -> ISO 2016-W01-1 (Monday)
  (ok (equal '(2016 1 1) (multiple-value-list (date-from-fixed +iso-week+ (date-rd (make-date 2016 1 4))))))
  ;; 2021-01-01 -> ISO 2020-W53-5 (Friday)
  (ok (equal '(2020 53 5) (multiple-value-list (date-from-fixed +iso-week+ (date-rd (make-date 2021 1 1)))))))

(deftest iso-week-roundtrip
  (dolist (rd (list 1 719163 738886 738945 730120 999999 -1000))
    (multiple-value-bind (iy iw iwd) (date-from-fixed +iso-week+ rd)
      (ok (= rd (fixed-from-date +iso-week+ iy iw iwd))
          (format nil "iso-week roundtrip rd=~a" rd)))))

(deftest date-field-protocol
  (let ((d (make-date 2024 3 15)))
    (ok (= 2024 (date-field d :year)))
    (ok (= 3 (date-field d :month)))
    (ok (= 15 (date-field d :day)))
    (ok (= 2024 (date-field d :iso-year +iso-week+)))))

(deftest date-add-days-and-weeks
  (ok (value= (make-date 2024 1 11) (date-add (make-date 2024 1 1) :days 10)))
  (ok (value= (make-date 2024 1 15) (date-add (make-date 2024 1 1) :weeks 2))))

(deftest date-add-month-overflow-error-by-default
  (ok (signals (date-add (make-date 2024 1 31) :months 1) 'datetime-arithmetic-error)))

(deftest date-add-month-overflow-clamp
  (ok (value= (make-date 2024 2 29) (date-add (make-date 2024 1 31) :months 1 :overflow :clamp)))
  (ok (value= (make-date 2023 2 28) (date-add (make-date 2023 1 31) :months 1 :overflow :clamp))))

(deftest date-add-month-overflow-carry
  ;; Jan 31 + 1 month, carried: Feb 29 (2024) + 2 extra days = Mar 2.
  (ok (value= (make-date 2024 3 2) (date-add (make-date 2024 1 31) :months 1 :overflow :carry))))

(deftest date-add-years-leap-day
  (ok (signals (date-add (make-date 2024 2 29) :years 1) 'datetime-arithmetic-error))
  (ok (value= (make-date 2025 2 28) (date-add (make-date 2024 2 29) :years 1 :overflow :clamp))))

(deftest date-diff-days-months-years
  (ok (= 366 (date-diff (make-date 2025 1 1) (make-date 2024 1 1) :days)))
  (ok (= 14 (date-diff (make-date 2025 3 1) (make-date 2024 1 1) :months)))
  (ok (= 1 (date-diff (make-date 2025 1 1) (make-date 2024 1 1) :years)))
  (ok (= 0 (date-diff (make-date 2024 6 1) (make-date 2024 1 1) :years))))

(deftest with-fields-date
  (ok (value= (make-date 2023 3 15) (with-fields (make-date 2024 3 15) :year 2023)))
  (ok (value= (make-date 2024 6 15) (with-fields (make-date 2024 3 15) :month 6)))
  (ok (signals (with-fields (make-date 2024 1 31) :month 2) 'datetime-arithmetic-error))
  (ok (value= (make-date 2024 2 29) (with-fields (make-date 2024 1 31) :month 2 :overflow :clamp))))

(deftest with-fields-time-of-day
  (let ((tod (make-time-of-day 10 30 45)))
    (ok (value= (make-time-of-day 12 30 45) (with-fields tod :hour 12)))
    (ok (value= (make-time-of-day 10 15 45) (with-fields tod :minute 15)))))

(deftest with-fields-moment
  (let ((m (make-moment (make-date 2024 3 15) (make-time-of-day 10 30 0))))
    (ok (value= (make-moment (make-date 2024 3 15) (make-time-of-day 12 30 0))
                (with-fields m :hour 12)))
    (ok (value= (make-moment (make-date 2023 3 15) (make-time-of-day 10 30 0))
                (with-fields m :year 2023)))))

(deftest calendar-month-basics
  (let ((cm (make-calendar-month 2024 2)))
    (ok (= 29 (calendar-month-length cm)))
    (ok (value= (make-date 2024 2 1) (calendar-month-first-date cm)))
    (ok (value= (make-date 2024 2 29) (calendar-month-last-date cm))))
  ;; month overflow carries into year via %NORMALIZE-YEAR-MONTH
  (let ((cm (make-calendar-month 2024 13)))
    (ok (= 2025 (calendar-month-year cm)))
    (ok (= 1 (calendar-month-month cm)))))

(deftest annual-date-in-year
  (let ((leap-day (make-annual-date 2 29)))
    (ok (value= (make-date 2024 2 29) (annual-date-in-year leap-day 2024)))
    (ok (value= (make-date 2023 2 28) (annual-date-in-year leap-day 2023)))
    (ok (signals (annual-date-in-year leap-day 2023 :overflow :error) 'datetime-arithmetic-error))))

(deftest interval-protocol
  (let* ((a (make-instant 0))
         (b (make-instant 100))
         (iv (make-interval a b)))
    (ok (interval-contains-p iv (make-instant 50)))
    (ok (interval-contains-p iv a))
    (ng (interval-contains-p iv b))
    (ok (= 100 (duration-seconds (interval-duration iv))))
    (ok (interval-overlaps-p iv (make-interval (make-instant 50) (make-instant 150))))
    (ng (interval-overlaps-p iv (make-interval (make-instant 100) (make-instant 150))))))
