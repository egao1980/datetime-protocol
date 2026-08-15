(in-package #:datetime-protocol/tests)

;;;; Shadowed-operator arithmetic. + - < <= > >= = /= min max must behave
;;;; like plain CL:* on numbers (numeric fallback) and dispatch to the
;;;; datetime-aware PLUS/MINUS/LESS/VALUE= methods otherwise.

(deftest numeric-fallback
  (ok (= 6 (+ 1 2 3)))
  (ok (= -4 (- 1 5)))
  (ok (= -1 (- 1)))
  (ok (< 1 2 3))
  (ok (<= 1 1 2))
  (ok (> 3 2 1))
  (ok (>= 3 3 2))
  (ok (= 1 1 1))
  (ok (/= 1 2 3))
  (ng (/= 1 2 1))
  (ok (= 1 (min 3 1 2)))
  (ok (= 3 (max 1 3 2))))

(deftest date-plus-integer-is-days
  (ok (value= (make-date 2024 1 11) (+ (make-date 2024 1 1) 10)))
  (ok (value= (make-date 2024 1 11) (+ 10 (make-date 2024 1 1))))
  (ok (value= (make-date 2023 12 31) (- (make-date 2024 1 1) 1))))

(deftest date-minus-date-is-integer-days
  (ok (= 10 (- (make-date 2024 1 11) (make-date 2024 1 1))))
  (ok (= -10 (- (make-date 2024 1 1) (make-date 2024 1 11)))))

(deftest date-plus-period
  (ok (value= (make-date 2024 2 1) (+ (make-date 2024 1 1) (months 1))))
  (ok (value= (make-date 2025 1 1) (+ (make-date 2024 1 1) (years 1))))
  (ok (value= (make-date 2024 1 8) (+ (make-date 2024 1 1) (weeks 1))))
  (ok (value= (make-date 2024 1 1) (+ (make-date 2024 1 8) (days -7))))
  ;; period + date commutes
  (ok (value= (make-date 2024 2 1) (+ (months 1) (make-date 2024 1 1)))))

(deftest date-minus-period
  (ok (value= (make-date 2023 12 1) (- (make-date 2024 1 1) (months 1)))))

(deftest period-plus-period
  (ok (value= (make-period :years 1 :months 2 :days 3)
              (+ (years 1) (make-period :months 2 :days 3)))))

(deftest instant-plus-duration
  (ok (value= (make-instant 150) (+ (make-instant 100) (seconds 50))))
  (ok (value= (make-instant 40) (- (make-instant 100) (seconds 60))))
  (ok (value= (make-instant 100 500000000) (+ (make-instant 100) (nanos 500000000)))))

(deftest instant-minus-instant-is-duration
  (let ((d (- (make-instant 100) (make-instant 40))))
    (ok (durationp d))
    (ok (= 60 (duration-seconds d)))))

(deftest instant-plus-period-signals
  (ok (signals (+ (make-instant 0) (years 1)) 'datetime-arithmetic-error))
  (ok (signals (+ (years 1) (make-instant 0)) 'datetime-arithmetic-error))
  (ok (signals (- (make-instant 0) (years 1)) 'datetime-arithmetic-error)))

(deftest duration-arithmetic
  (ok (duration-zero-p (make-duration 0)))
  (ng (duration-zero-p (make-duration 1)))
  (ok (duration-negative-p (make-duration -1)))
  (ng (duration-negative-p (make-duration 1)))
  (ok (= 1500000000 (duration-total-nanos (make-duration 1 500000000)))))

(deftest period-zero-p-check
  (ok (period-zero-p (make-period)))
  (ng (period-zero-p (years 1))))

(deftest comparisons-across-dates
  (ok (< (make-date 2020 1 1) (make-date 2021 1 1)))
  (ok (<= (make-date 2020 1 1) (make-date 2020 1 1)))
  (ok (> (make-date 2021 1 1) (make-date 2020 1 1)))
  (ok (= (make-date 2020 1 1) (make-date 2020 1 1)))
  (ok (/= (make-date 2020 1 1) (make-date 2021 1 1))))

(deftest min-max-dates
  (ok (value= (make-date 2020 1 1) (min (make-date 2021 1 1) (make-date 2020 1 1))))
  (ok (value= (make-date 2021 1 1) (max (make-date 2021 1 1) (make-date 2020 1 1)))))

(deftest calendar-month-arithmetic
  (ok (value= (make-calendar-month 2024 3) (+ (make-calendar-month 2024 1) 2)))
  (ok (value= (make-calendar-month 2025 1) (+ (make-calendar-month 2024 12) 1)))
  (ok (= 2 (- (make-calendar-month 2024 3) (make-calendar-month 2024 1))))
  (ok (< (make-calendar-month 2024 1) (make-calendar-month 2024 2))))

(deftest time-of-day-arithmetic
  (ok (value= (make-time-of-day 11 0 0) (+ (make-time-of-day 10 0 0) (hours 1))))
  ;; wraps within the day
  (ok (value= (make-time-of-day 1 0 0) (+ (make-time-of-day 23 0 0) (hours 2))))
  (let ((d (- (make-time-of-day 10 30 0) (make-time-of-day 10 0 0))))
    (ok (= 1800 (duration-seconds d)))))

(deftest date-plus-date-minus-explicit-names
  (ok (value= (make-date 2024 1 11) (date+ (make-date 2024 1 1) 10)))
  (ok (= 10 (date- (make-date 2024 1 11) (make-date 2024 1 1)))))

(deftest unit-constructors
  (ok (value= (make-period :years 1) (years 1)))
  (ok (value= (make-period :months 1) (months 1)))
  (ok (value= (make-period :days 7) (weeks 1)))
  (ok (value= (make-period :days 3) (days 3)))
  (ok (value= (make-duration 3600) (hours 1)))
  (ok (value= (make-duration 60) (minutes 1)))
  (ok (value= (make-duration 1) (seconds 1)))
  (ok (value= (make-duration 0 500) (nanos 500))))
