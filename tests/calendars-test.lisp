(in-package #:datetime-protocol/tests)

;;;; Golden values cross-checked against widely published dates (Western/
;;;; Orthodox Easter, Hebrew Rosh Hashanah/Passover) and, for the Islamic
;;;; calendar, independently against the Kuwaiti/civil tabular JDN formula
;;;; (JDN = floor((11y+3)/30) + 354y + 30m - floor((m-1)/2) + d + 1948055)
;;;; via the Fliegel-Van Flandern JDN->Gregorian algorithm — not against this
;;;; protocol's own code.

;;; --- Easter -----------------------------------------------------------

(deftest easter-western-golden-values
  (ok (value= (make-date 2024 3 31) (easter-western 2024)))
  (ok (value= (make-date 2025 4 20) (easter-western 2025)))
  (ok (value= (make-date 2016 3 27) (easter-western 2016)))
  (ok (value= (make-date 2000 4 23) (easter-western 2000))))

(deftest easter-orthodox-golden-values
  (ok (value= (make-date 2024 5 5) (easter-orthodox 2024)))
  ;; Western and Orthodox Easter coincided in 2025.
  (ok (value= (make-date 2025 4 20) (easter-orthodox 2025)))
  (ok (value= (make-date 2016 5 1) (easter-orthodox 2016))))

;;; --- Hebrew -------------------------------------------------------------

(deftest rosh-hashanah-golden-values
  (ok (value= (make-date 2023 9 16) (rosh-hashanah 5784)))
  (ok (value= (make-date 2024 10 3) (rosh-hashanah 5785)))
  (ok (value= (make-date 2025 9 23) (rosh-hashanah 5786))))

(deftest passover-golden-values
  (ok (value= (make-date 2024 4 23) (passover 5784)))
  (ok (value= (make-date 2025 4 13) (passover 5785))))

(deftest hebrew-leap-year-golden-values
  (ok (hebrew-leap-year-p 5784))
  (ng (hebrew-leap-year-p 5785))
  (ok (= 8 (hebrew-nisan-month 5784)))
  (ok (= 7 (hebrew-nisan-month 5785))))

(deftest hebrew-year-length-in-valid-range
  (dolist (year '(5784 5785 5786 5787 5700 5500))
    (ok (member (hebrew-year-length year) '(353 354 355 383 384 385))
        (format nil "year ~a" year))))

(deftest hebrew-date-roundtrip
  (dolist (year '(5700 5750 5784 5785 5786 5800))
    (dolist (month (list 1 (hebrew-nisan-month year)))
      (multiple-value-bind (y m d) (hebrew-date-from-fixed (fixed-from-hebrew-date year month 1))
        (ok (and (cl:= y year) (cl:= m month) (cl:= d 1))
            (format nil "roundtrip year=~a month=~a" year month))))))

(deftest hebrew-date-object-accessors
  (let ((hd (hebrew-date-from-date (rosh-hashanah 5785))))
    (ok (= 5785 (hebrew-date-year hd)))
    (ok (= 1 (hebrew-date-month hd)))
    (ok (= 1 (hebrew-date-day hd)))))

;;; --- Islamic --------------------------------------------------------------

(deftest islamic-new-year-golden-values
  ;; Verified independently against the Kuwaiti/civil tabular JDN formula
  ;; (globalcalcs.com), not merely round-tripped through this file's own code.
  (ok (equal '(2024 7 8) (multiple-value-list (date-from-fixed +gregorian+ (fixed-from-islamic-date 1446 1 1)))))
  (ok (equal '(2023 7 19) (multiple-value-list (date-from-fixed +gregorian+ (fixed-from-islamic-date 1445 1 1)))))
  (ok (equal '(2024 7 17) (multiple-value-list (date-from-fixed +gregorian+ (fixed-from-islamic-date 1446 1 10))))))

(deftest islamic-epoch-anchor
  ;; Civil (Friday) epoch: 1/1/1 AH = Julian 622-07-16 = proleptic Gregorian 622-07-19.
  (ok (equal '(622 7 19) (multiple-value-list (date-from-fixed +gregorian+ (fixed-from-islamic-date 1 1 1))))))

(deftest islamic-leap-year-cycle
  ;; Type II (Kuwaiti) 30-year cycle: leap years are 2 5 7 10 13 16 18 21 24 26 29.
  (let ((leap-years '(2 5 7 10 13 16 18 21 24 26 29)))
    (dotimes (y 30)
      (let ((year (cl:1+ y)))
        (if (member year leap-years)
            (ok (islamic-leap-year-p year) (format nil "year ~a should be leap" year))
            (ng (islamic-leap-year-p year) (format nil "year ~a should not be leap" year)))))))

(deftest islamic-date-roundtrip
  (dolist (rd (list (fixed-from-islamic-date 1 1 1) (fixed-from-islamic-date 1446 1 1)
                     (fixed-from-islamic-date 1446 12 29) (fixed-from-islamic-date 1500 6 15)))
    (multiple-value-bind (y m d) (islamic-date-from-fixed rd)
      (ok (= rd (fixed-from-islamic-date y m d)) (format nil "roundtrip rd=~a" rd)))))

(deftest islamic-date-object-accessors
  (let ((id (islamic-date-from-date (date-from-rd (fixed-from-islamic-date 1446 1 1)))))
    (ok (= 1446 (islamic-date-year id)))
    (ok (= 1 (islamic-date-month id)))
    (ok (= 1 (islamic-date-day id)))))

;;; --- Astronomy / Chinese (location-aware) --------------------------------

(deftest japan-equinoxes-tokyo
  "Cabinet 春分日/秋分日 — civil date at Tokyo/JST."
  (ok (value= (make-date 2024 3 20) (spring-equinox-date 2024)))
  (ok (value= (make-date 2024 9 22) (autumn-equinox-date 2024)))
  (ok (value= (make-date 2026 3 20) (spring-equinox-date 2026)))
  (ok (value= (make-date 2026 9 23) (autumn-equinox-date 2026)))
  (ok (value= (make-date 2027 3 21) (spring-equinox-date 2027))))

(deftest qingming-beijing
  (ok (value= (make-date 2024 4 4) (qingming-date 2024)))
  (ok (value= (make-date 2025 4 4) (qingming-date 2025)))
  (ok (value= (make-date 2026 4 5) (qingming-date 2026))))

(deftest chinese-new-year-beijing
  (ok (value= (make-date 2024 2 10) (chinese-new-year-date 2024)))
  (ok (value= (make-date 2025 1 29) (chinese-new-year-date 2025)))
  (ok (value= (make-date 2026 2 17) (chinese-new-year-date 2026)))
  (ok (value= (make-date 2024 2 9) (chinese-new-year-eve-date 2024)))
  (ok (value= (make-date 2026 6 19) (duanwu-date 2026)))
  (ok (value= (make-date 2026 9 25) (zhongqiu-date 2026))))

(deftest sunrise-requires-lat-lon
  "Sunrise/sunset differ by locus; polar night returns NIL."
  (let ((rd (date-rd (make-date 2026 3 20))))
    (ok (sunrise rd +tokyo+))
    (ok (sunset rd +tokyo+))
    (ok (sunrise rd +delhi+))
    ;; Same UT day, different standard clocks at Delhi vs Ujjain (longitude).
    (ng (= (sunrise rd +delhi+) (sunrise rd +ujjain+))))
  (let ((arctic (location 78.2d0 15.6d0 :zone 1 :name "Longyearbyen")))
    ;; Midwinter polar night — no sunrise.
    (ok (null (sunrise (date-rd (make-date 2026 1 15)) arctic)))))

(deftest jewish-shabbat-until-nightfall
  "Melacha forbidden from Friday sunset until Saturday nightfall (Jerusalem)."
  (let* ((fri (date-rd (make-date 2026 3 20))) ; Friday
         (sat-noon-ut (midday-ut (1+ fri) +jerusalem+)))
    (ok (jewish-melacha-forbidden-p sat-noon-ut fri))
    ;; After Vilna nightfall Saturday — permitted again.
    (multiple-value-bind (start end)
        (jewish-shabbat-interval fri)
      (declare (ignore start))
      (ok end)
      (ng (jewish-melacha-forbidden-p (+ end 0.01d0) fri)))))

(deftest islamic-fast-until-maghrib
  "Ramadan-style fast: forbidden from Fajr until Maghrib at Mecca."
  (let* ((rd (date-rd (make-date 2026 3 20)))
         (mid (midday-ut rd +mecca+)))
    (ok (islamic-fasting-p mid rd))
    (multiple-value-bind (fajr maghrib)
        (islamic-fasting-interval rd)
      (ok fajr)
      (ok maghrib)
      (ok (< fajr mid maghrib))
      ;; After Maghrib — iftar; fasting window closed.
      (ng (islamic-fasting-p (+ maghrib 0.01d0) rd)))))
