(in-package #:datetime-protocol/tests)

;;;; Every lunisolar month start 1900–2100 against official tables frozen in
;;;; data/tests/lunisolar-gold.sexp (scripts/generate_lunisolar_gold.py):
;;;;   hko  — Hong Kong Observatory conversion tables (Chinese, UTC+8)
;;;;   kasi — Korea Astronomy and Space Science Institute (Korean)
;;;;   hnd  — Hồ Ngọc Đức published tables (Vietnamese)
;;;; A row is (LUNAR-YEAR MONTH LEAP-P GY GM GD). Both directions are checked:
;;;; LUNISOLAR-FROM-FIXED of the Gregorian day, FIXED-FROM-LUNISOLAR of the
;;;; lunar date.

(defparameter *lunisolar-gold-path*
  (asdf:system-relative-pathname "datetime-protocol" "data/tests/lunisolar-gold.sexp"))

(defparameter *lunisolar-gold*
  (with-open-file (in *lunisolar-gold-path* :external-format :utf-8)
    (let ((*read-eval* nil))
      (read in))))

(defun lunisolar-block (source)
  (find source (getf *lunisolar-gold* :blocks)
        :key (lambda (b) (getf b :source)) :test #'equal))

(defun %lunisolar-mismatches (source zone &key (from 1900) (to 2100))
  "Rows of SOURCE (Gregorian years FROM..TO) that ZONE gets wrong, as
((GY GM GD) MESSAGE) lists."
  (let ((bad '()))
    (dolist (row (getf (lunisolar-block source) :rows) (nreverse bad))
      (destructuring-bind (ly month leap gy gm gd) row
        (when (<= from gy to)
          (let ((rd (fixed-from-date +gregorian+ gy gm gd)))
            (multiple-value-bind (y m l d) (lunisolar-from-fixed rd :zone zone)
              (let ((back (handler-case
                              (fixed-from-lunisolar ly month 1 :leap leap :zone zone)
                            (invalid-lunisolar-date () nil))))
                (unless (and (= y ly) (= m month) (eq (and l t) (and leap t))
                             (= d 1) (eql back rd))
                  (push (list (list gy gm gd)
                              (format nil "~a ~4,'0d-~2,'0d-~2,'0d gold ~d/~:[~;L~]~d got ~d/~:[~;L~]~d/~d back ~a"
                                      source gy gm gd ly leap month y l m d back))
                        bad))))))))))

(defun %assert-lunisolar-source (source zone &key (from 1900) (to 2100) (min-rows 1000)
                                        known)
  "Every row of SOURCE in FROM..TO matches ZONE, except exactly the KNOWN
Gregorian month starts."
  (let ((rows (getf (lunisolar-block source) :rows)))
    (ok (> (length rows) min-rows) (format nil "~a block has ~d rows" source (length rows)))
    (let* ((bad (%lunisolar-mismatches source zone :from from :to to))
           (unexpected (remove-if (lambda (b) (member (first b) known :test #'equal)) bad))
           (fixed (set-difference known (mapcar #'first bad) :test #'equal)))
      (loop for (nil line) in unexpected repeat 15 do (ok nil line))
      (ok (null unexpected)
          (format nil "~a ~d–~d unexpected mismatches: ~d" source from to (length unexpected)))
      (ok (null fixed) (format nil "~a known mismatches now matching: ~s" source fixed)))))

(deftest lunisolar-gold-hko-china
  "Chinese 农历 (UTC+8) month starts and leap months vs HKO, 1901–2100.
Before 1929 the published calendar came from the 時憲曆 / 紫金山 tables, not
modern ephemerides; no single meridian reproduces them (UTC+8 misses these
three, Beijing local time five others), so they are frozen as known."
  (%assert-lunisolar-source "hko" +china-lunisolar-zone+ :from 1901 :to 2100
                                                         :known '((1914 11 17) (1916 2 3) (1920 11 10))))

(deftest lunisolar-gold-kasi-korea
  "Korean 음력 month starts and leap months vs KASI, 1900–2050."
  (%assert-lunisolar-source "kasi" +korea-lunisolar-zone+ :from 1900 :to 2050))

(deftest lunisolar-gold-hnd-vietnam
  "Vietnamese âm lịch month starts and leap months vs Hồ Ngọc Đức, 1900–2100.
UTC+8 to 1967 and UTC+7 from 1968 match every row from 1929; the 1914, 1916
and 1920 rows are HKO's pre-1929 historical months (see the HKO test), and
1906-04 / 1925-01 are where HND's pre-1929 table departs from HKO."
  (%assert-lunisolar-source "hnd" +vietnam-lunisolar-zone+ :from 1900 :to 2100
                                                           :known '((1906 4 23) (1914 11 17) (1916 2 3)
                                                                    (1920 11 10) (1925 1 25))))

;;; --- Regressions from egao1980/cl-stack-calendars#38 -----------------------

(deftest lunisolar-2033-leap-eleventh-month
  "2033: the month from 2033-08-25 has no 中气 but its 岁 has 12 months, so it is
month 8; the leap month is 闰十一月 from 2033-12-22 (HKO T2033e.txt)."
  (ok (value= (make-date 2033 9 8) (zhongqiu-date 2033)))
  (ok (value= (make-date 2033 8 25) (chinese-lunar-date 2033 8 1)))
  (ok (value= (make-date 2033 12 22) (chinese-lunar-date 2033 11 1 :leap t)))
  (ok (value= (make-date 2033 11 22) (chinese-lunar-date 2033 11 1)))
  (ok (value= (make-date 2034 2 19) (chinese-new-year-date 2034))))

(deftest lunisolar-korea-meridian
  "KASI: 설날 2027-02-07 (new moon 2027-02-06 15:56 UT = 00:56 KST), 설날
2028-01-27, 추석 2040-09-21."
  (ok (value= (make-date 2027 2 7) (chinese-new-year-date 2027 :zone +korea-lunisolar-zone+)))
  (ok (value= (make-date 2027 2 6) (chinese-new-year-date 2027)))
  (ok (value= (make-date 2028 1 27) (chinese-new-year-date 2028 :zone +korea-lunisolar-zone+)))
  (ok (value= (make-date 2040 9 21) (zhongqiu-date 2040 :zone +korea-lunisolar-zone+)))
  (ok (value= (make-date 2033 9 8) (zhongqiu-date 2033 :zone +korea-lunisolar-zone+))))

(deftest lunisolar-vietnam-meridian
  "Vietnam UTC+7: Tết 2007-02-17, Tết 2030-02-02, Giỗ Tổ (3/10) 2009-04-04,
2037-04-24, 2038-04-13; Tết 1968-01-29 (first UTC+7 year)."
  (let ((vn +vietnam-lunisolar-zone+))
    (ok (value= (make-date 2007 2 17) (chinese-new-year-date 2007 :zone vn)))
    (ok (value= (make-date 2030 2 2) (chinese-new-year-date 2030 :zone vn)))
    (ok (value= (make-date 2009 4 4) (chinese-lunar-date 2009 3 10 :zone vn)))
    (ok (value= (make-date 2037 4 24) (chinese-lunar-date 2037 3 10 :zone vn)))
    (ok (value= (make-date 2038 4 13) (chinese-lunar-date 2038 3 10 :zone vn)))
    (ok (value= (make-date 1968 1 29) (chinese-new-year-date 1968 :zone vn)))
    (ok (value= (make-date 1968 1 30) (chinese-new-year-date 1968)))))

(deftest lunisolar-zone-designators
  (ok (= 8d0 (lunisolar-zone-offset +china-lunisolar-zone+ 738000)))
  (ok (= 9d0 (lunisolar-zone-offset 9 738000)))
  (ok (= 7d0 (lunisolar-zone-offset +hanoi+ 738000)))
  (ok (= 8d0 (lunisolar-zone-offset +vietnam-lunisolar-zone+
                                    (fixed-from-date +gregorian+ 1967 12 31))))
  (ok (= 7d0 (lunisolar-zone-offset +vietnam-lunisolar-zone+
                                    (fixed-from-date +gregorian+ 1968 1 1))))
  (ok (signals (lunisolar-zone "UTC+8") 'type-error)))

(deftest lunisolar-round-trip
  (let ((rd (fixed-from-date +gregorian+ 2023 4 20)))
    ;; 2023 has 闰二月 from 2023-03-22 (HKO).
    (multiple-value-bind (y m l d) (lunisolar-from-fixed rd)
      (ok (equal (list y m (and l t) d) '(2023 3 nil 1))))
    (multiple-value-bind (y m l d) (lunisolar-from-fixed (fixed-from-date +gregorian+ 2023 3 22))
      (ok (equal (list y m (and l t) d) '(2023 2 t 1))))
    (ok (= (fixed-from-date +gregorian+ 2023 3 22) (fixed-from-lunisolar 2023 2 1 :leap t)))
    (ok (value= (make-date 2024 2 9) (chinese-new-year-eve-date 2024)))
    (multiple-value-bind (y m l d) (lunisolar-from-fixed (fixed-from-date +gregorian+ 2024 2 9))
      (ok (equal (list y m (and l t) d) '(2023 12 nil 30))))))

(deftest lunisolar-invalid-date-restarts
  "2024 has no leap month; day 30 of a 29-day month does not exist."
  (ok (signals (fixed-from-lunisolar 2024 6 1 :leap t) 'invalid-lunisolar-date))
  (ok (signals (chinese-lunar-date 2024 6 1 :leap t) 'invalid-lunisolar-date))
  (ok (= (fixed-from-lunisolar 2024 6 1)
         (handler-bind ((invalid-lunisolar-date #'continue))
           (fixed-from-lunisolar 2024 6 1 :leap t))))
  (ok (= 42 (handler-bind ((invalid-lunisolar-date (lambda (c) (use-value 42 c))))
              (fixed-from-lunisolar 2024 6 1 :leap t))))
  ;; 2024 month 1 (2024-02-10 .. 03-09) has 29 days.
  (ok (signals (fixed-from-lunisolar 2024 1 30) 'invalid-lunisolar-date))
  (ok (= 7 (handler-bind ((invalid-lunisolar-date (lambda (c) (use-value 7 c))))
             (fixed-from-lunisolar 2024 1 30)))))
