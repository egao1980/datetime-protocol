(in-package #:datetime-protocol)

;;;; East Asian lunisolar calendar (astronomical): Chinese 农历, Korean 음력,
;;;; Vietnamese âm lịch. Festival helpers: 春节 (lunar 1/1), 端午 (5/5),
;;;; 中秋 (8/15), 除夕 (day before 1/1). Solar terms (清明) live in
;;;; astronomy.lisp.
;;;;
;;;; Rules (GB/T 33661-2017 《农历的编算和颁行》): a month starts on the civil
;;;; day of the new moon; month 11 contains the winter solstice; a 岁 runs
;;;; from one winter-solstice month (11) to the next. Only when a 岁 holds 13
;;;; months is one of them a leap month, and it is the FIRST month of that 岁
;;;; with no major solar term (中气). Months are numbered from month 11 of the
;;;; 岁, skipping that one leap month only.
;;;;
;;;; The three calendars share these rules and differ only in the meridian
;;;; whose midnights bound the civil day: a LUNISOLAR-ZONE (see
;;;; +CHINA-LUNISOLAR-ZONE+, +KOREA-LUNISOLAR-ZONE+,
;;;; +VIETNAM-LUNISOLAR-ZONE+). Every public function takes :ZONE (a
;;;; LUNISOLAR-ZONE, hours east of UT, or an ASTRO-LOCATION); without it the
;;;; zone of :LOCATION (default +BEIJING+, UTC+8) is used. Verified month by
;;;; month against HKO, KASI and Hồ Ngọc Đức tables 1900–2100
;;;; (data/tests/lunisolar-gold.sexp, tests/lunisolar-gold-test.lisp).

(defstruct (lunisolar-zone (:constructor %make-lunisolar-zone (name periods)))
  "Civil meridian a lunisolar calendar reckons its days at.
PERIODS is a list of (START-RD . OFFSET), START-RD ascending: OFFSET (offset
from UT as a fraction of a day) is in force from fixed day START-RD (NIL for
the first period) until the next period starts. NAME cites the authority."
  (name nil :type (or null string))
  (periods nil :type list))

(defun make-lunisolar-zone (name initial-hours &rest changes)
  "Build a LUNISOLAR-ZONE named NAME. INITIAL-HOURS is the offset east of UT
in hours (8, 17/2, ...). CHANGES alternate a Gregorian (YEAR MONTH DAY) and the
offset in hours that applies from that day on."
  (check-type initial-hours real)
  (assert (evenp (length changes)) (changes)
          "CHANGES must alternate (YEAR MONTH DAY) and hours: ~s" changes)
  (%make-lunisolar-zone
   name
   (cons (cons nil (/ (float initial-hours 0d0) 24d0))
         (loop for (ymd hours) on changes by #'cddr
               collect (destructuring-bind (y m d) ymd
                         (check-type hours real)
                         (cons (fixed-from-date +gregorian+ y m d)
                               (/ (float hours 0d0) 24d0)))))))

(defun lunisolar-zone (designator)
  "Coerce DESIGNATOR to a LUNISOLAR-ZONE: a LUNISOLAR-ZONE itself, a REAL
(hours east of UT) or an ASTRO-LOCATION (its standard zone)."
  (etypecase designator
    (lunisolar-zone designator)
    (real (make-lunisolar-zone (format nil "UTC~@d" designator) designator))
    (astro-location
     (%make-lunisolar-zone (location-name designator)
                           (list (cons nil (location-zone designator)))))))

(defun %zone-days (zone rd)
  (let ((offset 0d0))
    (dolist (period (lunisolar-zone-periods zone) offset)
      (let ((start (car period)))
        (if (or (null start) (>= rd start))
            (setf offset (cdr period))
            (return offset))))))

(defun lunisolar-zone-offset (zone rd)
  "Offset of ZONE (a LUNISOLAR-ZONE designator) from UT, in hours, on fixed
day RD."
  (* 24d0 (%zone-days (lunisolar-zone zone) rd)))

(defparameter +china-lunisolar-zone+
  (make-lunisolar-zone
   "China: 北京时间 UTC+8 (GB/T 33661-2017; HKO conversion tables)" 8)
  "Chinese 农历 meridian: 120°E, UTC+8. GB/T 33661-2017 reckons the calendar
in Beijing time; the Hong Kong Observatory tables
(https://www.hko.gov.hk/en/gts/time/conversion.htm) match it at every month
start 1929–2100.")

(defparameter +korea-lunisolar-zone+
  (make-lunisolar-zone
   "Korea: UTC+8 to 1911, KST UTC+9 (135°E) from 1912-01-01 (KASI 음양력 data)"
   8 '(1912 1 1) 9)
  "Korean 음력 meridian, fitted to every KASI month start 1900–2050
(https://astro.kasi.re.kr/life/pageView/8). From 1912-01-01 Korea keeps
UTC+9 (IANA tzdata Asia/Seoul); KASI's earlier months follow the UTC+8
(Chinese 時憲曆) reckoning. The civil UTC+8:30 periods (1908-04-01 to
1911-12-31 and 1954-03-21 to 1961-08-09) are not used: KASI's 1908-04-30 and
1911-12-20 month starts contradict UTC+8:30, and no month start in 1954–1961
differs between UTC+8:30 and UTC+9.")

(defparameter +vietnam-lunisolar-zone+
  (make-lunisolar-zone
   "Vietnam: UTC+8 to 1967, UTC+7 from 1968 (Quyết định 121-CP, 08-08-1967)"
   8 '(1968 1 1) 7)
  "Vietnamese âm lịch meridian. Decision 121-CP of the DRV Council of
Government (8 August 1967) moved the calendar to UTC+7 (105°E) from 1968;
earlier years follow the Chinese UTC+8 calendar. Fitted to Hồ Ngọc Đức's
published tables (amlich-hnd.js TK20/TK21/TK22,
https://web.archive.org/web/2023/https://www.informatik.uni-leipzig.de/~duc/amlich/):
every month start 1929–2100 matches, while UTC+7 throughout contradicts 30
pre-1968 rows. Tết 1968 is 1968-01-29 in Hanoi, 1968-01-30 at UTC+8.")

(defun %lunisolar-zone-arg (zone location)
  (lunisolar-zone (or zone location)))

(defun %china-midnight-ut (rd zone)
  "UT moment of civil midnight at the start of fixed day RD in ZONE."
  (- (float rd 0d0) (%zone-days zone rd)))

(defun %china-day-from-ut (moment zone)
  "Civil RD in ZONE containing UT MOMENT."
  (floor (+ moment (%zone-days zone (floor moment)))))

(defun %current-major-solar-term (rd zone)
  "Index 1–12 of the last major solar term (中气) at ZONE midnight of RD."
  (let ((s (solar-longitude (%china-midnight-ut rd zone))))
    (1+ (mod (+ (floor s 30) 2) 12))))

(defun %chinese-new-moon-on-or-after (rd zone)
  (%china-day-from-ut (new-moon-at-or-after (%china-midnight-ut rd zone)) zone))

(defun %chinese-new-moon-before (rd zone)
  (%china-day-from-ut (new-moon-before (%china-midnight-ut rd zone)) zone))

(defun %chinese-no-major-solar-term-p (month-start-rd zone)
  (= (%current-major-solar-term month-start-rd zone)
     (%current-major-solar-term
      (%chinese-new-moon-on-or-after (1+ month-start-rd) zone) zone)))

(defun %chinese-winter-solstice-on-or-before (rd zone)
  "Civil RD in ZONE of the winter solstice on or before RD."
  (let* ((approx (estimate-prior-solar-longitude
                  270d0
                  (%china-midnight-ut (1+ rd) zone)))
         (start (1- (floor approx))))
    (loop for day from start
          when (> (solar-longitude (%china-midnight-ut (1+ day) zone)) 270d0)
            return day)))

(defun %chinese-prior-leap-month-p (m-prime m zone)
  "True when some month starting in [M-PRIME, M] has no major solar term."
  (loop for x = m then (%chinese-new-moon-before x zone)
        while (>= x m-prime)
          thereis (%chinese-no-major-solar-term-p x zone)))

(defun %lunisolar-month-of (m zone)
  "Lunar year, month and leap flag of the month starting on fixed day M.
The lunar year is the Gregorian year in which its month 1 starts. A 岁 runs
from the month holding the winter solstice S1 to the month holding the next;
only when it has 13 months (GB/T 33661-2017) is one a leap month, the first of
them with no major solar term. Other months without one keep their number:
2033's month 8 (from 2033-08-25) has none, but that 岁 has 12 months; the leap
month is 闰十一月 in the next 岁."
  (let* ((s1 (%chinese-winter-solstice-on-or-before m zone))
         (s2 (%chinese-winter-solstice-on-or-before (+ s1 370) zone))
         (m12 (%chinese-new-moon-on-or-after (1+ s1) zone))
         (next-m11 (%chinese-new-moon-before (1+ s2) zone))
         (leap-sui-p (= 12 (round (- next-m11 m12) +mean-synodic-month+)))
         (index (round (- m m12) +mean-synodic-month+))
         (month (let ((n (mod (- index
                                 (if (and leap-sui-p
                                          (%chinese-prior-leap-month-p m12 m zone))
                                     1 0))
                              12)))
                  (if (zerop n) 12 n)))
         (leap-p (and leap-sui-p
                      (>= m m12)
                      (%chinese-no-major-solar-term-p m zone)
                      (not (%chinese-prior-leap-month-p
                            m12 (%chinese-new-moon-before m zone) zone))))
         (s1-year (date-year (date-from-rd s1))))
    (values (if (and (>= month 11) (<= index 2)) s1-year (1+ s1-year))
            month
            leap-p)))

(defun lunisolar-from-fixed (rd &key zone (location +beijing+))
  "Lunisolar date of fixed day RD: (values LUNAR-YEAR MONTH LEAP-P DAY).
LUNAR-YEAR is the Gregorian year in which that lunar year's month 1 starts.
ZONE (or LOCATION's zone) is the meridian of the civil day."
  (let* ((zone (%lunisolar-zone-arg zone location))
         (m (%chinese-new-moon-before (1+ rd) zone)))
    (multiple-value-bind (year month leap-p) (%lunisolar-month-of m zone)
      (values year month leap-p (1+ (- rd m))))))

(defun chinese-new-year-rd (gregorian-year &key zone (location +beijing+))
  "Rata Die of lunar New Year (lunar 1/1) falling in GREGORIAN-YEAR."
  (%new-year-on-or-before
   (fixed-from-date +gregorian+ gregorian-year 7 1)
   (%lunisolar-zone-arg zone location)))

(defun chinese-new-year-on-or-before-rd (rd &key zone (location +beijing+))
  "Rata Die of the last lunar New Year on or before fixed day RD."
  (%new-year-on-or-before rd (%lunisolar-zone-arg zone location)))

(defun %chinese-new-year-in-sui (rd zone)
  (let* ((s1 (%chinese-winter-solstice-on-or-before rd zone))
         (s2 (%chinese-winter-solstice-on-or-before (+ s1 370) zone))
         (m12 (%chinese-new-moon-on-or-after (1+ s1) zone))
         (m13 (%chinese-new-moon-on-or-after (1+ m12) zone))
         (next-m11 (%chinese-new-moon-before (1+ s2) zone)))
    (if (and (= (round (/ (- next-m11 m12) +mean-synodic-month+)) 12)
             (or (%chinese-no-major-solar-term-p m12 zone)
                 (%chinese-no-major-solar-term-p m13 zone)))
        (%chinese-new-moon-on-or-after (1+ m13) zone)
        m13)))

(defun %new-year-on-or-before (rd zone)
  (let ((ny (%chinese-new-year-in-sui rd zone)))
    (if (>= rd ny)
        ny
        (%chinese-new-year-in-sui (- rd 180) zone))))

(defun fixed-from-lunisolar (lunar-year month day &key leap zone (location +beijing+))
  "Fixed day (RD) of lunar MONTH/DAY (the leap month when LEAP) of the lunar
year whose month 1 starts in Gregorian year LUNAR-YEAR, in ZONE (or LOCATION's
zone). Signals INVALID-LUNISOLAR-DATE, with restarts USE-VALUE (return a
supplied RD) and, for a missing leap month, CONTINUE (use the regular month)."
  (check-type lunar-year integer)
  (check-type month (integer 1 12))
  (check-type day (integer 1 30))
  (let* ((zone (%lunisolar-zone-arg zone location))
         (want (+ (* 2 month) (if leap 1 0)))
         (ny (%new-year-on-or-before
              (fixed-from-date +gregorian+ lunar-year 7 1) zone))
         (start (%chinese-new-moon-on-or-after
                 (+ ny (round (* (1- month) +mean-synodic-month+)) -15) zone))
         (direction 0))
    (loop
      (multiple-value-bind (y m l) (%lunisolar-month-of start zone)
        (let* ((have (+ (* 2 m) (if l 1 0)))
               (step (cond ((and (= y lunar-year) (= have want)) 0)
                           ((or (< y lunar-year) (and (= y lunar-year) (< have want))) 1)
                           (t -1))))
          (cond ((zerop step) (return))
                ((= step (- direction))
                 (return-from fixed-from-lunisolar
                   (restart-case
                       (error 'invalid-lunisolar-date
                              :year lunar-year :month month :day day :leap leap
                              :message "no such leap month in that year")
                     (use-value (rd)
                       :report "Return a supplied fixed day instead."
                       rd)
                     (continue ()
                       :report "Use the regular (non-leap) month."
                       (fixed-from-lunisolar lunar-year month day
                                             :zone zone)))))
                (t (setf direction step
                         start (if (plusp step)
                                   (%chinese-new-moon-on-or-after (1+ start) zone)
                                   (%chinese-new-moon-before start zone))))))))
    (let ((length (- (%chinese-new-moon-on-or-after (1+ start) zone) start)))
      (if (<= day length)
          (+ start (1- day))
          (restart-case
              (error 'invalid-lunisolar-date
                     :year lunar-year :month month :day day :leap leap
                     :message (format nil "month has ~d days" length))
            (use-value (rd)
              :report "Return a supplied fixed day instead."
              rd))))))

(defun chinese-new-year-date (gregorian-year &key zone (location +beijing+))
  "Gregorian DATE of lunar New Year (春节, 설날, Tết: month 1 day 1) in
GREGORIAN-YEAR."
  (date-from-rd (chinese-new-year-rd gregorian-year :zone zone :location location)))

(defun chinese-new-year-eve-date (gregorian-year &key zone (location +beijing+))
  "Gregorian DATE of 除夕 (day before lunar New Year)."
  (date-from-rd (1- (chinese-new-year-rd gregorian-year :zone zone :location location))))

(defun chinese-lunar-date (gregorian-year month day &key zone (location +beijing+) leap)
  "Gregorian DATE of lunar MONTH/DAY (leap month when LEAP) in the lunar year
whose New Year falls in GREGORIAN-YEAR. Signals INVALID-LUNISOLAR-DATE when
that year has no such leap month or the month is shorter than DAY."
  (date-from-rd (fixed-from-lunisolar gregorian-year month day
                                      :leap leap :zone zone :location location)))

(defun duanwu-date (gregorian-year &key zone (location +beijing+))
  "端午节 / 단오 / Tết Đoan ngọ — lunar 5/5."
  (chinese-lunar-date gregorian-year 5 5 :zone zone :location location))

(defun zhongqiu-date (gregorian-year &key zone (location +beijing+))
  "中秋节 / 추석 / Tết Trung thu — lunar 8/15."
  (chinese-lunar-date gregorian-year 8 15 :zone zone :location location))
