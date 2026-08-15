(in-package #:datetime-protocol)

;;;; Traditional Chinese lunisolar calendar (astronomical), sufficient for
;;;; State Council festival dates: 春节 (lunar 1/1), 端午 (5/5), 中秋 (8/15),
;;;; and 除夕 (day before 1/1). Solar terms (清明) live in astronomy.lisp.
;;;;
;;;; Algorithm: winter-solstice–anchored months; leap month = month without a
;;;; major solar term (中气) when 13 lunations fall between consecutive
;;;; solstices. Civil midnights are those of an ASTRO-LOCATION (default
;;;; +BEIJING+ — lat/lon + CST). Pass another locus only for research;
;;;; statutory 放假办法 dates use Beijing.

(defun %china-midnight-ut (rd &optional (loc +beijing+))
  "UT moment of civil midnight at the start of fixed day RD at LOC."
  (universal-from-standard (float rd 0d0) loc))

(defun %china-day-from-ut (moment &optional (loc +beijing+))
  "Civil RD at LOC containing UT MOMENT."
  (floor (standard-from-universal moment loc)))

(defun %current-major-solar-term (rd &optional (loc +beijing+))
  "Index 1–12 of the last major solar term (中气) at LOC midnight of RD."
  (let ((s (solar-longitude (%china-midnight-ut rd loc))))
    (1+ (mod (+ (floor s 30) 2) 12))))

(defun %chinese-new-moon-on-or-after (rd &optional (loc +beijing+))
  (%china-day-from-ut (new-moon-at-or-after (%china-midnight-ut rd loc)) loc))

(defun %chinese-new-moon-before (rd &optional (loc +beijing+))
  (%china-day-from-ut (new-moon-before (%china-midnight-ut rd loc)) loc))

(defun %chinese-no-major-solar-term-p (month-start-rd &optional (loc +beijing+))
  (= (%current-major-solar-term month-start-rd loc)
     (%current-major-solar-term
      (%chinese-new-moon-on-or-after (1+ month-start-rd) loc) loc)))

(defun %chinese-winter-solstice-on-or-before (rd &optional (loc +beijing+))
  "Civil RD at LOC of the winter solstice on or before RD."
  (let* ((approx (estimate-prior-solar-longitude
                  270d0
                  (%china-midnight-ut (1+ rd) loc)))
         (start (1- (floor approx))))
    (loop for day from start
          when (> (solar-longitude (%china-midnight-ut (1+ day) loc)) 270d0)
            return day)))

(defun chinese-new-year-rd (gregorian-year &key (location +beijing+))
  "Rata Die of Chinese New Year (lunar 1/1) falling in GREGORIAN-YEAR."
  (chinese-new-year-on-or-before-rd
   (fixed-from-date +gregorian+ gregorian-year 7 1)
   :location location))

(defun chinese-new-year-on-or-before-rd (rd &key (location +beijing+))
  (let ((ny (%chinese-new-year-in-sui rd location)))
    (if (>= rd ny)
        ny
        (%chinese-new-year-in-sui (- rd 180) location))))

(defun %chinese-new-year-in-sui (rd loc)
  (let* ((s1 (%chinese-winter-solstice-on-or-before rd loc))
         (s2 (%chinese-winter-solstice-on-or-before (+ s1 370) loc))
         (m12 (%chinese-new-moon-on-or-after (1+ s1) loc))
         (m13 (%chinese-new-moon-on-or-after (1+ m12) loc))
         (next-m11 (%chinese-new-moon-before (1+ s2) loc)))
    (if (and (= (round (/ (- next-m11 m12) +mean-synodic-month+)) 12)
             (or (%chinese-no-major-solar-term-p m12 loc)
                 (%chinese-no-major-solar-term-p m13 loc)))
        (%chinese-new-moon-on-or-after (1+ m13) loc)
        m13)))

(defun chinese-new-year-date (gregorian-year &key (location +beijing+))
  "Gregorian DATE of 春节 (lunar month 1 day 1) in GREGORIAN-YEAR."
  (date-from-rd (chinese-new-year-rd gregorian-year :location location)))

(defun chinese-new-year-eve-date (gregorian-year &key (location +beijing+))
  "Gregorian DATE of 除夕 (day before Chinese New Year)."
  (date-from-rd (1- (chinese-new-year-rd gregorian-year :location location))))

(defun chinese-lunar-date (gregorian-year month day &key (location +beijing+) leap)
  "Gregorian DATE of Chinese lunar MONTH/DAY in the Chinese year whose New Year
falls in GREGORIAN-YEAR. Skips leap months so MONTH is the ordinal non-leap
month (端午/中秋). LEAP is reserved."
  (declare (ignore leap))
  (let ((moon (chinese-new-year-rd gregorian-year :location location)))
    (loop repeat (1- month) do
      (setf moon (%chinese-new-moon-on-or-after (1+ moon) location))
      (loop while (%chinese-no-major-solar-term-p moon location)
            do (setf moon (%chinese-new-moon-on-or-after (1+ moon) location))))
    (date-from-rd (+ moon (1- day)))))

(defun duanwu-date (gregorian-year &key (location +beijing+))
  "端午节 — lunar 5/5."
  (chinese-lunar-date gregorian-year 5 5 :location location))

(defun zhongqiu-date (gregorian-year &key (location +beijing+))
  "中秋节 — lunar 8/15."
  (chinese-lunar-date gregorian-year 8 15 :location location))
