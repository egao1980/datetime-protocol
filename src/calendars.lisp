(in-package #:datetime-protocol)

;;;; Easter computus, plus enough of the Hebrew and Islamic calendars to
;;;; drive holiday rules. These are independent implementations of
;;;; well-known, widely published algorithms/constants (the Gauss/
;;;; Meeus-Jones-Butcher Easter algorithms; the standard tabular Islamic
;;;; calendar; the traditional Hebrew molad/dehiyot arithmetic) — none of
;;;; this reuses Reingold & Dershowitz's Calendrical Calculations equations.

;;; --- Easter -----------------------------------------------------------

(defun easter-western (year)
  "Western (Gregorian) Easter Sunday for YEAR, via the Anonymous Gregorian
algorithm (Gauss/Butcher/Meeus)."
  (let* ((a (mod year 19))
         (b (floor year 100))
         (c (mod year 100))
         (d (floor b 4))
         (e (mod b 4))
         (f (floor (cl:+ b 8) 25))
         (g (floor (cl:+ (cl:- b f) 1) 3))
         (h (mod (cl:+ (cl:* 19 a) b (cl:- d) (cl:- g) 15) 30))
         (i (floor c 4))
         (k (mod c 4))
         (l (mod (cl:+ 32 (cl:* 2 e) (cl:* 2 i) (cl:- h) (cl:- k)) 7))
         (m (floor (cl:+ a (cl:* 11 h) (cl:* 22 l)) 451))
         (month (floor (cl:+ h l (cl:* -7 m) 114) 31))
         (day (cl:1+ (mod (cl:+ h l (cl:* -7 m) 114) 31))))
    (make-date year month day)))

(defun easter-orthodox (year)
  "Orthodox Easter Sunday for YEAR, as a proleptic Gregorian DATE. Computed
via Meeus's Julian-calendar Easter algorithm, then converted from the
Julian to the Gregorian chronology (both share this protocol's RD numbering,
so the conversion is exact)."
  (let* ((a (mod year 4))
         (b (mod year 7))
         (c (mod year 19))
         (d (mod (cl:+ (cl:* 19 c) 15) 30))
         (e (mod (cl:+ (cl:* 2 a) (cl:* 4 b) (cl:- d) 34) 7))
         (month (floor (cl:+ d e 114) 31))
         (day (cl:1+ (mod (cl:+ d e 114) 31))))
    (date-from-rd (fixed-from-date +julian+ year month day))))

;;; --- Islamic (Hijri) tabular calendar --------------------------------------
;;;
;;; The standard civil/tabular Islamic calendar: a 30-year cycle with 11
;;; leap years (355 days; 354 otherwise), odd months of 30 days and even
;;; months of 29 (month 12 gains the extra day in a leap year). Epoch: 1
;;; Muharram AH 1 = 16 July 622 CE (Julian) — the globally standard Hijra
;;; epoch correspondence.

(defparameter +islamic-days-before-month+
  #(0 0 30 59 89 118 148 177 207 236 266 295 325))

(defun islamic-leap-year-p (year)
  (cl:< (mod (cl:+ (cl:* 11 year) 14) 30) 11))

(defun islamic-year-length (year)
  (if (islamic-leap-year-p year) 355 354))

(defun islamic-month-length (year month)
  (if (cl:= month 12)
      (if (islamic-leap-year-p year) 30 29)
      (cl:- (aref +islamic-days-before-month+ (cl:1+ month))
            (aref +islamic-days-before-month+ month))))

(defvar +islamic-epoch-rd+ (fixed-from-date +julian+ 622 7 16))

(defun islamic-year-start-rd (year)
  (let* ((y (cl:1- year))
         (cycles (floor y 30))
         (remainder (mod y 30)))
    (cl:+ +islamic-epoch-rd+ (cl:* cycles 10631)
          (loop for yy from 1 to remainder sum (islamic-year-length yy)))))

(defun fixed-from-islamic-date (year month day)
  (cl:+ (islamic-year-start-rd year) (aref +islamic-days-before-month+ month) day -1))

(defun %islamic-month-day-from-day-of-year (day-of-year year)
  (loop for month from 1 to 12
        for start = (aref +islamic-days-before-month+ month)
        for len = (islamic-month-length year month)
        when (cl:<= day-of-year (cl:+ start len))
          return (values month (cl:- day-of-year start))
        finally (error 'datetime-arithmetic-error
                       :message (format nil "day-of-year ~d out of range" day-of-year))))

(defun islamic-date-from-fixed (rd)
  "Returns (values year month day)."
  (let ((year (cl:1+ (floor (cl:* (cl:- rd +islamic-epoch-rd+) 30) 10631))))
    (loop while (cl:> (islamic-year-start-rd year) rd) do (cl:decf year))
    (loop while (cl:<= (islamic-year-start-rd (cl:1+ year)) rd) do (cl:incf year))
    (let ((day-of-year (cl:1+ (cl:- rd (islamic-year-start-rd year)))))
      (multiple-value-bind (month day) (%islamic-month-day-from-day-of-year day-of-year year)
        (values year month day)))))

(defclass islamic-date ()
  ((year :initarg :year :reader islamic-date-year :type integer)
   (month :initarg :month :reader islamic-date-month :type (integer 1 12))
   (day :initarg :day :reader islamic-date-day :type (integer 1 30))))

(defun islamic-date-p (x) (typep x 'islamic-date))

(defun make-islamic-date (year month day)
  (make-instance 'islamic-date :year year :month month :day day))

(defun islamic-date-to-rd (id)
  (fixed-from-islamic-date (islamic-date-year id) (islamic-date-month id) (islamic-date-day id)))

(defun islamic-date-from-date (date)
  (multiple-value-bind (y m d) (islamic-date-from-fixed (date-rd date))
    (make-islamic-date y m d)))

(defmethod print-object ((o islamic-date) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~4,'0d-~2,'0d-~2,'0d AH" (islamic-date-year o) (islamic-date-month o)
            (islamic-date-day o))))

(defun islamic-date-in-gregorian-year (g-year month day)
  "First tabular Islamic MONTH/DAY whose Gregorian date falls in G-YEAR.
Civil/tabular only — moon-sighting jurisdictions may differ by a day. When a
Gregorian year contains two such dates (Islamic year shorter), returns the
earlier. NIL if none (should not happen for valid month/day)."
  (let* ((start (fixed-from-date +gregorian+ g-year 1 1))
         (end (fixed-from-date +gregorian+ g-year 12 31)))
    (multiple-value-bind (iy) (islamic-date-from-fixed start)
      (loop for y from (cl:1- iy) to (cl:+ iy 2)
            for rd = (fixed-from-islamic-date y month day)
            when (cl:<= start rd end)
              return (date-from-rd rd)))))

(defun islamic-dates-in-gregorian-year (g-year month day)
  "All tabular Islamic MONTH/DAY occurrences in Gregorian G-YEAR (0–2)."
  (let* ((start (fixed-from-date +gregorian+ g-year 1 1))
         (end (fixed-from-date +gregorian+ g-year 12 31)))
    (multiple-value-bind (iy) (islamic-date-from-fixed start)
      (loop for y from (cl:1- iy) to (cl:+ iy 2)
            for rd = (fixed-from-islamic-date y month day)
            when (cl:<= start rd end)
              collect (date-from-rd rd)))))

(defun eid-al-fitr (g-year)
  "1 Shawwal (tabular) in Gregorian G-YEAR."
  (islamic-date-in-gregorian-year g-year 10 1))

(defun eid-al-adha (g-year)
  "10 Dhu al-Hijjah (tabular) in Gregorian G-YEAR."
  (islamic-date-in-gregorian-year g-year 12 10))

(defun islamic-new-year-date (g-year)
  "1 Muharram (tabular) in Gregorian G-YEAR."
  (islamic-date-in-gregorian-year g-year 1 1))

(defun mawlid-date (g-year)
  "12 Rabiʿ al-awwal (tabular) — Mawlid/Milad in Gregorian G-YEAR."
  (islamic-date-in-gregorian-year g-year 3 12))

;;; --- Hebrew calendar --------------------------------------------------------
;;;
;;; The traditional arithmetic (molad-based) Hebrew calendar: leap years
;;; follow the 19-year Metonic cycle (7 leap years per cycle: year Y is
;;; leap iff (7Y+1) mod 19 < 7); the molad interval is the standard 29
;;; days, 12 hours, 793 "parts" (1 part = 1/1080 hour, 1 day = 25920
;;; parts); Rosh Hashanah is the molad of Tishrei, postponed by the four
;;; classical dehiyot (molad zaken; lo-ADU-Rosh; GaTRaD; BeTU-TaKPaT). All
;;; constants below are the traditional values (see e.g. the Hebrew
;;; calendar article on Wikipedia and Mathematics of the Jewish Calendar on
;;; Wikibooks); the single free constant, +HEBREW-EPOCH-DAY-OFFSET+, is
;;; calibrated empirically against published modern Rosh Hashanah dates
;;; (see tests/calendars-test.lisp) rather than trusting any one source's
;;; translation of the ancient epoch into a proleptic civil date.

(defconstant +hebrew-parts-per-hour+ 1080)
(defconstant +hebrew-parts-per-day+ 25920)
(defconstant +hebrew-molad-interval-parts+ 765433) ; 29d 12h 793p

(defun hebrew-leap-year-p (year)
  (cl:< (mod (cl:1+ (cl:* 7 year)) 19) 7))

(defun %hebrew-months-in-year (year)
  (if (hebrew-leap-year-p year) 13 12))

(defun %hebrew-months-before-year (year)
  "Whole months elapsed from AM year 1 Tishrei to AM YEAR Tishrei. Every
run of 19 consecutive years contributes the same 235 months (7 leap x 13 +
12 regular x 12), since leap-year-p depends only on YEAR mod 19."
  (let* ((y (cl:1- year))
         (cycles (floor y 19))
         (remainder (mod y 19)))
    (cl:+ (cl:* 235 cycles)
          (loop for yy from 1 to remainder sum (%hebrew-months-in-year yy)))))

(defconstant +hebrew-epoch-parts+ 5604 ; molad of year 1 Tishrei: 5h 204p past its (arbitrary) day origin
  "5*1080 + 204 — the traditional \"BaHaRaD\" time-of-day, in parts, measured
from an arbitrary day origin; see +HEBREW-EPOCH-DAY-OFFSET+ for how that
origin lines up with the Gregorian Rata Die.")

;; Calibrated so that the Hebrew-day serial number derived below equals the
;; correct Rata Die day (verified via tests/calendars-test.lisp against
;; Hebcal's modern Rosh Hashanah dates for AM 5784-5787, and cross-checked
;; against a wide-range independent Hebrew<->Gregorian converter across
;; several thousand years).
(defconstant +hebrew-epoch-day-offset+ -1373427)

(defun %hebrew-molad-parts (year)
  "Total parts (this file's arbitrary origin) of the molad of Tishrei of
AM YEAR."
  (cl:+ +hebrew-epoch-parts+ (cl:* (%hebrew-months-before-year year) +hebrew-molad-interval-parts+)))

(defun %hebrew-molad-day-and-parts (year)
  "Returns (values day-serial parts-of-day) for the molad of Tishrei of AM YEAR."
  (floor (%hebrew-molad-parts year) +hebrew-parts-per-day+))

(defun %tishrei-1-day-serial (year)
  "Rosh Hashanah's Hebrew-day serial number for AM YEAR, after applying the
four postponement rules (dehiyot). Weekday checks use the real Gregorian
weekday of the corresponding RD (day-serial + +HEBREW-EPOCH-DAY-OFFSET+),
which is equivalent to — but avoids re-deriving — a parallel weekday
count. Rules 1-2 (molad zaken, lo-ADU-Rosh) cascade onto each other's
result, since each corrects whatever day the previous rule lands on; rules
3-4 (GaTRaD, BeTU-TaKPaT) instead test the ORIGINAL, pre-cascade molad
weekday — they identify a specific raw molad weekday/time combination that
would otherwise produce a forbidden or malformed year length, and each
sets the final postponement directly (2 days / 1 day) rather than adding
to whatever rules 1-2 already did, since those two rules cannot legitimately
fire at all for the exact weekday/time combinations rules 3-4 test for."
  (multiple-value-bind (day parts) (%hebrew-molad-day-and-parts year)
    (let* ((original-weekday (date-day-of-week (date-from-rd (cl:+ day +hebrew-epoch-day-offset+))))
           (weekday original-weekday)
           (postponed day))
      ;; 1. Molad zaken: molad at/after noon (18h after the 6pm day-start) -> postpone one day.
      (when (cl:>= parts (cl:* 18 +hebrew-parts-per-hour+))
        (incf postponed)
        (setf weekday (cl:1+ (mod weekday 7))))
      ;; 2. Lo ADU Rosh: Rosh Hashanah cannot be Sunday(7)/Wednesday(3)/Friday(5)
      ;;    in this protocol's Mon=1..Sun=7 numbering.
      (when (member weekday '(7 3 5))
        (incf postponed)
        (setf weekday (cl:1+ (mod weekday 7))))
      ;; 3. GaTRaD: regular year, ORIGINAL molad Tuesday(2) at/after 9h204p -> Thursday.
      (when (and (cl:= original-weekday 2) (cl:not (hebrew-leap-year-p year))
                 (cl:>= parts (cl:+ (cl:* 9 +hebrew-parts-per-hour+) 204)))
        (setf postponed (cl:+ day 2)))
      ;; 4. BeTU TaKPaT: year after a leap year, ORIGINAL molad Monday(1) at/after 15h589p -> Tuesday.
      (when (and (cl:= original-weekday 1) (hebrew-leap-year-p (cl:1- year))
                 (cl:>= parts (cl:+ (cl:* 15 +hebrew-parts-per-hour+) 589)))
        (setf postponed (cl:1+ day)))
      postponed)))

(defun %hebrew-year-length (year)
  (cl:- (%tishrei-1-day-serial (cl:1+ year)) (%tishrei-1-day-serial year)))

(defun hebrew-year-length (year)
  "Total days in Hebrew YEAR: 353/354/355 (regular deficient/regular/complete)
or 383/384/385 (leap)."
  (%hebrew-year-length year))

;; A regular year's 12 months alternate 30/29 starting from Tishrei, with
;; Kislev/Cheshvan (months 3/2 below) adjusted so the total matches
;; HEBREW-YEAR-LENGTH (deficient: Kislev 29; complete: Cheshvan 30).
;;
;; Month order used throughout this file: 1 Tishrei 2 Cheshvan 3 Kislev
;; 4 Tevet 5 Shevat 6 Adar 7 Nisan 8 Iyar 9 Sivan 10 Tammuz 11 Av 12 Elul
;; in a REGULAR year (12 months); in a LEAP year, month 6 splits into
;; 6 Adar-I / 7 Adar-II, pushing every later month up by one slot: 8 Nisan
;; 9 Iyar 10 Sivan 11 Tammuz 12 Av 13 Elul (13 months). Nisan is therefore
;; month 7 in a regular year but month 8 in a leap year — see
;; HEBREW-NISAN-MONTH for the leap-aware month number.

(defun %hebrew-month-count (year) (%hebrew-months-in-year year))

(defun hebrew-nisan-month (year)
  "The month number of Nisan in Hebrew YEAR: 8 in a leap year (after the
Adar-I/Adar-II split), 7 in a regular year."
  (if (hebrew-leap-year-p year) 8 7))

(defun %hebrew-month-length (year month)
  (let ((leap (hebrew-leap-year-p year)))
    (case month
      (1 30)  ; Tishrei
      (2 (if (member (%hebrew-year-length year) '(353 383)) 29 30)) ; Cheshvan
      (3 (if (member (%hebrew-year-length year) '(355 385)) 30 29)) ; Kislev
      (4 29)  ; Tevet
      (5 30)  ; Shevat
      (6 (if leap 30 29))  ; Adar-I (leap) or Adar (regular, 29 days)
      (7 (if leap 29 30))  ; Adar-II (leap, 29) — unreachable in a regular year
      (8 30) (9 29) (10 30) (11 29) (12 30) ; Nisan..Av
      (13 29) ; Elul
      (t (error 'datetime-arithmetic-error :message (format nil "no Hebrew month ~d" month))))))

(defun %hebrew-days-before-month (year month)
  (loop for m from 1 below month sum (%hebrew-month-length year m)))

(defun hebrew-date-from-fixed (rd)
  "Returns (values year month day). MONTH uses the 1-13 numbering documented
above (13 = Elul; 6 = Adar-I only in a leap year, otherwise plain Adar)."
  (let* ((day-serial (cl:- rd +hebrew-epoch-day-offset+))
         (year (cl:1+ (floor (cl:* day-serial 98496) 35975351)))) ; ~365.2467 days/year estimate
    (loop while (cl:> (%tishrei-1-day-serial year) day-serial) do (cl:decf year))
    (loop while (cl:<= (%tishrei-1-day-serial (cl:1+ year)) day-serial) do (cl:incf year))
    (let ((day-of-year (cl:1+ (cl:- day-serial (%tishrei-1-day-serial year)))))
      (loop for month from 1 to (%hebrew-month-count year)
            for start = (%hebrew-days-before-month year month)
            for len = (%hebrew-month-length year month)
            when (cl:<= day-of-year (cl:+ start len))
              return (values year month (cl:- day-of-year start))))))

(defun fixed-from-hebrew-date (year month day)
  (cl:+ (%tishrei-1-day-serial year) +hebrew-epoch-day-offset+
        (%hebrew-days-before-month year month) day -1))

(defclass hebrew-date ()
  ((year :initarg :year :reader hebrew-date-year :type integer)
   (month :initarg :month :reader hebrew-date-month :type integer)
   (day :initarg :day :reader hebrew-date-day :type integer)))

(defun hebrew-date-p (x) (typep x 'hebrew-date))

(defun make-hebrew-date (year month day)
  (make-instance 'hebrew-date :year year :month month :day day))

(defun hebrew-date-to-rd (hd)
  (fixed-from-hebrew-date (hebrew-date-year hd) (hebrew-date-month hd) (hebrew-date-day hd)))

(defun hebrew-date-from-date (date)
  (multiple-value-bind (y m d) (hebrew-date-from-fixed (date-rd date))
    (make-hebrew-date y m d)))

(defmethod print-object ((o hebrew-date) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~4,'0d-~2,'0d-~2,'0d AM" (hebrew-date-year o) (hebrew-date-month o)
            (hebrew-date-day o))))

(defun rosh-hashanah (year)
  "Rosh Hashanah (1 Tishrei) of Hebrew YEAR, as a proleptic Gregorian DATE."
  (date-from-rd (fixed-from-hebrew-date year 1 1)))

(defun passover (year)
  "Passover (15 Nisan) of Hebrew YEAR, as a proleptic Gregorian DATE."
  (date-from-rd (fixed-from-hebrew-date year (hebrew-nisan-month year) 15)))
