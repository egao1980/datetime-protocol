(in-package #:datetime-protocol)

;;;; Immutable value types. Every class below is a plain CLOS value object:
;;;; slots are populated once at MAKE-INSTANCE time via the constructor
;;;; functions in this file and never mutated afterwards. "Modifying" a value
;;;; always means building a new instance (see WITH-FIELD / WITH-FIELDS).

(defconstant +nanos-per-second+ 1000000000)
(defconstant +nanos-per-day+ 86400000000000)
(defconstant +seconds-per-day+ 86400)

;;; --- instant ---------------------------------------------------------

(defclass instant ()
  ((seconds :initarg :seconds :reader instant-seconds :type integer)
   (nanos :initarg :nanos :reader instant-nanos :initform 0 :type (integer 0 999999999)))
  (:documentation "An exact point on the timeline: whole SECONDS since the
Unix epoch (1970-01-01T00:00:00Z) plus NANOS within that second."))

(defun instantp (x) (typep x 'instant))

(defun make-instant (seconds &optional (nanos 0))
  "Build an INSTANT, normalizing NANOS into [0, 1e9)."
  (multiple-value-bind (extra-seconds n) (floor nanos +nanos-per-second+)
    (make-instance 'instant :seconds (cl:+ seconds extra-seconds) :nanos n)))

(defmethod print-object ((o instant) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~d.~9,'0d" (instant-seconds o) (instant-nanos o))))

;;; --- duration ----------------------------------------------------------

(defclass duration ()
  ((seconds :initarg :seconds :reader duration-seconds :type integer)
   (nanos :initarg :nanos :reader duration-nanos :initform 0 :type (integer 0 999999999)))
  (:documentation "An exact amount of time: SECONDS (which may be negative)
plus a normalized NANOS remainder in [0, 1e9)."))

(defun durationp (x) (typep x 'duration))

(defun make-duration (seconds &optional (nanos 0))
  (multiple-value-bind (extra-seconds n) (floor nanos +nanos-per-second+)
    (make-instance 'duration :seconds (cl:+ seconds extra-seconds) :nanos n)))

(defun duration-total-nanos (d)
  (cl:+ (cl:* (duration-seconds d) +nanos-per-second+) (duration-nanos d)))

(defun duration-zero-p (d)
  (and (cl:= 0 (duration-seconds d)) (cl:= 0 (duration-nanos d))))

(defun duration-negative-p (d)
  (cl:< (duration-total-nanos d) 0))

(defmethod print-object ((o duration) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~d.~9,'0d s" (duration-seconds o) (duration-nanos o))))

;;; --- period --------------------------------------------------------------

(defclass period ()
  ((years :initarg :years :reader period-years :initform 0 :type integer)
   (months :initarg :months :reader period-months :initform 0 :type integer)
   (days :initarg :days :reader period-days :initform 0 :type integer))
  (:documentation "A calendar-based amount of time (years/months/days), with
no fixed length in seconds — e.g. adding one MONTH may move the date by 28
to 31 days depending on where it lands."))

(defun periodp (x) (typep x 'period))

(defun make-period (&key (years 0) (months 0) (days 0))
  (make-instance 'period :years years :months months :days days))

(defun period-zero-p (p)
  (and (cl:= 0 (period-years p)) (cl:= 0 (period-months p)) (cl:= 0 (period-days p))))

(defmethod print-object ((o period) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~dY ~dM ~dD" (period-years o) (period-months o) (period-days o))))

;;; --- date (Rata Die day number) -------------------------------------------

(defclass date ()
  ((rd :initarg :rd :reader date-rd :type integer))
  (:documentation "A calendar date, stored as a Rata Die fixed-day integer
(RD 1 = proleptic Gregorian 0001-01-01; RD 719163 = 1970-01-01). Field
accessors below use the Gregorian chronology by default; see chronology.lisp
for other chronologies."))

(defun datep (x) (typep x 'date))

(defun date-from-rd (rd)
  (make-instance 'date :rd rd))

(defun make-date (year month day)
  "Build a DATE from a proleptic Gregorian YEAR/MONTH/DAY."
  (date-from-rd (fixed-from-date +gregorian+ year month day)))

(defun date-year (d) (nth-value 0 (date-from-fixed +gregorian+ (date-rd d))))
(defun date-month (d) (nth-value 1 (date-from-fixed +gregorian+ (date-rd d))))
(defun date-day (d) (nth-value 2 (date-from-fixed +gregorian+ (date-rd d))))

(defun date-day-of-week (d)
  "ISO weekday number: 1 = Monday .. 7 = Sunday."
  (cl:1+ (mod (cl:1- (date-rd d)) 7)))

(defun date-leap-year-p (d)
  (gregorian-leap-year-p (date-year d)))

(defmethod print-object ((o date) stream)
  (print-unreadable-object (o stream :type t)
    (multiple-value-bind (y m d) (date-from-fixed +gregorian+ (date-rd o))
      (format stream "~4,'0d-~2,'0d-~2,'0d" y m d))))

;;; --- time-of-day -----------------------------------------------------------

(defclass time-of-day ()
  ((nanos-of-day :initarg :nanos-of-day :reader time-of-day-nanos-of-day
                 :type (integer 0 86399999999999)))
  (:documentation "A time within a day (no date, no zone), stored as
nanoseconds since midnight."))

(defun time-of-day-p (x) (typep x 'time-of-day))

(defun time-of-day-from-nanos (nanos-of-day)
  (unless (cl:<= 0 nanos-of-day (cl:1- +nanos-per-day+))
    (error 'datetime-arithmetic-error
           :message (format nil "nanos-of-day ~d out of range" nanos-of-day)))
  (make-instance 'time-of-day :nanos-of-day nanos-of-day))

(defun make-time-of-day (hour &optional (minute 0) (second 0) (nano 0))
  (time-of-day-from-nanos
   (cl:+ (cl:* (cl:+ (cl:* (cl:+ (cl:* hour 60) minute) 60) second) +nanos-per-second+) nano)))

(defvar +midnight+ (time-of-day-from-nanos 0))

(defun time-of-day-hour (tod) (floor (time-of-day-nanos-of-day tod) 3600000000000))
(defun time-of-day-minute (tod) (mod (floor (time-of-day-nanos-of-day tod) 60000000000) 60))
(defun time-of-day-second (tod) (mod (floor (time-of-day-nanos-of-day tod) +nanos-per-second+) 60))
(defun time-of-day-nano (tod) (mod (time-of-day-nanos-of-day tod) +nanos-per-second+))

(defmethod print-object ((o time-of-day) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~2,'0d:~2,'0d:~2,'0d.~9,'0d"
            (time-of-day-hour o) (time-of-day-minute o)
            (time-of-day-second o) (time-of-day-nano o))))

;;; --- moment (zone-naive date + time) --------------------------------------

(defclass moment ()
  ((date :initarg :date :reader moment-date :type date)
   (time :initarg :time :reader moment-time :type time-of-day))
  (:documentation "A zone-naive combination of a DATE and a TIME-OF-DAY —
the Common Lisp analogue of the \"local date-time\" concept used throughout
Reingold & Dershowitz's Calendrical Calculations."))

(defun momentp (x) (typep x 'moment))

(defun make-moment (date &optional (time +midnight+))
  (make-instance 'moment :date date :time time))

(defun %moment-total-nanos (m)
  (cl:+ (cl:* (date-rd (moment-date m)) +nanos-per-day+)
        (time-of-day-nanos-of-day (moment-time m))))

(defun %moment-from-total-nanos (total)
  (multiple-value-bind (rd nanos-of-day) (floor total +nanos-per-day+)
    (make-moment (date-from-rd rd) (time-of-day-from-nanos nanos-of-day))))

(defmethod print-object ((o moment) stream)
  (print-unreadable-object (o stream :type t)
    (multiple-value-bind (y mo d) (date-from-fixed +gregorian+ (date-rd (moment-date o)))
      (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~9,'0d"
              y mo d
              (time-of-day-hour (moment-time o)) (time-of-day-minute (moment-time o))
              (time-of-day-second (moment-time o)) (time-of-day-nano (moment-time o))))))

;;; --- zoned-moment ----------------------------------------------------------

(defclass zoned-moment ()
  ((moment :initarg :moment :reader zoned-moment-moment :type moment)
   (offset-seconds :initarg :offset-seconds :reader zoned-moment-offset-seconds :type integer)
   (zone :initarg :zone :reader zoned-moment-zone))
  (:documentation "A MOMENT anchored to a real instant via OFFSET-SECONDS
(seconds east of UTC) in a particular ZONE. Construct via MOMENT-IN-ZONE /
INSTANT-IN-ZONE in timezone.lisp, which resolve DST gaps and overlaps;
MAKE-ZONED-MOMENT is the raw, unchecked constructor."))

(defun zoned-moment-p (x) (typep x 'zoned-moment))

(defun make-zoned-moment (moment offset-seconds zone)
  (make-instance 'zoned-moment :moment moment :offset-seconds offset-seconds :zone zone))

(defun zoned-moment-date (zm) (moment-date (zoned-moment-moment zm)))
(defun zoned-moment-time (zm) (moment-time (zoned-moment-moment zm)))

(defmethod print-object ((o zoned-moment) stream)
  (print-unreadable-object (o stream :type t)
    (let ((off (zoned-moment-offset-seconds o)))
      (format stream "~a~:[+~;-~]~2,'0d:~2,'0d"
              (zoned-moment-moment o)
              (cl:< off 0)
              (floor (abs off) 3600)
              (mod (floor (abs off) 60) 60)))))

;;; --- calendar-month --------------------------------------------------------

(defclass calendar-month ()
  ((year :initarg :year :reader calendar-month-year :type integer)
   (month :initarg :month :reader calendar-month-month :type (integer 1 12)))
  (:documentation "A Gregorian year+month pair with no day component."))

(defun calendar-month-p (x) (typep x 'calendar-month))

(defun make-calendar-month (year month)
  (multiple-value-bind (y m) (%normalize-year-month year month)
    (make-instance 'calendar-month :year y :month m)))

(defun calendar-month-length (cm)
  (days-in-gregorian-month (calendar-month-year cm) (calendar-month-month cm)))

(defun calendar-month-first-date (cm)
  (make-date (calendar-month-year cm) (calendar-month-month cm) 1))

(defun calendar-month-last-date (cm)
  (make-date (calendar-month-year cm) (calendar-month-month cm) (calendar-month-length cm)))

(defmethod print-object ((o calendar-month) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~4,'0d-~2,'0d" (calendar-month-year o) (calendar-month-month o))))

;;; --- annual-date (recurring month/day, e.g. holidays/birthdays) -----------

(defclass annual-date ()
  ((month :initarg :month :reader annual-date-month :type (integer 1 12))
   (day :initarg :day :reader annual-date-day :type (integer 1 31)))
  (:documentation "A recurring MONTH/DAY with no year, e.g. a birthday or a
fixed-date holiday."))

(defun annual-date-p (x) (typep x 'annual-date))

(defun make-annual-date (month day)
  (make-instance 'annual-date :month month :day day))

(defun annual-date-in-year (ad year &key (overflow :clamp))
  "The DATE this recurring MONTH/DAY falls on in YEAR (Feb 29 clamps to
Feb 28 in a non-leap year by default; pass :OVERFLOW :ERROR to signal)."
  (let* ((month (annual-date-month ad))
         (max-day (days-in-gregorian-month year month))
         (day (annual-date-day ad)))
    (make-date year month
               (cond ((cl:<= day max-day) day)
                     ((eq overflow :clamp) max-day)
                     (t (error 'datetime-arithmetic-error
                               :message (format nil "~d has no day ~d in ~d" month day year)))))))

(defmethod print-object ((o annual-date) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "--~2,'0d-~2,'0d" (annual-date-month o) (annual-date-day o))))

;;; --- interval (half-open [start, end) of instants) ------------------------

(defclass interval ()
  ((start :initarg :start :reader interval-start :type instant)
   (end :initarg :end :reader interval-end :type instant))
  (:documentation "A half-open interval [START, END) of INSTANTs."))

(defun intervalp (x) (typep x 'interval))

(defun make-interval (start end)
  (make-instance 'interval :start start :end end))

(defmethod print-object ((o interval) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "[~a, ~a)" (interval-start o) (interval-end o))))

(defun %instant-key (i)
  (cl:+ (cl:* (instant-seconds i) +nanos-per-second+) (instant-nanos i)))

(defun interval-duration (iv)
  (let ((total (cl:- (%instant-key (interval-end iv)) (%instant-key (interval-start iv)))))
    (multiple-value-bind (sec nan) (floor total +nanos-per-second+)
      (make-duration sec nan))))

(defun interval-contains-p (iv instant)
  (let ((k (%instant-key instant)))
    (and (cl:>= k (%instant-key (interval-start iv)))
         (cl:< k (%instant-key (interval-end iv))))))

(defun interval-overlaps-p (a b)
  (and (cl:< (%instant-key (interval-start a)) (%instant-key (interval-end b)))
       (cl:< (%instant-key (interval-start b)) (%instant-key (interval-end a)))))
