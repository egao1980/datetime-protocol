(in-package #:datetime-protocol)

;;;; Chronology protocol: a CHRONOLOGY converts between a Rata Die fixed-day
;;;; integer (RD) and a triple of calendar fields. RD 1 is proleptic
;;;; Gregorian 0001-01-01 (a Monday); RD 719163 is 1970-01-01 (a Thursday),
;;;; so a Unix day number equals RD - 719163.
;;;;
;;;; The algorithms below are derived independently from first principles
;;;; (cumulative day-of-year tables + a year-start estimate/correction loop,
;;;; verified against Python's `datetime.date.toordinal`/`isocalendar`) —
;;;; they do not reuse the equations from Reingold & Dershowitz's
;;;; Calendrical Calculations / calendrica.

(defconstant +unix-epoch-rd+ 719163)

(defclass chronology ()
  ((id :initarg :id :reader chronology-id))
  (:documentation "A calendar system that maps between Rata Die day numbers
and a triple of calendar fields."))

(defclass gregorian-chronology (chronology) ())
(defclass julian-chronology (chronology) ())
(defclass iso-week-chronology (chronology) ())

(defvar +gregorian+ (make-instance 'gregorian-chronology :id :gregorian)
  "The proleptic Gregorian chronology: (fixed-from-date +gregorian+ year month day).")
(defvar +julian+ (make-instance 'julian-chronology :id :julian)
  "The proleptic Julian chronology (used by e.g. the Orthodox computus).")
(defvar +iso-week+ (make-instance 'iso-week-chronology :id :iso-week)
  "The ISO 8601 week-numbering chronology: fields are (iso-year iso-week iso-weekday).")

(defgeneric fixed-from-date (chronology field1 field2 field3)
  (:documentation "Convert calendar fields under CHRONOLOGY to a Rata Die
fixed-day integer. For +GREGORIAN+/+JULIAN+ the fields are (year month day);
for +ISO-WEEK+ they are (iso-year iso-week iso-weekday)."))

(defgeneric date-from-fixed (chronology rd)
  (:documentation "Convert a Rata Die RD to (values field1 field2 field3)
under CHRONOLOGY."))

;;; --- shared month tables ---------------------------------------------------

(defparameter +days-before-month+
  #(0 0 31 59 90 120 151 181 212 243 273 304 334 365)
  "Index M (1-based) holds the non-leap cumulative day count before month M;
index 13 is the non-leap year length, used as the upper sentinel.")

(defun gregorian-leap-year-p (year)
  (and (cl:zerop (mod year 4))
       (or (cl:not (cl:zerop (mod year 100))) (cl:zerop (mod year 400)))))

(defun julian-leap-year-p (year)
  (cl:zerop (mod year 4)))

(defun days-in-gregorian-month (year month)
  (if (cl:= month 2)
      (if (gregorian-leap-year-p year) 29 28)
      (cl:- (aref +days-before-month+ (cl:1+ month)) (aref +days-before-month+ month))))

(defun days-in-julian-month (year month)
  (if (cl:= month 2)
      (if (julian-leap-year-p year) 29 28)
      (cl:- (aref +days-before-month+ (cl:1+ month)) (aref +days-before-month+ month))))

(defun %days-before-month (month leap-p)
  (cl:+ (aref +days-before-month+ month) (if (and (cl:> month 2) leap-p) 1 0)))

(defun %month-length (month leap-p)
  (if (cl:= month 2)
      (if leap-p 29 28)
      (cl:- (aref +days-before-month+ (cl:1+ month)) (aref +days-before-month+ month))))

(defun %month-day-from-day-of-year (day-of-year leap-p)
  (loop for month from 1 to 12
        for start = (%days-before-month month leap-p)
        for len = (%month-length month leap-p)
        when (cl:<= day-of-year (cl:+ start len))
          return (values month (cl:- day-of-year start))
        finally (error 'datetime-arithmetic-error
                       :message (format nil "day-of-year ~d out of range" day-of-year))))

;;; --- Gregorian --------------------------------------------------------------

(defun gregorian-year-start-rd (year)
  "RD of YEAR-01-01 (proleptic Gregorian)."
  (let ((y (cl:1- year)))
    (cl:+ 1 (cl:* 365 y) (floor y 4) (cl:- (floor y 100)) (floor y 400))))

(defun %date-from-fixed-generic (rd year-start-fn leap-p-fn)
  "Shared inverse: estimate the year from RD's average day count, correct
with a small bounded loop, then scan the month table."
  (let ((year (cl:1+ (floor (cl:* (cl:1- rd) 400) 146097))))
    (loop while (cl:> (funcall year-start-fn year) rd) do (cl:decf year))
    (loop while (cl:<= (funcall year-start-fn (cl:1+ year)) rd) do (cl:incf year))
    (let* ((leap (funcall leap-p-fn year))
           (day-of-year (cl:1+ (cl:- rd (funcall year-start-fn year)))))
      (multiple-value-bind (month day) (%month-day-from-day-of-year day-of-year leap)
        (values year month day)))))

(defmethod fixed-from-date ((chronology gregorian-chronology) year month day)
  (cl:+ (gregorian-year-start-rd year) (%days-before-month month (gregorian-leap-year-p year))
        day -1))

(defmethod date-from-fixed ((chronology gregorian-chronology) rd)
  (%date-from-fixed-generic rd #'gregorian-year-start-rd #'gregorian-leap-year-p))

;;; --- Julian -----------------------------------------------------------------
;;;
;;; Calibrated independently against the modern (1900-2099) fact that a
;;; Julian calendar date plus 13 days equals the Gregorian date for the same
;;; physical day (equivalently, cross-checked against the historical 1582
;;; reform: Julian 1582-10-04 was immediately followed by Gregorian
;;; 1582-10-15). Both anchors give the same proleptic epoch offset.

(defun julian-year-start-rd (year)
  "RD of YEAR-01-01 (proleptic Julian)."
  (let ((y (cl:1- year)))
    (cl:+ -1 (cl:* 365 y) (floor y 4))))

(defmethod fixed-from-date ((chronology julian-chronology) year month day)
  (cl:+ (julian-year-start-rd year) (%days-before-month month (julian-leap-year-p year))
        day -1))

(defmethod date-from-fixed ((chronology julian-chronology) rd)
  (%date-from-fixed-generic rd #'julian-year-start-rd #'julian-leap-year-p))

;;; --- ISO week-numbering calendar (ISO 8601 §4.3.2.2) ------------------------

(defun iso-weekday (rd)
  "1 = Monday .. 7 = Sunday. Calibrated against RD 1 (proleptic Gregorian
0001-01-01, a Monday by construction of Rata Die) and RD 719163
(1970-01-01, a Thursday)."
  (cl:1+ (mod (cl:1- rd) 7)))

(defun %iso-week1-monday-rd (iso-year)
  "RD of the Monday starting ISO week 1 of ISO-YEAR — the Monday on or
before Gregorian ISO-YEAR-01-04 (the first Thursday of the Gregorian year
always falls in ISO week 1)."
  (let* ((jan4-rd (fixed-from-date +gregorian+ iso-year 1 4))
         (jan4-weekday (iso-weekday jan4-rd)))
    (cl:- jan4-rd (cl:1- jan4-weekday))))

(defmethod fixed-from-date ((chronology iso-week-chronology) iso-year iso-week iso-weekday)
  (cl:+ (%iso-week1-monday-rd iso-year) (cl:* (cl:1- iso-week) 7) (cl:1- iso-weekday)))

(defmethod date-from-fixed ((chronology iso-week-chronology) rd)
  (let* ((weekday (iso-weekday rd))
         (monday-rd (cl:- rd (cl:1- weekday)))
         (thursday-rd (cl:+ monday-rd 3)))
    (multiple-value-bind (iso-year month day) (date-from-fixed +gregorian+ thursday-rd)
      (declare (ignore month day))
      (values iso-year
              (cl:1+ (floor (cl:- monday-rd (%iso-week1-monday-rd iso-year)) 7))
              weekday))))

(defun %normalize-year-month (year month)
  "Roll MONTH into 1..12, carrying overflow/underflow into YEAR."
  (multiple-value-bind (extra-years zero-based-month) (floor (cl:1- month) 12)
    (values (cl:+ year extra-years) (cl:1+ zero-based-month))))
