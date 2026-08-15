(in-package #:datetime-protocol)

;;;; Field access and arithmetic on DATE (and the WITH-FIELDS "wither"
;;;; protocol for the other value types). Split out from chronology.lisp
;;;; because these generic functions specialize on the DATE class, so they
;;;; must load after types.lisp defines it; the pure RD<->fields conversions
;;;; in chronology.lisp have no such dependency and load first.

(defgeneric date-field (date field &optional chronology)
  (:documentation "Return FIELD of DATE under CHRONOLOGY (default
+GREGORIAN+): one of :YEAR :MONTH :DAY, or for +ISO-WEEK+, one of
:ISO-YEAR :ISO-WEEK :ISO-WEEKDAY."))

(defmethod date-field ((d date) field &optional (chronology +gregorian+))
  (multiple-value-bind (f1 f2 f3) (date-from-fixed chronology (date-rd d))
    (if (typep chronology 'iso-week-chronology)
        (ecase field (:iso-year f1) (:iso-week f2) (:iso-weekday f3))
        (ecase field (:year f1) (:month f2) (:day f3)))))

(defun %date-with-year-month-day (year month day overflow)
  "Build a Gregorian DATE from possibly out-of-range fields, applying
OVERFLOW (:CLAMP :CARRY :ERROR) when DAY exceeds the target month's length."
  (multiple-value-bind (y m) (%normalize-year-month year month)
    (let ((max-day (days-in-gregorian-month y m)))
      (cond
        ((cl:<= day max-day) (make-date y m day))
        (t (ecase overflow
             (:clamp (make-date y m max-day))
             (:carry (date-from-rd (cl:+ (fixed-from-date +gregorian+ y m 1) (cl:1- day))))
             (:error (error 'datetime-arithmetic-error
                            :message (format nil "day ~d does not exist in ~4,'0d-~2,'0d (use :overflow :clamp/:carry)"
                                              day y m)))))))))

(defgeneric date-add (date field n &key overflow)
  (:documentation "Add N units of FIELD (:DAYS :WEEKS :MONTHS :YEARS) to
DATE. Month/year overflow (e.g. Jan 31 + 1 month) is controlled by
:OVERFLOW (:CLAMP :CARRY :ERROR, default :ERROR)."))

(defmethod date-add ((d date) (field (eql :days)) n &key overflow)
  (declare (ignore overflow))
  (date-from-rd (cl:+ (date-rd d) n)))

(defmethod date-add ((d date) (field (eql :weeks)) n &key overflow)
  (declare (ignore overflow))
  (date-from-rd (cl:+ (date-rd d) (cl:* n 7))))

(defmethod date-add ((d date) (field (eql :months)) n &key (overflow :error))
  (multiple-value-bind (y m day) (date-from-fixed +gregorian+ (date-rd d))
    (%date-with-year-month-day y (cl:+ m n) day overflow)))

(defmethod date-add ((d date) (field (eql :years)) n &key (overflow :error))
  (multiple-value-bind (y m day) (date-from-fixed +gregorian+ (date-rd d))
    (%date-with-year-month-day (cl:+ y n) m day overflow)))

(defgeneric date-diff (date1 date2 field)
  (:documentation "Whole units of FIELD (:DAYS :MONTHS :YEARS) elapsed from
DATE2 to DATE1 (i.e. DATE1 - DATE2, in FIELD units)."))

(defmethod date-diff ((d1 date) (d2 date) (field (eql :days)))
  (cl:- (date-rd d1) (date-rd d2)))

(defmethod date-diff ((d1 date) (d2 date) (field (eql :months)))
  (multiple-value-bind (y1 m1 day1) (date-from-fixed +gregorian+ (date-rd d1))
    (multiple-value-bind (y2 m2 day2) (date-from-fixed +gregorian+ (date-rd d2))
      (let ((total (cl:- (cl:+ (cl:* y1 12) m1) (cl:+ (cl:* y2 12) m2))))
        (cond ((and (cl:> total 0) (cl:< day1 day2)) (cl:1- total))
              ((and (cl:< total 0) (cl:> day1 day2)) (cl:1+ total))
              (t total))))))

(defmethod date-diff ((d1 date) (d2 date) (field (eql :years)))
  (floor (date-diff d1 d2 :months) 12))

(defgeneric with-field (date field value &key overflow)
  (:documentation "Return a new DATE with FIELD (:YEAR :MONTH :DAY) set to
VALUE, applying :OVERFLOW when the day no longer fits the target month."))

(defmethod with-field ((d date) (field (eql :year)) value &key (overflow :error))
  (multiple-value-bind (y m day) (date-from-fixed +gregorian+ (date-rd d))
    (declare (ignore y))
    (%date-with-year-month-day value m day overflow)))

(defmethod with-field ((d date) (field (eql :month)) value &key (overflow :error))
  (multiple-value-bind (y m day) (date-from-fixed +gregorian+ (date-rd d))
    (declare (ignore m))
    (%date-with-year-month-day y value day overflow)))

(defmethod with-field ((d date) (field (eql :day)) value &key (overflow :error))
  (multiple-value-bind (y m day) (date-from-fixed +gregorian+ (date-rd d))
    (declare (ignore day))
    (%date-with-year-month-day y m value overflow)))

(defgeneric with-fields (object &key &allow-other-keys)
  (:documentation "Return a new value of the same type as OBJECT with the
given fields replaced — the Lisp-shaped \"wither\" this protocol uses in
place of Java-style plusXxx/withXxx/atZone methods."))

(defmethod with-fields ((d date) &key year month day (overflow :error))
  (multiple-value-bind (y m dd) (date-from-fixed +gregorian+ (date-rd d))
    (%date-with-year-month-day (or year y) (or month m) (or day dd) overflow)))

(defmethod with-fields ((tod time-of-day) &key hour minute second nano)
  (make-time-of-day (or hour (time-of-day-hour tod))
                     (or minute (time-of-day-minute tod))
                     (or second (time-of-day-second tod))
                     (or nano (time-of-day-nano tod))))

(defmethod with-fields ((m moment) &key year month day hour minute second nano (overflow :error))
  (make-moment (with-fields (moment-date m) :year year :month month :day day :overflow overflow)
               (with-fields (moment-time m) :hour hour :minute minute :second second :nano nano)))

(defmethod with-fields ((cm calendar-month) &key year month)
  (make-calendar-month (or year (calendar-month-year cm)) (or month (calendar-month-month cm))))

(defmethod with-fields ((ad annual-date) &key month day)
  (make-annual-date (or month (annual-date-month ad)) (or day (annual-date-day ad))))
