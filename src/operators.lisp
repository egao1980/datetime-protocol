(in-package #:datetime-protocol)

;;;; This package shadows CL:+ - < <= > >= = /= MIN MAX. Each shadowed
;;;; symbol is an ordinary, variadic, CL-compatible function (so plain
;;;; numeric code that :USEs this package keeps working exactly as before);
;;;; it dispatches pairwise to a named, exported, EXTENSIBLE generic
;;;; function (PLUS, MINUS, LESS, ...) that new datetime-like types can add
;;;; methods to. Every generic has a fallback method on (number number) that
;;;; simply calls the CL:* operator, so numeric behavior is unchanged.

;;; --- PLUS / + ----------------------------------------------------------

(defgeneric plus (a b)
  (:documentation "Binary + for datetime values; numbers fall back to CL:+."))

(defmethod plus ((a number) (b number)) (cl:+ a b))

(defmethod plus ((a date) (b integer)) (date-from-rd (cl:+ (date-rd a) b)))
(defmethod plus ((a integer) (b date)) (plus b a))

(defmethod plus ((a date) (b period))
  (let ((d (date-add a :years (period-years b))))
    (setf d (date-add d :months (period-months b)))
    (date-add d :days (period-days b))))
(defmethod plus ((a period) (b date)) (plus b a))

(defmethod plus ((a instant) (b duration))
  (multiple-value-bind (sec nan)
      (floor (cl:+ (cl:* (instant-seconds a) +nanos-per-second+) (instant-nanos a)
                   (cl:* (duration-seconds b) +nanos-per-second+) (duration-nanos b))
             +nanos-per-second+)
    (make-instant sec nan)))
(defmethod plus ((a duration) (b instant)) (plus b a))

(defmethod plus ((a instant) (b period))
  (error 'datetime-arithmetic-error
         :message "an INSTANT has no calendar fields — convert to a ZONED-MOMENT first"))
(defmethod plus ((a period) (b instant)) (plus b a))

(defmethod plus ((a duration) (b duration))
  (multiple-value-bind (sec nan) (floor (cl:+ (duration-total-nanos a) (duration-total-nanos b))
                                         +nanos-per-second+)
    (make-duration sec nan)))

(defmethod plus ((a period) (b period))
  (make-period :years (cl:+ (period-years a) (period-years b))
               :months (cl:+ (period-months a) (period-months b))
               :days (cl:+ (period-days a) (period-days b))))

(defmethod plus ((a moment) (b period))
  (make-moment (plus (moment-date a) b) (moment-time a)))
(defmethod plus ((a period) (b moment)) (plus b a))

(defmethod plus ((a moment) (b duration))
  (%moment-from-total-nanos (cl:+ (%moment-total-nanos a) (duration-total-nanos b))))
(defmethod plus ((a duration) (b moment)) (plus b a))

(defmethod plus ((a calendar-month) (b integer))
  (multiple-value-bind (y m) (%normalize-year-month (calendar-month-year a)
                                                     (cl:+ (calendar-month-month a) b))
    (make-calendar-month y m)))
(defmethod plus ((a integer) (b calendar-month)) (plus b a))

(defmethod plus ((a time-of-day) (b duration))
  (time-of-day-from-nanos (mod (cl:+ (time-of-day-nanos-of-day a) (duration-total-nanos b))
                                +nanos-per-day+)))
(defmethod plus ((a duration) (b time-of-day)) (plus b a))

(defun + (&rest args)
  (cond ((null args) 0)
        ((null (rest args)) (first args))
        (t (reduce #'plus args))))

;;; --- MINUS / - -----------------------------------------------------------

(defgeneric minus (a b)
  (:documentation "Binary - for datetime values; numbers fall back to CL:-."))
(defgeneric negate (a)
  (:documentation "Unary negation; numbers fall back to CL:-."))

(defmethod minus ((a number) (b number)) (cl:- a b))
(defmethod negate ((a number)) (cl:- a))

(defmethod minus ((a date) (b integer)) (date-from-rd (cl:- (date-rd a) b)))
(defmethod minus ((a date) (b date)) (cl:- (date-rd a) (date-rd b)))
(defmethod minus ((a date) (b period)) (plus a (negate b)))

(defmethod negate ((p period))
  (make-period :years (cl:- (period-years p)) :months (cl:- (period-months p))
               :days (cl:- (period-days p))))

(defmethod minus ((a instant) (b duration)) (plus a (negate b)))
(defmethod minus ((a instant) (b instant))
  (multiple-value-bind (sec nan)
      (floor (cl:- (cl:+ (cl:* (instant-seconds a) +nanos-per-second+) (instant-nanos a))
                   (cl:+ (cl:* (instant-seconds b) +nanos-per-second+) (instant-nanos b)))
             +nanos-per-second+)
    (make-duration sec nan)))
(defmethod minus ((a instant) (b period))
  (error 'datetime-arithmetic-error
         :message "an INSTANT has no calendar fields — convert to a ZONED-MOMENT first"))

(defmethod negate ((d duration))
  (multiple-value-bind (sec nan) (floor (cl:- (duration-total-nanos d)) +nanos-per-second+)
    (make-duration sec nan)))
(defmethod minus ((a duration) (b duration)) (plus a (negate b)))

(defmethod minus ((a period) (b period)) (plus a (negate b)))

(defmethod minus ((a moment) (b period)) (plus a (negate b)))
(defmethod minus ((a moment) (b duration)) (plus a (negate b)))
(defmethod minus ((a moment) (b moment))
  (multiple-value-bind (sec nan) (floor (cl:- (%moment-total-nanos a) (%moment-total-nanos b))
                                         +nanos-per-second+)
    (make-duration sec nan)))

(defmethod minus ((a calendar-month) (b integer))
  (plus a (cl:- b)))
(defmethod minus ((a calendar-month) (b calendar-month))
  (cl:- (cl:+ (cl:* (calendar-month-year a) 12) (calendar-month-month a))
        (cl:+ (cl:* (calendar-month-year b) 12) (calendar-month-month b))))

(defmethod minus ((a time-of-day) (b duration)) (plus a (negate b)))
(defmethod minus ((a time-of-day) (b time-of-day))
  (multiple-value-bind (sec nan)
      (floor (cl:- (time-of-day-nanos-of-day a) (time-of-day-nanos-of-day b)) +nanos-per-second+)
    (make-duration sec nan)))

(defun - (&rest args)
  (cond ((null args) (error 'datetime-arithmetic-error :message "- needs at least one argument"))
        ((null (rest args)) (negate (first args)))
        (t (reduce #'minus args))))

;;; --- comparisons: < <= > >= = /= min max --------------------------------

(defgeneric less (a b)
  (:documentation "True if A sorts strictly before B; numbers fall back to CL:<."))
(defgeneric value= (a b)
  (:documentation "True if A and B represent the same value; numbers fall back to CL:=."))

(defmethod less ((a number) (b number)) (cl:< a b))
(defmethod value= ((a number) (b number)) (cl:= a b))

(defmacro define-key-comparison (type key-fn)
  "Install LESS/VALUE= methods for TYPE that compare (KEY-FN obj)."
  `(progn
     (defmethod less ((a ,type) (b ,type)) (cl:< (,key-fn a) (,key-fn b)))
     (defmethod value= ((a ,type) (b ,type)) (cl:= (,key-fn a) (,key-fn b)))))

(define-key-comparison date date-rd)
(define-key-comparison time-of-day time-of-day-nanos-of-day)
(define-key-comparison instant %instant-key)
(define-key-comparison duration duration-total-nanos)
(define-key-comparison moment %moment-total-nanos)

(defmethod less ((a zoned-moment) (b zoned-moment))
  (less (zoned-moment-to-instant a) (zoned-moment-to-instant b)))
(defmethod value= ((a zoned-moment) (b zoned-moment))
  (value= (zoned-moment-to-instant a) (zoned-moment-to-instant b)))

(defun %calendar-month-key (cm) (cl:+ (cl:* (calendar-month-year cm) 12) (calendar-month-month cm)))
(define-key-comparison calendar-month %calendar-month-key)

(defun %annual-date-key (ad) (cl:+ (cl:* (annual-date-month ad) 100) (annual-date-day ad)))
(define-key-comparison annual-date %annual-date-key)

(defmethod value= ((a period) (b period))
  (and (cl:= (period-years a) (period-years b))
       (cl:= (period-months a) (period-months b))
       (cl:= (period-days a) (period-days b))))

(defun less-or-equal (a b) (cl:not (less b a)))
(defun greater (a b) (less b a))
(defun greater-or-equal (a b) (cl:not (less a b)))
(defun value/= (a b) (cl:not (value= a b)))

(defun < (&rest args)
  (loop for (a b) on args while b always (less a b)))
(defun <= (&rest args)
  (loop for (a b) on args while b always (less-or-equal a b)))
(defun > (&rest args)
  (loop for (a b) on args while b always (greater a b)))
(defun >= (&rest args)
  (loop for (a b) on args while b always (greater-or-equal a b)))
(defun = (&rest args)
  (loop for (a b) on args while b always (value= a b)))
(defun /= (&rest args)
  (loop for (a . rest) on args always (loop for b in rest always (value/= a b))))

(defun min (&rest args)
  (reduce (lambda (a b) (if (less b a) b a)) args))
(defun max (&rest args)
  (reduce (lambda (a b) (if (less a b) b a)) args))

;;; --- DATE+ / DATE- ---------------------------------------------------------
;;; Explicit, unshadowed spellings for callers that don't want to :USE this
;;; package's shadowed arithmetic — Lisp-shaped names, not plusDays/minusDays.

(defun date+ (a-date amount) (plus a-date amount))
(defun date- (a-date amount) (minus a-date amount))

;;; --- unit constructors -----------------------------------------------------

(defun years (n) (make-period :years n))
(defun months (n) (make-period :months n))
(defun weeks (n) (make-period :days (cl:* n 7)))
(defun days (n) (make-period :days n))
(defun hours (n) (make-duration (cl:* n 3600)))
(defun minutes (n) (make-duration (cl:* n 60)))
(defun seconds (n) (make-duration n))
(defun nanos (n) (make-duration 0 n))
