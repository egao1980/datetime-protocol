(in-package #:datetime-protocol)

;;;; Clock protocol: a pluggable source of "now", so application code never
;;;; calls the underlying OS clock directly and tests can swap in a
;;;; deterministic FIXED-CLOCK.

(defconstant +universal-to-unix-offset+ 2208988800
  "(get-universal-time) counts seconds since 1900-01-01; Unix time counts
seconds since 1970-01-01. The difference is exactly this many seconds.")

(defclass clock () ()
  (:documentation "Base class for a source of the current INSTANT."))

(defgeneric clock-now (clock)
  (:documentation "The current INSTANT according to CLOCK. NOW is the
convenience entry point built on top of this extension point."))

(defclass system-clock (clock) ()
  (:documentation "Reads the operating system's real-time clock."))

(defmethod clock-now ((clock system-clock))
  #+sbcl
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (make-instant seconds (cl:* microseconds 1000)))
  #-sbcl
  (make-instant (cl:- (get-universal-time) +universal-to-unix-offset+)))

(defclass fixed-clock (clock)
  ((instant :initarg :instant :reader fixed-clock-instant))
  (:documentation "Always returns the same INSTANT — for deterministic tests."))

(defmethod clock-now ((clock fixed-clock)) (fixed-clock-instant clock))

(defun make-fixed-clock (instant) (make-instance 'fixed-clock :instant instant))

(defvar *clock* (make-instance 'system-clock)
  "The ambient CLOCK used by NOW/TODAY when no clock is given explicitly.")

(defun now (&optional (clock *clock*))
  "The current INSTANT, from CLOCK (default *CLOCK*)."
  (clock-now clock))

(defun today (&optional (zone +utc+) (clock *clock*))
  "The current DATE in ZONE (default +UTC+), from CLOCK (default *CLOCK*)."
  (zoned-moment-date (instant-in-zone (now clock) zone)))

(defmacro with-fixed-clock ((instant) &body body)
  "Bind *CLOCK* to a FIXED-CLOCK reading INSTANT for the duration of BODY."
  `(let ((*clock* (make-fixed-clock ,instant)))
     ,@body))
