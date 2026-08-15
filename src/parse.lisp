(in-package #:datetime-protocol)

;;;; ISO 8601 / RFC 3339 parsing and printing, plus RFC 7231 HTTP-date.
;;;; Hand-rolled fixed-width field parsing — no regex dependency.

;;; --- small parsing helpers -------------------------------------------------

(defun %require-char (string pos ch)
  (unless (and (cl:< pos (length string)) (char= (char string pos) ch))
    (error 'datetime-parse-error :input string
           :message (format nil "expected ~c at position ~d" ch pos)))
  (cl:1+ pos))

(defun %parse-fixed-int (string pos width)
  "Parse WIDTH decimal digits starting at POS. Returns (values int new-pos)."
  (let ((end (cl:+ pos width)))
    (unless (cl:<= end (length string))
      (error 'datetime-parse-error :input string :message "unexpected end of input"))
    (values (parse-integer string :start pos :end end) end)))

(defun %parse-fraction-nanos (string pos)
  "Parse an optional '.' + digits fractional-second at POS. Returns
(values nanos new-pos); nanos is 0 and new-pos = pos when there is none."
  (if (and (cl:< pos (length string)) (char= (char string pos) #\.))
      (let ((start (cl:1+ pos)) (end (cl:1+ pos)))
        (loop while (and (cl:< end (length string)) (digit-char-p (char string end)))
              do (incf end))
        (let* ((digits (subseq string start end))
               (nine (if (cl:>= (length digits) 9)
                         (subseq digits 0 9)
                         (concatenate 'string digits
                                      (make-string (cl:- 9 (length digits)) :initial-element #\0)))))
          (values (parse-integer nine) end)))
      (values 0 pos)))

(defun %parse-offset (string pos)
  "Parse \"Z\" or \"+HH:MM\"/\"-HH:MM\"/\"+HHMM\". Returns (values offset-seconds new-pos)."
  (cond
    ((and (cl:< pos (length string)) (member (char string pos) '(#\Z #\z)))
     (values 0 (cl:1+ pos)))
    ((and (cl:< pos (length string)) (member (char string pos) '(#\+ #\-)))
     (let ((sign (if (char= (char string pos) #\-) -1 1)))
       (multiple-value-bind (hh p2) (%parse-fixed-int string (cl:1+ pos) 2)
         (let ((p3 (if (and (cl:< p2 (length string)) (char= (char string p2) #\:)) (cl:1+ p2) p2)))
           (multiple-value-bind (mm p4) (%parse-fixed-int string p3 2)
             (values (cl:* sign (cl:+ (cl:* hh 3600) (cl:* mm 60))) p4))))))
    (t (error 'datetime-parse-error :input string :message "expected an offset (Z or +/-HH:MM)"))))

(defun %parse-bracket-zone (string pos)
  "Parse an optional \"[Zone/Id]\" suffix. Returns (values id-or-nil new-pos)."
  (if (and (cl:< pos (length string)) (char= (char string pos) #\[))
      (let ((close (position #\] string :start pos)))
        (unless close
          (error 'datetime-parse-error :input string :message "unterminated [zone-id]"))
        (values (subseq string (cl:1+ pos) close) (cl:1+ close)))
      (values nil pos)))

(defun %parse-date-time-prefix (string)
  "Parse \"YYYY-MM-DD[T ]HH:MM:SS[.fraction]\". Returns (values moment new-pos)."
  (multiple-value-bind (year p1) (%parse-fixed-int string 0 4)
    (let ((p2 (%require-char string p1 #\-)))
      (multiple-value-bind (month p3) (%parse-fixed-int string p2 2)
        (let ((p4 (%require-char string p3 #\-)))
          (multiple-value-bind (day p5) (%parse-fixed-int string p4 2)
            (let ((p6 (if (and (cl:< p5 (length string))
                                (member (char string p5) '(#\T #\t #\Space)))
                          (cl:1+ p5)
                          p5)))
              (multiple-value-bind (hour p7) (%parse-fixed-int string p6 2)
                (let ((p8 (%require-char string p7 #\:)))
                  (multiple-value-bind (minute p9) (%parse-fixed-int string p8 2)
                    (let ((p10 (%require-char string p9 #\:)))
                      (multiple-value-bind (second p11) (%parse-fixed-int string p10 2)
                        (multiple-value-bind (nano p12) (%parse-fraction-nanos string p11)
                          (values (make-moment (make-date year month day)
                                                (make-time-of-day hour minute second nano))
                                  p12))))))))))))))

(defmacro %wrap-parse-errors ((input) &body body)
  `(handler-case (progn ,@body)
     (datetime-parse-error (e) (error e))
     (error (e) (error 'datetime-parse-error :input ,input :message (princ-to-string e)))))

;;; --- ISO 8601 -------------------------------------------------------------

(defun parse-iso-date (string)
  "Parse \"YYYY-MM-DD\"."
  (%wrap-parse-errors (string)
    (multiple-value-bind (year p1) (%parse-fixed-int string 0 4)
      (let ((p2 (%require-char string p1 #\-)))
        (multiple-value-bind (month p3) (%parse-fixed-int string p2 2)
          (let ((p4 (%require-char string p3 #\-)))
            (multiple-value-bind (day p5) (%parse-fixed-int string p4 2)
              (unless (cl:= p5 (length string))
                (error 'datetime-parse-error :input string :message "trailing characters"))
              (make-date year month day))))))))

(defun parse-iso-time (string)
  "Parse \"HH:MM:SS[.fraction]\"."
  (%wrap-parse-errors (string)
    (multiple-value-bind (hour p1) (%parse-fixed-int string 0 2)
      (let ((p2 (%require-char string p1 #\:)))
        (multiple-value-bind (minute p3) (%parse-fixed-int string p2 2)
          (let ((p4 (%require-char string p3 #\:)))
            (multiple-value-bind (second p5) (%parse-fixed-int string p4 2)
              (multiple-value-bind (nano p6) (%parse-fraction-nanos string p5)
                (unless (cl:= p6 (length string))
                  (error 'datetime-parse-error :input string :message "trailing characters"))
                (make-time-of-day hour minute second nano)))))))))

(defun parse-moment (string)
  "Parse a zone-naive \"YYYY-MM-DDTHH:MM:SS[.fraction]\"."
  (%wrap-parse-errors (string)
    (multiple-value-bind (moment pos) (%parse-date-time-prefix string)
      (unless (cl:= pos (length string))
        (error 'datetime-parse-error :input string :message "trailing characters"))
      moment)))

(defun parse-rfc3339 (string)
  "Parse an RFC 3339 / ISO 8601 date-time with an explicit offset (\"Z\" or
+/-HH:MM), returning a ZONED-MOMENT. An optional bracketed zone id suffix
(\"...+01:00[Europe/Berlin]\") attaches a history-aware NAMED-ZONE (resolved
through tzdata's alias table when cl-stack-tzdata is loaded) instead of a
bare fixed offset."
  (%wrap-parse-errors (string)
    (multiple-value-bind (moment p1) (%parse-date-time-prefix string)
      (multiple-value-bind (offset p2) (%parse-offset string p1)
        (multiple-value-bind (zone-name p3) (%parse-bracket-zone string p2)
          (unless (cl:= p3 (length string))
            (error 'datetime-parse-error :input string :message "trailing characters"))
          (make-zoned-moment moment offset
                              (if zone-name (resolve-zone-id zone-name) (make-fixed-offset-zone offset))))))))

(defun %format-fraction-nanos (nano)
  (if (cl:zerop nano)
      ""
      (concatenate 'string "." (string-right-trim "0" (format nil "~9,'0d" nano)))))

(defun print-iso-date (date)
  (multiple-value-bind (y m d) (date-from-fixed +gregorian+ (date-rd date))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)))

(defun print-iso-time (tod)
  (format nil "~2,'0d:~2,'0d:~2,'0d~a"
          (time-of-day-hour tod) (time-of-day-minute tod) (time-of-day-second tod)
          (%format-fraction-nanos (time-of-day-nano tod))))

(defun print-iso-moment (moment)
  (format nil "~aT~a" (print-iso-date (moment-date moment)) (print-iso-time (moment-time moment))))

(defgeneric print-rfc3339 (value)
  (:documentation "Print VALUE (an INSTANT or ZONED-MOMENT) as RFC 3339."))

(defmethod print-rfc3339 ((value instant))
  (print-rfc3339 (instant-in-zone value +utc+)))

(defmethod print-rfc3339 ((value zoned-moment))
  (let ((offset (zoned-moment-offset-seconds value)))
    (format nil "~a~a" (print-iso-moment (zoned-moment-moment value))
            (if (cl:zerop offset) "Z" (%format-offset offset)))))

;;; --- RFC 7231 HTTP-date ----------------------------------------------------

(defparameter +http-day-names+ #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter +http-month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun print-http-date (instant)
  "Print INSTANT as an RFC 7231 IMF-fixdate, e.g. \"Sun, 06 Nov 1994 08:49:37 GMT\"."
  (let* ((zm (instant-in-zone instant +utc+))
         (date (zoned-moment-date zm))
         (tod (zoned-moment-time zm)))
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref +http-day-names+ (cl:1- (date-day-of-week date)))
            (date-day date) (aref +http-month-names+ (cl:1- (date-month date))) (date-year date)
            (time-of-day-hour tod) (time-of-day-minute tod) (time-of-day-second tod))))

(defun %split-on-spaces (string)
  (loop with start = 0
        with len = (length string)
        while (cl:< start len)
        for space = (or (position #\Space string :start start) len)
        when (cl:> space start) collect (subseq string start space)
        do (setf start (cl:1+ space))))

(defun %http-month-index (name)
  (or (position name +http-month-names+ :test #'string-equal)
      (error 'datetime-parse-error :message (format nil "unknown month ~s" name))))

(defun parse-http-date (string)
  "Parse an RFC 7231 IMF-fixdate (\"Sun, 06 Nov 1994 08:49:37 GMT\"), returning
an INSTANT. The obsolete RFC 850 and asctime() formats are not supported."
  (%wrap-parse-errors (string)
    (let* ((comma (position #\, string))
           (rest (string-left-trim " " (subseq string (cl:1+ (or comma -1)))))
           (parts (%split-on-spaces rest)))
      (destructuring-bind (day-str month-str year-str time-str &optional zone) parts
        (declare (ignore zone))
        (let ((day (parse-integer day-str))
              (month (cl:1+ (%http-month-index month-str)))
              (year (parse-integer year-str)))
          (multiple-value-bind (hour p1) (%parse-fixed-int time-str 0 2)
            (let ((p2 (%require-char time-str p1 #\:)))
              (multiple-value-bind (minute p3) (%parse-fixed-int time-str p2 2)
                (let ((p4 (%require-char time-str p3 #\:)))
                  (multiple-value-bind (second p5) (%parse-fixed-int time-str p4 2)
                    (declare (ignore p5))
                    (zoned-moment-to-instant
                     (make-zoned-moment (make-moment (make-date year month day)
                                                       (make-time-of-day hour minute second))
                                         0 +utc+))))))))))))
