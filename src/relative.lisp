(in-package #:datetime-protocol)

;;;; Natural-language time phrase → INTERVAL.
;;;; English + ISO. Month-only is a calendar-month span. Never treat a
;;;; recognized month name as a leftover keyword.

(defparameter +relative-month-names+
  '(("january" . 1) ("jan" . 1)
    ("february" . 2) ("feb" . 2)
    ("march" . 3) ("mar" . 3)
    ("april" . 4) ("apr" . 4)
    ("may" . 5)
    ("june" . 6) ("jun" . 6)
    ("july" . 7) ("jul" . 7)
    ("august" . 8) ("aug" . 8)
    ("september" . 9) ("sept" . 9) ("sep" . 9)
    ("october" . 10) ("oct" . 10)
    ("november" . 11) ("nov" . 11)
    ("december" . 12) ("dec" . 12)))

(defparameter +relative-weekday-names+
  '(("monday" . 1) ("mon" . 1)
    ("tuesday" . 2) ("tue" . 2) ("tues" . 2)
    ("wednesday" . 3) ("wed" . 3)
    ("thursday" . 4) ("thu" . 4) ("thur" . 4) ("thurs" . 4)
    ("friday" . 5) ("fri" . 5)
    ("saturday" . 6) ("sat" . 6)
    ("sunday" . 7) ("sun" . 7)))

(defparameter +ambiguous-month-names+
  '("may" "march" "mar"))

(defparameter +part-of-day-names+
  '(("morning" . :morning)
    ("midday" . :midday)
    ("noon" . :midday)
    ("afternoon" . :afternoon)
    ("evening" . :evening)
    ("night" . :night)
    ("late-night" . :late-night)
    ("latenight" . :late-night)))

(defun %date-start-instant (date zone)
  (zoned-moment-to-instant
   (moment-in-zone (make-moment date +midnight+) zone)))

(defun %date-end-instant (date zone)
  (%date-start-instant (date+ date 1) zone))

(defun %day-interval (date zone)
  (make-interval (%date-start-instant date zone) (%date-end-instant date zone)))

(defun %month-interval (cm zone)
  (make-interval (%date-start-instant (calendar-month-first-date cm) zone)
                 (%date-end-instant (calendar-month-last-date cm) zone)))

(defun %iso-week-start (date)
  (date- (date+ date 0) (days (cl:1- (date-day-of-week date)))))

(defun %week-interval (week-start zone)
  (make-interval (%date-start-instant week-start zone)
                 (%date-start-instant (date+ week-start 7) zone)))

(defun %hour-on-date (date hour zone)
  (multiple-value-bind (extra-days h) (floor hour 24)
    (let ((d (date+ date extra-days)))
      (zoned-moment-to-instant
       (moment-in-zone (make-moment d (make-time-of-day h 0 0)) zone)))))

(defun %apply-part-of-day (interval part date zone)
  "Narrow INTERVAL to PART of DATE. :NIGHT spans midnight into the next day."
  (declare (ignore interval))
  (ecase part
    (:morning (make-interval (%hour-on-date date 5 zone) (%hour-on-date date 12 zone)))
    (:midday (make-interval (%hour-on-date date 11 zone) (%hour-on-date date 14 zone)))
    (:afternoon (make-interval (%hour-on-date date 12 zone) (%hour-on-date date 17 zone)))
    (:evening (make-interval (%hour-on-date date 17 zone) (%hour-on-date date 21 zone)))
    (:night (make-interval (%hour-on-date date 21 zone) (%hour-on-date date 29 zone)))
    (:late-night (make-interval (%hour-on-date date 0 zone) (%hour-on-date date 5 zone)))))

(defun %lookup-alist (token alist)
  (cdr (assoc token alist :test #'string=)))

(defun %tokenize-relative (string)
  "Split STRING into (text start end) tokens. Hyphens inside ISO dates stay."
  (let ((s (string-downcase string))
        (len (length string))
        (tokens '()))
    (loop with i = 0
          while (cl:< i len)
          do (let ((ch (char s i)))
               (cond
                 ((or (alphanumericp ch) (char= ch #\-))
                  (let ((start i))
                    (loop while (and (cl:< i len)
                                     (let ((c (char s i)))
                                       (or (alphanumericp c) (char= c #\-))))
                          do (incf i))
                    (push (list (subseq s start i) start i) tokens)))
                 (t (incf i)))))
    (nreverse tokens)))

(defun %iso-date-token-p (text)
  (and (= (length text) 10)
       (char= (char text 4) #\-)
       (char= (char text 7) #\-)
       (ignore-errors (parse-iso-date text))))

(defun %parse-int-token (text)
  (when (and (plusp (length text)) (every #'digit-char-p text))
    (parse-integer text)))

(defun %month-named (text &key bare)
  (let ((month (%lookup-alist text +relative-month-names+)))
    (when month
      (if (and bare (member text +ambiguous-month-names+ :test #'string=))
          nil
          month))))

(defun %calendar-month-for (month today &key last)
  (let* ((year (date-year today))
         (cur (date-month today)))
    (cond
      (last
       (if (cl:> cur month)
           (make-calendar-month year month)
           (make-calendar-month (cl:1- year) month)))
      ((cl:> month cur)
       (make-calendar-month (cl:1- year) month))
      (t (make-calendar-month year month)))))

(defun %last-weekday (target today)
  (let* ((dow (date-day-of-week today))
         (delta (if (cl:= dow target) 7 (mod (cl:- dow target) 7))))
    (when (cl:zerop delta)
      (setf delta 7))
    (date- today (days delta))))

(defstruct (%rel-match (:constructor %make-rel-match)
                       (:conc-name %rm-))
  start end interval date part)

(defun %match-from (tokens i today zone)
  "Try to parse a time phrase starting at TOKENS[I].
   Returns (values match next-index) or NIL."
  (labels ((tok (k)
             (when (and (cl:>= k 0) (cl:< k (length tokens)))
               (first (nth k tokens))))
           (span (from to interval &optional date part)
             (values (%make-rel-match :start (second (nth from tokens))
                                      :end (third (nth (cl:1- to) tokens))
                                      :interval interval
                                      :date date
                                      :part part)
                     to)))
    (let ((a (tok i))
          (b (tok (cl:1+ i)))
          (c (tok (cl:+ i 2)))
          (d (tok (cl:+ i 3))))
      (cond
        ((null a) nil)
        ((and (string= a "the") (string= b "day") (string= c "before")
              (string= d "yesterday"))
         (let ((date (date- today (days 2))))
           (span i (cl:+ i 4) (%day-interval date zone) date)))
        ((and (string= a "day") (string= b "before") (string= c "yesterday"))
         (let ((date (date- today (days 2))))
           (span i (cl:+ i 3) (%day-interval date zone) date)))
        ((string= a "yesterday")
         (let ((date (date- today (days 1))))
           (span i (cl:1+ i) (%day-interval date zone) date)))
        ((string= a "today")
         (span i (cl:1+ i) (%day-interval today zone) today))
        ((string= a "tomorrow")
         (let ((date (date+ today 1)))
           (span i (cl:1+ i) (%day-interval date zone) date)))
        ((and (%parse-int-token a)
              (member b '("day" "days") :test #'string=)
              (string= c "ago"))
         (let ((date (date- today (days (%parse-int-token a)))))
           (span i (cl:+ i 3) (%day-interval date zone) date)))
        ((and (%parse-int-token a)
              (member b '("week" "weeks") :test #'string=)
              (string= c "ago"))
         (let ((start (%iso-week-start (date- today (weeks (%parse-int-token a))))))
           (span i (cl:+ i 3) (%week-interval start zone) start)))
        ((and (%parse-int-token a)
              (member b '("month" "months") :test #'string=)
              (string= c "ago"))
         (let ((cm (plus (make-calendar-month (date-year today) (date-month today))
                         (cl:- (%parse-int-token a)))))
           (span i (cl:+ i 3) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        ((and (string= a "last") (member b '("week") :test #'string=))
         (let ((start (%iso-week-start (date- today (weeks 1)))))
           (span i (cl:+ i 2) (%week-interval start zone) start)))
        ((and (string= a "this") (member b '("week") :test #'string=))
         (let ((start (%iso-week-start today)))
           (span i (cl:+ i 2) (%week-interval start zone) start)))
        ((and (string= a "last") (member b '("month") :test #'string=))
         (let ((cm (plus (make-calendar-month (date-year today) (date-month today)) -1)))
           (span i (cl:+ i 2) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        ((and (string= a "this") (member b '("month") :test #'string=))
         (let ((cm (make-calendar-month (date-year today) (date-month today))))
           (span i (cl:+ i 2) (%month-interval cm zone) today)))
        ((and (string= a "last") (%lookup-alist b +relative-weekday-names+))
         (let ((date (%last-weekday (%lookup-alist b +relative-weekday-names+) today)))
           (span i (cl:+ i 2) (%day-interval date zone) date)))
        ((and (string= a "last") (%month-named b :bare nil))
         (let ((cm (%calendar-month-for (%month-named b :bare nil) today :last t)))
           (span i (cl:+ i 2) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        ((and (string= a "this") (%month-named b :bare nil))
         (let ((cm (make-calendar-month (date-year today) (%month-named b :bare nil))))
           (span i (cl:+ i 2) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        ((and (string= a "in") (%month-named b :bare nil))
         (let ((cm (%calendar-month-for (%month-named b :bare nil) today)))
           (span i (cl:+ i 2) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        ((%iso-date-token-p a)
         (let ((date (parse-iso-date a)))
           (span i (cl:1+ i) (%day-interval date zone) date)))
        ((%month-named a :bare t)
         (let ((cm (%calendar-month-for (%month-named a :bare t) today)))
           (span i (cl:1+ i) (%month-interval cm zone)
                 (calendar-month-first-date cm))))
        (t nil)))))

(defun %match-part (tokens i)
  (let ((part (%lookup-alist (first (nth i tokens)) +part-of-day-names+)))
    (when part
      (values part (cl:1+ i) (second (nth i tokens)) (third (nth i tokens))))))

(defun %join-remainder (string consumed)
  "STRING with CONSUMED (start . end) spans removed. Collapse whitespace."
  (let ((keep (make-string (length string) :initial-element #\Space)))
    (replace keep string)
    (dolist (span consumed)
      (loop for i from (car span) below (cdr span)
            do (setf (char keep i) #\Space)))
    (let ((parts (loop for start = 0 then (cl:1+ end)
                       for end = (position #\Space keep :start start)
                       for tok = (string-trim " ,.;:" (subseq keep start (or end (length keep))))
                       unless (zerop (length tok))
                         collect tok
                       while end)))
      (if parts
          (format nil "~{~a~^ ~}" parts)
          ""))))

(defun parse-time-range (string &key now zone)
  "Parse a natural-language time phrase in STRING.
   Returns (values interval remainder matched-p).
   INTERVAL is half-open [start, end) of INSTANTs in ZONE (default +UTC+).
   NOW defaults to (NOW). Unrecognized → (values nil STRING nil).
   A recognized month name is always a range, never leftover search text."
  (check-type string string)
  (let* ((zone (or zone +utc+))
         (now-instant (or now (now)))
         (today (zoned-moment-date (instant-in-zone now-instant zone)))
         (tokens (%tokenize-relative string))
         (consumed '())
         (interval nil)
         (date nil)
         (part nil)
         (i 0))
    (loop while (cl:< i (length tokens))
          do (multiple-value-bind (match next) (%match-from tokens i today zone)
               (cond
                 (match
                  (setf interval (%rm-interval match)
                        date (%rm-date match)
                        part (%rm-part match))
                  (push (cons (%rm-start match) (%rm-end match)) consumed)
                  (setf i next)
                  (when (cl:< i (length tokens))
                    (multiple-value-bind (p next2 pstart pend) (%match-part tokens i)
                      (when p
                        (setf part p)
                        (push (cons pstart pend) consumed)
                        (setf i next2)))))
                 (t
                  (multiple-value-bind (p next2 pstart pend) (%match-part tokens i)
                    (if p
                        (progn
                          (unless date
                            (setf date today
                                  interval (%day-interval today zone)))
                          (setf part p)
                          (push (cons pstart pend) consumed)
                          (setf i next2))
                        (incf i)))))))
    (when (and interval part date)
      (setf interval (%apply-part-of-day interval part date zone)))
    (if interval
        (values interval (%join-remainder string consumed) t)
        (values nil string nil))))
