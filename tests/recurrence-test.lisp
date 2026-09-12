(in-package #:datetime-protocol/tests)

;;;; Date-level recurrence + EVENT-SCHEDULE (solar / computus).

(defun %d (y m d) (make-date y m d))

(defun %dates= (a b)
  (and (= (length a) (length b)) (every #'value= a b)))

;;; --- constructors / RFC 5545 ---------------------------------------------

(deftest daily-count
  (let* ((s (daily :from (%d 2024 1 1) :count 5))
         (got (occurrences s)))
    (ok (= 5 (length got)))
    (ok (value= (%d 2024 1 1) (first got)))
    (ok (value= (%d 2024 1 5) (car (last got))))
    (ok (occurrence-p s (%d 2024 1 3)))
    (ng (occurrence-p s (%d 2023 12 31)))))

(deftest weekly-mo-we-interval-2
  "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE — DTSTART included (RFC, not dateutil)."
  (let* ((start (%d 2024 1 1))                         ; Monday
         (s (weekly :on '(:monday :wednesday) :every 2 :from start))
         (got (occurrences s :to (%d 2024 1 22))))
    (ok (%dates= (list (%d 2024 1 1) (%d 2024 1 3)
                       (%d 2024 1 15) (%d 2024 1 17))
                 got))))

(deftest weekly-dtstart-wednesday-skips-prior-monday
  (let* ((s (weekly :on '(:monday :wednesday) :from (%d 2024 1 3)))
         (got (occurrences s :count 3)))
    (ok (value= (%d 2024 1 3) (first got)))
    (ok (value= (%d 2024 1 8) (second got)))
    (ng (occurrence-p s (%d 2024 1 1)))))

(deftest monthly-last-friday
  (let* ((s (monthly :on '(:nth -1 :friday) :from (%d 2024 1 1)))
         (got (occurrences s :count 3)))
    (ok (value= (%d 2024 1 26) (first got)))
    (ok (value= (%d 2024 2 23) (second got)))
    (ok (value= (%d 2024 3 29) (third got)))))

(deftest yearly-thanksgiving
  "4th Thursday of November."
  (let ((s (yearly :on '(:month 11 :weekday :thursday :nth 4)
                   :from (%d 2023 1 1))))
    (ok (value= (%d 2023 11 23) (next-occurrence s (%d 2023 1 1) :inclusive t)))
    (ok (value= (%d 2024 11 28) (next-occurrence s (%d 2024 1 1) :inclusive t)))
    (ok (occurrence-p s (%d 2024 11 28)))
    (ng (occurrence-p s (%d 2024 11 21)))))

(deftest yearly-byday-is-nth-of-year
  "FREQ=YEARLY;BYDAY=2FR = 2nd Friday of the year (errata 3779), not of DTSTART's month."
  (let ((from-jan (parse-rrule "FREQ=YEARLY;BYDAY=2FR" :from (%d 2024 1 1)))
        (from-aug (parse-rrule "FREQ=YEARLY;BYDAY=2FR" :from (%d 2024 8 1))))
    (ok (value= (%d 2024 1 12) (next-occurrence from-jan (%d 2024 1 1) :inclusive t)))
    (ok (occurrence-p from-jan (%d 2024 1 12)))
    (ng (occurrence-p from-aug (%d 2024 8 9)))
    (ok (value= (%d 2025 1 10) (next-occurrence from-aug (%d 2024 8 1) :inclusive t)))))

(deftest parse-rrule-roundtrip
  (let* ((text "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE")
         (s (parse-rrule text :from (%d 2024 1 1))))
    (ok (string= text (print-rrule s)))
    (ok (%dates= (occurrences s :count 4)
                 (occurrences (parse-rrule (print-rrule s) :from (%d 2024 1 1))
                              :count 4)))))

(deftest parse-rrule-dtstart-block
  (let ((s (parse-rrule (format nil "DTSTART:20240101~%RRULE:FREQ=DAILY;COUNT=3"))))
    (ok (value= (%d 2024 1 1) (schedule-origin s)))
    (ok (= 3 (length (occurrences s))))))

(deftest parse-rrule-count-xor-until
  (ok (signals (parse-rrule "FREQ=DAILY;COUNT=3;UNTIL=20240110"
                            :from (%d 2024 1 1))
               'datetime-arithmetic-error))
  (ok (signals (daily :from (%d 2024 1 1) :count 3 :until (%d 2024 1 10))
               'datetime-arithmetic-error)))

(deftest parse-rrule-unsupported-hourly
  (ok (signals (parse-rrule "FREQ=HOURLY" :from (%d 2024 1 1))
               'unsupported-recurrence))
  (ok (signals (parse-rrule "FREQ=DAILY;BYHOUR=9" :from (%d 2024 1 1))
               'unsupported-recurrence)))

(deftest unbounded-schedule-signals
  (ok (signals (occurrences (daily :from (%d 2024 1 1)))
               'unbounded-schedule)))

(deftest query-window-half-open
  (let ((s (daily :from (%d 2024 1 1))))
    (ok (%dates= (list (%d 2024 1 1) (%d 2024 1 2))
                 (occurrences s :from (%d 2024 1 1) :to (%d 2024 1 3))))))

(deftest rule-count-from-dtstart
  "COUNT is from DTSTART; query :count is N in the window."
  (let ((s (daily :from (%d 2024 1 1) :count 5)))
    (ok (%dates= (list (%d 2024 1 4) (%d 2024 1 5))
                 (occurrences s :from (%d 2024 1 4))))))

;;; --- combinators ---------------------------------------------------------

(deftest schedule-union-and-except
  (let* ((a (date-set (%d 2024 1 1) (%d 2024 1 3) (%d 2024 1 5)))
         (b (date-set (%d 2024 1 3) (%d 2024 1 7)))
         (u (schedule-union a b))
         (e (schedule-except a b)))
    (ok (%dates= (list (%d 2024 1 1) (%d 2024 1 3) (%d 2024 1 5) (%d 2024 1 7))
                 (occurrences u :from (%d 2024 1 1) :to (%d 2024 1 10))))
    (ok (%dates= (list (%d 2024 1 1) (%d 2024 1 5))
                 (occurrences e :from (%d 2024 1 1) :to (%d 2024 1 10))))))

(deftest offset-schedule-good-friday
  (let ((gf (offset-schedule (event-schedule :computus :easter-western) -2)))
    (ok (value= (%d 2024 3 29) (next-occurrence gf (%d 2024 1 1) :inclusive t)))
    (ok (occurrence-p gf (%d 2024 3 29)))
    (ng (occurrence-p gf (%d 2024 3 31)))))

(deftest annual-date-as-schedule
  (let ((ad (make-annual-date 2 29)))
    (ok (occurrence-p ad (%d 2024 2 29)))
    (ok (%dates= (list (%d 2024 2 29) (%d 2028 2 29))
                 (occurrences ad :from (%d 2024 1 1) :to (%d 2029 1 1))))))

(deftest register-event-extension
  (unwind-protect
       (progn
         (register-event :solstice
                         (lambda (source &key)
                           (declare (ignore source))
                           (yearly :on '(:month 6 :day 21)))
                         :source t)
         (let ((s (event-schedule t :solstice)))
           (ok (occurrence-p s (%d 2024 6 21)))))
    (remhash (cons t :solstice) datetime-protocol::*event-schedule-registry*)))

(deftest unknown-event-signals
  (ok (signals (event-schedule t :not-a-real-event) 'unknown-event)))

(deftest predicate-schedule-next
  (let ((s (predicate-schedule (lambda (d) (cl:= 5 (date-day-of-week d))))))
    (ok (value= (%d 2024 1 5) (next-occurrence s (%d 2024 1 1) :inclusive t)))
    (ok (occurrence-p s (%d 2024 1 5)))))

;;; --- solar / ritual ------------------------------------------------------

(deftest tokyo-sunrise-sunset-schedule
  (let* ((rise (event-schedule +tokyo+ :sunrise))
         (set (event-schedule +tokyo+ :sunset))
         (d (%d 2026 3 20))
         (sr (next-occurrence rise d :inclusive t))
         (ss (next-occurrence set d :inclusive t)))
    (ok (momentp sr))
    (ok (momentp ss))
    (ok (value= d (occurrence-date sr)))
    (ok (value= d (occurrence-date ss)))
    (ok (occurrence-p rise d))
    (ok (< (time-of-day-nanos-of-day (moment-time sr))
           (time-of-day-nanos-of-day (moment-time ss))))
    (let ((delhi (next-occurrence (event-schedule +delhi+ :sunrise) d :inclusive t)))
      (ng (= (time-of-day-nanos-of-day (moment-time sr))
             (time-of-day-nanos-of-day (moment-time delhi)))))))

(deftest solar-window-yields-moments
  (let* ((s (event-schedule +tokyo+ :sunset))
         (got (occurrences s :from (%d 2026 3 20) :to (%d 2026 3 23))))
    (ok (= 3 (length got)))
    (ok (every #'momentp got))
    (ok (equal (list 20 21 22)
               (mapcar (lambda (m) (date-day (occurrence-date m))) got)))))

(deftest dawn-dusk-depression
  (let* ((d (%d 2026 3 20))
         (civil (next-occurrence (event-schedule +tokyo+ :dawn) d :inclusive t))
         (naut (next-occurrence (event-schedule +tokyo+ :dawn :depression 12d0)
                                d :inclusive t))
         (dusk (next-occurrence (event-schedule +tokyo+ :dusk) d :inclusive t)))
    (ok (momentp civil))
    (ok (momentp naut))
    (ok (< (time-of-day-nanos-of-day (moment-time naut))
           (time-of-day-nanos-of-day (moment-time civil))))
    (ok (< (time-of-day-nanos-of-day (moment-time civil))
           (time-of-day-nanos-of-day (moment-time dusk))))))

(deftest polar-night-skipped
  (let* ((arctic (location 78.2d0 15.6d0 :zone 1 :name "Longyearbyen"))
         (s (event-schedule arctic :sunrise))
         (midwinter (%d 2026 1 15)))
    (ng (occurrence-p s midwinter))
    (ok (null (sunrise (date-rd midwinter) arctic)))
    (let ((next (next-occurrence s midwinter :inclusive t)))
      (ok next)
      (ok (momentp next))
      (ok (> (date-rd (occurrence-date next)) (date-rd midwinter))))))

(deftest midday-and-ritual-solar
  (let ((d (%d 2026 3 20)))
    (ok (momentp (next-occurrence (event-schedule +tokyo+ :midday) d :inclusive t)))
    (ok (momentp (next-occurrence (event-schedule +jerusalem+ :jewish-sunset)
                                  d :inclusive t)))
    (ok (momentp (next-occurrence (event-schedule +jerusalem+ :jewish-nightfall)
                                  d :inclusive t)))
    (ok (momentp (next-occurrence (event-schedule +mecca+ :islamic-maghrib)
                                  d :inclusive t)))
    (ok (momentp (next-occurrence (event-schedule +mecca+ :islamic-fajr)
                                  d :inclusive t)))
    (ok (momentp (next-occurrence (event-schedule +mecca+ :islamic-isha)
                                  d :inclusive t)))
    (let ((fajr (next-occurrence (event-schedule +mecca+ :islamic-fajr) d :inclusive t))
          (maghrib (next-occurrence (event-schedule +mecca+ :islamic-maghrib)
                                    d :inclusive t)))
      (ok (< (time-of-day-nanos-of-day (moment-time fajr))
             (time-of-day-nanos-of-day (moment-time maghrib)))))))

(deftest previous-sunset
  (let* ((s (event-schedule +tokyo+ :sunset))
         (d (%d 2026 3 20))
         (prev (previous-occurrence s d :inclusive t)))
    (ok (momentp prev))
    (ok (value= d (occurrence-date prev)))
    (ok (value= (%d 2026 3 19)
                (occurrence-date (previous-occurrence s d))))))

;;; --- computus / civil events ---------------------------------------------

(deftest easter-and-equinox-schedules
  (ok (value= (%d 2024 3 31)
              (next-occurrence (event-schedule :computus :easter-western)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 5 5)
              (next-occurrence (event-schedule :computus :easter-orthodox)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 3 20)
              (next-occurrence (event-schedule +tokyo+ :spring-equinox)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 4 4)
              (next-occurrence (event-schedule +beijing+ :qingming)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 2 10)
              (next-occurrence (event-schedule +beijing+ :chinese-new-year)
                               (%d 2024 1 1) :inclusive t))))

(deftest hebrew-islamic-schedules
  (ok (value= (%d 2024 10 3)
              (next-occurrence (event-schedule :computus :rosh-hashanah)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 4 23)
              (next-occurrence (event-schedule :computus :passover)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (eid-al-fitr 2024)
              (next-occurrence (event-schedule :computus :eid-al-fitr)
                               (%d 2024 1 1) :inclusive t)))
  (ok (value= (%d 2024 1 1)
              (next-occurrence (event-schedule t :new-year)
                               (%d 2024 1 1) :inclusive t))))
