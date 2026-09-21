(in-package #:datetime-protocol/tests)

(defun %start (date &optional (zone +utc+))
  (zoned-moment-to-instant
   (moment-in-zone (make-moment date +midnight+) zone)))

(defun %now-sep-21 ()
  "2026-09-21T15:00:00Z — Monday."
  (zoned-moment-to-instant
   (moment-in-zone (make-moment (make-date 2026 9 21) (make-time-of-day 15 0 0))
                   +utc+)))

(defun %range (string &optional (now (%now-sep-21)))
  (parse-time-range string :now now :zone +utc+))

(deftest relative-unrecognized
  (multiple-value-bind (iv rest matched) (%range "budget numbers")
    (ok (null iv))
    (ok (string= "budget numbers" rest))
    (ok (not matched))))

(deftest relative-today-yesterday
  (let ((now (%now-sep-21)))
    (multiple-value-bind (iv rest matched) (%range "yesterday" now)
      (ok matched)
      (ok (string= "" rest))
      (ok (interval-contains-p iv (%start (make-date 2026 9 20) +utc+)))
      (ng (interval-contains-p iv (%start (make-date 2026 9 21) +utc+))))
    (multiple-value-bind (iv rest matched) (%range "today" now)
      (declare (ignore rest))
      (ok matched)
      (ok (interval-contains-p iv now)))))

(deftest relative-n-days-ago
  (multiple-value-bind (iv rest matched) (%range "3 days ago")
    (ok matched)
    (ok (string= "" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 9 18) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 9 19) +utc+)))))

(deftest relative-last-week
  (multiple-value-bind (iv rest matched) (%range "last week")
    (ok matched)
    (ok (string= "" rest))
    ;; 2026-09-21 is Monday; last week = 2026-09-14 .. 2026-09-21
    (ok (interval-contains-p iv (%start (make-date 2026 9 14) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 9 21) +utc+)))))

(deftest relative-last-tuesday
  (multiple-value-bind (iv rest matched) (%range "last Tuesday")
    (ok matched)
    (ok (string= "" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 9 15) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 9 16) +utc+)))))

(deftest relative-yesterday-evening-remainder
  (multiple-value-bind (iv rest matched)
      (%range "yesterday evening, about the budget")
    (ok matched)
    (ok (string= "about the budget" rest))
    (ok (interval-contains-p
         iv
         (zoned-moment-to-instant
          (moment-in-zone (make-moment (make-date 2026 9 20)
                                       (make-time-of-day 18 0 0))
                          +utc+))))
    (ng (interval-contains-p
         iv
         (zoned-moment-to-instant
          (moment-in-zone (make-moment (make-date 2026 9 20)
                                       (make-time-of-day 10 0 0))
                          +utc+))))))

(deftest relative-iso-date
  (multiple-value-bind (iv rest matched) (%range "what happened on 2026-07-19")
    (ok matched)
    (ok (string= "what happened on" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 7 19) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 7 20) +utc+)))))

(deftest relative-month-only-is-range
  ;; now = September 2026 → July is this year
  (multiple-value-bind (iv rest matched) (%range "in July budget")
    (ok matched)
    (ok (string= "budget" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 7 1) +utc+)))
    (ok (interval-contains-p iv (%start (make-date 2026 7 31) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 8 1) +utc+))))
  ;; now = March 2026 → July rolls back to 2025
  (let ((now (zoned-moment-to-instant
              (moment-in-zone (make-moment (make-date 2026 3 10)
                                           (make-time-of-day 12 0 0))
                              +utc+))))
    (multiple-value-bind (iv rest matched) (parse-time-range "July" :now now)
      (declare (ignore rest))
      (ok matched)
      (ok (interval-contains-p iv (%start (make-date 2025 7 15) +utc+)))
      (ng (interval-contains-p iv (%start (make-date 2026 7 15) +utc+))))))

(deftest relative-may-not-stolen-from-verb
  (multiple-value-bind (iv rest matched) (%range "may we discuss budget")
    (ok (null iv))
    (ok (string= "may we discuss budget" rest))
    (ok (not matched)))
  (multiple-value-bind (iv rest matched) (%range "in May")
    (ok matched)
    (ok (string= "" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 5 1) +utc+)))))

(deftest relative-last-month
  (multiple-value-bind (iv rest matched) (%range "last month")
    (ok matched)
    (ok (string= "" rest))
    (ok (interval-contains-p iv (%start (make-date 2026 8 15) +utc+)))
    (ng (interval-contains-p iv (%start (make-date 2026 9 1) +utc+)))))

(deftest relative-night-spans-midnight
  (multiple-value-bind (iv rest matched) (%range "yesterday night")
    (declare (ignore rest))
    (ok matched)
    (ok (interval-contains-p
         iv
         (zoned-moment-to-instant
          (moment-in-zone (make-moment (make-date 2026 9 20)
                                       (make-time-of-day 23 0 0))
                          +utc+))))
    (ok (interval-contains-p
         iv
         (zoned-moment-to-instant
          (moment-in-zone (make-moment (make-date 2026 9 21)
                                       (make-time-of-day 3 0 0))
                          +utc+))))
    (ng (interval-contains-p
         iv
         (zoned-moment-to-instant
          (moment-in-zone (make-moment (make-date 2026 9 21)
                                       (make-time-of-day 8 0 0))
                          +utc+))))))
