(in-package #:datetime-protocol/tests)

;;;; ISO 8601 / RFC 3339 / RFC 7231 parse+print roundtrips.

(deftest parse-print-iso-date
  (ok (value= (make-date 2024 3 15) (parse-iso-date "2024-03-15")))
  (ok (string= "2024-03-15" (print-iso-date (make-date 2024 3 15))))
  (ok (signals (parse-iso-date "2024-03-15X") 'datetime-parse-error))
  (ok (signals (parse-iso-date "2024/03/15") 'datetime-parse-error)))

(deftest parse-print-iso-time
  (ok (value= (make-time-of-day 8 49 37) (parse-iso-time "08:49:37")))
  (ok (string= "08:49:37" (print-iso-time (make-time-of-day 8 49 37))))
  (ok (value= (make-time-of-day 8 49 37 500000000) (parse-iso-time "08:49:37.5")))
  (ok (string= "08:49:37.5" (print-iso-time (make-time-of-day 8 49 37 500000000))))
  (ok (value= (make-time-of-day 8 49 37 123000000) (parse-iso-time "08:49:37.123"))))

(deftest parse-moment-roundtrip
  (let ((m (make-moment (make-date 2024 10 27) (make-time-of-day 8 0 0))))
    (ok (value= m (parse-moment "2024-10-27T08:00:00")))
    (ok (value= m (parse-moment "2024-10-27 08:00:00")))))

(deftest parse-print-rfc3339-utc
  (let ((zm (parse-rfc3339 "2024-10-27T08:00:00Z")))
    (ok (value= (make-date 2024 10 27) (zoned-moment-date zm)))
    (ok (= 0 (zoned-moment-offset-seconds zm)))
    (ok (string= "2024-10-27T08:00:00Z" (print-rfc3339 zm)))))

(deftest parse-print-rfc3339-offset
  (let ((zm (parse-rfc3339 "2024-10-27T08:00:00-04:00")))
    (ok (= -14400 (zoned-moment-offset-seconds zm)))
    (ok (string= "2024-10-27T08:00:00-04:00" (print-rfc3339 zm)))))

(deftest parse-rfc3339-basic-offset-no-colon
  (let ((zm (parse-rfc3339 "2024-10-27T08:00:00+0200")))
    (ok (= 7200 (zoned-moment-offset-seconds zm)))))

(deftest parse-rfc3339-fraction-and-offset
  (let ((zm (parse-rfc3339 "2024-10-27T08:00:00.250Z")))
    (ok (= 250000000 (time-of-day-nano (zoned-moment-time zm))))))

(deftest print-rfc3339-instant-roundtrip
  (let* ((i (make-instant 1730016000))
         (printed (print-rfc3339 i))
         (zm (parse-rfc3339 printed)))
    (ok (value= i (zoned-moment-to-instant zm)))))

(deftest parse-rfc3339-invalid-signals
  (ok (signals (parse-rfc3339 "2024-10-27T08:00:00") 'datetime-parse-error))
  (ok (signals (parse-rfc3339 "not-a-date") 'datetime-parse-error)))

(deftest print-parse-http-date-roundtrip
  ;; RFC 7231 example date, 1994-11-06 is a Sunday.
  (let ((i (zoned-moment-to-instant
            (moment-in-zone (make-moment (make-date 1994 11 6) (make-time-of-day 8 49 37)) +utc+))))
    (ok (string= "Sun, 06 Nov 1994 08:49:37 GMT" (print-http-date i)))
    (ok (value= i (parse-http-date "Sun, 06 Nov 1994 08:49:37 GMT")))))
