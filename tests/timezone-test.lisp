(in-package #:datetime-protocol/tests)

;;;; Timezone protocol. +UTC+ / fixed-offset zones are always exercised;
;;;; named-zone / DST gap-overlap tests are skipped when cl-stack-tzdata is
;;;; not loaded in this image (it is a soft dependency of datetime-protocol).

(deftest utc-is-fixed-offset-zero
  (ok (zone-id-p +utc+))
  (ok (= 0 (zone-offset-for-instant +utc+ (make-instant 0)))))

(deftest fixed-offset-zone-roundtrip
  (let* ((zone (make-fixed-offset-zone -18000)) ; -05:00
         (m (make-moment (make-date 2024 6 1) (make-time-of-day 12 0 0)))
         (zm (moment-in-zone m zone)))
    (ok (= -18000 (zoned-moment-offset-seconds zm)))
    (ok (value= m (zoned-moment-moment zm)))
    (let ((back (instant-in-zone (zoned-moment-to-instant zm) zone)))
      (ok (value= m (zoned-moment-moment back))))))

(deftest instant-in-zone-utc
  (let* ((i (make-instant 1000000000))
         (zm (instant-in-zone i +utc+)))
    (ok (= 0 (zoned-moment-offset-seconds zm)))
    (ok (value= i (zoned-moment-to-instant zm)))))

(deftest minimal-repository-without-tzdata
  ;; Independent of whether tzdata happens to be loaded in this image: a
  ;; fresh MINIMAL-TZ-REPOSITORY always resolves "UTC" and rejects others.
  ;; REPOSITORY-RESOLVE-ID is an internal protocol generic (not exported);
  ;; this white-box test reaches it via the home package.
  (let ((repo (make-instance 'datetime-protocol::minimal-tz-repository)))
    (ok (string= "UTC" (datetime-protocol::repository-resolve-id repo "UTC")))
    (ok (signals (datetime-protocol::repository-resolve-id repo "America/New_York") 'zone-not-found))))

(deftest zone-not-found-without-tzdata-name
  (unless (tzdata-available-p)
    (ok (signals (resolve-zone-id "America/New_York") 'zone-not-found))))

;;; --- named-zone / DST tests: only meaningful when cl-stack-tzdata is loaded ---

(deftest named-zone-offset-new-york-winter
  (when (tzdata-available-p)
    (let ((zone (resolve-zone-id "America/New_York")))
      ;; 2024-01-15 is EST (UTC-5), not DST.
      (ok (= -18000 (zone-offset-for-instant
                      zone (zoned-moment-to-instant
                            (moment-in-zone (make-moment (make-date 2024 1 15) (make-time-of-day 12 0 0))
                                             +utc+))))))))

(deftest named-zone-offset-new-york-summer
  (when (tzdata-available-p)
    (let ((zone (resolve-zone-id "America/New_York")))
      ;; 2024-07-15 is EDT (UTC-4).
      (ok (= -14400 (zone-offset-for-instant
                      zone (zoned-moment-to-instant
                            (moment-in-zone (make-moment (make-date 2024 7 15) (make-time-of-day 12 0 0))
                                             +utc+))))))))

(deftest dst-spring-forward-gap
  (when (tzdata-available-p)
    ;; America/New_York: 2024-03-10 02:30 local does not exist (clocks jump
    ;; from 02:00 EST straight to 03:00 EDT).
    (let* ((zone (resolve-zone-id "America/New_York"))
           (m (make-moment (make-date 2024 3 10) (make-time-of-day 2 30 0))))
      (ok (eq :gap (zone-local-info zone m)))
      (ok (signals (moment-in-zone m zone :on-gap :strict) 'nonexistent-local-time))
      (ok (= -18000 (zoned-moment-offset-seconds (moment-in-zone m zone :on-gap :earlier))))
      (ok (= -14400 (zoned-moment-offset-seconds (moment-in-zone m zone :on-gap :later)))))))

(deftest dst-fall-back-overlap
  (when (tzdata-available-p)
    ;; America/New_York: 2024-11-03 01:30 local happens twice (EDT then EST).
    (let* ((zone (resolve-zone-id "America/New_York"))
           (m (make-moment (make-date 2024 11 3) (make-time-of-day 1 30 0))))
      (ok (eq :overlap (zone-local-info zone m)))
      (ok (signals (moment-in-zone m zone :on-overlap :strict) 'ambiguous-local-time))
      (ok (= -14400 (zoned-moment-offset-seconds (moment-in-zone m zone :on-overlap :earlier))))
      (ok (= -18000 (zoned-moment-offset-seconds (moment-in-zone m zone :on-overlap :later)))))))

(deftest named-zone-alias-resolution
  (when (tzdata-available-p)
    ;; "US/Eastern" is a historical alias for "America/New_York".
    (let ((zone (resolve-zone-id "US/Eastern")))
      (ok (string= "America/New_York" (zone-name zone))))))

(deftest zoned-moment-arithmetic-across-dst
  (when (tzdata-available-p)
    (let* ((zone (resolve-zone-id "America/New_York"))
           (before (moment-in-zone (make-moment (make-date 2024 3 9) (make-time-of-day 12 0 0)) zone)))
      ;; Adding 1 day (a PERIOD) re-resolves wall-clock time in the zone,
      ;; landing on the same 12:00 local time even though DST changed.
      (let ((after (+ before (days 1))))
        (ok (value= (make-date 2024 3 10) (zoned-moment-date after)))
        (ok (value= (make-time-of-day 12 0 0) (zoned-moment-time after)))
        (ok (= -14400 (zoned-moment-offset-seconds after))))
      ;; Adding a 24h DURATION instead tracks the exact instant, so wall-clock
      ;; time shifts by the 1-hour DST jump.
      (let ((after (+ before (hours 24))))
        (ok (value= (make-time-of-day 13 0 0) (zoned-moment-time after)))))))
