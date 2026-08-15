(defpackage #:datetime-protocol
  (:nicknames #:stack-datetime)
  (:use #:cl)
  (:shadow #:+ #:- #:< #:<= #:> #:>= #:= #:/= #:min #:max)
  (:export
   ;; conditions
   #:datetime-error
   #:datetime-error-message
   #:datetime-parse-error
   #:datetime-parse-error-input
   #:datetime-arithmetic-error
   #:zone-not-found
   #:zone-not-found-zone-id
   #:ambiguous-zone-abbreviation
   #:ambiguous-zone-abbreviation-abbreviation
   #:ambiguous-zone-abbreviation-candidates
   #:ambiguous-local-time
   #:ambiguous-local-time-moment
   #:ambiguous-local-time-earlier-offset
   #:ambiguous-local-time-later-offset
   #:nonexistent-local-time
   #:nonexistent-local-time-moment

   ;; types
   #:instant
   #:instantp
   #:make-instant
   #:instant-seconds
   #:instant-nanos
   #:duration
   #:durationp
   #:make-duration
   #:duration-seconds
   #:duration-nanos
   #:duration-zero-p
   #:duration-negative-p
   #:duration-total-nanos
   #:period
   #:periodp
   #:make-period
   #:period-years
   #:period-months
   #:period-days
   #:period-zero-p
   #:date
   #:datep
   #:make-date
   #:date-from-rd
   #:date-rd
   #:date-year
   #:date-month
   #:date-day
   #:date-day-of-week
   #:date-leap-year-p
   #:time-of-day
   #:time-of-day-p
   #:make-time-of-day
   #:time-of-day-from-nanos
   #:time-of-day-nanos-of-day
   #:time-of-day-hour
   #:time-of-day-minute
   #:time-of-day-second
   #:time-of-day-nano
   #:+midnight+
   #:moment
   #:momentp
   #:make-moment
   #:moment-date
   #:moment-time
   #:zoned-moment
   #:zoned-moment-p
   #:make-zoned-moment
   #:zoned-moment-moment
   #:zoned-moment-offset-seconds
   #:zoned-moment-zone
   #:zoned-moment-date
   #:zoned-moment-time
   #:calendar-month
   #:calendar-month-p
   #:make-calendar-month
   #:calendar-month-year
   #:calendar-month-month
   #:calendar-month-length
   #:calendar-month-first-date
   #:calendar-month-last-date
   #:annual-date
   #:annual-date-p
   #:make-annual-date
   #:annual-date-month
   #:annual-date-day
   #:annual-date-in-year
   #:interval
   #:intervalp
   #:make-interval
   #:interval-start
   #:interval-end
   #:interval-duration
   #:interval-contains-p
   #:interval-overlaps-p

   ;; operators (shadowed)
   #:+
   #:-
   #:<
   #:<=
   #:>
   #:>=
   #:=
   #:/=
   #:min
   #:max

   ;; named generic functions backing the operators
   #:plus
   #:minus
   #:negate
   #:less
   #:less-or-equal
   #:greater
   #:greater-or-equal
   #:value=
   #:value/=
   #:date+
   #:date-

   ;; unit constructors
   #:years
   #:months
   #:weeks
   #:days
   #:hours
   #:minutes
   #:seconds
   #:nanos

   ;; chronology protocol
   #:chronology
   #:chronology-id
   #:+gregorian+
   #:+julian+
   #:+iso-week+
   #:fixed-from-date
   #:date-from-fixed
   #:date-field
   #:date-add
   #:date-diff
   #:with-field
   #:with-fields
   #:days-in-gregorian-month
   #:days-in-julian-month

   ;; clock protocol
   #:clock
   #:clock-now
   #:*clock*
   #:now
   #:today
   #:system-clock
   #:fixed-clock
   #:fixed-clock-instant
   #:make-fixed-clock
   #:with-fixed-clock

   ;; timezone protocol
   #:tz-repository
   #:*tz-repository*
   #:default-tz-repository
   #:tzdata-available-p
   #:zone-id
   #:zone-id-p
   #:fixed-offset-zone
   #:make-fixed-offset-zone
   #:named-zone
   #:make-named-zone
   #:zone-offset-seconds
   #:zone-name
   #:+utc+
   #:zone-offset-for-instant
   #:zone-local-info
   #:moment-in-zone
   #:zoned-moment-to-instant
   #:instant-in-zone
   #:resolve-zone-id
   #:zone-abbreviation-for-instant

   ;; parsing / printing
   #:print-iso-date
   #:print-iso-time
   #:print-rfc3339
   #:print-http-date
   #:parse-iso-date
   #:parse-iso-time
   #:parse-rfc3339
   #:parse-http-date
   #:parse-moment

   ;; calendars (base package also hosts the reexports for /calendars subsystem)
   #:easter-western
   #:easter-orthodox
   #:hebrew-date
   #:hebrew-date-p
   #:make-hebrew-date
   #:hebrew-date-year
   #:hebrew-date-month
   #:hebrew-date-day
   #:hebrew-date-from-fixed
   #:fixed-from-hebrew-date
   #:hebrew-date-to-rd
   #:hebrew-date-from-date
   #:hebrew-year-length
   #:hebrew-leap-year-p
   #:hebrew-nisan-month
   #:rosh-hashanah
   #:passover
   #:islamic-date
   #:islamic-date-p
   #:make-islamic-date
   #:islamic-date-year
   #:islamic-date-month
   #:islamic-date-day
   #:islamic-date-from-fixed
   #:fixed-from-islamic-date
   #:islamic-date-to-rd
   #:islamic-date-from-date
   #:islamic-leap-year-p
   #:islamic-year-length))

(in-package #:datetime-protocol)
