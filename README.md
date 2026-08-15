# datetime-protocol

Lispy **CLOS** date/time protocol for [cl-stack](https://github.com/egao1980/cl-stack): instants, durations, periods, calendar dates, and time zones, with no hard dependencies in the core system.

| System | Nick | Role |
|--------|------|------|
| `datetime-protocol` | `stack-datetime` | `instant`/`duration`/`period`/`date`/`moment`/`zoned-moment`, chronology, clock, time zone, ISO 8601/RFC 3339/RFC 7231 |
| `datetime-protocol/calendars` | — | Easter, Hebrew, Islamic; solar astronomy; Chinese lunisolar; Jewish/Muslim sunrise–sunset ritual times |

**Not here:** IANA zone data itself → [`cl-stack-tzdata`](https://github.com/egao1980/cl-stack-tzdata) (soft dependency, loaded automatically when present).

## Shape

This protocol deliberately avoids `java.time`'s vocabulary — no `LocalDateTime`, `ZonedDateTime`, `YearMonth`, or `plusDays`/`atZone`-style methods:

| Concept | This protocol |
|---------|----------------|
| Exact point on the timeline | `instant` (Unix seconds + nanos) |
| Exact span of time | `duration` |
| Calendar-based span (Y/M/D) | `period` |
| Calendar date | `date` (Rata Die fixed-day number; RD 1 = proleptic Gregorian 0001-01-01) |
| Time within a day | `time-of-day` |
| Zone-naive date + time | `moment` |
| `moment` anchored to a zone/offset | `zoned-moment` |
| Year + month, no day | `calendar-month` |
| Recurring month/day (holidays, birthdays) | `annual-date` |
| Half-open span of instants | `interval` |

Arithmetic is done through shadowed operators (`+ - < <= > >= = /= min max`) backed by exported generic functions (`plus`, `minus`, `less`, `value=`, ...) with a numeric fallback to `cl:+` etc., plus Lisp-shaped `date+`/`date-`/`with-fields` for callers who don't want to `:use` the shadowed package.

```lisp
(asdf:load-system "datetime-protocol")
(use-package :stack-datetime)

(+ (make-date 2024 1 31) (months 1))       ; => error (month overflow, default :error)
(date-add (make-date 2024 1 31) :months 1 :overflow :clamp)  ; => #<DATE 2024-02-29>
(- (make-date 2024 1 11) (make-date 2024 1 1))               ; => 10 (days)
(print-rfc3339 (parse-rfc3339 "2024-10-27T08:00:00-04:00"))  ; roundtrip
(with-fields (make-date 2024 2 29) :year 2023 :overflow :clamp)  ; => #<DATE 2023-02-28>
```

Time zones work with just `+utc+`/fixed offsets out of the box; loading [`cl-stack-tzdata`](https://github.com/egao1980/cl-stack-tzdata) (autodetected via `asdf:find-system`) unlocks named IANA zones, DST gap/overlap resolution (`:on-gap`/`:on-overlap` `:earlier`/`:later`/`:strict`), and history-aware zone-id aliases.

```lisp
(asdf:load-system "cl-stack-tzdata")
(moment-in-zone (parse-moment "2024-03-10T02:30:00") (resolve-zone-id "America/New_York"))
;; => NONEXISTENT-LOCAL-TIME by default (spring-forward gap); pass :on-gap :later/:earlier to resolve
```

## Calendars

`datetime-protocol/calendars` adds enough of the Hebrew, Islamic, and Easter computus to drive holiday rules:

```lisp
(asdf:load-system "datetime-protocol/calendars")
(easter-western 2024)   ; => #<DATE 2024-03-31>
(easter-orthodox 2024)  ; => #<DATE 2024-05-05>
(rosh-hashanah 5785)    ; => #<DATE 2024-10-03>
(passover 5785)         ; => #<DATE 2025-04-13> (15 Nisan 5785)
```

## License

MIT
