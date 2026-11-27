# datetime-protocol

Lispy **CLOS** date/time protocol for [cl-stack](https://github.com/egao1980/cl-stack): instants, durations, periods, calendar dates, and time zones, with no hard dependencies in the core system.

| System | Nick | Role |
|--------|------|------|
| `datetime-protocol` | `stack-datetime` | `instant`/`duration`/`period`/`date`/`moment`/`zoned-moment`, chronology, clock, time zone, ISO 8601/RFC 3339/RFC 7231, `parse-time-range` |
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

### Lunisolar (Chinese, Korean, Vietnamese)

One engine, GB/T 33661-2017 rules; the calendars differ only in the meridian of the civil day, passed as `:zone`:

```lisp
(chinese-new-year-date 2027)                                 ; => 2027-02-06 (UTC+8)
(chinese-new-year-date 2027 :zone +korea-lunisolar-zone+)    ; => 2027-02-07 (설날, KST)
(chinese-new-year-date 2030 :zone +vietnam-lunisolar-zone+)  ; => 2030-02-02 (Tết, UTC+7)
(zhongqiu-date 2033)                                         ; => 2033-09-08
(chinese-lunar-date 2033 11 1 :leap t)                       ; => 2033-12-22 (闰十一月)
(lunisolar-from-fixed (date-rd (make-date 2024 2 9)))        ; => 2023 12 NIL 30
```

| Zone | Meridian | Fitted to |
|------|----------|-----------|
| `+china-lunisolar-zone+` (default) | UTC+8 | [HKO](https://www.hko.gov.hk/en/gts/time/conversion.htm), every month 1929–2100 |
| `+korea-lunisolar-zone+` | UTC+8 to 1911, UTC+9 from 1912 | [KASI](https://astro.kasi.re.kr/life/pageView/8), every month 1900–2050 |
| `+vietnam-lunisolar-zone+` | UTC+8 to 1967, UTC+7 from 1968 (Decision 121-CP) | Hồ Ngọc Đức tables, every month 1929–2100 |

`:zone` also takes hours (`9`) or an `astro-location`; `make-lunisolar-zone` builds a historical schedule. `fixed-from-lunisolar` signals `invalid-lunisolar-date` (restarts `use-value`, and `continue` for a missing leap month). Month-start gold: [`data/tests/LUNISOLAR.md`](data/tests/LUNISOLAR.md).

Conversion / festival gold (CPython, dateutil, pyluach, Kuwaiti JDN, 内閣府, HKO, IANA): `data/tests/chrono-gold.sexp`. Refresh: `uv run scripts/generate_chrono_gold.py`. See [`data/tests/CHRONO.md`](data/tests/CHRONO.md).

## Recurrence / events

Date-level `schedule` protocol: `occurrence-p`, `map-occurrences` (half-open `[from, to)`), `next-occurrence`, `previous-occurrence`. RFC 5545 RECUR is interchange (`parse-rrule` / `print-rrule`), not the DX. HOURLY / BYHOUR / BYYEARDAY / BYWEEKNO signal `unsupported-recurrence`. COUNT xor UNTIL. `FREQ=YEARLY;BYDAY=2FR` is the 2nd Friday of the **year** ([errata 3779](https://www.rfc-editor.org/errata/eid3779)).

```lisp
(weekly :on '(:monday :wednesday) :every 2 :from d0)
(monthly :on '(:nth -1 :friday) :from d0 :count 12)
(yearly :on '(:month 11 :weekday :thursday :nth 4))   ; Thanksgiving
(parse-rrule "FREQ=WEEKLY;BYDAY=MO,WE;INTERVAL=2" :from dtstart)
(offset-schedule (event-schedule :computus :easter-western) -2)  ; Good Friday
```

`event-schedule` is the extension point. Specialize it, or `register-event`.

```lisp
(event-schedule +tokyo+ :sunset)
(event-schedule +tokyo+ :dawn :depression 12d0)       ; nautical
(event-schedule +jerusalem+ :jewish-sunset)
(event-schedule +mecca+ :islamic-fajr)
(event-schedule :computus :easter-western)
(next-occurrence (event-schedule +tokyo+ :sunrise) (today))
```

Solar events yield `moment`s (standard zone of the location); polar night is skipped (`occurrence-p` false). Holiday / business-day / weekend methods live on the calendar that owns the law (`cl-stack-calendars`).

## Relative ranges

`(parse-time-range string &key now zone)` → `(values interval remainder matched-p)`.
English + ISO. `"yesterday evening, budget"` → `[20:00 yesterday, 21:00)` leftover `"budget"`.
Month-only (`July`, `in July`) is a calendar-month span; a month after `now` rolls back a year.
Bare `"may"` is not stolen from the verb — use `in May` / `last May`.

## License

MIT
