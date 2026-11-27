# Chrono gold

Pinned snapshot: `chrono-gold.sexp`. Refresh (needs network for 内閣府 + HKO):

```
uv run scripts/generate_chrono_gold.py
```

Tests lock against the frozen sexp — CI does not run Python.

| Source | Kind | What we compare |
|--------|------|-----------------|
| CPython `datetime.date` | stdlib | RD (`toordinal`), ISO week (`isocalendar`) |
| [python-dateutil](https://dateutil.readthedocs.io/) Easter | computational | Western + Orthodox 1583–2299 |
| [pyluach](https://github.com/simlist/pyluach) | computational | Hebrew holidays + year length / leap (not calendrica) |
| Kuwaiti / civil tabular JDN | published formula | Islamic Y-M-D → Gregorian. **Not** Umm al-Qura |
| Historical Julian | published table | 1582-10-04 + 1d = Gregorian 1582-10-15; 1900–2099 +13 |
| [内閣府 syukujitsu.csv](https://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv) | official | 春分の日 / 秋分の日 (Tokyo/JST civil) |
| [HKO conversion tables](https://www.hko.gov.hk/en/gts/time/conversion1_text.htm) | official | 春节 / 清明 / 端午 / 中秋 1901–2100 |
| `zoneinfo` (IANA) | tzdata | UTC offsets + DST gap/overlap wall times |

`java.time` is the same proleptic Gregorian/ISO chronology as CPython — not a second source. It has no Hebrew / Islamic civil / Chinese lunisolar.

Asserted HKO range: **1929–2100** (CST). Pre-1929 紫金山/HKO tables predate modern ephemerides. No known skips: 2033 中秋 was a leap-month bug (cl-stack-calendars#38). Every month start (Chinese, Korean, Vietnamese) is locked in [`LUNISOLAR.md`](LUNISOLAR.md).
