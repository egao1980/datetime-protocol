# Lunisolar month-start gold

Pinned snapshot: `lunisolar-gold.sexp`, every lunar month start (and leap month) from official tables. Refresh (network; Python only through uv, isolated):

```
uv run --no-project --with lunardate --with korean-lunar-calendar \
  python -I scripts/generate_lunisolar_gold.py
```

CI reads the frozen sexp only. Row: `(LUNAR-YEAR MONTH LEAP-P GY GM GD)`. The lunar year is the Gregorian year in which its month 1 starts.

## Primary sources (asserted)

| Id | Source | Range | Calendar |
|----|--------|-------|----------|
| `hko` | Hong Kong Observatory, [Gregorian-Lunar Calendar Conversion Table](https://www.hko.gov.hk/en/gts/time/conversion.htm) (files `https://www.hko.gov.hk/en/gts/time/calendar/text/files/T{year}e.txt`) | 1901–2100 | Chinese 农历, UTC+8 |
| `kasi` | Korea Astronomy and Space Science Institute, [음양력 변환](https://astro.kasi.re.kr/life/pageView/8) (JSON `https://astro.kasi.re.kr/life/solc?yyyy=&mm=&dd=`). KASI is the national almanac authority under the 천문법. | 1900–2050 (service range) | Korean 음력 |
| `hnd` | Hồ Ngọc Đức, *Âm lịch Việt Nam*, year tables `TK20`/`TK21`/`TK22` in `amlich-hnd.js` v0.8 (2004). The Leipzig site is offline; read from the [Internet Archive copy](https://web.archive.org/web/2023id_/https://www.informatik.uni-leipzig.de/~duc/amlich/JavaScript/amlich-hnd.js) (sha256 in the sexp). Decoded exactly as its `decodeLunarYear`. | 1900–2100 | Vietnamese âm lịch |

The English HKO files print a leap month as a repeat of the previous month number (2033/12/22 "11th Lunar Month" = 闰十一月).

## Secondary references (reported, never asserted)

`lunardate` (PyPI) against HKO 1901–2099 and `korean-lunar-calendar` (PyPI) against KASI 1900–2050: **0 disagreements** each at the 2026-11-27 refresh. A disagreement would be listed under `:secondary`; the primary wins.

## What the tests assert (`tests/lunisolar-gold-test.lisp`)

| Source | Zone | Result |
|--------|------|--------|
| HKO | `+china-lunisolar-zone+` (UTC+8) | every month 1929–2100. 1901–1928: 3 known rows (1914-11-17, 1916-02-03, 1920-11-10). The pre-1929 calendar came from 時憲曆 / 紫金山 tables, not modern ephemerides; Beijing local time (116°25′E) misses five other rows, so no meridian fits. |
| KASI | `+korea-lunisolar-zone+` (UTC+8 to 1911, UTC+9 from 1912-01-01) | every month 1900–2050. UTC+9 throughout misses 8 rows in 1903–1911; UTC+8:30 (civil time 1908-04-01..1911-12-31) misses 1908-04-30 and 1911-12-20; 1954–1961 (civil UTC+8:30) has no row that differs between UTC+8:30 and UTC+9. |
| HND | `+vietnam-lunisolar-zone+` (UTC+8 to 1967, UTC+7 from 1968, Decision 121-CP of 8 Aug 1967) | every month 1929–2100. 1900–1928: 5 known rows (1906-04-23, 1925-01-25 where HND departs from HKO, plus HKO's three). UTC+7 throughout misses 30 rows before 1968. |

Known rows are frozen: a fix that makes one match fails the test until the list is updated.
