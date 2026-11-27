#!/usr/bin/env python3
"""Freeze lunisolar month starts from official tables into data/tests/lunisolar-gold.sexp.

Run (network; Python only through uv, isolated):

    uv run --no-project --with lunardate --with korean-lunar-calendar \
        python -I scripts/generate_lunisolar_gold.py

Primary sources (the fixture asserts these):

  hko   Hong Kong Observatory, Gregorian-Lunar Calendar Conversion Tables
        https://www.hko.gov.hk/en/gts/time/conversion.htm
        files https://www.hko.gov.hk/en/gts/time/calendar/text/files/T{year}e.txt
        1901-2100. Chinese calendar (GB/T 33661-2017 rules, UTC+8).
  kasi  Korea Astronomy and Space Science Institute (KASI), the national
        almanac authority (천문법), 음양력 변환 service
        https://astro.kasi.re.kr/life/pageView/8  (JSON: /life/solc?yyyy=&mm=&dd=)
        1900-2050 (the service's published range).
  hnd   Ho Ngoc Duc, Am lich Viet Nam, published year tables TK20/TK21/TK22
        in amlich-hnd.js v0.8 (2004). The Leipzig site is offline; the file is
        read from the Internet Archive copy
        https://web.archive.org/web/2023id_/https://www.informatik.uni-leipzig.de/~duc/amlich/JavaScript/amlich-hnd.js
        1900-2100. Vietnamese calendar, UTC+7 (Decision 121-CP, 1967).

Secondary references (only cross-checked and reported, never asserted):
  lunardate (PyPI) for the Chinese table, korean-lunar-calendar (PyPI) for
  the Korean one. Where a secondary disagrees with a primary, the primary wins.

Each row is (LUNAR-YEAR MONTH LEAP-P GY GM GD): lunar month MONTH (LEAP-P
:leap or nil) of the lunar year whose month 1 starts in Gregorian year
LUNAR-YEAR begins on GY-GM-GD.
"""

from __future__ import annotations

import hashlib
import json
import re
import sys
import tempfile
import time
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from datetime import date, timedelta
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data" / "tests" / "lunisolar-gold.sexp"
CACHE = Path(tempfile.gettempdir()) / "lunisolar-gold-cache"
UA = "datetime-protocol lunisolar-gold refresh (https://github.com/egao1980/datetime-protocol)"

HKO_PAGE = "https://www.hko.gov.hk/en/gts/time/conversion.htm"
HKO_URI = "https://www.hko.gov.hk/en/gts/time/calendar/text/files/T{year}e.txt"
KASI_PAGE = "https://astro.kasi.re.kr/life/pageView/8"
KASI_URI = "https://astro.kasi.re.kr/life/solc?yyyy={y:04d}&mm={m:02d}&dd={d:02d}"
HND_URI = ("https://web.archive.org/web/2023id_/"
           "https://www.informatik.uni-leipzig.de/~duc/amlich/JavaScript/amlich-hnd.js")

MONTH_RE = re.compile(r"^(Leap\s+)?(\d+)(?:st|nd|rd|th)\s+Lunar\s+Month", re.I)
LINE_RE = re.compile(r"^(\d{4})/(\d{1,2})/(\d{1,2})\s+(.+)$")


def fetch(url: str, timeout: float = 60.0, tries: int = 4) -> bytes:
    for attempt in range(tries):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                return resp.read()
        except Exception:  # noqa: BLE001 - retry any transport error
            if attempt == tries - 1:
                raise
            time.sleep(2 * (attempt + 1))
    raise AssertionError("unreachable")


def cached(name: str, url: str) -> bytes:
    path = CACHE / name
    if path.exists() and path.stat().st_size > 0:
        return path.read_bytes()
    blob = fetch(url)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(blob)
    return blob


# --- HKO -------------------------------------------------------------------

def hko_rows() -> tuple[list[tuple], str]:
    def one(year: int) -> tuple[int, bytes]:
        return year, cached(f"hko/T{year}e.txt", HKO_URI.format(year=year))

    with ThreadPoolExecutor(max_workers=6) as pool:
        blobs = dict(pool.map(one, range(1901, 2101)))
    digest = hashlib.sha256()
    starts: list[tuple[date, int, bool]] = []
    for year in range(1901, 2101):
        blob = blobs[year]
        digest.update(blob)
        for line in blob.decode("utf-8", errors="replace").splitlines():
            m = LINE_RE.match(line.strip())
            if not m:
                continue
            gy, gm, gd, rest = int(m[1]), int(m[2]), int(m[3]), m[4]
            mm = MONTH_RE.match(rest.strip())
            if mm:
                month = int(mm[2])
                # The English tables print a leap month as a repeat of the
                # previous month's number (e.g. 2033/12/22 "11th Lunar Month").
                leap = bool(mm[1]) or bool(starts and starts[-1][1] == month)
                starts.append((date(gy, gm, gd), month, leap))
    rows = []
    lunar_year = None
    for d, month, leap in starts:
        if month == 1 and not leap:
            lunar_year = d.year
        ly = lunar_year if lunar_year is not None else d.year - 1
        rows.append((ly, month, leap, d.year, d.month, d.day))
    return rows, digest.hexdigest()


# --- KASI ------------------------------------------------------------------

def kasi_day(d: date) -> dict:
    blob = cached(f"kasi/{d.isoformat()}.json", KASI_URI.format(y=d.year, m=d.month, d=d.day))
    data = json.loads(blob)
    if isinstance(data, list):
        data = data[0]
    return data


def kasi_chain(first: date, last: date) -> list[tuple]:
    """Month starts from FIRST's month through the month containing LAST."""
    info = kasi_day(first)
    start = first - timedelta(days=int(info["LUNC_DD"]) - 1)
    rows = []
    while start <= last:
        info = kasi_day(start)
        if int(info["LUNC_DD"]) != 1:
            raise SystemExit(f"KASI {start}: expected day 1, got {info}")
        leap = info["LUNC_LEAP_MM"] == "윤"
        rows.append((int(info["LUNC_YYYY"]), int(info["LUNC_MM"]), leap,
                     start.year, start.month, start.day))
        start = start + timedelta(days=int(info["LUNC_EN_DD"]))
    return rows


def kasi_rows() -> list[tuple]:
    seeds = [(date(y, 1, 1), date(2050 if y == 2040 else y + 9, 12, 31))
             for y in range(1900, 2050, 10)]
    with ThreadPoolExecutor(max_workers=4) as pool:
        chunks = list(pool.map(lambda s: kasi_chain(*s), seeds))
    seen = {}
    for chunk in chunks:
        for row in chunk:
            seen[row[3:]] = row
    rows = [seen[k] for k in sorted(seen)]
    return [r for r in rows if date(*r[3:]) >= date(1900, 1, 31)
            and date(*r[3:]) <= date(2050, 12, 31)]


# --- Ho Ngoc Duc -----------------------------------------------------------

def hnd_tables(js: str) -> dict[str, list[int]]:
    tables = {}
    for name, body in re.findall(r"var\s+(TK\d+)\s*=\s*new\s+Array\(([^)]*)\)", js):
        tables[name] = [int(x, 16) for x in re.findall(r"0x[0-9a-fA-F]+", body)]
    return tables


def hnd_year(yy: int, k: int) -> list[tuple]:
    """Decode one year exactly as decodeLunarYear() in amlich-hnd.js."""
    lengths = (29, 30)
    offset_of_tet = k >> 17
    leap_month = k & 0xF
    leap_len = lengths[(k >> 16) & 1]
    j = k >> 4
    regular = [0] * 12
    for i in range(12):
        regular[12 - i - 1] = lengths[j & 1]
        j >>= 1
    cur = date(yy, 1, 1) + timedelta(days=offset_of_tet)
    out = []
    for mm in range(1, 13):
        out.append((yy, mm, False, cur.year, cur.month, cur.day))
        cur += timedelta(days=regular[mm - 1])
        if mm == leap_month:
            out.append((yy, mm, True, cur.year, cur.month, cur.day))
            cur += timedelta(days=leap_len)
    return out


def hnd_rows() -> tuple[list[tuple], str]:
    blob = cached("hnd/amlich-hnd.js", HND_URI)
    tables = hnd_tables(blob.decode("latin-1"))
    rows = []
    for yy in range(1900, 2101):
        if yy < 2000:
            k = tables["TK20"][yy - 1900]
        elif yy < 2100:
            k = tables["TK21"][yy - 2000]
        else:
            k = tables["TK22"][yy - 2100]
        rows.extend(hnd_year(yy, k))
    return rows, hashlib.sha256(blob).hexdigest()


# --- Secondary references --------------------------------------------------

def secondary_lunardate() -> list[tuple]:
    from lunardate import LunarDate  # type: ignore

    rows = []
    d = date(1900, 1, 31)
    end = date(2099, 12, 31)
    while d <= end:
        ld = LunarDate.fromSolarDate(d.year, d.month, d.day)
        if ld.day == 1:
            rows.append((ld.year, ld.month, bool(ld.isLeapMonth), d.year, d.month, d.day))
        d += timedelta(days=1)
    return rows


def secondary_korean() -> list[tuple]:
    from korean_lunar_calendar import KoreanLunarCalendar  # type: ignore

    cal = KoreanLunarCalendar()
    rows = []
    d = date(1900, 1, 31)
    end = date(2050, 12, 31)
    while d <= end:
        cal.setSolarDate(d.year, d.month, d.day)
        if cal.lunarDay == 1:
            rows.append((cal.lunarYear, cal.lunarMonth, bool(cal.isIntercalation),
                         d.year, d.month, d.day))
        d += timedelta(days=1)
    return rows


def compare(primary: list[tuple], secondary: list[tuple], lo: date, hi: date) -> list[str]:
    def window(rows):
        return {r[3:]: r[:3] for r in rows if lo <= date(*r[3:]) <= hi}

    a, b = window(primary), window(secondary)
    diffs = []
    for k in sorted(set(a) | set(b)):
        if a.get(k) != b.get(k):
            diffs.append(f"{date(*k)} primary={a.get(k)} secondary={b.get(k)}")
    return diffs


# --- Output ----------------------------------------------------------------

def sexp_row(r: tuple) -> str:
    ly, m, leap, gy, gm, gd = r
    return f"({ly} {m} {':leap' if leap else 'nil'} {gy} {gm} {gd})"


def write_block(fp, source: str, zone: str, years: tuple[int, int], rows: list[tuple]) -> None:
    fp.write(f"  (:source {json.dumps(source)} :kind :lunisolar-month-start\n")
    fp.write(f"   :zone {json.dumps(zone)} :years ({years[0]} {years[1]})\n")
    fp.write("   :rows (\n")
    for r in rows:
        fp.write(f"     {sexp_row(r)}\n")
    fp.write("   ))\n")


def check_anchors(name: str, rows: list[tuple], anchors: list[tuple]) -> None:
    index = {r[:3]: r[3:] for r in rows}
    for key, want in anchors:
        if index.get(key) != want:
            raise SystemExit(f"{name}: anchor {key} expected {want}, got {index.get(key)}")


def main() -> int:
    CACHE.mkdir(parents=True, exist_ok=True)
    hko, hko_sha = hko_rows()
    kasi = kasi_rows()
    hnd, hnd_sha = hnd_rows()
    # Published anchors (issue #38 and the respective almanacs).
    check_anchors("hko", hko, [((2033, 8, False), (2033, 8, 25)),
                               ((2033, 11, True), (2033, 12, 22))])
    check_anchors("kasi", kasi, [((2027, 1, False), (2027, 2, 7)),
                                 ((2028, 1, False), (2028, 1, 27))])
    check_anchors("hnd", hnd, [((2007, 1, False), (2007, 2, 17)),
                               ((2030, 1, False), (2030, 2, 2)),
                               ((1968, 1, False), (1968, 1, 29))])
    report = []
    for label, prim, sec, lo, hi in (
            ("hko vs lunardate", hko, secondary_lunardate(), date(1901, 2, 19), date(2099, 12, 31)),
            ("kasi vs korean-lunar-calendar", kasi, secondary_korean(), date(1900, 1, 31), date(2050, 12, 31))):
        diffs = compare(prim, sec, lo, hi)
        report.append((label, diffs))
        print(f"{label}: {len(diffs)} disagreements", file=sys.stderr)
        for line in diffs[:40]:
            print(f"  {line}", file=sys.stderr)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", encoding="utf-8") as fp:
        fp.write(";; Lunisolar month starts from official tables.\n")
        fp.write(";; Regenerate: uv run --no-project --with lunardate --with korean-lunar-calendar \\\n")
        fp.write(";;   python -I scripts/generate_lunisolar_gold.py   (see data/tests/LUNISOLAR.md)\n")
        fp.write(";; Row: (LUNAR-YEAR MONTH LEAP-P GY GM GD).\n(\n")
        fp.write(f" :generated {json.dumps(date.today().isoformat())}\n")
        fp.write(" :sources (\n")
        fp.write(f"  (:id \"hko\" :kind :official :uri {json.dumps(HKO_PAGE)}\n"
                 f"   :files {json.dumps(HKO_URI)} :sha256 {json.dumps(hko_sha)})\n")
        fp.write(f"  (:id \"kasi\" :kind :official :uri {json.dumps(KASI_PAGE)}\n"
                 f"   :api \"https://astro.kasi.re.kr/life/solc\")\n")
        fp.write(f"  (:id \"hnd\" :kind :published-table :uri {json.dumps(HND_URI)}\n"
                 f"   :sha256 {json.dumps(hnd_sha)} :note \"Ho Ngoc Duc amlich-hnd.js v0.8 TK20/TK21/TK22\")\n")
        fp.write(" )\n :secondary (\n")
        for label, diffs in report:
            fp.write(f"  (:compare {json.dumps(label)} :disagreements {len(diffs)}\n   :rows (\n")
            for line in diffs:
                fp.write(f"     {json.dumps(line)}\n")
            fp.write("   ))\n")
        fp.write(" )\n :blocks (\n")
        write_block(fp, "hko", "UTC+8", (1901, 2100), hko)
        write_block(fp, "kasi", "Korea (KST)", (1900, 2050), kasi)
        write_block(fp, "hnd", "Vietnam (UTC+7 from 1968)", (1900, 2100), hnd)
        fp.write(" )\n)\n")
    print(f"wrote {OUT} hko={len(hko)} kasi={len(kasi)} hnd={len(hnd)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
