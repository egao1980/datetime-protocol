#!/usr/bin/env python3
# /// script
# requires-python = ">=3.12"
# dependencies = [
#   "python-dateutil>=2.9",
#   "pyluach>=2.2",
# ]
# ///
"""Build data/tests/chrono-gold.sexp from independent sources.

CPython datetime / zoneinfo, dateutil Easter, pyluach Hebrew, published
Kuwaiti/tabular JDN, 内閣府 祝日 CSV, Hong Kong Observatory lunar tables.

Not calendrica / Reingold & Dershowitz. java.time is the same proleptic
Gregorian as CPython — skipped (no lunisolar / Hebrew / Islamic there).
"""

from __future__ import annotations

import calendar
import json
import math
import re
import sys
import tempfile
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

from dateutil.easter import EASTER_ORTHODOX, EASTER_WESTERN, easter
from pyluach import dates as hebrew_dates

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data" / "tests" / "chrono-gold.sexp"
CAO_URI = "https://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv"
HKO_URI = "https://www.hko.gov.hk/en/gts/time/calendar/text/files/T{year}e.txt"
UA = "datetime-protocol chrono-gold refresh (https://github.com/egao1980/datetime-protocol)"

# JDN of a Gregorian civil day = toordinal() + this (noon-based integer JDN).
ORDINAL_TO_JDN = 1_721_425

WEEKDAYS = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
MONTH_RE = re.compile(r"^(?:Leap\s+)?(\d+)(?:st|nd|rd|th)\s+Lunar\s+Month$", re.I)
LINE_RE = re.compile(r"^(\d{4})/(\d{1,2})/(\d{1,2})\s+(.+)$")


def fetch(url: str, timeout: float = 60.0) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def lisp_str(s: str) -> str:
    return json.dumps(s, ensure_ascii=False)


def lisp_atom(x: object) -> str:
    if x is None or x is False:
        return "nil"
    if x is True:
        return "t"
    if isinstance(x, int) and not isinstance(x, bool):
        return str(x)
    if isinstance(x, str):
        if x.startswith(":"):
            return x
        return lisp_str(x)
    if isinstance(x, (list, tuple)):
        return "(" + " ".join(lisp_atom(i) for i in x) + ")"
    raise TypeError(x)


def write_block(fp, source: str, kind: str, rows: list, extra: str = "") -> None:
    fp.write(f"  (:source {lisp_str(source)} :kind :{kind}{extra}\n")
    fp.write("   :rows (\n")
    for row in rows:
        fp.write(f"     {lisp_atom(row)}\n")
    fp.write("   ))\n")


def gregorian_dates() -> list[date]:
    out: set[date] = set()
    anchors = (
        (1, 1, 1), (1, 12, 31), (4, 2, 29), (100, 2, 28), (400, 2, 29),
        (1582, 10, 15), (1600, 2, 29), (1752, 9, 14), (1900, 1, 1), (1900, 2, 28),
        (1970, 1, 1), (1999, 12, 31), (2000, 1, 1), (2000, 2, 29), (2024, 2, 29),
        (2015, 12, 31), (2016, 1, 1), (2016, 1, 4), (2020, 12, 31), (2021, 1, 1),
        (2024, 12, 31), (2025, 1, 1), (2026, 1, 1), (2027, 1, 1),
    )
    for y, m, d in anchors:
        out.add(date(y, m, d))

    dense_years = (
        *range(1899, 1902), *range(1969, 1972), *range(1999, 2002), *range(2023, 2028)
    )
    for year in dense_years:
        start = date(year, 1, 1)
        for n in range((date(year, 12, 31) - start).days + 1):
            out.add(start + timedelta(days=n))

    for year in range(1600, 2101):
        if year % 4 != 0 and year not in dense_years and year % 25 != 0:
            continue
        for month in range(1, 13):
            last = calendar.monthrange(year, month)[1]
            out.add(date(year, month, 1))
            out.add(date(year, month, last))
    return sorted(out)


def cpython_gregorian_rows() -> list[tuple]:
    rows = []
    for d in gregorian_dates():
        iso_y, iso_w, iso_wd = d.isocalendar()
        rows.append((d.year, d.month, d.day, d.toordinal(), iso_y, iso_w, iso_wd))
    return rows


def dateutil_easter_rows(method: int) -> list[tuple]:
    rows = []
    for year in range(1583, 2300):
        e = easter(year, method)
        rows.append((year, e.year, e.month, e.day))
    return rows


def pyluach_hebrew_rows() -> list[tuple]:
    """Named holidays; pyluach months are Nisan=1, Tishrei=7."""
    rows = []
    specs = (
        ("rosh-hashanah", 7, 1),
        ("yom-kippur", 7, 10),
        ("sukkot", 7, 15),
        ("passover", 1, 15),
        ("shavuot", 3, 6),
    )
    for hy in range(5600, 5901):
        for name, hm, hd in specs:
            g = hebrew_dates.HebrewDate(hy, hm, hd).to_greg()
            rows.append((hy, f":{name}", int(g.year), int(g.month), int(g.day)))
    return rows


def pyluach_hebrew_years() -> list[tuple]:
    rows = []
    for hy in range(5600, 5901):
        start = hebrew_dates.HebrewDate(hy, 7, 1)
        nxt = hebrew_dates.HebrewDate(hy + 1, 7, 1)
        length = (nxt.to_pydate() - start.to_pydate()).days
        try:
            hebrew_dates.HebrewDate(hy, 13, 1)
            leap_p = True
        except ValueError:
            leap_p = False
        rows.append((hy, length, leap_p))
    return rows


def islamic_to_jdn(year: int, month: int, day: int) -> int:
    """Kuwaiti / civil tabular (Friday epoch). Published JDN formula — not Umm al-Qura."""
    return (11 * year + 3) // 30 + 354 * year + 30 * month - (month - 1) // 2 + day + 1_948_055


def jdn_to_gregorian(jdn: int) -> tuple[int, int, int]:
    """Fliegel–Van Flandern JDN → proleptic Gregorian."""
    el = jdn + 68569
    n = (4 * el) // 146097
    el = el - (146097 * n + 3) // 4
    i = (4000 * (el + 1)) // 1461001
    el = el - (1461 * i) // 4 + 31
    j = (80 * el) // 2447
    day = el - (2447 * j) // 80
    el = j // 11
    month = j + 2 - 12 * el
    year = 100 * (n - 49) + i + el
    return year, month, day


def kuwaiti_islamic_rows() -> list[tuple]:
    rows = []
    for year, month, day in (
        (1, 1, 1), (1, 10, 1), (1, 12, 10),
        (100, 1, 1), (500, 1, 1), (1000, 1, 1), (1400, 1, 1),
    ):
        gy, gm, gd = jdn_to_gregorian(islamic_to_jdn(year, month, day))
        rows.append((year, month, day, gy, gm, gd))
    for year in range(1400, 1501):
        for month, day in ((1, 1), (1, 10), (3, 12), (10, 1), (12, 10)):
            gy, gm, gd = jdn_to_gregorian(islamic_to_jdn(year, month, day))
            rows.append((year, month, day, gy, gm, gd))
    return rows


def assert_published_anchors() -> None:
    assert date(1, 1, 1).toordinal() == 1
    assert date(1970, 1, 1).toordinal() == 719163
    assert easter(2024, EASTER_WESTERN) == date(2024, 3, 31)
    assert easter(2024, EASTER_ORTHODOX) == date(2024, 5, 5)
    assert easter(2025, EASTER_WESTERN) == date(2025, 4, 20)
    rh = hebrew_dates.HebrewDate(5785, 7, 1).to_greg()
    ps = hebrew_dates.HebrewDate(5785, 1, 15).to_greg()
    assert (rh.year, rh.month, rh.day) == (2024, 10, 3)
    assert (ps.year, ps.month, ps.day) == (2025, 4, 13)
    assert jdn_to_gregorian(islamic_to_jdn(1, 1, 1)) == (622, 7, 19)
    assert jdn_to_gregorian(islamic_to_jdn(1446, 1, 1)) == (2024, 7, 8)
    assert jdn_to_gregorian(islamic_to_jdn(1445, 1, 1)) == (2023, 7, 19)
    assert date(622, 7, 19).toordinal() + ORDINAL_TO_JDN == islamic_to_jdn(1, 1, 1)


def julian_static_rows() -> list[tuple]:
    """Published correspondences, not a library Julian.

    Julian 1582-10-04 was followed by Gregorian 1582-10-15.
    1900–2099: Julian Y-M-D is the same physical day as Gregorian Y-M-(D+13).
    """
    # Offset 12 until Julian 1900-02-29 (Gregorian skipped that leap day);
    # offset 13 from 1900-03-01 through 2099.
    rows = [
        (1582, 10, 4, date(1582, 10, 15).toordinal() - 1, ":reform"),
        (1900, 1, 1, date(1900, 1, 13).toordinal(), ":plus-12"),
        (1900, 3, 1, date(1900, 3, 14).toordinal(), ":plus-13"),
    ]
    for year in (1901, 1970, 2000, 2024, 2099):
        g = date(year, 1, 1) + timedelta(days=13)
        rows.append((year, 1, 1, g.toordinal(), ":plus-13"))
        if calendar.isleap(year):
            g2 = date(year, 2, 29) + timedelta(days=13)
            rows.append((year, 2, 29, g2.toordinal(), ":plus-13"))
        else:
            g2 = date(year, 3, 1) + timedelta(days=13)
            rows.append((year, 3, 1, g2.toordinal(), ":plus-13"))
    return rows


def parse_cao(blob: bytes) -> list[tuple]:
    text = blob.decode("cp932")
    rows = []
    for line in text.splitlines():
        if "," not in line or line.startswith("国民"):
            continue
        day, name = line.split(",", 1)
        name = name.strip()
        if name not in {"春分の日", "秋分の日"}:
            continue
        y, m, d = (int(p) for p in day.split("/"))
        season = ":spring" if name == "春分の日" else ":autumn"
        rows.append((y, season, y, m, d))
    return rows


def parse_hko_year(year: int, text: str) -> list[tuple]:
    seen: set[int] = set()
    month = 0
    leap = False
    day = 0
    festivals: dict[str, date] = {}
    for raw in text.splitlines():
        line = raw.strip()
        match = LINE_RE.match(line)
        if not match:
            continue
        gy, gm, gd = int(match[1]), int(match[2]), int(match[3])
        rest = match[4]
        weekday = None
        for wd in WEEKDAYS:
            idx = rest.find(wd)
            if idx != -1:
                weekday = wd
                lunar = rest[:idx].strip()
                term = rest[idx + len(wd) :].strip()
                break
        if weekday is None:
            continue
        month_match = MONTH_RE.match(lunar)
        if month_match:
            month = int(month_match[1])
            leap = month in seen or lunar.lower().startswith("leap")
            seen.add(month)
            day = 1
        elif lunar.isdigit():
            day = int(lunar)
        else:
            continue
        g = date(gy, gm, gd)
        if month == 1 and day == 1 and not leap:
            festivals["chinese-new-year"] = g
        if month == 5 and day == 5 and not leap:
            festivals["duanwu"] = g
        if month == 8 and day == 15 and not leap:
            festivals["zhongqiu"] = g
        if term == "Bright & Clear":
            festivals["qingming"] = g
    return [(year, f":{name}", g.year, g.month, g.day) for name, g in festivals.items()]


def download_hko(year: int, cache: Path) -> str:
    cached = cache / f"T{year}e.txt"
    if cached.exists():
        return cached.read_text(encoding="utf-8", errors="replace")
    text = fetch(HKO_URI.format(year=year)).decode("utf-8", errors="replace")
    cached.write_text(text, encoding="utf-8")
    return text


def hko_festival_rows() -> list[tuple]:
    cache = Path(tempfile.gettempdir()) / "hko-lunar-gold"
    cache.mkdir(parents=True, exist_ok=True)
    years = list(range(1901, 2101))
    rows: list[tuple] = []
    errors: list[str] = []

    def one(year: int) -> tuple[int, list[tuple]]:
        return year, parse_hko_year(year, download_hko(year, cache))

    with ThreadPoolExecutor(max_workers=12) as pool:
        futs = [pool.submit(one, y) for y in years]
        for fut in as_completed(futs):
            try:
                year, parsed = fut.result()
            except (urllib.error.URLError, TimeoutError, OSError) as exc:
                errors.append(str(exc))
                continue
            if "chinese-new-year" not in {p[1].lstrip(":") for p in parsed}:
                # row stores :chinese-new-year
                pass
            rows.extend(parsed)
    rows.sort()
    cny = [r for r in rows if r[1] == ":chinese-new-year"]
    if len(cny) < 180:
        raise SystemExit(f"HKO CNY rows too few: {len(cny)}; errors={errors[:5]}")
    # Known published HKO/紫金山 CNY dates.
    want = {(2024, 2, 10), (2025, 1, 29), (2026, 2, 17), (2020, 1, 25)}
    have = {(r[2], r[3], r[4]) for r in cny}
    missing = want - have
    if missing:
        raise SystemExit(f"HKO missing published CNY {missing}")
    return rows


def zoneinfo_offset_rows() -> list[tuple]:
    zones = (
        "UTC",
        "America/New_York",
        "America/Chicago",
        "America/Denver",
        "America/Los_Angeles",
        "Europe/London",
        "Europe/Paris",
        "Europe/Berlin",
        "Europe/Moscow",
        "Asia/Tokyo",
        "Asia/Shanghai",
        "Asia/Hong_Kong",
        "Asia/Kolkata",
        "Australia/Sydney",
        "Pacific/Auckland",
    )
    rows = []
    for zone_id in zones:
        zone = ZoneInfo(zone_id)
        for year in range(2015, 2028):
            for month in range(1, 13):
                utc = datetime(year, month, 15, 12, 0, tzinfo=timezone.utc)
                unix = int(utc.timestamp())
                local = utc.astimezone(zone)
                offset = int(local.utcoffset().total_seconds())
                dst = bool(local.dst() and local.dst().total_seconds())
                rows.append((zone_id, unix, offset, dst))
    # Known DST instants (US 2024).
    for y, m, d, h, kind in (
        (2024, 3, 10, 7, ":gap-utc"),   # 02:00 EST → 03:00 EDT
        (2024, 11, 3, 6, ":overlap-utc"),
    ):
        utc = datetime(y, m, d, h, 0, tzinfo=timezone.utc)
        zone = ZoneInfo("America/New_York")
        local = utc.astimezone(zone)
        rows.append(("America/New_York", int(utc.timestamp()),
                     int(local.utcoffset().total_seconds()),
                     bool(local.dst() and local.dst().total_seconds())))
        _ = kind
    return rows


def zoneinfo_local_rows() -> list[tuple]:
    """Local wall times that are unique / gap / overlap (python-dateutil fold)."""
    rows = []
    samples = (
        ("America/New_York", 2024, 1, 15, 12, 0, ":normal"),
        ("America/New_York", 2024, 7, 15, 12, 0, ":normal"),
        ("America/New_York", 2024, 3, 10, 2, 30, ":gap"),
        ("America/New_York", 2024, 11, 3, 1, 30, ":overlap"),
        ("Europe/London", 2024, 3, 31, 1, 30, ":gap"),
        ("Europe/London", 2024, 10, 27, 1, 30, ":overlap"),
        ("Europe/Paris", 2024, 3, 31, 2, 30, ":gap"),
        ("Australia/Sydney", 2024, 10, 6, 2, 30, ":gap"),
        ("Australia/Sydney", 2024, 4, 7, 2, 30, ":overlap"),
        ("Asia/Tokyo", 2024, 6, 15, 12, 0, ":normal"),
    )
    for zone_id, y, m, d, h, minute, kind in samples:
        zone = ZoneInfo(zone_id)
        earlier = datetime(y, m, d, h, minute, tzinfo=zone, fold=0)
        later = datetime(y, m, d, h, minute, tzinfo=zone, fold=1)
        e_off = int(earlier.utcoffset().total_seconds())
        l_off = int(later.utcoffset().total_seconds())
        rows.append((zone_id, y, m, d, h, minute, kind, e_off, l_off))
    return rows


def tzdata_version() -> str:
    try:
        import tzdata
        return getattr(tzdata, "IANA_VERSION", "unknown")
    except ImportError:
        return "system"


def main() -> int:
    assert_published_anchors()
    print("downloading 内閣府 CSV + HKO 1901–2100 …", file=sys.stderr)
    cao_rows = parse_cao(fetch(CAO_URI))
    if len(cao_rows) < 100:
        raise SystemExit(f"CAO equinox rows too few: {len(cao_rows)}")
    hko_rows = hko_festival_rows()
    greg_rows = cpython_gregorian_rows()
    west = dateutil_easter_rows(EASTER_WESTERN)
    orth = dateutil_easter_rows(EASTER_ORTHODOX)
    heb = pyluach_hebrew_rows()
    heb_years = pyluach_hebrew_years()
    isl = kuwaiti_islamic_rows()
    jul = julian_static_rows()
    tz_off = zoneinfo_offset_rows()
    tz_loc = zoneinfo_local_rows()

    OUT.parent.mkdir(parents=True, exist_ok=True)
    generated = date.today().isoformat()
    with OUT.open("w", encoding="utf-8") as fp:
        fp.write(";; Chrono gold — regenerate: uv run scripts/generate_chrono_gold.py\n")
        fp.write(";; Independent of datetime-protocol and of calendrica/R&D.\n")
        fp.write("(\n")
        fp.write(f" :generated {lisp_str(generated)}\n")
        fp.write(" :sources (\n")
        fp.write(f"  (:id {lisp_str('cpython')} :kind :stdlib :version {lisp_str(sys.version.split()[0])}"
                 f" :license {lisp_str('PSF')})\n")
        fp.write(f"  (:id {lisp_str('dateutil')} :kind :computational :note"
                 f" {lisp_str('Easter EASTER_WESTERN / EASTER_ORTHODOX')} :license {lisp_str('BSD')})\n")
        fp.write(f"  (:id {lisp_str('pyluach')} :kind :computational"
                 f" :note {lisp_str('arithmetic Hebrew; Nisan=1')} :license {lisp_str('MIT')})\n")
        fp.write(f"  (:id {lisp_str('kuwaiti-jdn')} :kind :published-formula"
                 f" :note {lisp_str('JDN=floor((11y+3)/30)+354y+30m-floor((m-1)/2)+d+1948055; Friday epoch')})\n")
        fp.write(f"  (:id {lisp_str('historical-julian')} :kind :published-table"
                 f" :note {lisp_str('1582-10-04+1d=1582-10-15 Gregorian; 1900-2099 Julian+13')})\n")
        fp.write(f"  (:id {lisp_str('cao-jp')} :kind :official :uri {lisp_str(CAO_URI)}"
                 f" :license {lisp_str('Japan-government-work')})\n")
        fp.write(f"  (:id {lisp_str('hko')} :kind :official :uri"
                 f" {lisp_str('https://www.hko.gov.hk/en/gts/time/conversion1_text.htm')}"
                 f" :years (1901 2100) :license {lisp_str('HKO')})\n")
        fp.write(f"  (:id {lisp_str('zoneinfo')} :kind :iana :version {lisp_str(tzdata_version())})\n")
        fp.write(" )\n")
        fp.write(" :blocks (\n")
        write_block(fp, "cpython", "gregorian", greg_rows)
        write_block(fp, "dateutil", "easter-western", west)
        write_block(fp, "dateutil", "easter-orthodox", orth)
        write_block(fp, "pyluach", "hebrew-holiday", heb)
        write_block(fp, "pyluach", "hebrew-year", heb_years)
        write_block(fp, "kuwaiti-jdn", "islamic-civil", isl)
        write_block(fp, "historical-julian", "julian", jul)
        write_block(fp, "cao-jp", "jp-equinox", cao_rows)
        write_block(fp, "hko", "chinese-festival", hko_rows)
        write_block(fp, "zoneinfo", "tz-offset", tz_off)
        write_block(fp, "zoneinfo", "tz-local", tz_loc)
        fp.write(" )\n)\n")

    print(f"wrote {OUT} greg={len(greg_rows)} easter={len(west)}+{len(orth)} "
          f"hebrew={len(heb)} islamic={len(isl)} cao={len(cao_rows)} hko={len(hko_rows)} "
          f"tz={len(tz_off)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
