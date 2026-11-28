#!/usr/bin/env python3
"""Freeze 1 Farvardin of every Solar Hijri year 1206-1498 from the official
leap-year table into data/tests/persian-gold.sexp.

    uv run --no-project --with pypdf python -I scripts/generate_persian_gold.py

Primary source: the Calendar Center of the Institute of Geophysics, University
of Tehran (the body that computes Iran's official calendar), table
"سال‌های عادی و کبیسه هجری شمسی (از ۱۲۰۶ تا ۱۴۹۸)" (calendar.ut.ac.ir, which
answers scripts with a bot challenge, so the Wayback raw copy is read). Its
rows mark each leap year with * (four-year) or ** (five-year leap). Its date
column, headed "1 Farvardin", is not 1 Farvardin: in several years it
contradicts the table's own leap marks (1400 -> 20 March 2021, but 1399 is
starred and 1 Farvardin 1400 was Sunday 21 March 2021); it matches the date
of the equinox in Tehran time. So the dates here are chained from the leap
marks, anchored at 1 Farvardin 1379 = Monday 20 March 2000, the date the
Calendar Center's official holiday list for 1379 prints. The anchor is
checked against every official yearly list 1379-1404 below (the date of
their first holiday, 1 Farvardin, by its printed weekday).

This script never reads the Lisp, and does no astronomy.
"""

from __future__ import annotations

import io
import re
import sys
import time
import urllib.request
from datetime import date, timedelta
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data" / "tests" / "persian-gold.sexp"
CACHE = Path.home() / ".cache" / "datetime-protocol" / "persian"
UA = "Mozilla/5.0 (datetime-protocol persian gold)"
KABISE = ("https://web.archive.org/web/20250322111405id_/https://calendar.ut.ac.ir/documents/2139738/"
          "7092644/Kabise+Shamsi+1206-1498.pdf/fbc45bf4-df46-c298-381c-2bff2c57f0e0")
LIST_1379 = ("https://web.archive.org/web/2025/https://calendar.ut.ac.ir/documents/2139738/7092644/"
             "Holiday-1379.pdf")
ANCHOR = (1379, date(2000, 3, 20))
# 1 Farvardin as printed (weekday checked) in the official yearly holiday
# lists 1379-1404 and the 1405 calendar published by time.ir.
YEARLY = {
    1379: "2000-03-20", 1380: "2001-03-21", 1381: "2002-03-21", 1382: "2003-03-21", 1383: "2004-03-20",
    1384: "2005-03-21", 1385: "2006-03-21", 1386: "2007-03-21", 1387: "2008-03-20", 1388: "2009-03-21",
    1389: "2010-03-21", 1390: "2011-03-21", 1391: "2012-03-20", 1392: "2013-03-21", 1393: "2014-03-21",
    1394: "2015-03-21", 1395: "2016-03-20", 1396: "2017-03-21", 1397: "2018-03-21", 1398: "2019-03-21",
    1399: "2020-03-20", 1400: "2021-03-21", 1401: "2022-03-21", 1402: "2023-03-21", 1403: "2024-03-20",
    1404: "2025-03-21", 1405: "2026-03-21",
}


def fetch(url: str, tries: int = 8) -> bytes:
    delay = 5
    for attempt in range(tries):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=120) as resp:
                return resp.read()
        except Exception:  # noqa: BLE001 - retry, then give up
            if attempt == tries - 1:
                raise
            time.sleep(delay)
            delay = min(2 * delay, 120)
    raise AssertionError("unreachable")


def cached(name: str, url: str) -> bytes:
    path = CACHE / name
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(fetch(url))
    return path.read_bytes()


def leap_marks() -> dict[int, bool]:
    from pypdf import PdfReader
    reader = PdfReader(io.BytesIO(cached("Kabise-Shamsi-1206-1498.pdf", KABISE)))
    rows = {}
    for page in reader.pages:
        for line in page.extract_text().splitlines():
            m = re.match(r"^\s*(\d+)\S*\s+(\d{4})\s+(\d{4})(\**)", line)
            if m:
                rows[int(m.group(3))] = bool(m.group(4))
    missing = [y for y in range(1206, 1499) if y not in rows]
    assert not missing, missing
    return rows


def main() -> int:
    leap = leap_marks()
    farvardin = {ANCHOR[0]: ANCHOR[1]}
    for y in range(ANCHOR[0] + 1, 1499):
        farvardin[y] = farvardin[y - 1] + timedelta(days=366 if leap[y - 1] else 365)
    for y in range(ANCHOR[0] - 1, 1205, -1):
        farvardin[y] = farvardin[y + 1] - timedelta(days=366 if leap[y] else 365)
    for y, iso in YEARLY.items():
        assert str(farvardin[y]) == iso, (y, farvardin[y], iso)
    lines = [
        ";; 1 Farvardin of every Solar Hijri year 1206-1498, from the Calendar Center's",
        ";; official leap-year table (scripts/generate_persian_gold.py). Row: (SH-YEAR GY GM GD).",
        "(",
        " :sources",
        f" ((:title \"Calendar Center, Institute of Geophysics, University of Tehran: common and leap"
        f" Solar Hijri years 1206-1498\" :uri \"{KABISE}\")",
        f"  (:title \"Calendar Center: official holiday list 1379 (anchor, 1 Farvardin = Monday 2000-03-20);"
        f" the lists 1380-1404 and the 1405 calendar agree\" :uri \"{LIST_1379}\"))",
        f" :leap-years ({' '.join(str(y) for y in range(1206, 1499) if leap[y])})",
        " :rows",
        " (",
    ]
    lines += [f"  ({y} {d.year} {d.month} {d.day})" for y, d in sorted(farvardin.items())]
    lines.append(" ))")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"{len(farvardin)} rows -> {OUT.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
