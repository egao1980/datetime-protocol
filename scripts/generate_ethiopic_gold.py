#!/usr/bin/env python3
"""Freeze 1 Meskerem (Enkutatash) of every Ethiopian (Ethiopic) year 1900-2100
EC into data/tests/ethiopic-gold.sexp.

    uv run --no-project --with convertdate==2.4.0 python -I scripts/generate_ethiopic_gold.py

The Ethiopian calendar is arithmetic: 12 months of 30 days and Pagume of 5
days, 6 in a year Y with Y mod 4 = 3 (the year before a Gregorian leap
year ends with Pagume 6). The dates are chained from that rule, anchored at
1 Meskerem 2016 EC = Tuesday 12 September 2023 and checked against 1
Meskerem 2017 EC = Wednesday 11 September 2024, the dates the Ethiopian
News Agency reported (allAfrica copies below). convertdate's Coptic
calendar (Coptic year = EC - 276, same months) is a secondary check: the
script stops on any difference. It never reads the Lisp.
"""

from __future__ import annotations

from datetime import date, timedelta
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "data" / "tests" / "ethiopic-gold.sexp"
ANCHOR = (2016, date(2023, 9, 12))
CHECK = (2017, date(2024, 9, 11))
SOURCES = [
    ("ENA via allAfrica, 13 Sep 2023: Ethiopians enter 2016 on September 12",
     "https://allafrica.com/stories/202309130446.html"),
    ("ENA via allAfrica, 13 Sep 2024: New Year 2017 E.C. received on Wednesday, September 11, 2024",
     "https://allafrica.com/stories/202409130234.html"),
    ("Reingold & Dershowitz, Calendrical Calculations, ch. 4 (Coptic and Ethiopic calendars): "
     "Pagume has 6 days when year mod 4 = 3", "https://www.cs.tau.ac.il/~nachum/calendar-book/third-edition/"),
]


def leap(year: int) -> bool:
    return year % 4 == 3


def new_years(first: int, last: int) -> dict[int, date]:
    out = {ANCHOR[0]: ANCHOR[1]}
    for y in range(ANCHOR[0], last):
        out[y + 1] = out[y] + timedelta(days=366 if leap(y) else 365)
    for y in range(ANCHOR[0], first, -1):
        out[y - 1] = out[y] - timedelta(days=366 if leap(y - 1) else 365)
    return {y: d for y, d in out.items() if first <= y <= last}


def main() -> None:
    rows = new_years(1900, 2100)
    assert rows[CHECK[0]] == CHECK[1], rows[CHECK[0]]
    from convertdate import coptic
    for y, d in rows.items():
        assert coptic.to_gregorian(y - 276, 1, 1) == (d.year, d.month, d.day), (y, d)
    lines = [";; 1 Meskerem of every Ethiopian year 1900-2100 EC (scripts/generate_ethiopic_gold.py).",
             ";; Row: (EC-YEAR GY GM GD).", "(", " :sources", " ("]
    lines += [f'  (:title "{t}" :uri "{u}")' for t, u in SOURCES]
    lines += [" )", " :rows", " ("]
    lines += [f"  ({y} {d.year} {d.month} {d.day})" for y, d in sorted(rows.items())]
    lines += [" ))"]
    OUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"{len(rows)} rows -> {OUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
