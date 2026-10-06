"""
Adds a gregorian_date column to all CSVs in data/GMP/data_elements/ and
data/GMP/indicators/. Period codes are Ethiopian calendar (e.g. "200511" =
Ethiopian year 2005 month 11 Hamle → Gregorian 2013-07-08). This script
converts them to the Gregorian start date of that Ethiopian period.

Usage:
    python add_gregorian_dates.py
    python add_gregorian_dates.py --data-dir ../../data/GMP
"""

import argparse
import pandas as pd
from pathlib import Path
from datetime import date

# For each Ethiopian month: (gregorian_month, gregorian_day, year_offset)
# Ethiopian months 1–4 (Meskerem–Tahsas) fall in Sep–Dec of eth_year + 7.
# Ethiopian months 5–12 (Tir–Nehase) fall in Jan–Aug of eth_year + 8.
ETH_MONTH_GREG_START = {
    1:  (9,  11, 7),   # Meskerem  → Sep 11
    2:  (10, 11, 7),   # Tikimt    → Oct 11
    3:  (11, 10, 7),   # Hidar     → Nov 10
    4:  (12, 10, 7),   # Tahsas    → Dec 10
    5:  (1,   9, 8),   # Tir       → Jan  9
    6:  (2,   8, 8),   # Yekatit   → Feb  8
    7:  (3,  10, 8),   # Megabit   → Mar 10
    8:  (4,   9, 8),   # Miyazya   → Apr  9
    9:  (5,   9, 8),   # Ginbot    → May  9
    10: (6,   8, 8),   # Sene      → Jun  8
    11: (7,   8, 8),   # Hamle     → Jul  8
    12: (8,   7, 8),   # Nehase    → Aug  7
}

def eth_to_gregorian(eth_year, eth_month):
    gm, gd, offset = ETH_MONTH_GREG_START[eth_month]
    return date(eth_year + offset, gm, gd)

def period_to_gregorian(period, period_type):
    p = str(period).strip()

    try:
        if "Q" in p:
            eth_year, q = int(p[:4]), int(p[5])
            return eth_to_gregorian(eth_year, (q - 1) * 3 + 1)

        if "S" in p:
            eth_year, s = int(p[:4]), int(p[5])
            return eth_to_gregorian(eth_year, 1 if s == 1 else 7)

        if len(p) == 6 and p.isdigit():
            return eth_to_gregorian(int(p[:4]), int(p[4:]))

        if len(p) == 4 and p.isdigit():
            # Yearly: Ethiopian New Year = Sep 11 of (eth_year + 7)
            return date(int(p) + 7, 9, 11)

    except (ValueError, KeyError):
        pass

    return None


parser = argparse.ArgumentParser()
parser.add_argument("--data-dir", default="../../../data/GMP",
                    help="Path to the GMP data directory (default: ../../data/GMP)")
args = parser.parse_args()

data_dir = Path(args.data_dir)
if not data_dir.exists():
    raise SystemExit(f"Data directory not found: {data_dir}")

total_files = 0
for subdir in ["data_elements", "indicators"]:
    for csv_path in sorted((data_dir / subdir).glob("*.csv")):
        df = pd.read_csv(csv_path, dtype=str)

        pt_col = df["period_type"] if "period_type" in df.columns else "Monthly"
        df["gregorian_date"] = [
            period_to_gregorian(p, pt)
            for p, pt in zip(df["period"], pt_col)
        ]

        df.to_csv(csv_path, index=False)
        print(f"  {csv_path.name}: {len(df)} rows")
        total_files += 1

print(f"\nDone. Updated {total_files} files.")
