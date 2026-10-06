"""
Pulls DHIS2 analytics data using a JSON config file that specifies IDs and query options.
Saves a separate CSV per org unit level, with adaptive chunk sizes.
Re-run safe: skips periods already queried (whether or not they returned data).

Usage:
    python fetch_malnutrition.py --username X --password Y --config inputs/malnutrition_data_elements.json
    python fetch_malnutrition.py --username X --password Y --config inputs/malnutrition_data_elements.json --levels 3,4,5
    python fetch_malnutrition.py --username X --password Y --config inputs/malnutrition_data_elements.json --chunk-override 6

Config fields:
    type                  : string label appended to output filenames and queries folder
    ids_file              : path to a text file with one DHIS2 ID per line (# comments allowed)
    period_type           : e.g. "Monthly", "Yearly" (default: "Monthly")
    category_option_combo : true/false — disaggregate by category option combo (default: true)
    urban_rural           : true/false — include urban/rural dimension (default: true)
"""

import sys
import csv
import json
import time
import argparse
import requests
import urllib3
from pathlib import Path
from datetime import date
from requests.auth import HTTPBasicAuth

# ── Ethiopian → Gregorian conversion ─────────────────────────────────────────
# (gregorian_month, gregorian_day, year_offset) for each Ethiopian month 1–12.
# Months 1–4 fall in eth_year + 7; months 5–12 fall in eth_year + 8.
_ETH_GREG = {
    1:  (9,  11, 7),   2:  (10, 11, 7),   3:  (11, 10, 7),   4:  (12, 10, 7),
    5:  (1,   9, 8),   6:  (2,   8, 8),   7:  (3,  10, 8),   8:  (4,   9, 8),
    9:  (5,   9, 8),   10: (6,   8, 8),   11: (7,   8, 8),   12: (8,   7, 8),
}

def period_to_gregorian(period: str) -> str:
    """Return ISO Gregorian date string for a DHIS2 Ethiopian-calendar period code."""
    p = str(period).strip()
    try:
        if "Q" in p:
            gm, gd, off = _ETH_GREG[(int(p[5]) - 1) * 3 + 1]
            return date(int(p[:4]) + off, gm, gd).isoformat()
        if "S" in p:
            gm, gd, off = _ETH_GREG[1 if p[5] == "1" else 7]
            return date(int(p[:4]) + off, gm, gd).isoformat()
        if len(p) == 6 and p.isdigit():
            gm, gd, off = _ETH_GREG[int(p[4:])]
            return date(int(p[:4]) + off, gm, gd).isoformat()
        if len(p) == 4 and p.isdigit():
            return date(int(p) + 7, 9, 11).isoformat()
    except (ValueError, KeyError):
        pass
    return ""
from requests.adapters import HTTPAdapter

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

BASE = "http://dhis.moh.gov.et/api"

parser = argparse.ArgumentParser()
parser.add_argument("--username", "-u", required=True)
parser.add_argument("--password", "-p", required=True)
parser.add_argument("--config", "-c", required=True,
                    help="Path to JSON config file (e.g. inputs/malnutrition.json).")
parser.add_argument("--chunk-override", type=int, default=None,
                    help="Override chunk size for all levels (e.g. 3, 6, 12). "
                         "Increase on re-runs to fill gaps more efficiently.")
parser.add_argument("--levels", type=str, default=None,
                    help="Comma-separated list of org unit levels to fetch (e.g. 3,4,5). "
                         "Defaults to all levels.")
args = parser.parse_args()

# ── Load config ───────────────────────────────────────────────────────────────

cfg_path = Path(args.config)
if not cfg_path.exists():
    sys.exit(f"Config file not found: {cfg_path}")

with open(cfg_path, encoding="utf-8") as f:
    cfg = json.load(f)

cfg_type = cfg.get("type")
if not cfg_type:
    sys.exit("Config must include a 'type' field.")

ids_file = Path(cfg.get("ids_file", ""))
if not ids_file.exists():
    sys.exit(f"IDs file not found: {ids_file}")

with open(ids_file, encoding="utf-8") as f:
    SELECTED_IDS = [
        line.split("#")[0].strip()
        for line in f
        if line.split("#")[0].strip()
    ]

if not SELECTED_IDS:
    sys.exit(f"No IDs found in {ids_file}.")

PERIOD_TYPE = cfg.get("period_type", "Monthly")
USE_CO = cfg.get("category_option_combo", True)
USE_URBAN_RURAL = cfg.get("urban_rural", True)

selected_levels = set(int(lvl) for lvl in args.levels.split(",")) if args.levels else None

# ── Session ───────────────────────────────────────────────────────────────────

session = requests.Session()
session.auth = HTTPBasicAuth(args.username, args.password)
session.headers.update({"Accept": "application/json"})
session.verify = False

class SSLIgnoreAdapter(HTTPAdapter):
    def send(self, *args, **kwargs):
        kwargs["verify"] = False
        return super().send(*args, **kwargs)

session.mount("http://", SSLIgnoreAdapter())
session.mount("https://", SSLIgnoreAdapter())

# ── Constants ─────────────────────────────────────────────────────────────────

CHUNK_BY_LEVEL = {
    1: 12,
    2: 12,
    3: 6,
    4: 3,
    5: 1,
    6: 1,
}
DEFAULT_CHUNK = 3

MAX_RETRIES = 4
PAUSE_BETWEEN_CHUNKS = 3

URBAN_RURAL_DIM = "sfBbCQCcJBd"

# ── CSV columns (varies by config flags) ──────────────────────────────────────

def build_csv_cols(use_co, use_urban_rural):
    cols = ["data_element_id", "data_element_name"]
    if use_co:
        cols += ["category_option_combo_id", "category_option_combo_name"]
    cols += [
        "period", "period_type",
        "org_unit_level", "org_unit_level_name",
        "org_unit_id", "org_unit_name", "org_unit_hierarchy",
    ]
    if use_urban_rural:
        cols += ["urban_rural_id", "urban_rural"]
    cols += ["value", "gregorian_date"]
    return cols

CSV_COLS = build_csv_cols(USE_CO, USE_URBAN_RURAL)

# ── Helpers ───────────────────────────────────────────────────────────────────

def get(path, params=None):
    """GET with retry on transient SSL/connection errors."""
    for attempt in range(1, 4):
        try:
            r = session.get(f"{BASE}{path}", params=params, timeout=180)
            if r.status_code == 401:
                sys.exit("Authentication failed.")
            r.raise_for_status()
            return r.json()
        except Exception as e:
            if attempt == 3:
                sys.exit(f"Connection failed after 3 attempts: {e}")
            print(f"Connection attempt {attempt} failed, retrying in 10s...")
            time.sleep(10)

def fetch_chunk_with_retry(params, chunk_label):
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            r = session.get(f"{BASE}/analytics", params=params, timeout=300)
            r.raise_for_status()
            return r.json()
        except (requests.Timeout, requests.ConnectionError) as e:
            if attempt == MAX_RETRIES:
                print(f"    {chunk_label}: giving up after {MAX_RETRIES} attempts ({e})")
                return None
            wait = 15 * attempt
            print(f"    {chunk_label}: timeout (attempt {attempt}/{MAX_RETRIES}), retrying in {wait}s...")
            time.sleep(wait)
        except Exception as e:
            print(f"    {chunk_label}: error {e}")
            return None

def get_queried_periods(log_path):
    queried = set()
    if log_path.exists():
        with open(log_path, "r", encoding="utf-8") as f:
            for line in f:
                p = line.strip()
                if p:
                    queried.add(p)
        print(f"  Found queried-periods log with {len(queried)} periods — skipping those.")
    return queried

def log_queried_periods(log_path, periods):
    with open(log_path, "a", encoding="utf-8") as f:
        for p in periods:
            f.write(p + "\n")

def parse_row(row, use_co, use_urban_rural):
    """Unpack a DHIS2 analytics row depending on which dimensions were requested.

    DHIS2 always places co immediately after dx, then remaining dims in request order.
    Row layout:
      - With co + urban_rural : dx, co, pe, ou, urban_rural, value
      - With co only          : dx, co, pe, ou, value
      - With urban_rural only : dx, pe, ou, urban_rural, value
      - Neither               : dx, pe, ou, value
    """
    idx = 0
    dx_id = row[idx]
    idx += 1
    co_id = row[idx] if use_co else None
    idx += 1 if use_co else 0
    pe = row[idx]
    idx += 1
    ou_id = row[idx]
    idx += 1
    ur_id = row[idx] if use_urban_rural else None
    idx += 1 if use_urban_rural else 0
    value = row[idx]
    return dx_id, co_id, pe, ou_id, ur_id, value

def fetch_level(dx_ids, period_type, ou_level, ou_level_name, all_periods, csv_path, queries_dir):
    chunk_size = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(ou_level, DEFAULT_CHUNK)
    log_path = queries_dir / csv_path.with_suffix(".queried").name

    queried_periods = get_queried_periods(log_path)

    chunks = [all_periods[i:i+chunk_size] for i in range(0, len(all_periods), chunk_size)]
    chunks_to_run = [c for c in chunks if not all(p in queried_periods for p in c)]

    skipped = len(chunks) - len(chunks_to_run)
    print(f"  Chunk size: {chunk_size}  |  Total chunks: {len(chunks)}  |  "
          f"Skipping {skipped} already queried  |  Running {len(chunks_to_run)}")

    if not chunks_to_run:
        print(f"  Level {ou_level} already complete — nothing to do.")
        return 0

    write_header = not csv_path.exists()
    csv_file = open(csv_path, "a", newline="", encoding="utf-8")
    csv_writer = csv.DictWriter(csv_file, fieldnames=CSV_COLS)
    if write_header:
        csv_writer.writeheader()
    csv_file.flush()

    dx_str = ";".join(dx_ids)
    ou_str = f"LEVEL-{ou_level}"
    total_for_level = 0

    for i, chunk in enumerate(chunks_to_run, 1):
        pe_str = ";".join(chunk)
        dims = [f"dx:{dx_str}", f"pe:{pe_str}", f"ou:{ou_str}"]
        if USE_CO:
            dims.append("co")
        if USE_URBAN_RURAL:
            dims.append(URBAN_RURAL_DIM)

        params = {
            "dimension": dims,
            "skipMeta": "false",
            "showHierarchy": "true",
            "hierarchyMeta": "true",
            "displayProperty": "NAME",
            "paging": "false",
        }
        label = f"chunk {i}/{len(chunks_to_run)} ({chunk[0]}–{chunk[-1]})"
        data = fetch_chunk_with_retry(params, label)

        if data is None:
            continue

        raw = data.get("rows", [])
        meta = data.get("metaData", {}).get("items", {})
        hier = data.get("metaData", {}).get("ouHierarchy", {})

        for row in raw:
            dx_id, co_id, pe, ou_id, ur_id, value = parse_row(row, USE_CO, USE_URBAN_RURAL)
            record = {
                "data_element_id": dx_id,
                "data_element_name": meta.get(dx_id, {}).get("name", dx_id),
                "period": pe,
                "period_type": period_type,
                "org_unit_level": ou_level,
                "org_unit_level_name": ou_level_name,
                "org_unit_id": ou_id,
                "org_unit_name": meta.get(ou_id, {}).get("name", ou_id),
                "org_unit_hierarchy": hier.get(ou_id, ""),
                "value": value,
                "gregorian_date": period_to_gregorian(pe),
            }
            if USE_CO:
                record["category_option_combo_id"] = co_id
                record["category_option_combo_name"] = meta.get(co_id, {}).get("name", co_id)
            if USE_URBAN_RURAL:
                record["urban_rural_id"] = ur_id
                record["urban_rural"] = meta.get(ur_id, {}).get("name", ur_id)
            csv_writer.writerow(record)

        csv_file.flush()
        total_for_level += len(raw)
        log_queried_periods(log_path, chunk)

        print(f"    {label}: {len(raw)} rows  (level total: {total_for_level})")
        time.sleep(PAUSE_BETWEEN_CHUNKS)

    csv_file.close()
    return total_for_level

# ── Connect ───────────────────────────────────────────────────────────────────

print(f"\nConfig: {cfg_path}  |  Type: {cfg_type}")
print(f"IDs file: {ids_file}  |  {len(SELECTED_IDS)} IDs loaded")
print(f"Period type: {PERIOD_TYPE}  |  category_option_combo: {USE_CO}  |  urban_rural: {USE_URBAN_RURAL}\n")

print("Connecting...")
info = get("/system/info")
print(f"System: {info.get('systemName')}  v{info.get('version')}\n")

if args.chunk_override:
    print(f"Chunk override: {args.chunk_override} periods per chunk (all levels)\n")

print("IDs to fetch:")
for id_ in SELECTED_IDS:
    print(f"  {id_}")
print()

# ── Periods ───────────────────────────────────────────────────────────────────

# Ethiopian calendar starts ~Sep 11 each Gregorian year. DHIS2 Ethiopia stores
# data with Ethiopian calendar period codes, so we must generate Ethiopian periods.

ETH_MONTH_STARTS = [
    # (gregorian_month, gregorian_day) for start of each Ethiopian month 1–13
    (9, 11),  # 1  Meskerem
    (10, 11), # 2  Tikimt
    (11, 10), # 3  Hidar
    (12, 10), # 4  Tahsas
    (1,  9),  # 5  Tir
    (2,  8),  # 6  Yekatit
    (3, 10),  # 7  Megabit
    (4,  9),  # 8  Miyazya
    (5,  9),  # 9  Ginbot
    (6,  8),  # 10 Sene
    (7,  8),  # 11 Hamle
    (8,  7),  # 12 Nehase
    (9,  6),  # 13 Pagume
]

def gregorian_to_ethiopian(greg_date):
    """Return (eth_year, eth_month) for a Gregorian date."""
    m, d, y = greg_date.month, greg_date.day, greg_date.year
    # Sep 6–10 is Pagume (month 13) of the ending Ethiopian year
    if (m, d) >= (9, 11):
        eth_year = y - 7
        eth_month = 1
        for i, (sm, sd) in enumerate(ETH_MONTH_STARTS[:4], 1):
            if (m, d) >= (sm, sd):
                eth_month = i
    elif (m, d) >= (9, 6):  # Sep 6–10: Pagume of previous eth year
        eth_year = y - 8
        eth_month = 13
    else:  # Jan–Sep 5
        eth_year = y - 8
        eth_month = 4  # at minimum still Tahsas (month 4 started Dec 10 prior year)
        for i, (sm, sd) in enumerate(ETH_MONTH_STARTS[4:], 5):
            if (m, d) >= (sm, sd):
                eth_month = i
    return eth_year, eth_month

today = date.today()
eth_today_year, eth_today_month = gregorian_to_ethiopian(today)
start_year = 2005  # Ethiopian year 2005 EC

print(f"Ethiopian date: year={eth_today_year}, month={eth_today_month}  "
      f"(Gregorian: {today})\n")

def monthly_periods(sy, ey, em):
    out = []
    for y in range(sy, ey + 1):
        for m in range(1, 14):  # Ethiopian calendar has 13 months
            if y == ey and m > em:
                break
            out.append(f"{y}{m:02d}")
    return out

def quarterly_periods(sy, ey):
    out = []
    for y in range(sy, ey + 1):
        for q in range(1, 5):
            if y == ey and (q - 1) * 3 + 1 > eth_today_month:
                break
            out.append(f"{y}Q{q}")
    return out

def yearly_periods(sy, ey):
    return [str(y) for y in range(sy, ey + 1)]

def sixmonthly_periods(sy, ey):
    out = []
    for y in range(sy, ey + 1):
        for s in [1, 2]:
            if y == ey and s == 2 and eth_today_month < 7:
                break
            out.append(f"{y}S{s}")
    return out

def get_periods(period_type):
    pt = period_type.lower()
    ey = eth_today_year
    em = eth_today_month
    if "yearly" in pt or pt in ("financialjuly", "financialapr", "financialoct"):
        return yearly_periods(start_year, ey)
    elif "sixmonthly" in pt:
        return sixmonthly_periods(start_year, ey)
    elif "quarterly" in pt or "quarter" in pt:
        return quarterly_periods(start_year, ey)
    elif "weekly" in pt:
        return [f"{y}W01;{y}W53" for y in range(start_year, ey + 1)]
    else:
        return monthly_periods(start_year, ey, em)

# ── Org unit levels ───────────────────────────────────────────────────────────

print("Fetching organisation unit levels...")
ou_levels_resp = get("/organisationUnitLevels", {
    "fields": "id,name,level",
    "order": "level:asc",
    "paging": "false",
})
ou_levels = sorted(ou_levels_resp["organisationUnitLevels"], key=lambda x: x["level"])
if selected_levels:
    ou_levels = [lvl for lvl in ou_levels if lvl["level"] in selected_levels]
    print(f"Filtering to levels: {sorted(selected_levels)}")
for lvl in ou_levels:
    effective_chunk = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(lvl["level"], DEFAULT_CHUNK)
    print(f"  Level {lvl['level']}: {lvl['name']}  (chunk size: {effective_chunk})")
print()

# ── Pull data ─────────────────────────────────────────────────────────────────

grand_total = 0
queries_dir = Path(f"queries/{cfg_type}")
queries_dir.mkdir(parents=True, exist_ok=True)

all_periods = get_periods(PERIOD_TYPE)
print(f"\n══ Period type: {PERIOD_TYPE}  ({len(SELECTED_IDS)} IDs, {len(all_periods)} periods) ══")

for level_info in ou_levels:
    level = level_info["level"]
    level_name = level_info["name"]
    safe_name = level_name.replace(" ", "_")
    csv_path = Path(f"./{cfg_type}_level{level}_{safe_name}.csv")

    print(f"\n── Level {level} ({level_name}) → {csv_path.name} ──")
    n = fetch_level(SELECTED_IDS, PERIOD_TYPE, level, level_name, all_periods, csv_path, queries_dir)
    grand_total += n
    print(f"  Level {level} done: {n} new rows written")

print(f"\n\nAll done. Grand total rows written this run: {grand_total}")
