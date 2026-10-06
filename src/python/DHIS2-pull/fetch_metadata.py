"""
Pulls metadata (names, IDs, dataset info) for all GMP/malnutrition data elements
and indicators from the Ethiopian MoH DHIS2 instance.
Saves to malnutrition_metadata.csv — run this first to browse available variables
before deciding which subset to pull in fetch_malnutrition.py.

Usage:
    python fetch_metadata.py --username X --password Y
"""

import sys
import csv
import time
import argparse
import requests
import urllib3
from pathlib import Path
from requests.auth import HTTPBasicAuth
from requests.adapters import HTTPAdapter

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

BASE = "http://dhis.moh.gov.et/api"

parser = argparse.ArgumentParser()
parser.add_argument("--username", "-u", required=True)
parser.add_argument("--password", "-p", required=True)
args = parser.parse_args()

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

def get(path, params=None):
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

SEARCH_TERMS = [
    "malnutrition", "malnourish",
    "stunting", "stunted",
    "wasting", "wasted",
    "underweight",
    "acute malnutrition", "SAM", "MAM",
    "MUAC",
    "growth monitoring", "GMP",
    "maternal nutrition", "anaemia", "anemia",
    "low birth weight",
    "breastfeeding", "complementary feeding",
    "vitamin A", "micronutrient",
    "therapeutic feeding", "supplementary feeding",
]

print("\nConnecting...")
info = get("/system/info")
print(f"System: {info.get('systemName')}  v{info.get('version')}\n")
print("Fetching metadata for all search terms...")

rows = []
seen_de, seen_ind = set(), set()

for term in SEARCH_TERMS:
    print(f"  Searching: {term}")

    de_r = get("/dataElements", {
        "filter": f"name:ilike:{term}",
        "fields": "id,name,shortName,valueType,domainType,dataSets[name,periodType,openingDate]",
        "paging": "false",
    })
    for de in de_r.get("dataElements", []):
        if de["id"] not in seen_de:
            seen_de.add(de["id"])
            datasets = de.get("dataSets", [])
            rows.append({
                "type": "data_element",
                "id": de["id"],
                "name": de["name"],
                "short_name": de.get("shortName", ""),
                "value_type": de.get("valueType", ""),
                "domain_type": de.get("domainType", ""),
                "dataset_names": " | ".join(ds["name"] for ds in datasets),
                "period_types": " | ".join(ds.get("periodType", "") for ds in datasets),
                "opening_dates": " | ".join(ds.get("openingDate", "") for ds in datasets),
                "search_term": term,
            })

    ind_r = get("/indicators", {
        "filter": f"name:ilike:{term}",
        "fields": "id,name,shortName,indicatorType[name]",
        "paging": "false",
    })
    for ind in ind_r.get("indicators", []):
        if ind["id"] not in seen_ind:
            seen_ind.add(ind["id"])
            rows.append({
                "type": "indicator",
                "id": ind["id"],
                "name": ind["name"],
                "short_name": ind.get("shortName", ""),
                "value_type": ind.get("indicatorType", {}).get("name", ""),
                "domain_type": "",
                "dataset_names": "",
                "period_types": "",
                "opening_dates": "",
                "search_term": term,
            })

out_path = Path("./malnutrition_metadata.csv")
with open(out_path, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=[
        "type", "id", "name", "short_name", "value_type",
        "domain_type", "dataset_names", "period_types",
        "opening_dates", "search_term"
    ])
    writer.writeheader()
    writer.writerows(rows)

print(f"\nSaved {len(rows)} rows to {out_path}")
print(f"  Data elements: {sum(1 for r in rows if r['type'] == 'data_element')}")
print(f"  Indicators:    {sum(1 for r in rows if r['type'] == 'indicator')}")
print(f"\nOpen malnutrition_metadata.csv to browse variables.")
print(f"Copy IDs of interest into SELECTED_IDS in fetch_malnutrition.py.")
