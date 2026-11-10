"""Download ENTSO-E Transparency data for six countries, one month per request.

What it gets (2015-2026 by default):
  generation  actual generation per production type (A75), MW, 15/30/60 min
  prices      day-ahead prices (A44), EUR/MWh, hourly (15 min from Oct 2025)
  load        actual total load (A65), MW
  hydro       hydro reservoir filling (A72), MWh, weekly
  capacity    installed capacity per production type (A68), MW, yearly
  flows       cross-border physical flows (A11), MW, hourly

How it works:
  - one request per (dataset, area, month): small and well inside every API limit
  - each raw XML answer is saved under data/raw_data/entsoe/xml/...; a month that
    is already on disk is skipped, so you can stop and rerun at any time
  - failed requests are retried 5 times with growing waits; still-failing months
    are listed in data/raw_data/entsoe/failed.csv and retried on the next run
  - after downloading, all XML is parsed into tidy CSVs in data/entsoe/

Token: set the environment variable ENTSOE_TOKEN, or put the token alone in a
file called .entsoe_token in the repo root (it is git-ignored). Never commit it.

Usage (from the repo root):
  pip install requests pandas
  python code/entsoe_download.py                       # everything
  python code/entsoe_download.py --datasets generation --countries DE --start 2025-01 --end 2025-01
  python code/entsoe_download.py --parse-only          # rebuild CSVs from saved XML

In Jupyter: paste the file into a cell (or `%run code/entsoe_download.py`), then call
  run(["generation"], ["DE"], "2025-01", "2025-01", token="YOUR_TOKEN")
Files are written under ROOT; check print(ROOT) and set ROOT = "/path/to/repo" if needed.
"""
import argparse
import os
import sys
import time
import xml.etree.ElementTree as ET
from datetime import datetime, timedelta, timezone

import pandas as pd
import requests

try:  # run as a script: repo root is one level above code/
    ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
except NameError:  # pasted into Jupyter: use the notebook's folder, or set ROOT by hand
    ROOT = os.getcwd()
    if os.path.basename(ROOT) == "code":
        ROOT = os.path.dirname(ROOT)
RAW = os.path.join(ROOT, "data", "raw_data", "entsoe")
OUT = os.path.join(ROOT, "data", "entsoe")
URL = "https://web-api.tp.entsoe.eu/api"
PAUSE = 0.3  # seconds between requests (limit is 400 per minute)

# EIC codes (checked against entsoe-py 0.8.1 mappings)
COUNTRY = {  # control areas, used for generation, hydro and capacity
    "DE": "10Y1001A1001A83F", "IT": "10YIT-GRTN-----B", "NL": "10YNL----------L",
    "FR": "10YFR-RTE------C", "AT": "10YAT-APG------L", "ES": "10YES-REE------0",
}
LOAD_AREA = {**COUNTRY, "DE": "10Y1001A1001A82H"}  # German load is reported for the DE-LU zone
ZONES = {  # day-ahead price zones: (code, first month, last month) ; None = open
    "DE_AT_LU": ("10Y1001A1001A63L", None, "2018-09"),
    "DE_LU": ("10Y1001A1001A82H", "2018-10", None),
    "AT": ("10YAT-APG------L", "2018-10", None),
    "NL": ("10YNL----------L", None, None),
    "FR": ("10YFR-RTE------C", None, None),
    "ES": ("10YES-REE------0", None, None),
    "IT_NORD": ("10Y1001A1001A73I", None, None),
    "IT_CNOR": ("10Y1001A1001A70O", None, None),
    "IT_CSUD": ("10Y1001A1001A71M", None, None),
    "IT_SUD": ("10Y1001A1001A788", None, None),
    "IT_SICI": ("10Y1001A1001A75E", None, None),
    "IT_SARD": ("10Y1001A1001A74G", None, None),
    "IT_CALA": ("10Y1001C--00096J", "2021-01", None),
}
ZONE_COUNTRY = {"DE_AT_LU": "DE", "DE_LU": "DE"}
FLOWS = [  # (name, from, to); both directions are requested
    ("FR_ES", "10YFR-RTE------C", "10YES-REE------0"),
    ("FR_IT", "10YFR-RTE------C", "10Y1001A1001A73I"),
    ("FR_DE", "10YFR-RTE------C", "10Y1001A1001A82H"),
    ("DE_NL", "10Y1001A1001A82H", "10YNL----------L"),
    ("DE_AT", "10Y1001A1001A82H", "10YAT-APG------L"),
    ("AT_IT", "10YAT-APG------L", "10Y1001A1001A73I"),
]
PSR = {
    "B01": "biomass", "B02": "lignite", "B03": "coal_gas", "B04": "gas", "B05": "hard_coal",
    "B06": "oil", "B07": "oil_shale", "B08": "peat", "B09": "geothermal", "B10": "hydro_pumped",
    "B11": "hydro_run_of_river", "B12": "hydro_reservoir", "B13": "marine", "B14": "nuclear",
    "B15": "other_renewable", "B16": "solar", "B17": "waste", "B18": "wind_offshore",
    "B19": "wind_onshore", "B20": "other", "B25": "energy_storage",
}
ALL_DATASETS = ["generation", "prices", "load", "hydro", "capacity", "flows"]


# ---------------------------------------------------------------- requests
def months(start, end):
    y, m = map(int, start.split("-"))
    y1, m1 = map(int, end.split("-"))
    while (y, m) <= (y1, m1):
        yield f"{y:04d}-{m:02d}"
        y, m = (y + 1, 1) if m == 12 else (y, m + 1)


def month_bounds(ym):
    y, m = map(int, ym.split("-"))
    a = datetime(y, m, 1, tzinfo=timezone.utc)
    b = datetime(y + (m == 12), 1 if m == 12 else m + 1, 1, tzinfo=timezone.utc)
    return a.strftime("%Y%m%d%H%M"), b.strftime("%Y%m%d%H%M")


def in_window(ym, first, last):
    return (first is None or ym >= first) and (last is None or ym <= last)


def jobs(datasets, countries, start, end):
    """Yield (dataset, area_label, month, params)."""
    for ym in months(start, end):
        p0, p1 = month_bounds(ym)
        per = {"periodStart": p0, "periodEnd": p1}
        for ds in datasets:
            if ds == "generation":
                for c in countries:
                    yield ds, c, ym, {"documentType": "A75", "processType": "A16", "in_Domain": COUNTRY[c], **per}
            elif ds == "load":
                for c in countries:
                    yield ds, c, ym, {"documentType": "A65", "processType": "A16", "outBiddingZone_Domain": LOAD_AREA[c], **per}
            elif ds == "hydro":
                for c in countries:
                    if c != "NL":
                        yield ds, c, ym, {"documentType": "A72", "processType": "A16", "in_Domain": COUNTRY[c], **per}
            elif ds == "capacity" and ym.endswith("-01"):
                for c in countries:
                    yield ds, c, ym, {"documentType": "A68", "processType": "A33", "in_Domain": COUNTRY[c], **per}
            elif ds == "prices":
                for z, (code, first, last) in ZONES.items():
                    if ZONE_COUNTRY.get(z, z[:2]) in countries and in_window(ym, first, last):
                        yield ds, z, ym, {"documentType": "A44", "in_Domain": code, "out_Domain": code,
                                          "contract_MarketAgreement.type": "A01", **per}
            elif ds == "flows":
                for name, a, b in FLOWS:
                    x, y = name.split("_")
                    if x in countries or y in countries:
                        yield ds, name, ym, {"documentType": "A11", "out_Domain": a, "in_Domain": b, **per}
                        yield ds, f"{y}_{x}", ym, {"documentType": "A11", "out_Domain": b, "in_Domain": a, **per}


def raw_path(ds, area, ym):
    return os.path.join(RAW, "xml", ds, area, f"{ym}.xml")


def fetch(params, token, tries=5):
    """Return (status, text). status: 'ok', 'nodata' or 'error'."""
    wait = 5
    for k in range(tries):
        try:
            r = requests.get(URL, params={"securityToken": token, **params}, timeout=120)
            if r.status_code == 200:
                return "ok", r.text
            if "No matching data found" in r.text:
                return "nodata", r.text
            if r.status_code == 401:
                sys.exit("401 Unauthorized: check your token.")
            if r.status_code == 400:  # bad request: retrying will not help
                return "error", f"HTTP 400: {r.text[:300]}"
            msg = f"HTTP {r.status_code}: {r.text[:200]}"
        except requests.RequestException as e:
            msg = str(e)
        print(f"    retry {k + 1}/{tries} in {wait}s ({msg.strip()[:120]})")
        time.sleep(wait)
        wait *= 2
    return "error", msg


def download(datasets, countries, start, end, token):
    todo = [j for j in jobs(datasets, countries, start, end) if not os.path.exists(raw_path(j[0], j[1], j[2]))]
    print(f"{len(todo)} months to download")
    failed = []
    for i, (ds, area, ym, params) in enumerate(todo, 1):
        print(f"[{i}/{len(todo)}] {ds} {area} {ym}")
        status, text = fetch(params, token)
        if status == "error":
            failed.append({"dataset": ds, "area": area, "month": ym, "error": text[:300]})
        else:  # 'nodata' is saved too, so it is not asked again
            path = raw_path(ds, area, ym)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w", encoding="utf-8") as f:
                f.write(text)
        time.sleep(PAUSE)
    fpath = os.path.join(RAW, "failed.csv")
    if failed:
        pd.DataFrame(failed).to_csv(fpath, index=False)
        print(f"{len(failed)} months failed; listed in {fpath}. Rerun to retry them.")
    elif os.path.exists(fpath):
        os.remove(fpath)


# ---------------------------------------------------------------- parsing
def strip_ns(root):
    for el in root.iter():
        if "}" in el.tag:
            el.tag = el.tag.split("}", 1)[1]
    return root


RES = {"PT15M": 15, "PT30M": 30, "PT60M": 60, "P1D": 1440, "P7D": 10080, "P1Y": None}


def parse_xml(text, ds, area):
    """Return a list of rows: datetime_utc, area, dataset, variable, value, unit, resolution."""
    root = strip_ns(ET.fromstring(text))
    if root.tag.startswith("Acknowledgement"):
        return []
    rows = []
    for ts in root.iter("TimeSeries"):
        psr = ts.findtext("MktPSRType/psrType")
        if ds in ("generation", "capacity"):
            var = PSR.get(psr, psr or "total")
            # a series with outBiddingZone (and no inBiddingZone) is consumption, e.g. pumping
            if ts.find("outBiddingZone_Domain.mRID") is not None and ts.find("inBiddingZone_Domain.mRID") is None:
                var += "_consumption"
        elif ds == "prices":
            var = "price_day_ahead"
        elif ds == "load":
            var = "load"
        elif ds == "hydro":
            var = "hydro_reservoir_stored_energy"
        else:
            var = "flow"
        unit = ts.findtext("price_Measure_Unit.name") or ts.findtext("quantity_Measure_Unit.name") or ""
        curve = ts.findtext("curveType") or "A01"
        for per in ts.iter("Period"):
            start = pd.Timestamp(per.findtext("timeInterval/start"))
            end = pd.Timestamp(per.findtext("timeInterval/end"))
            res = per.findtext("resolution")
            step = RES.get(res)
            pts = {int(p.findtext("position")): float(p.findtext("price.amount") or p.findtext("quantity"))
                   for p in per.iter("Point")}
            if step is None:  # yearly values (installed capacity)
                rows += [(start, area, ds, var, v, unit, res) for v in pts.values()]
                continue
            n = int((end - start) / timedelta(minutes=step))
            last = None
            for pos in range(1, n + 1):
                if pos in pts:
                    last = pts[pos]
                elif curve != "A03":  # A01: a missing point is a real gap
                    continue
                # A03: a missing point repeats the previous value
                if last is not None:
                    rows.append((start + timedelta(minutes=step * (pos - 1)), area, ds, var, last, unit, res))
    return rows


def parse_all(datasets):
    os.makedirs(OUT, exist_ok=True)
    for ds in datasets:
        base = os.path.join(RAW, "xml", ds)
        if not os.path.isdir(base):
            continue
        frames = []
        for area in sorted(os.listdir(base)):
            rows = []
            for fn in sorted(os.listdir(os.path.join(base, area))):
                with open(os.path.join(base, area, fn), encoding="utf-8") as f:
                    rows += parse_xml(f.read(), ds, area)
            if rows:
                frames.append(pd.DataFrame(rows, columns=["datetime_utc", "area", "dataset", "variable", "value", "unit", "resolution"]))
        if not frames:
            continue
        df = pd.concat(frames, ignore_index=True)
        # the same timestamp can come in two monthly files only if a period overlaps; keep one
        df = df.drop_duplicates(subset=["datetime_utc", "area", "variable", "resolution"], keep="last")
        df = df.sort_values(["area", "variable", "datetime_utc"])
        path = os.path.join(OUT, f"entsoe_{ds}.csv.gz")
        df.to_csv(path, index=False)
        print(f"{ds}: {len(df):,} rows -> {path}")


# ---------------------------------------------------------------- main
def read_token(token=None):
    if token in (None, "", "YOUR_TOKEN"):  # fall back to an ENTSOE_TOKEN variable set in the notebook
        token = globals().get("ENTSOE_TOKEN")
    tok = token or os.environ.get("ENTSOE_TOKEN")
    path = os.path.join(ROOT, ".entsoe_token")
    if not tok and os.path.exists(path):
        tok = open(path).read().strip()
    if not tok:
        sys.exit("No token: set ENTSOE_TOKEN or create .entsoe_token in the repo root.")
    return tok


def run(datasets=ALL_DATASETS, countries=list(COUNTRY), start="2015-01", end=None, token=None, parse_only=False):
    """Notebook entry point, e.g. run(["generation"], ["DE"], "2025-01", "2025-01", token="...")."""
    end = end or datetime.now(timezone.utc).strftime("%Y-%m")
    if not parse_only:
        download(datasets, countries, start, end, read_token(token))
    parse_all(datasets)


if __name__ == "__main__" and "ipykernel" not in sys.modules:
    ap = argparse.ArgumentParser()
    ap.add_argument("--datasets", nargs="+", default=ALL_DATASETS, choices=ALL_DATASETS)
    ap.add_argument("--countries", nargs="+", default=list(COUNTRY), choices=list(COUNTRY))
    ap.add_argument("--start", default="2015-01", help="first month, YYYY-MM")
    ap.add_argument("--end", default=datetime.now(timezone.utc).strftime("%Y-%m"), help="last month, YYYY-MM")
    ap.add_argument("--parse-only", action="store_true")
    a = ap.parse_args()
    if not a.parse_only:
        download(a.datasets, a.countries, a.start, a.end, read_token())
    parse_all(a.datasets)
