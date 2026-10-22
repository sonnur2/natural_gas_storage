"""Build a daily station-level weather panel with heating degree days (HDD).

Source: ECA&D blended daily mean temperature (TG) series, one file per station
(data/raw_data/weather/TG_STAIDxxxxxx.txt) plus station metadata (stations.txt).

Output (data/weather/):
  weather_stations.csv   one row per station with data in the window
  weather_hdd_daily.parquet   station x day panel, 2015-01-01 to 2025-12-31
  weather_hdd_daily_csv/weather_hdd_daily_<CC>.csv.gz   same panel, one gzipped CSV per country

Cleaning:
  - TG is reported in 0.1 degC; converted to degC.
  - TG = -9999 or Q_TG = 9 -> missing; those rows are dropped.
  - Q_TG = 1 ('suspect') values are kept, with q_tg retained so they can be filtered.
  - HDD = max(14 - T, 0), T = daily mean temperature in degC.
"""
import argparse
import glob
import os
import re
from multiprocessing import Pool

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW = os.path.join(ROOT, "data", "raw_data", "weather")
OUT = os.path.join(ROOT, "data", "weather")

START, END = 20150101, 20251231
HDD_BASE = 14.0
MISSING = -9999


def dms_to_decimal(s):
    """'+56:52:00' -> 56.8667; sign applies to the whole value."""
    s = s.strip()
    m = re.fullmatch(r"([+-])(\d+):(\d+):(\d+)", s)
    if not m:
        return np.nan
    sign = -1.0 if m.group(1) == "-" else 1.0
    d, mi, se = (float(x) for x in m.groups()[1:])
    return sign * (d + mi / 60 + se / 3600)


def read_stations():
    rows = []
    with open(os.path.join(RAW, "stations.txt"), encoding="latin-1") as f:
        for line in f:
            parts = line.rstrip("\n").split(",")
            if len(parts) != 6 or not parts[0].strip().isdigit():
                continue
            rows.append({
                "station_id": int(parts[0]),
                "station_name": parts[1].strip(),
                "country": parts[2].strip(),
                "lat": dms_to_decimal(parts[3]),
                "lon": dms_to_decimal(parts[4]),
                "elevation_m": pd.to_numeric(parts[5].strip(), errors="coerce"),
            })
    st = pd.DataFrame(rows)
    st.loc[st["elevation_m"] == MISSING, "elevation_m"] = np.nan
    return st


def read_series(path):
    """Return the 2015-2025 rows of one station file as a DataFrame."""
    with open(path, encoding="latin-1") as f:
        lines = f.readlines()
    hdr = next(i for i, l in enumerate(lines) if l.startswith("STAID,"))
    recs = []
    for l in lines[hdr + 1:]:
        p = l.split(",")
        if len(p) != 5:
            continue
        date = int(p[2])
        if date < START:
            continue
        if date > END:
            break
        recs.append((int(p[0]), date, int(p[3]), int(p[4])))
    return pd.DataFrame(recs, columns=["station_id", "date", "tg_raw", "q_tg"])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--workers", type=int, default=os.cpu_count())
    args = ap.parse_args()

    files = sorted(glob.glob(os.path.join(RAW, "TG_STAID*.txt")))
    with Pool(args.workers) as pool:
        parts = pool.map(read_series, files, chunksize=50)
    df = pd.concat([p for p in parts if len(p)], ignore_index=True)
    n_raw = len(df)

    miss = (df["tg_raw"] == MISSING) | (df["q_tg"] == 9)
    print(f"rows in window: {n_raw:,}; missing (-9999 / Q=9): {miss.sum():,}; "
          f"suspect (Q=1): {(df['q_tg'] == 1).sum():,}")
    df = df[~miss].copy()

    df["tg_c"] = df["tg_raw"] / 10.0
    df["hdd"] = np.maximum(HDD_BASE - df["tg_c"], 0.0).round(1)
    df["date"] = pd.to_datetime(df["date"].astype(str), format="%Y%m%d")

    st = read_stations()
    df = df.merge(st, on="station_id", how="left", validate="many_to_one")
    unmatched = df["station_name"].isna().sum()
    if unmatched:
        print(f"WARNING: {unmatched:,} rows have no entry in stations.txt")

    cols = ["station_id", "station_name", "country", "lat", "lon", "elevation_m",
            "date", "tg_c", "q_tg", "hdd"]
    df = df[cols].sort_values(["station_id", "date"]).reset_index(drop=True)
    df["q_tg"] = df["q_tg"].astype("int8")

    os.makedirs(OUT, exist_ok=True)
    df.to_parquet(os.path.join(OUT, "weather_hdd_daily.parquet"), index=False)

    csv_dir = os.path.join(OUT, "weather_hdd_daily_csv")
    os.makedirs(csv_dir, exist_ok=True)
    for cc, g in df.groupby("country"):
        g.to_csv(os.path.join(csv_dir, f"weather_hdd_daily_{cc}.csv.gz"),
                 index=False, date_format="%Y-%m-%d")

    summ = (df.groupby("station_id")
              .agg(first_date=("date", "min"), last_date=("date", "max"),
                   n_days=("date", "size"), n_suspect=("q_tg", lambda q: (q == 1).sum()),
                   mean_tg_c=("tg_c", "mean"), mean_hdd=("hdd", "mean"))
              .reset_index())
    summ["coverage"] = (summ["n_days"] / ((pd.Timestamp("2025-12-31") - pd.Timestamp("2015-01-01")).days + 1)).round(3)
    summ = st.merge(summ, on="station_id", how="inner")
    summ.to_csv(os.path.join(OUT, "weather_stations.csv"), index=False, date_format="%Y-%m-%d")

    print(f"final rows: {len(df):,}; stations: {df['station_id'].nunique():,}; "
          f"countries: {df['country'].nunique()}")


if __name__ == "__main__":
    main()
