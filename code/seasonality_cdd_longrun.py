"""Long-run cooling (CDD) and heating (HDD) degree days by country, 1980-2025.

Source: ECA&D blended TG station files (data/raw_data/weather), full history.
Station rule: Q_TG == 0 only; keep stations with >= 90% valid days in 1980-2025
and >= 85% in every 5-year block (so composition is stable over time).
Country series = simple mean of daily T across kept stations, then degree days.

Conventions:
  HDD = max(14 - T, 0)            (project convention)
  CDD = max(T - 21, 0)            (proposed main convention)
  CDD_eurostat = T - 21 if T >= 24 else 0   (robustness, Eurostat definition)
Output: data/seasonality/cdd_hdd_country_year.csv
"""
import os, re
from multiprocessing import Pool
import numpy as np, pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW = os.path.join(ROOT, "data", "raw_data", "weather")
OUT = os.path.join(ROOT, "data", "seasonality"); os.makedirs(OUT, exist_ok=True)
COUNTRIES = ["ES", "IT", "FR", "DE", "PT", "GR", "NL", "AT", "HU", "PL", "BE", "HR", "RO"]
Y0, Y1 = 1980, 2025

def stations():
    rows = []
    with open(os.path.join(RAW, "stations.txt"), encoding="latin-1") as f:
        for line in f:
            p = [x.strip() for x in line.split(",")]
            if len(p) >= 6 and p[0].isdigit():
                rows.append((int(p[0]), p[2]))
    return pd.DataFrame(rows, columns=["staid", "cn"])

def load(staid):
    fn = os.path.join(RAW, f"TG_STAID{staid:06d}.txt")
    try:
        d = pd.read_csv(fn, skiprows=20, skipinitialspace=True, usecols=[2, 3, 4], names=["date", "tg", "q"], header=0)
    except Exception:
        return None
    d = d[(d.date >= Y0 * 10000) & (d.date <= Y1 * 10000 + 1231) & (d.tg != -9999) & (d.q == 0)]
    n_total = (pd.Timestamp(f"{Y1}-12-31") - pd.Timestamp(f"{Y0}-01-01")).days + 1
    if len(d) < 0.9 * n_total:
        return None
    d["date"] = pd.to_datetime(d.date.astype(str))
    blk = (d.date.dt.year - Y0) // 5
    if (blk.value_counts().reindex(range((Y1 - Y0) // 5 + 1), fill_value=0) < 0.85 * 365 * 5).iloc[:-1].any():
        return None
    return pd.Series(d.tg.values / 10.0, index=d.date, name=staid)

if __name__ == "__main__":
    st = stations(); st = st[st.cn.isin(COUNTRIES)]
    out, nst = [], {}
    for cn, g in st.groupby("cn"):
        with Pool(8) as p:
            ser = [s for s in p.map(load, g.staid.tolist()) if s is not None]
        nst[cn] = len(ser)
        if not ser:
            continue
        T = pd.concat(ser, axis=1).mean(axis=1)
        df = pd.DataFrame({"T": T})
        df["hdd"] = np.maximum(14 - df["T"], 0)
        df["cdd"] = np.maximum(df["T"] - 21, 0)
        df["cdd_eurostat"] = np.where(df["T"] >= 24, df["T"] - 21, 0)
        df["year"] = df.index.year; df["month"] = df.index.month
        summer = df[df.month.between(4, 9)].groupby("year")[["cdd", "cdd_eurostat", "T"]].agg(
            {"cdd": "sum", "cdd_eurostat": "sum", "T": "mean"}).rename(columns={"T": "t_summer"})
        jja = df[df.month.between(6, 8)].groupby("year")["T"].mean().rename("t_jja")
        hot = df[df.month.between(4, 9)].groupby("year")["T"].apply(lambda x: (x >= 24).sum()).rename("days_ge24")
        hdd = df.groupby("year")["hdd"].sum().rename("hdd_cal")
        r = pd.concat([summer, jja, hot, hdd], axis=1).reset_index().rename(columns={"index": "year"})
        r["country"] = cn; r["n_stations"] = len(ser)
        out.append(r)
        print(cn, len(ser), flush=True)
    pd.concat(out).to_csv(os.path.join(OUT, "cdd_hdd_country_year.csv"), index=False)
