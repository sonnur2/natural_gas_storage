"""Daily maximum temperature by country and heatwave flags.

Source: ECA&D blended daily maximum temperature (TX) station files,
data/raw_data/weather_max_temp/TX_STAIDxxxxxx.txt, same format as the TG files
(TX in 0.1 degC, -9999 missing, Q_TX 0 valid / 1 suspect / 9 missing).

Station per country: the representative station already used for mean temperature
(data/weather/country_representative_stations.csv), if it has TX data covering the recent
summers. The heatwave threshold needs 1961-1990, which many recent stations lack; then the
threshold comes from the nearest station with a full 1961-1990 record, shifted by the mean
summer difference between the two stations. Before the daily station starts, the series is
filled with that reference station (also shifted); see tx_source. Choices per country are
recorded in tmax_stations_used.csv (see choose()). Coverage is measured on June-August days,
because a gap in summer breaks heatwave runs.

Heatwave (definition chosen by s, Nov 2026): at least 3 consecutive days on which the
daily maximum temperature is
  - above the 98th percentile of all daily maxima at that station in 1961-1990, and
  - above 28 degC.
Only Q_TX == 0 values count; a missing day ends a run.

Output (data/weather/):
  tmax_stations_used.csv       station per country, threshold, coverage
  tmax_country_daily.csv.gz       country x day, 1961-2025: tx_c, hot_day, heatwave_day, ...
  heatwave_events.csv          one row per heatwave: start, end, length, peak tx
"""
import argparse
import glob
import os
import re

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW_TX = os.path.join(ROOT, "data", "raw_data", "weather_max_temp")
RAW_TG = os.path.join(ROOT, "data", "raw_data", "weather")
WEATHER = os.path.join(ROOT, "data", "weather")

REF = ("1961-01-01", "1990-12-31")
RECENT = ("2015-01-01", "2025-12-31")
START, END = "1961-01-01", "2025-12-31"
REF_MIN, RECENT_MIN = 0.90, 0.95   # share of valid June-August days required in each period
PCTL, ABS_MIN, MIN_RUN = 98, 28.0, 3
DEFAULT_COUNTRIES = ["DE", "IT", "NL", "FR", "AT", "ES"]


def dms(s):
    m = re.fullmatch(r"([+-])(\d+):(\d+):(\d+)", s.strip())
    if not m:
        return np.nan
    v = int(m[2]) + int(m[3]) / 60 + int(m[4]) / 3600
    return -v if m[1] == "-" else v


def read_stations():
    path = os.path.join(RAW_TX, "stations.txt")
    if not os.path.exists(path):
        path = os.path.join(RAW_TG, "stations.txt")
    rows = []
    with open(path, encoding="latin-1") as f:
        for line in f:
            p = [x.strip() for x in line.split(",")]
            if len(p) >= 6 and p[0].isdigit():
                rows.append((int(p[0]), p[1], p[2], dms(p[3]), dms(p[4])))
    st = pd.DataFrame(rows, columns=["station_id", "station_name", "country", "lat", "lon"])
    have = {int(re.search(r"(\d+)", os.path.basename(f))[1]) for f in glob.glob(os.path.join(RAW_TX, "TX_STAID*.txt"))}
    return st[st.station_id.isin(have)]


def read_tx(sid):
    fn = os.path.join(RAW_TX, f"TX_STAID{sid:06d}.txt")
    with open(fn, encoding="latin-1") as f:  # header length varies; data start after the 'STAID,' line
        lines = f.readlines()
    start = next(i for i, l in enumerate(lines) if l.strip().startswith("STAID"))
    d = pd.read_csv(fn, skiprows=start, skipinitialspace=True, encoding="latin-1")
    d.columns = [c.strip().upper() for c in d.columns]
    d = d.rename(columns={"TX": "tx", "Q_TX": "q_tx", "DATE": "date"})
    d = d[(d.tx != -9999) & (d.q_tx == 0)]
    d["date"] = pd.to_datetime(d.date.astype(str), format="%Y%m%d")
    d = d[(d.date >= START) & (d.date <= END)]
    return pd.Series(d.tx.values / 10.0, index=d.date, name="tx_c")


def coverage(s, a, b):
    """Share of June-August days with a valid value. Summer gaps would break heatwave runs."""
    days = pd.date_range(a, b, freq="D")
    jja = days[days.month.isin([6, 7, 8])]
    return s.index.isin(jja).sum() / len(jja)


def haversine(lat1, lon1, lat2, lon2):
    p = np.pi / 180
    a = np.sin((lat2 - lat1) * p / 2) ** 2 + np.cos(lat1 * p) * np.cos(lat2 * p) * np.sin((lon2 - lon1) * p / 2) ** 2
    return 12742 * np.arcsin(np.sqrt(a))


def choose(country, rep, st, cache):
    """Pick the daily station and the reference (1961-1990) station for one country.

    Daily station: the representative TG station if it has TX data with RECENT_MIN summer
    coverage in 2015-2025; otherwise the nearest station meeting both coverage rules.
    Reference station: the daily station itself if it covers 1961-1990; otherwise the nearest
    station that does, shifted by the mean June-August difference between the two over the
    days both report (so the threshold is expressed at the daily station's level).
    """
    def tx(sid):
        if sid not in cache:
            cache[sid] = read_tx(sid)
        return cache[sid]

    r = rep.loc[rep.country == country].iloc[0]
    cands = st[st.country == country].copy()
    cands["dist_km"] = haversine(r.lat, r.lon, cands.lat, cands.lon)
    cands = cands.sort_values("dist_km")
    daily = None
    if int(r.station_id) in set(cands.station_id) and coverage(tx(int(r.station_id)), *RECENT) >= RECENT_MIN:
        daily = cands[cands.station_id == r.station_id].iloc[0]
    else:
        for _, c in cands.iterrows():
            x = tx(int(c.station_id))
            if coverage(x, *RECENT) >= RECENT_MIN and coverage(x, *REF) >= REF_MIN:
                daily = c
                break
    if daily is None:
        return None
    ds = tx(int(daily.station_id))
    if coverage(ds, *REF) >= REF_MIN:
        return daily, daily, ds, ds, 0.0, int(r.station_id)
    cands["dist_daily_km"] = haversine(daily.lat, daily.lon, cands.lat, cands.lon)
    for _, c in cands.sort_values("dist_daily_km").iterrows():
        if c.station_id == daily.station_id:
            continue
        x = tx(int(c.station_id))
        if coverage(x, *REF) < REF_MIN:
            continue
        both = pd.concat([ds, x], axis=1, keys=["d", "r"], sort=True).dropna()
        both = both[both.index.month.isin([6, 7, 8])]
        if len(both) >= 270:  # at least about three summers of overlap
            shift = float((both.d - both.r).mean())
            return daily, c, ds, x, shift, int(r.station_id)
    return None


def flag_heatwaves(s, thr):
    df = s.asfreq("D").to_frame()
    df["hot_day"] = (df.tx_c > thr) & (df.tx_c > ABS_MIN)
    run_id = (df.hot_day != df.hot_day.shift()).cumsum()
    run_len = df.groupby(run_id).hot_day.transform("size")
    df["heatwave_day"] = df.hot_day & (run_len >= MIN_RUN)
    df["heatwave_onset"] = df.heatwave_day & ~df.heatwave_day.shift(fill_value=False)
    df["heatwave_id"] = df.heatwave_onset.cumsum().where(df.heatwave_day)
    return df


def main(countries):
    rep = pd.read_csv(os.path.join(WEATHER, "country_representative_stations.csv"))
    st = read_stations()
    used, daily, events = [], [], []
    cache = {}
    for c in countries:
        res = choose(c, rep, st, cache)
        if res is None:
            print(f"{c}: no usable TX station")
            continue
        drow, rrow, ds, rs, shift, rep_id = res
        thr = float(np.percentile(rs[REF[0]:REF[1]], PCTL)) + shift
        # full 1961-2025 series: daily station, and before it starts (or in its gaps) the
        # reference station shifted to the daily station's level
        series = ds.combine_first(rs + shift) if rrow.station_id != drow.station_id else ds
        source = pd.Series(np.where(series.index.isin(ds.index), "daily_station", "reference_station_shifted"),
                           index=series.index)
        df = flag_heatwaves(series.rename("tx_c"), thr)
        df["tx_source"] = source.reindex(df.index)
        df.index.name = "date"
        df = df.reset_index()
        df.insert(0, "country", c)
        df.insert(2, "station_id", int(drow.station_id))
        df["tx_p98_ref"] = round(thr, 2)
        daily.append(df)
        ev = df[df.heatwave_day].groupby("heatwave_id").agg(
            start=("date", "min"), end=("date", "max"), length_days=("date", "size"), peak_tx_c=("tx_c", "max"))
        ev.insert(0, "country", c)
        events.append(ev.reset_index(drop=True))
        used.append({"country": c, "station_id": int(drow.station_id), "station_name": drow.station_name.strip(),
                     "is_representative_tg_station": int(drow.station_id) == rep_id,
                     "dist_from_representative_km": round(float(drow.dist_km), 1),
                     "summer_coverage_2015_2025": round(coverage(ds, *RECENT), 3),
                     "reference_station_id": int(rrow.station_id), "reference_station_name": rrow.station_name.strip(),
                     "reference_summer_coverage_1961_1990": round(coverage(rs, *REF), 3),
                     "reference_shift_c": round(shift, 2), "tx_p98_1961_1990": round(thr, 2),
                     "n_heatwaves_1961_2025": len(ev),
                     "heatwave_days_1961_1990": int(df[df.date <= REF[1]].heatwave_day.sum()),
                     "heatwave_days_1996_2025": int(df[df.date >= "1996-01-01"].heatwave_day.sum())})
        print(f"{c}: daily {int(drow.station_id)} {drow.station_name.strip()} (rep={int(drow.station_id) == rep_id}); "
              f"reference {int(rrow.station_id)} {rrow.station_name.strip()} shift {shift:+.2f}C; "
              f"p98={thr:.1f}C; heatwaves={len(ev)}")
    pd.DataFrame(used).to_csv(os.path.join(WEATHER, "tmax_stations_used.csv"), index=False)
    pd.concat(daily).to_csv(os.path.join(WEATHER, "tmax_country_daily.csv.gz"), index=False, date_format="%Y-%m-%d")
    pd.concat(events).to_csv(os.path.join(WEATHER, "heatwave_events.csv"), index=False, date_format="%Y-%m-%d")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--countries", nargs="+", default=DEFAULT_COUNTRIES)
    main(ap.parse_args().countries)
