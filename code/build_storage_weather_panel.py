"""Facility x day storage panel (2011-2025) merged with country and neighbor HDD stats.

Steps
  1. Storage: stack data/raw_data/storage_facility_panel/eu_panel_*.csv (AGSI+),
     drop exact duplicate facility-days, turn '-' into missing, and attach
     company/operational dates from europe_gas_storage_facilities.csv.
     A facility is identified by `url` (facility_eic/country/company_eic): the same
     facility_eic can appear under two operators or as pre-/post-Brexit series.
     Rows with status 'N' (no data reported) are dropped.
  2. Country weather: the daily series of each country's representative station
     (data/weather/country_representative_stations.csv), read straight from the raw
     ECA&D files so it covers 2010-12-01 to 2026-01-31 (the margins feed the
     past/next windows). Only valid values (Q_TG = 0) are used, as in
     build_country_weather.py, so 2018-2025 is identical to weather_hdd_country_daily.csv.
     HDD = max(14 - T, 0).
  3. Country HDD stats per day t (means over days with data; a window needs at
     least half its days, else missing):
       hdd, tg_c          day t
       hdd_week           ISO calendar week (Mon-Sun) containing t
       hdd_month          calendar month containing t
       hdd_past7/14       t-7..t-1 / t-14..t-1
       hdd_next7/14       t+1..t+7 / t+1..t+14
       hdd_ma7/30         trailing t-6..t / t-29..t
       hdd_norm           2011-2025 mean HDD for t's day of year (+-7 days)
       hdd_anom           hdd - hdd_norm; *_past14_anom, *_next14_anom likewise
       hdd_gasyear_cum    HDD summed from 1 Oct (gas-year start) to t, missing
                          days filled with hdd_norm; missing if more than
                          10% of the gas year's days so far are filled
  4. Neighbors: land borders in data/geo/country_neighbors.csv. nb_<stat> is the
     mean of <stat> over the neighbors that have it on day t; nb_n counts the
     neighbors with day-t HDD. Excluded from the averages: neighbors without a
     weather series (AD, LI, LT, MC, MD, SM, VA) and RU, whose representative
     station is in Siberia (77.9E). Not counted as borders: Gibraltar (ES-GB),
     Ceuta/Melilla (ES-MA), French Guiana.
  5. Merge on (weather country, gas day). GB* (post-Brexit series) uses GB weather.

Output
  data/storage/storage_panel_2011_2025.parquet          step 1
  data/weather/country_hdd_stats_daily.csv              steps 2-4, country x day 2011-2025
  data/geo/country_neighbors_used.csv                   neighbor list with use flags
  data/storage_weather_panel.parquet (+ .csv.gz)        the merged facility x day panel
"""
import os

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW = os.path.join(ROOT, "data", "raw_data")
STORAGE_RAW = os.path.join(RAW, "storage_facility_panel")
WEATHER = os.path.join(ROOT, "data", "weather")
GEO = os.path.join(ROOT, "data", "geo")

START, END = "2011-01-01", "2025-12-31"
READ_START, READ_END = 20101201, 20260131
HDD_BASE = 14.0
PANELS = ["eu_panel_2011_2018.csv", "eu_panel_2018_2019.csv", "eu_panel_2020_2025.csv"]
NUMERIC = ["gasInStorage", "injection", "withdrawal", "netWithdrawal", "workingGasVolume",
           "injectionCapacity", "withdrawalCapacity", "contractedCapacity",
           "availableCapacity", "trend", "full"]
MAX_IMPUTED = 0.10
NB_EXCLUDE = {"RU": "representative station in Siberia (77.9E)"}


def build_storage():
    df = pd.concat([pd.read_csv(os.path.join(STORAGE_RAW, f), dtype=str) for f in PANELS],
                   ignore_index=True)
    n0 = len(df)
    df = df.drop_duplicates(["url", "gasDayStart"], keep="last")
    print(f"storage rows: {n0:,}; duplicate facility-days dropped: {n0 - len(df):,}")
    n_nodata = (df["status"] == "N").sum()
    df = df[df["status"] != "N"].copy()
    print(f"status N (no data) dropped: {n_nodata:,}")

    for c in NUMERIC + ["longitude", "latitude"]:
        df[c] = pd.to_numeric(df[c].replace("-", np.nan), errors="coerce")
    df["date"] = pd.to_datetime(df["gasDayStart"])
    df = df[(df["date"] >= START) & (df["date"] <= END)]
    df["company_eic"] = df["url"].str.split("/").str[2]
    df["weather_country"] = df["country"].str.rstrip("*")

    fac = pd.read_csv(os.path.join(STORAGE_RAW, "europe_gas_storage_facilities.csv"), dtype=str)
    fac = fac.drop_duplicates(["facility_eic", "country", "company_eic"])
    df = df.merge(fac[["facility_eic", "country", "company_eic", "company_name",
                       "operational_start", "operational_end"]],
                  on=["facility_eic", "country", "company_eic"], how="left",
                  validate="many_to_one")

    df = df.rename(columns={"url": "facility_id", "type": "facility_type"})
    cols = ["facility_id", "facility_eic", "name", "company_eic", "company_name",
            "country", "weather_country", "facility_type", "longitude", "latitude",
            "operational_start", "operational_end", "date", "gasDayEnd", "status",
            "updatedAt"] + NUMERIC
    return df[cols].sort_values(["facility_id", "date"]).reset_index(drop=True)


def read_station(station_id):
    path = os.path.join(RAW, "weather", f"TG_STAID{station_id:06d}.txt")
    with open(path, encoding="latin-1") as f:
        lines = f.readlines()
    hdr = next(i for i, l in enumerate(lines) if l.startswith("STAID,"))
    recs = []
    for l in lines[hdr + 1:]:
        p = l.split(",")
        if len(p) != 5:
            continue
        d = int(p[2])
        if d < READ_START:
            continue
        if d > READ_END:
            break
        if int(p[4]) == 0:
            recs.append((d, int(p[3]) / 10.0))
    s = pd.DataFrame(recs, columns=["date", "tg_c"])
    s["date"] = pd.to_datetime(s["date"].astype(str), format="%Y%m%d")
    return s


def window_mean(s, n):
    return s.rolling(n, min_periods=(n + 1) // 2).mean()


def country_stats(g):
    """g: one country's daily series on a complete date index."""
    h = g["hdd"]
    out = pd.DataFrame({"tg_c": g["tg_c"], "hdd": h}, index=g.index)
    for n in (7, 14):
        out[f"hdd_past{n}"] = window_mean(h, n).shift(1)
        out[f"hdd_next{n}"] = window_mean(h, n).shift(-n)
    out["hdd_ma7"] = window_mean(h, 7)
    out["hdd_ma30"] = window_mean(h, 30)

    iso = g.index.isocalendar()
    for col, keys in (("hdd_week", [iso["year"], iso["week"]]),
                      ("hdd_month", [g.index.year, g.index.month])):
        grp = h.groupby(keys)
        size = grp.transform("size")
        out[col] = grp.transform("mean").where(grp.transform("count") >= (size + 1) // 2)

    # Climatology: mean HDD by day of year over 2011-2025, smoothed +-7 days (circular).
    base = h[START:END]
    doy = base.index.dayofyear.where(base.index.dayofyear < 366, 365)
    clim = base.groupby(doy).mean().reindex(range(1, 366))
    clim = pd.concat([clim.iloc[-7:], clim, clim.iloc[:7]]).rolling(15, center=True,
                                                                     min_periods=8).mean()
    clim = clim.iloc[7:-7]
    d = g.index.dayofyear.where(g.index.dayofyear < 366, 365)
    out["hdd_norm"] = clim.reindex(d).to_numpy()
    anom = h - out["hdd_norm"]
    out["hdd_anom"] = anom
    out["hdd_past14_anom"] = window_mean(anom, 14).shift(1)
    out["hdd_next14_anom"] = window_mean(anom, 14).shift(-14)

    gas_year = g.index.year - (g.index.month < 10)
    cum = h.fillna(out["hdd_norm"]).groupby(gas_year).cumsum()
    imputed = h.isna().groupby(gas_year).cumsum() / h.groupby(gas_year).cumcount().add(1)
    out["hdd_gasyear_cum"] = cum.where(imputed <= MAX_IMPUTED)
    return out


def build_weather():
    rep = pd.read_csv(os.path.join(WEATHER, "country_representative_stations.csv"))
    idx = pd.date_range(pd.Timestamp(str(READ_START)), pd.Timestamp(str(READ_END)), name="date")
    frames = []
    for cc, sid in zip(rep["country"], rep["station_id"]):
        s = read_station(int(sid)).set_index("date").reindex(idx)
        s["hdd"] = np.maximum(HDD_BASE - s["tg_c"], 0.0).round(1)
        st = country_stats(s)
        st.insert(0, "station_id", int(sid))
        st.insert(0, "country", cc)
        frames.append(st.reset_index())
    w = pd.concat(frames, ignore_index=True)

    # Check against the 2018-2025 country file built earlier.
    old = pd.read_csv(os.path.join(WEATHER, "weather_hdd_country_daily.csv"), parse_dates=["date"])
    chk = old.merge(w[["country", "date", "hdd"]], on=["country", "date"], how="left",
                    suffixes=("_old", ""))
    bad = (chk["hdd_old"] - chk["hdd"]).abs().gt(1e-9) | chk["hdd"].isna()
    print(f"check vs weather_hdd_country_daily.csv: {bad.sum()} mismatches in {len(chk):,} rows")
    return w


def add_neighbors(w, countries):
    nb = pd.read_csv(os.path.join(GEO, "country_neighbors.csv"), keep_default_na=False)
    have = set(w["country"])
    nb["has_weather"] = nb["neighbor"].isin(have)
    nb["used_in_avg"] = nb["has_weather"] & ~nb["neighbor"].isin(NB_EXCLUDE)
    nb["exclusion_reason"] = np.where(
        ~nb["has_weather"], "no weather series",
        nb["neighbor"].map(NB_EXCLUDE).fillna(""))
    nb.to_csv(os.path.join(GEO, "country_neighbors_used.csv"), index=False)

    missing = sorted(set(countries) - set(nb["country"]))
    if missing:
        print(f"WARNING: no neighbor list for {missing}")

    used = nb[nb["used_in_avg"]][["country", "neighbor"]]
    stats = [c for c in w.columns if c.startswith(("hdd", "tg_c"))]
    m = used.merge(w.drop(columns="station_id").rename(columns={"country": "neighbor"}),
                   on="neighbor")
    agg = m.groupby(["country", "date"])[stats].mean().add_prefix("nb_")
    agg["nb_n"] = m.groupby(["country", "date"])["hdd"].count()
    lists = (nb.groupby("country")
               .agg(neighbors=("neighbor", ";".join),
                    neighbors_used=("neighbor", lambda s: ";".join(
                        s[nb.loc[s.index, "used_in_avg"]])))
               .reset_index())
    lists["n_neighbors_used"] = lists["neighbors_used"].str.count(";") + (lists["neighbors_used"] != "")

    w = w[w["country"].isin(countries)]
    w = w.merge(agg.reset_index(), on=["country", "date"], how="left")
    w["nb_n"] = w["nb_n"].fillna(0).astype(int)
    w = w.merge(lists, on="country", how="left")
    return w


def main():
    sto = build_storage()
    countries = sorted(sto["weather_country"].unique())
    w = build_weather()
    w = add_neighbors(w, countries)
    w = w[(w["date"] >= START) & (w["date"] <= END)]
    num = w.select_dtypes("float").columns
    w[num] = w[num].round(4)
    w = w.sort_values(["country", "date"]).reset_index(drop=True)

    os.makedirs(os.path.join(ROOT, "data", "storage"), exist_ok=True)
    sto.to_parquet(os.path.join(ROOT, "data", "storage", "storage_panel_2011_2025.parquet"),
                   index=False)
    w.to_csv(os.path.join(WEATHER, "country_hdd_stats_daily.csv"), index=False,
             date_format="%Y-%m-%d")

    w = w.rename(columns={"country": "weather_country", "station_id": "weather_station_id"})
    out = sto.merge(w, on=["weather_country", "date"], how="left", validate="many_to_one")
    out.to_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"), index=False)
    out.to_csv(os.path.join(ROOT, "data", "storage_weather_panel.csv.gz"), index=False,
               date_format="%Y-%m-%d")

    print(f"panel: {len(out):,} rows, {out['facility_id'].nunique()} facilities, "
          f"{out['country'].nunique()} countries, {out['date'].min():%Y-%m-%d} to "
          f"{out['date'].max():%Y-%m-%d}")
    print("share of rows with missing own hdd by country:")
    print(out.groupby("weather_country")["hdd"].apply(lambda s: s.isna().mean()).round(3)
             .to_string())


if __name__ == "__main__":
    main()
