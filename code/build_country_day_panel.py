"""Country x day analysis panel: power system (ENTSO-E), weather, storage, gas prices.

Countries: DE, IT, NL, FR, AT, ES. Days: 2015-01-01 to 2025-12-31 (calendar days in
Central European time, Europe/Brussels, which all six countries share).

Inputs
  data/entsoe/entsoe_<dataset>.csv[.gz]   from code/entsoe_download.py (local only, git-ignored)
  data/raw_data/weather/TG_STAID*.txt          mean temperature of the representative station
  data/weather/country_representative_stations.csv
  data/weather/tmax_country_daily.csv.gz       max temperature + heatwave flags (build_tmax_heatwave.py)
  data/storage_weather_panel.parquet           AGSI+ facility x day panel
  data/raw_data/price/ttf/ttf_1month.csv, data/raw_data/price/acer_price.csv   gas prices

Output
  data/panel/country_day_panel.csv.gz   (about 24,000 rows, small enough to commit)
  data/panel/codebook.csv               one line per column

Conventions
  energy in GWh per day (MW x hours / 1000); peaks in MW
  CDD = max(T_mean - 21, 0); HDD = max(14 - T_mean, 0)
  heatwave: >= 3 days with T_max above the station's 1961-1990 98th percentile and above 28C
  storage fill = gas in storage / working gas volume (%)
  prices: DE and AT use the joint DE-AT-LU zone before 1 Oct 2018; IT uses the North zone
"""
import os

import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ENTSOE = os.path.join(ROOT, "data", "entsoe")
WEATHER = os.path.join(ROOT, "data", "weather")
OUT = os.path.join(ROOT, "data", "panel")
COUNTRIES = ["DE", "IT", "NL", "FR", "AT", "ES"]
START, END = "2015-01-01", "2025-12-31"
TZ = "Europe/Brussels"
STEP_H = {"PT15M": 0.25, "PT30M": 0.5, "PT60M": 1.0}
CHUNK = 2_000_000

FUEL = {"gas": "gas", "nuclear": "nuclear", "solar": "solar",
        "wind_onshore": "wind", "wind_offshore": "wind",
        "hydro_run_of_river": "hydro_ror", "hydro_reservoir": "hydro_res", "hydro_pumped": "hydro_pump",
        "lignite": "coal", "hard_coal": "coal", "coal_gas": "coal", "oil": "oil",
        "biomass": "other", "waste": "other", "geothermal": "other", "other_renewable": "other",
        "other": "other", "marine": "other", "peat": "other", "oil_shale": "other", "energy_storage": "other"}
PRICE_ZONE = {"NL": "NL", "FR": "FR", "ES": "ES", "IT": "IT_NORD"}


def entsoe_file(ds):
    for ext in (".csv", ".csv.gz"):
        f = os.path.join(ENTSOE, f"entsoe_{ds}{ext}")
        if os.path.exists(f):
            return f
    print(f"  missing ENTSO-E file for {ds}; its columns will be empty")
    return None


def read_chunks(ds):
    f = entsoe_file(ds)
    if f is None:
        return
    cols = ["datetime_utc", "area", "variable", "value", "resolution"]
    for ch in pd.read_csv(f, usecols=cols, chunksize=CHUNK):
        ch["datetime_utc"] = pd.to_datetime(ch.datetime_utc, utc=True)
        ch["date"] = ch.datetime_utc.dt.tz_convert(TZ).dt.tz_localize(None).dt.normalize()
        yield ch[(ch.date >= START) & (ch.date <= END)]


def finest(df, keys):
    """If a series comes at two resolutions on the same day, keep the finer one."""
    order = {"PT15M": 0, "PT30M": 1, "PT60M": 2}
    df = df.assign(_r=df.resolution.map(order))
    best = df.groupby(keys)._r.transform("min")
    return df[df._r == best].drop(columns="_r")


# ---------------------------------------------------------------- power
def generation():
    parts = []
    for ch in read_chunks("generation"):
        ch = ch[ch.area.isin(COUNTRIES) & ch.resolution.isin(STEP_H) & ~ch.variable.str.endswith("_consumption")]
        ch = ch.assign(fuel=ch.variable.map(FUEL).fillna("other"), gwh=ch.value * ch.resolution.map(STEP_H) / 1000,
                       hour=ch.datetime_utc.dt.tz_convert(TZ).dt.hour)
        parts.append(ch.groupby(["area", "date", "variable", "fuel", "resolution"]).agg(
            gwh=("gwh", "sum"), mw_max=("value", "max"), n=("value", "size")).reset_index())
        ev = ch[(ch.fuel == "gas") & ch.hour.between(18, 22)]
        parts.append(ev.groupby(["area", "date", "variable", "resolution"]).gwh.sum().reset_index().assign(fuel="gas_evening"))
    if not parts:
        return None
    g = pd.concat(parts).groupby(["area", "date", "variable", "fuel", "resolution"], as_index=False).agg(
        gwh=("gwh", "sum"), mw_max=("mw_max", "max"))
    g = finest(g, ["area", "date", "variable", "fuel"])
    wide = g.pivot_table(index=["area", "date"], columns="fuel", values="gwh", aggfunc="sum")
    wide.columns = [f"gen_{c}_gwh" for c in wide.columns]
    wide["gen_hydro_gwh"] = wide.filter(regex="gen_hydro_(ror|res|pump)_gwh").sum(axis=1, min_count=1)
    parts_total = [f"gen_{f}_gwh" for f in ["gas", "nuclear", "solar", "wind", "hydro", "coal", "oil", "other"]]
    wide["gen_total_gwh"] = wide[[c for c in parts_total if c in wide.columns]].sum(axis=1, min_count=1)
    wide["gas_share_pct"] = 100 * wide.gen_gas_gwh / wide.gen_total_gwh
    peak = g[g.fuel == "gas"].groupby(["area", "date"]).mw_max.max().rename("gen_gas_peak_mw")
    return wide.join(peak).rename_axis(["country", "date"])


def load():
    parts = []
    for ch in read_chunks("load"):
        ch = ch[ch.area.isin(COUNTRIES) & ch.resolution.isin(STEP_H)]
        ch = ch.assign(gwh=ch.value * ch.resolution.map(STEP_H) / 1000)
        parts.append(ch.groupby(["area", "date", "resolution"]).agg(gwh=("gwh", "sum"), mw=("value", "max")).reset_index())
    if not parts:
        return None
    l = pd.concat(parts).groupby(["area", "date", "resolution"], as_index=False).agg(gwh=("gwh", "sum"), mw=("mw", "max"))
    l = finest(l, ["area", "date"])
    return l.set_index(["area", "date"])[["gwh", "mw"]].rename(
        columns={"gwh": "load_gwh", "mw": "load_peak_mw"}).rename_axis(["country", "date"])


def prices():
    parts = []
    for ch in read_chunks("prices"):
        parts.append(ch.groupby(["area", "date", "resolution"]).value.agg(["mean", "max", "min"]).reset_index())
    if not parts:
        return None
    p = pd.concat(parts).groupby(["area", "date", "resolution"], as_index=False).agg(
        mean=("mean", "mean"), max=("max", "max"), min=("min", "min"))
    p = finest(p, ["area", "date"])
    rows = []
    for c in COUNTRIES:
        if c in ("DE", "AT"):
            z = p[((p.area == "DE_AT_LU") & (p.date < "2018-10-01")) |
                  ((p.area == ("DE_LU" if c == "DE" else "AT")) & (p.date >= "2018-10-01"))]
        else:
            z = p[p.area == PRICE_ZONE[c]]
        rows.append(z.assign(country=c))
    p = pd.concat(rows).set_index(["country", "date"])[["mean", "max", "min"]]
    return p.rename(columns=lambda s: f"power_price_{s}_eur_mwh")


def flows():
    parts = []
    for ch in read_chunks("flows"):
        ch = ch[ch.resolution.isin(STEP_H)]
        ch = ch.assign(gwh=ch.value * ch.resolution.map(STEP_H) / 1000)
        parts.append(ch.groupby(["area", "date"]).gwh.sum().reset_index())
    if not parts:
        return None
    f = pd.concat(parts).groupby(["area", "date"], as_index=False).gwh.sum()
    f[["src", "dst"]] = f.area.str.split("_", expand=True)
    out = f.groupby(["src", "date"]).gwh.sum().rename_axis(["country", "date"])
    inn = f.groupby(["dst", "date"]).gwh.sum().rename_axis(["country", "date"])
    net = out.sub(inn, fill_value=0).rename("net_export_listed_borders_gwh")
    return net[net.index.get_level_values(0).isin(COUNTRIES)].to_frame()


def hydro_reservoir():
    f = entsoe_file("hydro")
    if f is None:
        return None
    h = pd.read_csv(f, usecols=["datetime_utc", "area", "value"])
    h["date"] = pd.to_datetime(h.datetime_utc, utc=True).dt.tz_convert(TZ).dt.tz_localize(None).dt.normalize()
    h = h[h.area.isin(COUNTRIES)].groupby(["area", "date"]).value.last() / 1000  # MWh -> GWh
    days = pd.date_range(START, END, freq="D")
    out = []
    for c, s in h.groupby(level=0):  # weekly values carried forward to every day of the week
        s = s.droplevel(0).reindex(s.droplevel(0).index.union(days)).ffill(limit=6).reindex(days)
        out.append(pd.DataFrame({"country": c, "date": days, "hydro_reservoir_gwh": s.values}))
    return pd.concat(out).set_index(["country", "date"])


# ---------------------------------------------------------------- weather, storage, gas prices
def read_tg(sid):
    """Daily mean temperature of one ECA&D station straight from the raw TG file (valid days only)."""
    fn = os.path.join(ROOT, "data", "raw_data", "weather", f"TG_STAID{sid:06d}.txt")
    with open(fn, encoding="latin-1") as f:
        start = next(i for i, l in enumerate(f) if l.strip().startswith("STAID"))
    d = pd.read_csv(fn, skiprows=start, skipinitialspace=True, encoding="latin-1")
    d.columns = [c.strip().upper() for c in d.columns]
    d = d[(d.TG != -9999) & (d.Q_TG == 0)]
    return pd.Series(d.TG.values / 10.0, index=pd.to_datetime(d.DATE.astype(str), format="%Y%m%d"))


def weather():
    rep = pd.read_csv(os.path.join(WEATHER, "country_representative_stations.csv"))
    rows = []
    for c in COUNTRIES:  # same representative station as the existing HDD series, but for 2015-2025
        sid = int(rep.loc[rep.country == c, "station_id"].iloc[0])
        t = read_tg(sid)[START:END]
        rows.append(pd.DataFrame({"country": c, "date": t.index, "tg_station_id": sid, "t_mean_c": t.values}))
    w = pd.concat(rows)
    w["hdd"] = np.maximum(14 - w.t_mean_c, 0)
    w["cdd"] = np.maximum(w.t_mean_c - 21, 0)
    w = w.set_index(["country", "date"])
    f = os.path.join(WEATHER, "tmax_country_daily.csv.gz")
    if os.path.exists(f):
        x = pd.read_csv(f, parse_dates=["date"])
        x = x[x.country.isin(COUNTRIES)].rename(columns={"tx_c": "t_max_c", "station_id": "tx_station_id"})
        x = x.set_index(["country", "date"])[["tx_station_id", "t_max_c", "tx_p98_ref", "hot_day",
                                              "heatwave_day", "heatwave_onset", "heatwave_id"]]
        w = w.join(x, how="outer")
    else:
        print("  tmax_country_daily.csv.gz not found: run code/build_tmax_heatwave.py first")
    return w


def storage():
    s = pd.read_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"),
                        columns=["country", "date", "gasInStorage", "injection", "withdrawal",
                                 "workingGasVolume", "injectionCapacity", "withdrawalCapacity"])
    s = s[s.country.isin(COUNTRIES)].copy()
    s["date"] = pd.to_datetime(s.date)
    g = s.groupby(["country", "date"]).sum(min_count=1)
    g = g.rename(columns={"gasInStorage": "storage_twh", "injection": "injection_gwh", "withdrawal": "withdrawal_gwh",
                          "workingGasVolume": "wgv_twh", "injectionCapacity": "injection_cap_gwh",
                          "withdrawalCapacity": "withdrawal_cap_gwh"})
    g["fill_pct"] = 100 * g.storage_twh / g.wgv_twh
    g["injection_util_pct"] = 100 * g.injection_gwh / g.injection_cap_gwh
    g["net_injection_gwh"] = g.injection_gwh - g.withdrawal_gwh
    return g


def gas_prices():
    t = pd.read_csv(os.path.join(ROOT, "data", "raw_data", "price", "ttf", "ttf_1month.csv"), encoding="utf-8-sig")
    t = pd.Series(t.Price.astype(float).values, index=pd.to_datetime(t.Date, format="%m/%d/%Y")).sort_index()
    a = pd.read_csv(os.path.join(ROOT, "data", "raw_data", "price", "acer_price.csv"))
    a = pd.Series(a["NORTH-WEST EUROPE PRICE (EUR/MWh)"].astype(float).values, index=pd.to_datetime(a.DATE)).sort_index()
    p = pd.DataFrame({"ttf_front_month_eur_mwh": t, "acer_nwe_eur_mwh": a})
    p.index.name = "date"
    return p[START:END]


# ---------------------------------------------------------------- assemble
def main():
    os.makedirs(OUT, exist_ok=True)
    days = pd.date_range(START, END, freq="D")
    panel = pd.DataFrame(index=pd.MultiIndex.from_product([COUNTRIES, days], names=["country", "date"]))
    for name, fn in [("generation", generation), ("load", load), ("prices", prices), ("flows", flows),
                     ("hydro reservoirs", hydro_reservoir), ("weather", weather), ("storage", storage)]:
        print(f"building {name} ...")
        part = fn()
        if part is not None:
            panel = panel.join(part, how="left")
    panel = panel.reset_index()
    panel = panel.merge(gas_prices().reset_index(), on="date", how="left")
    panel["gas_price_eur_mwh"] = panel.ttf_front_month_eur_mwh.fillna(panel.acer_nwe_eur_mwh)
    for c in ["ttf_front_month_eur_mwh", "acer_nwe_eur_mwh", "gas_price_eur_mwh"]:  # weekends: last trading day
        panel[c] = panel.groupby("country")[c].ffill(limit=4)
    panel["year"], panel["month"] = panel.date.dt.year, panel.date.dt.month
    panel["dow"] = panel.date.dt.dayofweek
    panel["gas_summer"] = panel.month.between(4, 9)
    panel["gas_year"] = np.where(panel.month >= 10, panel.year, panel.year - 1)  # gas year starting 1 Oct
    panel.to_csv(os.path.join(OUT, "country_day_panel.csv.gz"), index=False, date_format="%Y-%m-%d")

    cb = pd.DataFrame({"column": panel.columns,
                       "non_missing_share": [round(panel[c].notna().mean(), 3) for c in panel.columns],
                       "first_date": [panel.loc[panel[c].notna(), "date"].min() for c in panel.columns],
                       "last_date": [panel.loc[panel[c].notna(), "date"].max() for c in panel.columns]})
    cb.to_csv(os.path.join(OUT, "codebook.csv"), index=False, date_format="%Y-%m-%d")
    print(f"\npanel: {len(panel):,} rows x {panel.shape[1]} columns -> {OUT}")
    print(cb.to_string(index=False))


if __name__ == "__main__":
    main()
