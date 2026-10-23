"""Data overview: the AGSI+ storage facility panel, for the talk's data section.

Input   data/storage/storage_panel_2011_2025.parquet (facility x gas day, AGSI+)
Sample  EU = EU-27 facilities (drops UA, which AGSI+ reports as one aggregate, and
        GB/GB*). Flow and fill aggregates start 2016-04-01, when most German sites
        enter the panel. Capacity snapshot = gas day 2025-12-31.
Demand  EU-27 gross inland consumption 2025 = 13,093,256 TJ (GCV), Eurostat
        nrg_cb_gas / Statistics Explained "Natural gas supply statistics"
        (3,637 TWh; 1 TWh = 3,600 TJ). AGSI+ volumes are GCV too.
Known gap  UGS Bergermeer and UGS Grijpskerk (NL, ~73 TWh together) are in the raw
        files only up to 2019-12-31 (as is VGS Sediane B, FR, 13 TWh), so EU
        capacity drops by about 90 TWh in Jan 2020.

Output  output/data_overview/
  summary.csv               headline numbers quoted on the slide
  tab_country.csv           capacity, facilities, inj/wd capacity by country
  tab_type.csv              capacity and facilities by storage type
  tab_gas_year.csv          EU injections, withdrawals, min/max fill by gas year
  fig1_capacity_country.png working gas capacity by country, end 2025
  fig2_capacity_type.png    capacity and facility count by storage type
  fig3_flows.png            EU daily injections and withdrawals, 2016-2025
  fig4_fill_eu.png          EU stock (TWh) and capacity over time
  fig5_fill_gasyear.png     EU fill % by day of the gas year, one line per year
  fig6_fill_top5.png        fill % in the five largest EU storage countries
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "data_overview")
os.makedirs(OUT, exist_ok=True)

SNAP = "2025-12-31"
FLOW_START = "2016-04-01"
EU_DEMAND_TWH = 13_093_256 / 3_600
NON_EU = ["UA", "GB", "GB*"]
TYPES = {"DSR": "Depleted field", "ASF": "Salt cavern", "ASR": "Aquifer",
         "GRP": "Group of sites", "SRC": "Rock cavern"}

BLUE, ORANGE, AQUA, YELLOW, MAGENTA = "#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"
GRAY, LIGHT = "#8a8984", "#d6d5d0"
INK, INK2 = "#0b0b0b", "#52514e"
plt.rcParams.update({
    "figure.dpi": 150, "savefig.dpi": 200, "font.size": 12, "axes.titlesize": 14,
    "axes.titleweight": "bold", "axes.titlelocation": "left", "axes.labelcolor": INK2,
    "axes.edgecolor": "#c9c8c2", "axes.spines.top": False, "axes.spines.right": False,
    "xtick.color": INK2, "ytick.color": INK2, "axes.grid": True, "grid.color": "#ebeae6",
    "grid.linewidth": 0.8, "axes.axisbelow": True, "legend.frameon": False, "lines.linewidth": 2,
})


def titles(ax, title, sub):
    ax.set_title(title, pad=24)
    ax.text(0, 1.02, sub, transform=ax.transAxes, color=INK2, fontsize=10, va="bottom")


def load():
    d = pd.read_parquet(os.path.join(ROOT, "data", "storage", "storage_panel_2011_2025.parquet"))
    d["type"] = d.facility_type.map(TYPES).fillna("Not reported")
    d["eu"] = ~d.country.isin(NON_EU)
    d["gas_year"] = d.date.dt.year - (d.date.dt.month < 10)
    return d


def eu_daily(d):
    e = d[d.eu].groupby("date").agg(
        gis=("gasInStorage", "sum"), wgv=("workingGasVolume", "sum"),
        inj=("injection", "sum"), wd=("withdrawal", "sum"), n=("facility_id", "nunique"))
    e["fill"] = 100 * e.gis / e.wgv
    return e


def tables(d, e):
    s = d[d.date == SNAP]
    by_c = s.groupby("country").agg(
        facilities=("facility_id", "nunique"), wgv_twh=("workingGasVolume", "sum"),
        inj_cap_gwhd=("injectionCapacity", "sum"), wd_cap_gwhd=("withdrawalCapacity", "sum"),
        fill_pct=("full", "mean")).sort_values("wgv_twh", ascending=False)
    by_c["fill_pct"] = 100 * s.groupby("country").gasInStorage.sum() / by_c.wgv_twh
    by_c.round(1).to_csv(os.path.join(OUT, "tab_country.csv"))

    se = s[s.eu]
    by_t = se.groupby("type").agg(facilities=("facility_id", "nunique"),
                                  wgv_twh=("workingGasVolume", "sum"),
                                  wd_cap_gwhd=("withdrawalCapacity", "sum"))
    by_t["share_wgv"] = 100 * by_t.wgv_twh / by_t.wgv_twh.sum()
    by_t["days_to_empty"] = 1000 * by_t.wgv_twh / by_t.wd_cap_gwhd
    by_t = by_t.sort_values("wgv_twh", ascending=False)
    by_t.round(1).to_csv(os.path.join(OUT, "tab_type.csv"))

    f = e[(e.index >= "2016-10-01") & (e.index < "2025-10-01")].copy()
    f["gas_year"] = f.index.year - (f.index.month < 10)
    gy = f.groupby("gas_year").agg(inj_twh=("inj", "sum"), wd_twh=("wd", "sum"),
                                   fill_min=("fill", "min"), fill_max=("fill", "max"),
                                   wgv_twh=("wgv", "mean"))
    gy[["inj_twh", "wd_twh"]] /= 1000
    gy["wd_share_demand"] = 100 * gy.wd_twh / EU_DEMAND_TWH
    gy.round(1).to_csv(os.path.join(OUT, "tab_gas_year.csv"))

    eu_c = by_c.drop(NON_EU, errors="ignore")
    eu_wgv = eu_c.wgv_twh.sum()
    big = se.groupby("facility_id").workingGasVolume.sum().sort_values(ascending=False)
    summ = {
        "facility_ids_ever": d.facility_id.nunique(),
        "countries_ever_incl_uk_ua": d.country.replace({"GB*": "GB"}).nunique(),
        "eu_countries_ever": d[d.eu].country.nunique(),
        "facilities_reporting_snap_all": s.facility_id.nunique(),
        "facilities_reporting_snap_eu": se.facility_id.nunique(),
        "eu_countries_snap": se.country.nunique(),
        "wgv_twh_snap_all": by_c.wgv_twh.sum(),
        "wgv_twh_snap_eu": eu_wgv,
        "wgv_twh_snap_eu_plus_nl_gap": eu_wgv + 45.65 + 27.67,
        "wgv_twh_snap_ua": by_c.wgv_twh.get("UA", np.nan),
        "eu_demand_2025_twh": EU_DEMAND_TWH,
        "eu_wgv_share_of_demand_pct": 100 * eu_wgv / EU_DEMAND_TWH,
        "eu_wgv_days_of_demand": 365 * eu_wgv / EU_DEMAND_TWH,
        "top5_country_share_pct": 100 * eu_c.wgv_twh.head(5).sum() / eu_wgv,
        "top10_facility_share_pct": 100 * big.head(10).sum() / eu_wgv,
        "eu_inj_cap_twhd": se.injectionCapacity.sum() / 1000,
        "eu_wd_cap_twhd": se.withdrawalCapacity.sum() / 1000,
        "eu_days_to_fill_at_cap": eu_wgv / (se.injectionCapacity.sum() / 1000),
        "eu_days_to_empty_at_cap": eu_wgv / (se.withdrawalCapacity.sum() / 1000),
        "eu_mean_annual_wd_twh_gy2016_2024": gy.wd_twh.mean(),
        "eu_peak_day_wd_gwh": e.wd.max(), "eu_peak_day_wd_date": e.wd.idxmax().date(),
        "eu_fill_min_pct": e[e.index >= FLOW_START].fill.min(),
        "eu_fill_min_date": e[e.index >= FLOW_START].fill.idxmin().date(),
    }
    pd.Series(summ).to_csv(os.path.join(OUT, "summary.csv"), header=["value"])
    return by_c, by_t, gy, summ


def fig_country(by_c):
    x = by_c.sort_values("wgv_twh")
    fig, ax = plt.subplots(figsize=(9, 6.5))
    cols = [LIGHT if c in NON_EU else BLUE for c in x.index]
    ax.barh(x.index, x.wgv_twh, color=cols, height=.7)
    for i, (c, r) in enumerate(x.iterrows()):
        ax.text(r.wgv_twh + 3, i, f"{r.wgv_twh:.0f}  ({int(r.facilities)})",
                va="center", fontsize=9, color=INK2)
    ax.grid(axis="y", visible=False)
    ax.set_xlabel("Working gas capacity (TWh)")
    ax.set_xlim(0, x.wgv_twh.max() * 1.15)
    titles(ax, "Where the storage is",
           "Working gas capacity by country, gas day 31 Dec 2025 (reporting facilities in brackets); gray = non-EU")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig1_capacity_country.png"))
    plt.close(fig)


def fig_type(by_t):
    x = by_t.sort_values("wgv_twh")
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 4.2), sharey=True)
    a1.barh(x.index, x.wgv_twh, color=BLUE, height=.65)
    a2.barh(x.index, x.facilities, color=ORANGE, height=.65)
    for i, r in enumerate(x.itertuples()):
        a1.text(r.wgv_twh + 10, i, f"{r.wgv_twh:.0f} TWh ({r.share_wgv:.0f}%)" if r.wgv_twh >= 1 else f"{r.wgv_twh:.1f} TWh", va="center", fontsize=9, color=INK2)
        a2.text(r.facilities + .5, i, f"{r.facilities}", va="center", fontsize=9, color=INK2)
    a1.set_xlim(0, x.wgv_twh.max() * 1.35)
    a2.set_xlim(0, x.facilities.max() * 1.15)
    for a in (a1, a2):
        a.grid(axis="y", visible=False)
    a1.set_xlabel("Working gas capacity (TWh)")
    a2.set_xlabel("Facilities reporting")
    titles(a1, "Storage types, EU", "Capacity sits in depleted fields; salt caverns are many but small")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig2_capacity_type.png"))
    plt.close(fig)


def fig_flows(e):
    x = e[e.index >= FLOW_START][["inj", "wd"]].rolling(7, center=True).mean() / 1000
    fig, ax = plt.subplots(figsize=(11, 4.8))
    ax.fill_between(x.index, x.inj, color=BLUE, lw=0, label="Injection")
    ax.fill_between(x.index, -x.wd, color=ORANGE, lw=0, label="Withdrawal")
    ax.axhline(0, color=INK2, lw=.8)
    ax.set_ylabel("TWh per day (7-day mean)")
    ax.legend(loc="lower left", ncol=2)
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    titles(ax, "Storage is a seasonal business",
           "EU-27 daily injections (up) and withdrawals (down), AGSI+ facility sum")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig3_flows.png"))
    plt.close(fig)


def fig_stock(e):
    x = e[e.index >= FLOW_START]
    fig, ax = plt.subplots(figsize=(11, 4.8))
    ax.plot(x.index, x.wgv, color=GRAY, lw=1.5, ls="--", label="Working gas capacity")
    ax.fill_between(x.index, x.gis, color=BLUE, alpha=.25, lw=0)
    ax.plot(x.index, x.gis, color=BLUE, label="Gas in storage")
    ax.annotate("Bergermeer + Grijpskerk\nleave the panel (data gap)", (pd.Timestamp("2020-01-01"), 1040),
                xytext=(10, 30), textcoords="offset points", fontsize=9, color=INK2,
                arrowprops=dict(arrowstyle="-", color=GRAY))
    ax.set_ylim(0, 1250)
    ax.set_ylabel("TWh")
    ax.legend(loc="lower left", ncol=2)
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    titles(ax, "The EU tank", "EU-27 gas in storage and working gas capacity, daily; capacity steps are facilities entering or leaving the panel")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig4_fill_eu.png"))
    plt.close(fig)


def gas_year_frame(fill):
    f = fill.to_frame("fill")
    f["gy"] = f.index.year - (f.index.month < 10)
    f["doy"] = (f.index - pd.to_datetime(f.gy.astype(str) + "-10-01").values).days
    return f


def fig_gasyear(e):
    f = gas_year_frame(e.fill[e.index >= "2016-10-01"])
    fig, ax = plt.subplots(figsize=(10, 5.2))
    hi = {2020: (ORANGE, "2021"), 2021: (BLUE, "2022")}
    for gy, g in f.groupby("gy"):
        if gy in hi:
            continue
        ax.plot(g.doy, g.fill, color=LIGHT, lw=1.3)
    for gy, (col, _) in hi.items():
        g = f[f.gy == gy]
        ax.plot(g.doy, g.fill, color=col, lw=2.2)
        ax.text(g.doy.iloc[-1] + 3, g.fill.iloc[-1], f"GY {gy}/{str(gy + 1)[2:]}",
                color=INK, fontsize=9, va="center")
    ax.plot([], [], color=LIGHT, label="Other gas years 2016/17 to 2024/25")
    ax.legend(loc="lower right")
    ticks = pd.date_range("2001-10-01", "2002-09-01", freq="MS")
    ax.set_xticks((ticks - pd.Timestamp("2001-10-01")).days)
    ax.set_xticklabels(ticks.strftime("%b"))
    ax.set_xlim(0, 400)
    ax.set_ylim(0, 100)
    ax.set_ylabel("Fill level (% of working gas capacity)")
    titles(ax, "Fill levels over the gas year",
           "EU-27, 1 Oct to 30 Sep; the summer-2021 refill (orange) was the weakest, so winter 2021/22 (blue) started lowest")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig5_fill_gasyear.png"))
    plt.close(fig)


def fig_top5(d, e, by_c):
    top = by_c.drop(NON_EU, errors="ignore").head(5).index
    dd = d[d.country.isin(top) & (d.date >= FLOW_START)]
    c = dd.groupby(["country", "date"])[["gasInStorage", "workingGasVolume"]].sum()
    c["fill"] = 100 * c.gasInStorage / c.workingGasVolume
    eu = e[e.index >= FLOW_START].fill
    fig, axs = plt.subplots(len(top), 1, figsize=(10, 10), sharex=True, sharey=True)
    for ax, k in zip(axs, top):
        x = c.loc[k].fill
        ax.plot(eu.index, eu, color=LIGHT, lw=1.3)
        ax.plot(x.index, x, color=BLUE, lw=1.6)
        cap = by_c.loc[k, "wgv_twh"]
        ax.text(0, 1.03, f"{k}  ({cap:.0f} TWh)", transform=ax.transAxes, fontweight="bold",
                va="bottom", fontsize=11, color=INK)
        ax.set_ylim(0, 105)
        ax.set_yticks([0, 50, 100])
    axs[0].plot([], [], color=BLUE, label="Country")
    axs[0].plot([], [], color=LIGHT, label="EU-27")
    axs[-1].xaxis.set_major_locator(mdates.YearLocator())
    axs[-1].xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    axs[2].set_ylabel("Fill level (%)")
    axs[0].legend(loc="lower right", bbox_to_anchor=(1, 1), ncol=2, fontsize=9)
    fig.tight_layout(rect=(0, 0, 1, .94))
    fig.suptitle("Fill levels in the five largest EU storage countries", x=.07, ha="left",
                 fontweight="bold", fontsize=14)
    fig.text(.07, .945, "Gas in storage / working gas capacity, daily, top 5 by end-2025 capacity; "
             "NL Jan 2020 drop = data gap", color=INK2, fontsize=10)
    fig.savefig(os.path.join(OUT, "fig6_fill_top5.png"))
    plt.close(fig)


def main():
    d = load()
    e = eu_daily(d)
    by_c, by_t, gy, summ = tables(d, e)
    for k, v in summ.items():
        print(f"{k:35s} {v:.1f}" if isinstance(v, float) else f"{k:35s} {v}")
    print(by_t.round(1), gy.round(1), sep="\n")
    fig_country(by_c)
    fig_type(by_t)
    fig_flows(e)
    fig_stock(e)
    fig_gasyear(e)
    fig_top5(d, e, by_c)


if __name__ == "__main__":
    main()
