"""Q2a: Rehden against the rest of Germany. Does market power reduce security?

Input   data/storage_weather_panel.parquet (facility x gas day, build_storage_weather_panel.py)
Groups  Rehden       UGS Rehden (EIC 21Z000000000271O), astora until 2022, then SEFE
        rest of DE   all other German facilities except the other astora/SEFE site
                     (UGS Jemgum H (SEFE Storage)) and the 2019 VSP NORD aggregate,
                     which reports zero volume
Fill    % full = sum gas in storage / sum working gas volume (capacity-weighted)
Band    min-max of rest-of-DE fill by day of year over gas days 2016-04-06 to 2020-12-31
        (many DE sites enter AGSI+ on 2016-04-06, so earlier days have fewer sites)
Gap     TWh Rehden would hold at the rest-of-DE fill rate minus what it held;
        days = gap / average daily German consumption (1,013 TWh in 2021, BDEW
        estimate via Clean Energy Wire, so 2.78 TWh/d; winter days are higher)
Events  2022-04-04 Bundesnetzagentur appointed trustee of Gazprom Germania
        2022-04-30 German Gas Storage Act (Gasspeichergesetz) in force
        2022-06-20 Gazprom Germania renamed SEFE

Output  output/q2_rehden/
  fig_q2a_rehden_vs_de.png       % full by day, 2015-2022, events marked (main figure)
  fig_q2a_rehden_vs_de_2025.png  same through 2025 (state-owned period)
  fig_q2a_gap_twh.png            security gap in TWh and days of German demand
  key_dates.csv                  fills and gaps on 1 Oct and 1 Nov each year
  slide_numbers.txt              the numbers for the slide line
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "q2_rehden")
os.makedirs(OUT, exist_ok=True)

REHDEN = "21Z000000000271O"
EXCLUDE = ["21W0000000001148",  # UGS Jemgum H (SEFE Storage), same owner as Rehden
           "21W0000000001261"]  # VSP NORD (Rehden, Jemgum) 2019 aggregate, zero volume
DE_DEMAND_TWH_DAY = 1013 / 365
BAND_START, BAND_END = "2016-04-06", "2020-12-31"
EVENTS = [("2022-04-04", "Trustee appointed"),
          ("2022-04-30", "Gas Storage Act"),
          ("2022-06-20", "Renamed SEFE")]

BLUE, ORANGE, GRAY = "#2a78d6", "#eb6834", "#8a8984"
INK, INK2 = "#0b0b0b", "#52514e"
plt.rcParams.update({
    "figure.dpi": 150, "savefig.dpi": 200, "font.size": 12, "axes.titlesize": 14,
    "axes.titleweight": "bold", "axes.titlelocation": "left", "axes.labelcolor": INK2,
    "axes.edgecolor": "#c9c8c2", "axes.spines.top": False, "axes.spines.right": False,
    "xtick.color": INK2, "ytick.color": INK2, "axes.grid": True, "grid.color": "#ebeae6",
    "grid.linewidth": 0.8, "legend.frameon": False, "lines.linewidth": 2,
})


def titles(ax, title, sub):
    ax.set_title(title, pad=24)
    ax.text(0, 1.02, sub, transform=ax.transAxes, color=INK2, fontsize=10, va="bottom")


def load():
    cols = ["facility_eic", "name", "country", "date", "gasInStorage", "workingGasVolume"]
    d = pd.read_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"), columns=cols)
    d = d[(d.country == "DE") & (d.date >= "2015-01-01")]
    d = d[~d.facility_eic.isin(EXCLUDE) & (d.workingGasVolume > 0)]
    d["group"] = np.where(d.facility_eic == REHDEN, "rehden", "rest")
    g = d.groupby(["date", "group"])[["gasInStorage", "workingGasVolume"]].sum().unstack()
    s = pd.DataFrame({
        "rehden_twh": g["gasInStorage"]["rehden"], "rehden_wgv": g["workingGasVolume"]["rehden"],
        "rest_twh": g["gasInStorage"]["rest"], "rest_wgv": g["workingGasVolume"]["rest"],
    })
    s["rehden_pct"] = 100 * s.rehden_twh / s.rehden_wgv
    s["rest_pct"] = 100 * s.rest_twh / s.rest_wgv
    s["gap_twh"] = (s.rest_pct - s.rehden_pct) / 100 * s.rehden_wgv
    s["gap_days"] = s.gap_twh / DE_DEMAND_TWH_DAY
    return s


def band(s):
    """Rest-of-DE seasonal range 2016-2020, by day of year (29 Feb dropped), tiled over all days."""
    b = s.loc[BAND_START:BAND_END, "rest_pct"].to_frame()
    b = b[~((b.index.month == 2) & (b.index.day == 29))]
    b["md"] = b.index.strftime("%m-%d")
    r = b.groupby("md").rest_pct.agg(["min", "max"])
    md = s.index.strftime("%m-%d").where(s.index.strftime("%m-%d") != "02-29", "02-28")
    return pd.DataFrame({"lo": r["min"].reindex(md).values, "hi": r["max"].reindex(md).values},
                        index=s.index)


def fill_plot(s, b, end, fname, title):
    x = s.loc[:end]
    bb = b.loc[:end]
    fig, ax = plt.subplots(figsize=(12, 5.2))
    ax.fill_between(bb.index, bb.lo, bb.hi, color=GRAY, alpha=0.18, lw=0,
                    label="Rest of Germany, 2016–2020 range")
    ax.plot(x.index, x.rest_pct, color=GRAY, lw=1.6, label="Rest of Germany (capacity-weighted)")
    ax.plot(x.index, x.rehden_pct, color=ORANGE, lw=2.2, label="Rehden (largest German site)")
    for dt, lab in EVENTS:
        ax.axvline(pd.Timestamp(dt), color=INK2, lw=0.8, ls=":")
    ax.text(pd.Timestamp(EVENTS[0][0]), 103, "Trustee (4 Apr 2022), Storage Act (30 Apr),\nrenamed SEFE (20 Jun) ",
            color=INK2, fontsize=9, va="top", ha="right")
    ax.set_ylim(0, 105)
    ax.set_ylabel("% full")
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.legend(loc="lower left", fontsize=10, ncol=3, bbox_to_anchor=(0, -0.22))
    titles(ax, title, "Gas in storage as % of working gas volume, daily. Source: GIE AGSI+. "
           "Rest of Germany excludes the other astora/SEFE site (Jemgum H).")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, fname), bbox_inches="tight")
    plt.close(fig)


def gap_plot(s):
    x = s.loc[:"2022-12-31"]
    fig, ax = plt.subplots(figsize=(12, 4.2))
    ax.fill_between(x.index, 0, x.gap_twh, where=x.gap_twh > 0, color=ORANGE, alpha=0.5, lw=0)
    ax.fill_between(x.index, 0, x.gap_twh, where=x.gap_twh <= 0, color=BLUE, alpha=0.4, lw=0)
    ax.axhline(0, color=INK2, lw=0.8)
    ax.set_ylabel("TWh")
    sec = ax.secondary_yaxis("right", functions=(lambda t: t / DE_DEMAND_TWH_DAY,
                                                 lambda dd: dd * DE_DEMAND_TWH_DAY))
    sec.set_ylabel("days of average German demand")
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    titles(ax, "Gas missing at Rehden relative to the rest of Germany",
           "TWh Rehden would hold at the rest-of-Germany fill rate minus what it held. "
           "Above zero = Rehden emptier.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q2a_gap_twh.png"), bbox_inches="tight")
    plt.close(fig)


def key_dates(s, b):
    rows = []
    for y in range(2015, 2026):
        for md in ["10-01", "11-01"]:
            dt = pd.Timestamp(f"{y}-{md}")
            if dt in s.index:
                r = s.loc[dt]
                rows.append({"date": dt.date(), "rehden_pct": r.rehden_pct, "rest_pct": r.rest_pct,
                             "rest_band_lo": b.loc[dt, "lo"], "rest_band_hi": b.loc[dt, "hi"],
                             "rehden_twh": r.rehden_twh, "rehden_wgv": r.rehden_wgv,
                             "gap_twh": r.gap_twh, "gap_days": r.gap_days})
    t = pd.DataFrame(rows).round(2)
    t.to_csv(os.path.join(OUT, "key_dates.csv"), index=False)
    return t


def slide_numbers(s, t):
    k = t.set_index("date")
    o, n = k.loc[pd.Timestamp("2021-10-01").date()], k.loc[pd.Timestamp("2021-11-01").date()]
    pre = t[(pd.to_datetime(t.date).dt.year.between(2016, 2020)) & (pd.to_datetime(t.date).dt.month == 11)]
    low = s.loc["2021-01-01":"2022-06-30", "rehden_pct"]
    lines = [
        f"1 Oct 2021: Rehden {o.rehden_pct:.1f}% full, rest of Germany {o.rest_pct:.1f}%.",
        f"1 Nov 2021: Rehden {n.rehden_pct:.1f}% full, rest of Germany {n.rest_pct:.1f}%.",
        f"Gap on 1 Nov 2021: {n.gap_twh:.1f} TWh = {n.gap_days:.1f} days of average German demand "
        f"({DE_DEMAND_TWH_DAY:.2f} TWh/d).",
        f"Rehden on 1 Nov, 2016-2020 average: {pre.rehden_pct.mean():.1f}% "
        f"(rest of Germany {pre.rest_pct.mean():.1f}%).",
        f"Rehden minimum 2021-22: {low.min():.2f}% on {low.idxmin().date()}.",
        f"1 Nov 2022 (after trusteeship): Rehden {k.loc[pd.Timestamp('2022-11-01').date()].rehden_pct:.1f}%, "
        f"rest {k.loc[pd.Timestamp('2022-11-01').date()].rest_pct:.1f}%.",
    ]
    txt = "\n".join(lines)
    with open(os.path.join(OUT, "slide_numbers.txt"), "w") as f:
        f.write(txt + "\n")
    print(txt)


def main():
    s = load()
    b = band(s)
    fill_plot(s, b, "2022-12-31", "fig_q2a_rehden_vs_de.png",
              "Rehden emptied in 2021 while the rest of Germany filled")
    fill_plot(s, b, "2025-12-31", "fig_q2a_rehden_vs_de_2025.png",
              "Rehden emptied in 2021, and again in 2025 under state ownership")
    gap_plot(s)
    t = key_dates(s, b)
    print(t.to_string(index=False))
    slide_numbers(s, t)


if __name__ == "__main__":
    main()
