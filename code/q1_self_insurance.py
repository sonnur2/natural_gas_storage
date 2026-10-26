"""Q1: does the market self-insure? How much of a spring storage gap is still missing in winter?

Input   data/storage_weather_panel.parquet (facility x gas day, build_storage_weather_panel.py)
Sample  EU member states with storage, years 2017-2021 (before the mandate). 2016 is left
        out because most German sites enter AGSI+ on 2016-04-06, after the 1 April date.
        Drops GB/GB* (not under the EU rule), UA, IE and NL (Bergermeer and Grijpskerk stop
        at 2019-12-31 in the raw files), as in q3a_additionality.py.
Fill    country % full = sum gas in storage / sum working gas volume
Gaps    gap_apr = fill on 1 April minus the country's 2017-2021 average fill on 1 April
        gap_nov = fill on 1 November minus the country's 2017-2021 average on 1 November
        both in pp of capacity
Slope   gap_nov on gap_apr with country FE, weighted by working gas volume, SEs clustered
        by country. 0 = the market refills the whole spring gap, 1 = none of it.
IV      instrument = the preceding winter's cumulative HDD (1 Oct - 31 Mar, country weather)
        minus the country's 2011-2025 mean; isolates spring gaps caused by cold winters
        (BE before 2020, ES before 2019, CZ 2020, PT after 2020 have no HDD and drop out)
Threshold  with a 90% target, the target binds when usual Nov fill + slope x gap_apr < 90,
        i.e. when the spring gap is more than (usual Nov fill - 90) / slope points below normal;
        usual Nov fill = the country's 2017-2021 median, so 2021 alone does not set the bar

Output  output/q1_self_insurance/
  fig_q1_scatter.png       1 Nov gap against 1 April gap, bubbles sized by capacity,
                           coloured by the preceding winter (main figure)
  fig_q1_threshold.png     per-country threshold: how far below normal spring storage
                           must be before a 90% target binds
  regressions.csv          OLS and IV slopes, with and without 2021
  country_years.csv        the country-year data
  thresholds.csv           normal Nov fill and threshold by country
  slide_numbers.txt        the numbers for the slide lines
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyfixest as pf

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "q1_self_insurance")
os.makedirs(OUT, exist_ok=True)

DROP = ["GB", "GB*", "UA", "IE", "NL"]
YEARS = range(2017, 2022)
TARGET = 90
LABEL = ["DE", "IT", "AT"]

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
    cols = ["country", "date", "gasInStorage", "workingGasVolume", "hdd_gasyear_cum"]
    d = pd.read_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"), columns=cols)
    d = d[~d.country.isin(DROP) & (d.workingGasVolume > 0)]
    c = d.groupby(["country", "date"]).agg(gis=("gasInStorage", "sum"), wgv=("workingGasVolume", "sum"),
                                           hdd_cum=("hdd_gasyear_cum", "mean")).reset_index()
    c["fill"] = 100 * c.gis / c.wgv
    return c


def country_years(c):
    at = lambda md: c[c.date.dt.strftime("%m-%d") == md].assign(year=lambda x: x.date.dt.year) \
        .set_index(["country", "year"])
    apr, nov, mar = at("04-01"), at("11-01"), at("03-31")
    winter = mar.hdd_cum.rename("winter_hdd")
    w = winter.reset_index()
    w["winter_hdd_anom"] = w.winter_hdd - w.groupby("country").winter_hdd.transform("mean")
    y = pd.DataFrame({"fill_apr": apr.fill, "fill_nov": nov.fill, "wgv": nov.wgv}).join(
        w.set_index(["country", "year"]).winter_hdd_anom)
    y = y.reset_index()
    y = y[y.year.isin(YEARS)].dropna(subset=["fill_apr", "fill_nov", "wgv"])
    for v in ["apr", "nov"]:
        y[f"normal_{v}"] = y.groupby("country")[f"fill_{v}"].transform("mean")
        y[f"gap_{v}"] = y[f"fill_{v}"] - y[f"normal_{v}"]
    y.round(3).to_csv(os.path.join(OUT, "country_years.csv"), index=False)
    return y


def regressions(y):
    rows, fits = [], {}
    specs = {
        "OLS, all years": (y, "gap_nov ~ gap_apr | country"),
        "OLS, without 2021": (y[y.year != 2021], "gap_nov ~ gap_apr | country"),
        "IV (winter HDD), all years": (y.dropna(subset=["winter_hdd_anom"]),
                                      "gap_nov ~ 1 | country | gap_apr ~ winter_hdd_anom"),
        "IV (winter HDD), without 2021": (y[y.year != 2021].dropna(subset=["winter_hdd_anom"]),
                                         "gap_nov ~ 1 | country | gap_apr ~ winter_hdd_anom"),
        "First stage: gap_apr on winter HDD": (y.dropna(subset=["winter_hdd_anom"]),
                                               "gap_apr ~ winter_hdd_anom | country"),
    }
    for name, (s, f) in specs.items():
        fit = pf.feols(f, data=s, weights="wgv", vcov={"CRV1": "country"})
        t = fit.tidy().iloc[0]
        fits[name] = fit
        rows.append({"model": name, "var": t.name, "coef": t["Estimate"], "se": t["Std. Error"],
                     "p": t["Pr(>|t|)"], "n": fit._N, "countries": s.country.nunique()})
    r = pd.DataFrame(rows)
    r.round(4).to_csv(os.path.join(OUT, "regressions.csv"), index=False)
    print(r.round(3).to_string(index=False))
    return r


def scatter(y, b):
    fig, ax = plt.subplots(figsize=(10, 7))
    lim = (-62, 38)
    xx = np.array(lim)
    ax.plot(xx, xx, color=INK2, lw=1, ls="--")
    ax.text(-36, -33, "45°: nothing refilled", fontsize=10, color=INK2, rotation=36)
    ax.axhline(0, color=INK2, lw=1)
    ax.text(-60, 1.2, "fully refilled", fontsize=10, color=INK2)
    ax.plot(xx, b * xx, color=INK, lw=2.2)
    ax.text(-60, b * -60 - 4, f"fitted: {b:.2f}", fontsize=11, color=INK, fontweight="bold")
    cold = y.winter_hdd_anom > 0
    scale = 1500 / y.wgv.max()
    for mask, col, lab in [(cold, BLUE, "after a colder-than-usual winter"),
                           (~cold & y.winter_hdd_anom.notna(), ORANGE, "after a milder winter"),
                           (y.winter_hdd_anom.isna(), GRAY, "no weather data")]:
        q = y[mask]
        ax.scatter(q.gap_apr, q.gap_nov, s=q.wgv * scale, color=col, alpha=0.5,
                   edgecolor="white", lw=0.8, label=lab, zorder=3)
    for r in y[y.country.isin(LABEL)].itertuples():
        ax.annotate(f"{r.country} {str(r.year)[2:]}", (r.gap_apr, r.gap_nov), fontsize=8, color=INK,
                    xytext=(4, 4), textcoords="offset points")
    ax.set_xlim(lim)
    ax.set_ylim(-40, 32)
    ax.set_xlabel("1 April: fill minus the country's usual level, pp of capacity")
    ax.set_ylabel("1 November: fill minus the country's usual level, pp")
    leg = ax.legend(loc="lower right", fontsize=9, markerscale=0.4)
    titles(ax, f"Of a 10-point spring shortfall, about {10 * b:.0f} points are still missing in November",
           "One bubble per country-year, 2017–2021; area = working gas volume. Source: GIE AGSI+.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q1_scatter.png"), bbox_inches="tight")
    plt.close(fig)


def thresholds(y, b):
    # usual Nov fill = median 2017-2021, so one bad year (2021) does not set the bar
    t = y.groupby("country").agg(normal_nov=("fill_nov", "median"), wgv=("wgv", "mean"))
    t["threshold_pp"] = (t.normal_nov - TARGET) / b   # spring gap (below normal) at which 90% binds
    t = t.sort_values("wgv", ascending=False)
    y = y.join(t.threshold_pp, on="country")
    y["binds"] = y.gap_apr < -y.threshold_pp
    t.round(2).to_csv(os.path.join(OUT, "thresholds.csv"))

    q = t.sort_values("threshold_pp")
    fig, ax = plt.subplots(figsize=(10, 5.5))
    ax.barh(q.index, q.threshold_pp.clip(lower=0), color=BLUE, alpha=0.8)
    for k, (c, v) in enumerate(q.threshold_pp.items()):
        if v <= 0:
            ax.text(0.3, k, f"usual fill {t.loc[c, 'normal_nov']:.0f}%: binds even in a normal year",
                    va="center", fontsize=9, color=ORANGE)
        else:
            ax.text(v + 0.4, k, f"{v:.0f} pp", va="center", fontsize=9, color=INK2)
    ax.invert_yaxis()
    ax.set_xlabel("spring shortfall below normal at which a 90% target starts to bind, pp")
    titles(ax, "How empty must spring storage be before a 90% target binds?",
           f"(usual 1 Nov fill − 90) / {b:.2f}. Usual fill = 2017–2021 median. "
           "Orange: usual fill already below 90%.")
    ax.set_xlim(0, 30)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q1_threshold.png"), bbox_inches="tight")
    plt.close(fig)
    return t, y


def main():
    c = load()
    y = country_years(c)
    r = regressions(y).set_index("model")
    b = r.loc["OLS, all years", "coef"]
    scatter(y, b)
    t, y = thresholds(y, b)

    w = t.wgv / t.wgv.sum()
    pos = t.threshold_pp > 0
    binds = y[y.binds]
    cold_share = (binds.winter_hdd_anom > 0).mean() if binds.winter_hdd_anom.notna().any() else np.nan
    lines = [
        f"OLS slope {b:.2f} (se {r.loc['OLS, all years', 'se']:.2f}): of a 10-point spring shortfall, "
        f"{10 * b:.1f} points are still missing on 1 Nov.",
        f"Without 2021: {r.loc['OLS, without 2021', 'coef']:.2f} (se {r.loc['OLS, without 2021', 'se']:.2f}).",
        f"IV (winter HDD): {r.loc['IV (winter HDD), all years', 'coef']:.2f} "
        f"(se {r.loc['IV (winter HDD), all years', 'se']:.2f}); without 2021: "
        f"{r.loc['IV (winter HDD), without 2021', 'coef']:.2f}.",
        f"First stage: {r.loc['First stage: gap_apr on winter HDD', 'coef']:.3f} pp per HDD "
        f"(se {r.loc['First stage: gap_apr on winter HDD', 'se']:.3f}).",
        f"Capacity-weighted threshold (countries whose usual fill is above 90%): "
        f"{(t.threshold_pp[pos] * w[pos]).sum() / w[pos].sum():.1f} pp below normal in spring.",
        f"Countries whose usual 1 Nov fill is already below 90%: {', '.join(t.index[~pos])}.",
        f"Country-years 2017-2021 where the target would have bound: {len(binds)} of {len(y)} "
        f"({binds.wgv.sum() / y.wgv.sum():.0%} of capacity-years); "
        f"share after a colder-than-usual winter: {cold_share:.0%}.",
        "Binding country-years: " + ", ".join(f"{r.country} {r.year}" for r in binds.itertuples()),
    ]
    txt = "\n".join(lines)
    with open(os.path.join(OUT, "slide_numbers.txt"), "w") as f:
        f.write(txt + "\n")
    print(txt)
    print(t.round(1).to_string())


if __name__ == "__main__":
    main()
