"""Q3a: additionality. Did storage end up fuller than the market would have chosen?

Input   data/storage_weather_panel.parquet (facility x gas day, build_storage_weather_panel.py)
Sample  EU member states with storage, gas days 2016-01-01 to 2025-12-31.
        Drops GB/GB* (not under the EU rule), UA, IE (one month of data) and NL: Bergermeer
        and Grijpskerk stop at 2019-12-31 in the raw files, so the NL series changes
        composition mid-sample (re-download from AGSI+ to add it back; see
        data_overview_storage.py). FR also loses Sediane B (13 TWh) after 2019; kept.
Fill    country % full = sum gas in storage / sum working gas volume, daily
Gap     fill minus the country's 2016-2021 average fill on the same day of year
Groups  distance to the 2022 target on 1 April 2022 = 80 - fill; "far below" = above the
        median distance, "close" = at or below it
Targets Regulation (EU) 2022/1032: 80% by 1 Nov 2022, 90% by 1 Nov 2023 and 2024.
        Regulation (EU) 2025/1733: 90% at any point between 1 Oct and 1 Dec, with up to
        10 pp deviation under difficult conditions (more for some cases), so for 2025 the
        bunching uses the highest fill in that window. Countries whose obligation is capped
        at 35% of consumption have lower effective targets; they show as labelled outliers.
Placebo the same split on 1 April 2021 (no mandate) for the convergence plot

Output  output/q3a_additionality/
  fig_q3a_did.png             monthly fill gap by group, 2019-2025 (panel 1)
  fig_q3a_bunching.png        fill minus target on the target date, pre vs post (panel 2)
  fig_q3a_combined.png        both panels side by side, for the slide
  fig_q3a_placebo_2021.png    the same split on 1 April 2021
  country_groups.csv          fill and distance on 1 April 2022, group, by country
  bunching.csv                fill and target on each target date, by country-year
  did_monthly.csv             the series behind fig_q3a_did.png
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "q3a_additionality")
os.makedirs(OUT, exist_ok=True)

DROP = ["GB", "GB*", "UA", "IE", "NL"]
PRE = ("2016-01-01", "2021-12-31")
TARGET_2022 = 80
REG_IN_FORCE = pd.Timestamp("2022-07-01")   # Regulation (EU) 2022/1032
LOOSENING = pd.Timestamp("2025-07-18")      # Regulation (EU) 2025/1733 adopted
# (date or window, target %) per year; pre-mandate years use 1 Nov vs 90% as a placebo
TARGETS = {2022: ("11-01", 80), 2023: ("11-01", 90), 2024: ("11-01", 90),
           2025: (("10-01", "12-01"), 90)}

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
    cols = ["country", "date", "gasInStorage", "workingGasVolume"]
    d = pd.read_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"), columns=cols)
    d = d[~d.country.isin(DROP) & (d.date >= "2016-01-01") & (d.workingGasVolume > 0)]
    c = d.groupby(["country", "date"])[["gasInStorage", "workingGasVolume"]].sum().reset_index()
    c["fill"] = 100 * c.gasInStorage / c.workingGasVolume
    c["md"] = c.date.dt.strftime("%m-%d").replace("02-29", "02-28")
    pre = c[c.date.between(*PRE)].groupby(["country", "md"]).fill.mean().rename("pre_avg")
    c = c.join(pre, on=["country", "md"])
    c["gap"] = c.fill - c.pre_avg
    return c


def groups(c, asof, target):
    f = c[c.date == pd.Timestamp(asof)].set_index("country").fill
    g = pd.DataFrame({"fill": f, "distance": target - f})
    g["group"] = np.where(g.distance > g.distance.median(), "far", "close")
    return g.sort_values("distance", ascending=False)


def did_series(c, g):
    x = c.join(g.group, on="country").dropna(subset=["group"])
    x["month"] = x.date.dt.to_period("M").dt.to_timestamp()
    # country-month means first, so each country counts once per month
    cm = x.groupby(["group", "country", "month"]).gap.mean().reset_index()
    m = cm.groupby(["group", "month"]).gap.agg(["mean", "count"]).reset_index()
    return m


def did_plot(ax, m, g, asof, start, end, marks):
    m = m[m.month.between(start, end)]
    lab = {"far": f"Far below target on {asof:%-d %b %Y} ({(g.group == 'far').sum()} countries)",
           "close": f"Close to target ({(g.group == 'close').sum()} countries)"}
    for grp, col in [("far", ORANGE), ("close", BLUE)]:
        s = m[m.group == grp]
        ax.plot(s.month, s["mean"], color=col, lw=2.2, label=lab[grp])
    ax.axhline(0, color=INK2, lw=0.8)
    for dt, txt in marks:
        ax.axvline(dt, color=INK2, lw=0.9, ls=":")
        ax.text(dt, ax.get_ylim()[1], f" {txt}", fontsize=9, color=INK2, va="top")
    ax.set_ylabel("fill minus 2016–2021 average, pp")
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.legend(loc="lower left", fontsize=9)


def bunching(c):
    rows = []
    for y in range(2016, 2026):
        if y in TARGETS:
            when, tgt = TARGETS[y]
        else:
            when, tgt = ("11-01", 90)
        if isinstance(when, tuple):
            w = c[c.date.between(f"{y}-{when[0]}", f"{y}-{when[1]}")]
            f = w.groupby("country").fill.max()
        else:
            f = c[c.date == pd.Timestamp(f"{y}-{when}")].set_index("country").fill
        for k, v in f.items():
            rows.append({"year": y, "country": k, "fill": v, "target": tgt,
                         "dist": v - tgt, "mandate": y >= 2022})
    b = pd.DataFrame(rows)
    b.round(2).to_csv(os.path.join(OUT, "bunching.csv"), index=False)
    return b


def bunching_plot(ax, b):
    bins = np.arange(-40, 22, 2)
    pre, post = b[~b.mandate], b[b.mandate]
    ax.hist(pre.dist.clip(-39, 21), bins=bins, color=GRAY, alpha=0.55,
            label=f"2016–2021, 1 Nov vs 90% ({len(pre)} country-years)")
    ax.hist(post.dist.clip(-39, 21), bins=bins, histtype="step", color=ORANGE, lw=2.2,
            label=f"2022–2025, vs the year's target ({len(post)} country-years)")
    ax.axvline(0, color=INK, lw=1.2)
    ax.text(0.5, ax.get_ylim()[1] * 0.97, " target", fontsize=9, color=INK, va="top")
    ax.text(0.02, 0.70,
            f"Below the line: {(pre.dist < 0).mean():.0%} of country-years before 2022,\n"
            f"{(post.dist < 0).mean():.0%} after ({(post[post.dist < 0].year == 2025).sum()} of "
            f"{(post.dist < 0).sum()} in 2025, when\na 10 pp deviation is allowed)",
            transform=ax.transAxes, fontsize=9, color=ORANGE, va="top")
    ax.set_xlabel("fill on the target date minus target, pp (clipped at −40 and +20)")
    ax.set_ylabel("country-years")
    ax.legend(loc="upper left", fontsize=9)


def main():
    c = load()
    g = groups(c, "2022-04-01", TARGET_2022)
    g.round(2).to_csv(os.path.join(OUT, "country_groups.csv"))
    print(g.round(1).to_string())
    m = did_series(c, g)
    m.assign(mean=m['mean'].round(3)).to_csv(os.path.join(OUT, "did_monthly.csv"), index=False)
    marks = [(REG_IN_FORCE, "EU rule in force"), (LOOSENING, "2025 loosening")]

    fig, ax = plt.subplots(figsize=(11, 5))
    did_plot(ax, m, g, pd.Timestamp("2022-04-01"), "2019-01-01", "2025-12-01", marks)
    titles(ax, "Countries far below target in April 2022 caught up and stayed above their past",
           "Monthly mean of country fill minus its 2016–2021 average for the same day. "
           "Source: GIE AGSI+.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q3a_did.png"), bbox_inches="tight")
    plt.close(fig)

    b = bunching(c)
    fig, ax = plt.subplots(figsize=(9, 5))
    bunching_plot(ax, b)
    titles(ax, "Since 2022, few countries miss the target, and most overshoot it",
           "Country fill on the target date minus the target. 2025: highest fill, 1 Oct–1 Dec.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q3a_bunching.png"), bbox_inches="tight")
    plt.close(fig)

    fig, (a1, a2) = plt.subplots(1, 2, figsize=(17, 5.6), gridspec_kw={"width_ratios": [1.35, 1]})
    did_plot(a1, m, g, pd.Timestamp("2022-04-01"), "2019-01-01", "2025-12-01", marks)
    titles(a1, "A. Fill relative to the country's 2016–2021 average",
           "Monthly mean across countries, split by distance to the 80% target on 1 April 2022.")
    bunching_plot(a2, b)
    titles(a2, "B. Fill on the target date minus the target",
           "Country-years. 2025: highest fill between 1 Oct and 1 Dec.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q3a_combined.png"), bbox_inches="tight")
    plt.close(fig)

    # placebo: same split one year earlier, no mandate in force
    g21 = groups(c, "2021-04-01", TARGET_2022)
    m21 = did_series(c, g21)
    fig, ax = plt.subplots(figsize=(11, 5))
    did_plot(ax, m21, g21, pd.Timestamp("2021-04-01"), "2019-01-01", "2022-06-01",
             [(pd.Timestamp("2021-11-01"), "1 Nov 2021")])
    titles(ax, "Placebo: the same split on 1 April 2021, before any mandate",
           "Monthly mean of country fill minus its 2016–2021 average for the same day.")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig_q3a_placebo_2021.png"), bbox_inches="tight")
    plt.close(fig)

    post = b[b.mandate]
    pre = b[~b.mandate]
    print(f"\npost-2022: share within [0,+5] pp of target = {(post.dist.between(0, 5)).mean():.2f}; "
          f"below target = {(post.dist < 0).mean():.2f}")
    print(f"pre-2022 (vs 90%): share within [0,+5] = {(pre.dist.between(0, 5)).mean():.2f}; "
          f"below 90% = {(pre.dist < 0).mean():.2f}")
    print(post.sort_values("dist").head(8).round(1).to_string(index=False))
    for grp in ["far", "close"]:
        s = m[m.group == grp].set_index("month")["mean"]
        print(grp, "gap Apr 2022 =", round(s.loc["2022-04-01"], 1), " Nov 2022 =", round(s.loc["2022-11-01"], 1),
              " Nov 2023 =", round(s.loc["2023-11-01"], 1))


if __name__ == "__main__":
    main()
