"""Fact 1: storage absorbs the weather shock. Withdrawals vs heating degree days.

Input   data/storage_weather_panel.parquet (facility x gas day, build_storage_weather_panel.py)
Sample  gas days 2015-01-01 to 2025-12-31; drops the Ukraine aggregate (all UA storage
        in one series) and facility-days with withdrawal capacity <= 0 or missing own HDD
        (so BE before Oct 2020, PT after 2020 and the ES gaps fall out), and sets
        wd_pct missing above 150% of capacity (reporting errors).
Outcome wd_pct  = withdrawal / withdrawal capacity x 100 (share of the facility's
                  technical send-out used that day, comparable across facilities)
        withdrawal (GWh/d) and net withdrawal (GWh/d) in levels
Weather hdd = max(14 - T, 0) of the facility's country; nb_hdd = mean over land neighbors.
SEs     two-way clustered by facility and gas day (weather is common to all
        facilities of a country on a day).

Output  output/fact1/
  tab_fe_specs.{csv,tex}       wd_pct and GWh/d on HDD under increasingly tight FEs
  tab_extensions.{csv,tex}     winter only, neighbor HDD, past/next week, net withdrawal
  coef_hdd_bins.csv            nonparametric HDD response
  fig1_binscatter.png          raw mean withdrawal share by 1-HDD bin
  fig2_hdd_bins.png            HDD-bin coefficients, facility + year-month + dow FEs
  fig3_eu_winter_2018.png      EU daily withdrawals and HDD, winter 2017/18
  fig4_eu_scatter.png          EU daily withdrawals vs storage-weighted HDD, 2015-2025
  fig5_winter_severity.png     winter HDD vs net winter withdrawal and 1 April fill
  winter_summary.csv           EU winter aggregates behind fig5
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyfixest as pf

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "fact1")
os.makedirs(OUT, exist_ok=True)

START, END = "2015-01-01", "2025-12-31"
VCOV = {"CRV1": "facility_id+date"}

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
    d = pd.read_parquet(os.path.join(ROOT, "data", "storage_weather_panel.parquet"))
    d = d[(d.date >= START) & (d.date <= END) & (d.country != "UA")].copy()
    d["wd_pct"] = 100 * d.withdrawal / d.withdrawalCapacity.where(d.withdrawalCapacity > 0)
    # above 150% of technical capacity is a reporting error (0.7% of rows, mostly two DE sites)
    d.loc[d.wd_pct > 150, "wd_pct"] = np.nan
    d["year"] = d.date.dt.year
    d["moy"] = d.date.dt.month
    d["ym"] = d.date.dt.strftime("%Y-%m")
    d["dow"] = d.date.dt.dayofweek
    d["fac_ym"] = d.facility_id + "|" + d.ym
    d["fac_moy"] = d.facility_id + "|" + d.moy.astype(str)
    d["winter"] = d.moy.isin([11, 12, 1, 2, 3])
    # gas winter label: Oct 2017 - Mar 2018 -> 2017/18
    gy = np.where(d.moy >= 10, d.year, d.year - 1)
    d["gas_winter"] = [f"{y}/{str(y + 1)[2:]}" for y in gy]
    return d


def reg_sample(d):
    return d[d.wd_pct.notna() & d.hdd.notna()].copy()


def tidy(fits, names):
    rows = []
    for name, f in zip(names, fits):
        t = f.tidy()
        for v in t.index:
            rows.append({"model": name, "var": v, "coef": t.loc[v, "Estimate"],
                         "se": t.loc[v, "Std. Error"], "p": t.loc[v, "Pr(>|t|)"],
                         "n": f._N, "r2_within": getattr(f, "_r2_within", np.nan)})
    return pd.DataFrame(rows)


def fe_specs(s):
    fml = {
        "(1) fac+dow": "{y} ~ hdd | facility_id + dow",
        "(2) fac+year+moy+dow": "{y} ~ hdd | facility_id + year + moy + dow",
        "(3) fac+ym+dow": "{y} ~ hdd | facility_id + ym + dow",
        "(4) fac x ym + dow": "{y} ~ hdd | fac_ym + dow",
        "(5) anom | fac x moy+year+dow": "{y} ~ hdd_anom | fac_moy + year + dow",
    }
    out = []
    for y in ["wd_pct", "withdrawal"]:
        fits = [pf.feols(f.format(y=y), data=s, vcov=VCOV) for f in fml.values()]
        t = tidy(fits, list(fml))
        t["outcome"] = y
        out.append(t)
        print(f"\n== {y} ==")
        pf.etable(fits, type="df").pipe(print)
    return pd.concat(out)


def extensions(s):
    w = s[s.winter]
    fits = {
        "winter: hdd": pf.feols("wd_pct ~ hdd | facility_id + ym + dow", data=w, vcov=VCOV),
        "own + neighbor": pf.feols("wd_pct ~ hdd + nb_hdd | facility_id + ym + dow",
                                   data=s, vcov=VCOV),
        "past/next week": pf.feols("wd_pct ~ hdd + hdd_past7 + hdd_next7 | facility_id + ym + dow",
                                   data=s, vcov=VCOV),
        "net wd GWh/d": pf.feols("netWithdrawal ~ hdd | facility_id + ym + dow",
                                 data=s, vcov=VCOV),
        "net wd, winter": pf.feols("netWithdrawal ~ hdd | facility_id + ym + dow",
                                   data=w, vcov=VCOV),
    }
    pf.etable(list(fits.values()), type="df").pipe(print)
    return tidy(list(fits.values()), list(fits))


def to_tex(t, path, caption):
    """Compact coef (se) table: rows = variables, columns = models."""
    t = t.copy()
    stars = lambda p: "***" if p < .01 else "**" if p < .05 else "*" if p < .1 else ""
    t["cell"] = [f"{c:.3f}{stars(p)}" for c, p in zip(t.coef, t.p)]
    t["secell"] = [f"({x:.3f})" for x in t.se]
    cols = list(dict.fromkeys(t.model))
    heads = [c.replace(" x ", r" $\times$ ").replace("|", ";") for c in cols]
    lines = [r"\begin{tabular}{l" + "c" * len(cols) + "}", r"\hline",
             " & " + " & ".join(heads) + r" \\", r"\hline"]
    for v in dict.fromkeys(t["var"]):
        sub = t[t["var"] == v].set_index("model")
        lines.append(v.replace("_", r"\_") + " & " + " & ".join(sub.cell.get(c, "") for c in cols) + r" \\")
        lines.append(" & " + " & ".join(sub.secell.get(c, "") for c in cols) + r" \\")
    n = t.groupby("model", sort=False).n.first()
    lines += [r"\hline", "N & " + " & ".join(f"{n[c]:,}" for c in cols) + r" \\",
              r"\hline", r"\end{tabular}", f"% {caption}"]
    open(path, "w").write("\n".join(lines) + "\n")


# ---------------- figures ----------------

def fig_binscatter(s):
    b = s.assign(bin=np.minimum(np.floor(s.hdd), 24))
    g = b.groupby(["bin", "winter"]).wd_pct.agg(["mean", "size"]).reset_index()
    g = g[g["size"] >= 200]
    fig, ax = plt.subplots(figsize=(8, 4.8))
    for win, col, lab in [(True, BLUE, "Nov–Mar"), (False, ORANGE, "Apr–Oct")]:
        x = g[g.winter == win]
        ax.plot(x.bin + .5, x["mean"], "-o", color=col, ms=6, mec="white", mew=1.5)
        ax.annotate(lab, (x.bin.iloc[-1] + .5, x["mean"].iloc[-1]), xytext=(6, 0),
                    textcoords="offset points", va="center", color=INK)
    ax.set_xlabel("Heating degree days, own country (°C below 14)")
    ax.set_ylabel("Withdrawal, % of withdrawal capacity")
    titles(ax, "Colder days, more gas out of storage",
             "Mean facility-day withdrawal by 1-HDD bin, 2015–2025")
    ax.set_ylim(0)
    ax.set_xlim(0, 27)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig1_binscatter.png"))
    plt.close(fig)


def fig_bins(s):
    edges = [0, 1e-9, 2, 4, 6, 8, 10, 12, 14, 16, 18, 100]
    labs = ["0", "0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16-18", "18+"]
    s = s.assign(hbin=pd.cut(s.hdd, edges, labels=labs, include_lowest=True, right=False))
    s["hbin"] = s.hbin.astype(str)
    s.loc[s.hdd == 0, "hbin"] = "0"
    f = pf.feols("wd_pct ~ i(hbin, ref='0') | facility_id + ym + dow", data=s, vcov=VCOV)
    t = f.tidy().reset_index()
    t["bin"] = t["Coefficient"].str.extract(r"(?:\[T\.|::)([^\]]+)\]?$")[0]
    t = pd.concat([pd.DataFrame({"bin": ["0"], "Estimate": [0.0], "Std. Error": [0.0]}), t])
    t["order"] = t.bin.map({l: i for i, l in enumerate(labs)})
    t = t.sort_values("order")
    t[["bin", "Estimate", "Std. Error"]].to_csv(os.path.join(OUT, "coef_hdd_bins.csv"), index=False)
    x = np.arange(len(t))
    fig, ax = plt.subplots(figsize=(8, 4.8))
    ax.fill_between(x, t.Estimate - 1.96 * t["Std. Error"], t.Estimate + 1.96 * t["Std. Error"],
                    color=BLUE, alpha=.15, lw=0)
    ax.plot(x, t.Estimate, "-o", color=BLUE, ms=7, mec="white", mew=1.5)
    ax.axhline(0, color=GRAY, lw=1)
    ax.set_xticks(x, t.bin)
    ax.set_xlabel("Heating degree days (bin)")
    ax.set_ylabel("Δ withdrawal, pp of capacity vs. HDD = 0")
    titles(ax, "Withdrawals rise with cold, within the same month",
             "Facility, year-month and day-of-week FEs; 95% CI, two-way clustered")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig2_hdd_bins.png"))
    plt.close(fig)
    return t


def eu_daily(d):
    """EU daily totals; HDD weighted by facility working gas volume (own HDD, then neighbor)."""
    d = d.copy()
    d["hdd_w"] = d.hdd.fillna(d.nb_hdd)
    w = d.workingGasVolume.where(d.workingGasVolume > 0)
    d["w"] = w.where(d.hdd_w.notna())
    d["wh"] = d.w * d.hdd_w
    e = d.groupby("date").agg(withdrawal=("withdrawal", "sum"), injection=("injection", "sum"),
                              gas=("gasInStorage", "sum"), wgv=("workingGasVolume", "sum"),
                              wh=("wh", "sum"), w=("w", "sum"))
    e["hdd"] = e.wh / e.w
    e["wd_twh"] = e.withdrawal / 1000
    e["net_twh"] = (e.withdrawal - e.injection) / 1000
    e["fill"] = 100 * e.gas / e.wgv
    return e


def fig_winter_2018(e):
    x = e.loc["2017-11-01":"2018-03-31"]
    fig, (a1, a2) = plt.subplots(2, 1, figsize=(9, 5.6), sharex=True,
                                 gridspec_kw={"height_ratios": [1, 1.3]})
    a1.fill_between(x.index, x.hdd, color=GRAY, alpha=.35, lw=0)
    a1.plot(x.index, x.hdd, color=INK2, lw=1.5)
    a1.set_ylabel("HDD")
    titles(a1, "Beast from the East: storage met the cold snap",
             "EU storage facilities, winter 2017/18; HDD weighted by working gas volume")
    a2.plot(x.index, x.wd_twh, color=BLUE)
    a2.set_ylabel("Withdrawals, TWh/d")
    a2.set_ylim(0)
    pk = x.wd_twh.idxmax()
    a2.annotate(f"{x.wd_twh.max():.1f} TWh/d\n{pk:%d %b %Y}", (pk, x.wd_twh.max()),
                xytext=(-80, -10), textcoords="offset points", color=INK, fontsize=10,
                arrowprops=dict(arrowstyle="-", color=GRAY))
    import matplotlib.dates as mdates
    a2.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig3_eu_winter_2018.png"))
    plt.close(fig)


def fig_eu_scatter(e):
    x = e.loc[START:END].dropna(subset=["hdd"])
    x = x.assign(winter=x.index.month.isin([11, 12, 1, 2, 3]))
    fig, ax = plt.subplots(figsize=(8, 4.8))
    for win, col, lab in [(False, ORANGE, "Apr–Oct"), (True, BLUE, "Nov–Mar")]:
        y = x[x.winter == win]
        ax.scatter(y.hdd, y.wd_twh, s=9, color=col, alpha=.35, lw=0, label=lab)
    b = np.polyfit(x[x.winter].hdd, x[x.winter].wd_twh, 1)
    xs = np.linspace(0, x.hdd.max(), 50)
    ax.plot(xs, np.polyval(b, xs), color=INK, lw=1.5)
    ax.annotate(f"winter slope: {b[0] * 1000:.0f} GWh/d per HDD", (xs[-12], np.polyval(b, xs[-12])),
                xytext=(-150, 25), textcoords="offset points", color=INK, fontsize=10)
    ax.set_xlabel("EU heating degree days (storage-weighted)")
    ax.set_ylabel("EU withdrawals, TWh/d")
    titles(ax, "EU withdrawals scale with how cold the day is", "One dot per gas day, 2015–2025")
    ax.legend(loc="upper left", markerscale=2.5)
    ax.set_ylim(0)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig4_eu_scatter.png"))
    plt.close(fig)
    return b


def winter_summary(e):
    x = e.copy()
    m = x.index.month
    x = x[(m >= 11) | (m <= 3)]
    gy = np.where(x.index.month >= 10, x.index.year, x.index.year - 1)
    x["gw"] = gy
    w = x.groupby("gw").agg(hdd_sum=("hdd", "sum"), net_twh=("net_twh", "sum"),
                            wd_twh=("wd_twh", "sum"), n=("hdd", "size"))
    fill1nov = e.fill.reindex([pd.Timestamp(f"{y}-11-01") for y in w.index]).values
    fill1apr = e.fill.reindex([pd.Timestamp(f"{y + 1}-04-01") for y in w.index]).values
    w["fill_1nov"], w["fill_1apr"] = fill1nov, fill1apr
    w = w[(w.n >= 150) & (w.index >= 2015) & (w.index <= 2024)]
    w.index = [f"{y}/{str(y + 1)[2:]}" for y in w.index]
    w.to_csv(os.path.join(OUT, "winter_summary.csv"))
    fig, axs = plt.subplots(1, 2, figsize=(11, 4.6))
    for ax, col, lab in [(axs[0], "net_twh", "Net withdrawal Nov–Mar, TWh"),
                         (axs[1], "fill_1apr", "EU storage fill on 1 April, %")]:
        ax.scatter(w.hdd_sum, w[col], s=60, color=BLUE, edgecolor="white", lw=1.5, zorder=3)
        for k, r in w.iterrows():
            ax.annotate(k, (r.hdd_sum, r[col]), xytext=(5, 4), textcoords="offset points",
                        fontsize=9, color=INK2)
        ax.set_xlabel("Winter HDD, Nov–Mar (storage-weighted)")
        ax.set_ylabel(lab)
    axs[0].set_title("Harder winters drain more storage…")
    axs[1].set_title("…and end with emptier tanks")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig5_winter_severity.png"))
    plt.close(fig)
    return w


def main():
    d = load()
    s = reg_sample(d)
    print(f"regression sample: {len(s):,} facility-days, {s.facility_id.nunique()} facilities, "
          f"{s.country.nunique()} countries")
    print(s.groupby("winter").wd_pct.describe())

    t1 = fe_specs(s)
    t1.to_csv(os.path.join(OUT, "tab_fe_specs.csv"), index=False)
    for y in ["wd_pct", "withdrawal"]:
        to_tex(t1[t1.outcome == y], os.path.join(OUT, f"tab_fe_specs_{y}.tex"),
               f"Outcome {y}; SEs two-way clustered by facility and gas day")
    t2 = extensions(s)
    t2.to_csv(os.path.join(OUT, "tab_extensions.csv"), index=False)
    to_tex(t2, os.path.join(OUT, "tab_extensions.tex"),
           "Facility, year-month, dow FEs; SEs two-way clustered by facility and gas day")

    fig_binscatter(s)
    print(fig_bins(s))
    e = eu_daily(d)
    fig_winter_2018(e)
    b = fig_eu_scatter(e)
    print("EU winter slope (GWh/d per HDD):", b[0] * 1000)
    print(winter_summary(e))
    mean_w = s[s.winter]
    print("winter mean wd_pct:", mean_w.wd_pct.mean(), " mean HDD:", mean_w.hdd.mean(),
          " mean withdrawal GWh/d:", mean_w.withdrawal.mean())


if __name__ == "__main__":
    main()
