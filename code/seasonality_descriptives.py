"""Descriptive check for the 'distorted seasonality' brief.
Figures -> output/seasonality/ ; needs data/seasonality/cdd_hdd_country_year.csv
(from seasonality_cdd_longrun.py) and the EU monthly demand file.
"""
import os, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "output", "seasonality"); os.makedirs(OUT, exist_ok=True)
plt.rcParams.update({"font.size": 10, "axes.spines.top": False, "axes.spines.right": False})
COL = {"ES": "#c0392b", "IT": "#e67e22", "FR": "#2e86c1", "DE": "#34495e", "HU": "#7d3c98"}

# 1. CDD and HDD, 1980-2025
c = pd.read_csv(os.path.join(ROOT, "data", "seasonality", "cdd_hdd_country_year.csv"))
fig, ax = plt.subplots(1, 2, figsize=(11, 4))
for cn, col in COL.items():
    g = c[c.country == cn].set_index("year")
    ax[0].plot(g.index, g.cdd, color=col, alpha=.35, lw=1)
    ax[0].plot(g.index, g.cdd.rolling(5, center=True).mean(), color=col, lw=2, label=cn)
    ax[1].plot(g.index, g.hdd_cal.rolling(5, center=True).mean() / g.loc[1980:1989, "hdd_cal"].mean() * 100, color=col, lw=2, label=cn)
ax[0].set_title("Cooling degree days, Apr-Sep\nCDD = max(T - 21, 0); thin = annual, thick = 5-yr mean")
ax[1].set_title("Heating degree days, calendar year\nHDD = max(14 - T, 0), 5-yr mean, 1980s = 100")
ax[0].legend(frameon=False, ncol=5); ax[1].axhline(100, color="grey", lw=.5)
fig.tight_layout(); fig.savefig(os.path.join(OUT, "fig_s1_cdd_hdd_longrun.png"), dpi=200)

# 2. EU gas-for-power by month
d = pd.read_csv(os.path.join(ROOT, "data", "seasonality", "eu_monthly_demand.csv"))
p = d.pivot(index="month", columns="year", values="power")
fig, ax = plt.subplots(figsize=(7, 4))
cm = plt.cm.Blues(np.linspace(.35, 1, len(p.columns)))
for (y, col) in zip(p.columns, cm):
    ax.plot(p.index, p[y], color="#c0392b" if y == 2026 else col, lw=2.5 if y == 2026 else 1.3, label=y)
ax.axvspan(3.5, 9.5, color="orange", alpha=.07); ax.set_xticks(range(1, 13))
ax.set_ylabel("TWh"); ax.set_title("EU gas demand for power, by month (shaded = gas summer)")
ax.legend(frameon=False, ncol=4, fontsize=8)
fig.tight_layout(); fig.savefig(os.path.join(OUT, "fig_s2_gas_for_power_monthly.png"), dpi=200)

# 3. TTF summer vs adjacent winters
t = pd.read_csv(os.path.join(ROOT, "data", "raw_data", "price", "ttf", "ttf_1month.csv"), encoding="utf-8-sig")
t.index = pd.to_datetime(t.Date, format="%m/%d/%Y"); t = t.Price.astype(float).sort_index()
a = pd.read_csv(os.path.join(ROOT, "data", "raw_data", "price", "acer_price.csv"))
a.index = pd.to_datetime(a.DATE); a = a["NORTH-WEST EUROPE PRICE (EUR/MWh)"].astype(float).sort_index()
m = pd.concat([t[:"2025-03-31"], a["2025-04-01":]], sort=True).resample("MS").mean().to_frame("p")
m["y"], m["mo"] = m.index.year, m.index.month
m["wy"] = np.where(m.mo >= 10, m.y, m.y - 1)
S = m[m.mo.between(4, 9)].groupby("y").p.mean(); W = m[~m.mo.between(4, 9)].groupby("wy").p.mean()
r = pd.DataFrame({"summer": S, "prev_winter": W.reindex(S.index - 1).values, "next_winter": W.reindex(S.index).values}, index=S.index)
r["ratio_prev"] = r.summer / r.prev_winter; r["ratio_next"] = r.summer / r.next_winter
r.round(2).to_csv(os.path.join(OUT, "tab_ttf_summer_winter.csv"))
fig, ax = plt.subplots(figsize=(7, 4))
x = np.arange(len(r)); ax.bar(x - .2, r.ratio_prev, .4, label="summer / preceding winter", color="#2e86c1")
ax.bar(x + .2, r.ratio_next, .4, label="summer / following winter", color="#c0392b")
ax.axhline(1, color="k", lw=.7); ax.set_xticks(x, [f"{y}{'*' if y == 2026 else ''}" for y in r.index])
ax.set_title("Front-month gas price: summer (Apr-Sep) relative to winter (Oct-Mar)\n>1 means summer gas was dearer; *2026 = Apr-May only")
ax.legend(frameon=False); fig.tight_layout(); fig.savefig(os.path.join(OUT, "fig_s3_ttf_summer_winter.png"), dpi=200)
print(r.round(2))
