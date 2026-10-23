"""EU monthly gas demand by sector (industry, household, power), for the talk's data section.

Input   data/raw_data/eu_monthly_demand/{industry,household,power}_eu-gas-demand-monthly-twh.xlsx
        Bruegel gas demand tracker, sheet "Monthly demand"; EU = sum of the countries in
        Bruegel's monthly total-demand series. TWh per month, Jan 2019 to Sep 2026.

Output  data/eu_monthly_demand.csv                  month x sector, wide, plus total (TWh)
        output/data_overview/tab_demand_year.csv    demand by calendar year and sector
        output/data_overview/fig7_demand_monthly.png  stacked monthly bars by sector
"""
import os

import matplotlib

matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import pandas as pd

from data_overview_storage import AQUA, BLUE, INK2, ORANGE, titles

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RAW = os.path.join(ROOT, "data", "raw_data", "eu_monthly_demand")
OUT = os.path.join(ROOT, "output", "data_overview")
SECTORS = {"industry": ("Industry", BLUE), "power": ("Power generation", AQUA),
           "household": ("Households", ORANGE)}


def load():
    d = pd.concat([pd.read_excel(os.path.join(RAW, f"{s}_eu-gas-demand-monthly-twh.xlsx"),
                                 sheet_name="Monthly demand") for s in SECTORS])
    d["date"] = pd.to_datetime(dict(year=d.Year, month=d.Month, day=1))
    w = d.pivot(index="date", columns="Type", values="Monthly demand (TWh)")[list(SECTORS)]
    assert w.notna().all().all(), "sector files cover different months"
    w["total"] = w.sum(axis=1)
    w.insert(0, "month", w.index.month)
    w.insert(0, "year", w.index.year)
    return w


def fig_monthly(w):
    fig, ax = plt.subplots(figsize=(11, 4.8))
    bottom = 0
    for s, (lab, col) in SECTORS.items():
        ax.bar(w.index, w[s], bottom=bottom, width=25, color=col, lw=0, label=lab, align="edge")
        bottom = bottom + w[s]
    ax.set_ylabel("TWh per month")
    ax.set_xlim(pd.Timestamp("2018-12-15"), w.index[-1] + pd.Timedelta(days=45))
    ax.set_ylim(0, 540)
    ax.legend(loc="upper right", ncol=3, fontsize=10)
    ax.grid(axis="x", visible=False)
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    titles(ax, "Who burns the gas",
           f"EU monthly gas demand by sector, Jan 2019 to {w.index[-1]:%b %Y} (Bruegel); "
           "the winter peak is households")
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, "fig7_demand_monthly.png"))
    plt.close(fig)


def main():
    w = load()
    w.round(2).to_csv(os.path.join(ROOT, "data", "eu_monthly_demand.csv"), date_format="%Y-%m")
    y = w.groupby("year")[list(SECTORS) + ["total"]].sum()
    y["months"] = w.groupby("year").size()
    y.round(1).to_csv(os.path.join(OUT, "tab_demand_year.csv"))
    print(y.round(0))
    fig_monthly(w)


if __name__ == "__main__":
    main()
