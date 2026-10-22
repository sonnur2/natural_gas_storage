"""Daily country-level temperature and HDD from one representative station per country.

For each country, the representative station is the one whose mean HDD
(weather_stations.csv, 2015-2025) is closest to the country's mean HDD
(the average of its stations' mean HDD). Only stations with at least
MIN_COVERAGE of days valid (q_tg == 0) over 2018-2025 are eligible, so the
chosen station gives a near-complete daily series. In countries where no
station reaches MIN_COVERAGE, the eligible stations are those within
MIN_COVERAGE of the country's best coverage; their series have gaps
(see coverage_2018_2025).

Output (data/weather/):
  country_representative_stations.csv   the chosen station per country
  weather_hdd_country_daily.csv         country x day, 2018-01-01 to 2025-12-31
"""
import os

import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WEATHER = os.path.join(ROOT, "data", "weather")

START, END = "2018-01-01", "2025-12-31"
MIN_COVERAGE = 0.95


def main():
    st = pd.read_csv(os.path.join(WEATHER, "weather_stations.csv"))
    panel = pd.read_parquet(os.path.join(WEATHER, "weather_hdd_daily.parquet"),
                            columns=["station_id", "date", "tg_c", "q_tg", "hdd"])
    panel = panel[(panel["date"] >= START) & (panel["date"] <= END) & (panel["q_tg"] == 0)]

    n_days = (pd.Timestamp(END) - pd.Timestamp(START)).days + 1
    cov = (panel.groupby("station_id").size() / n_days).rename("coverage_2018_2025")
    st = st.merge(cov, on="station_id", how="left").fillna({"coverage_2018_2025": 0})

    st["country_mean_hdd"] = st.groupby("country")["mean_hdd"].transform("mean")
    st["n_stations"] = st.groupby("country")["station_id"].transform("size")
    st["abs_dev"] = (st["mean_hdd"] - st["country_mean_hdd"]).abs()

    best = st.groupby("country")["coverage_2018_2025"].transform("max")
    floor = (MIN_COVERAGE * best).where(best < MIN_COVERAGE, MIN_COVERAGE)
    elig = st[(st["coverage_2018_2025"] >= floor) & (st["coverage_2018_2025"] > 0)]
    rep = elig.sort_values(["country", "abs_dev"]).groupby("country").head(1)
    low = rep.loc[rep["coverage_2018_2025"] < MIN_COVERAGE, "country"].tolist()
    if low:
        print(f"no station with >= {MIN_COVERAGE:.0%} coverage 2018-2025: {', '.join(low)}")

    rep = rep[["country", "station_id", "station_name", "lat", "lon", "elevation_m",
               "n_stations", "country_mean_hdd", "mean_hdd", "abs_dev", "coverage_2018_2025"]]
    rep = rep.rename(columns={"mean_hdd": "station_mean_hdd"}).round(4)
    rep.to_csv(os.path.join(WEATHER, "country_representative_stations.csv"), index=False)

    out = panel.merge(rep[["country", "station_id"]], on="station_id")
    out = out[["country", "date", "station_id", "tg_c", "hdd"]].sort_values(["country", "date"])
    out.to_csv(os.path.join(WEATHER, "weather_hdd_country_daily.csv"), index=False,
               date_format="%Y-%m-%d")
    print(f"countries: {len(rep)}; rows: {len(out):,}")


if __name__ == "__main__":
    main()
