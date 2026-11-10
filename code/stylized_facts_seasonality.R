# Stylized facts: is "gas demand is low in summer" becoming blurry?
#
# Time-evolving descriptives, in four blocks:
#   A  climate      heatwave days, summer Tmax, hot days, CDD vs HDD          (1961/1980-2025, 6 countries)
#   B  gas demand   EU monthly gas demand by sector: summer/winter ratio,
#                   power's share of summer demand, the July-August power bump (2019-2026, EU)
#   C  storage      injection profile through the summer, by year             (2016-2025, 6 countries)
#   D  power        load and generation by month and year                     (2015-2025, 6 countries; needs the panel)
#
# Inputs
#   data/weather/tmax_country_daily.csv.gz        build_tmax_heatwave.R
#   data/seasonality/cdd_hdd_country_year.csv     country-mean CDD (Apr-Sep, T_mean > 21) and HDD, 1980-2025
#   data/seasonality/eu_monthly_demand.csv        EU gas demand by sector, TWh per month, 2019-2026
#   data/storage_weather_panel.csv.gz             AGSI+ facility x day
#   data/panel/country_day_panel.csv.gz           build_country_day_panel.R (block D is skipped if missing)
# Output: output/stylized_facts/  figures (png) and tables (csv)
#
# Packages: install.packages(c("data.table", "ggplot2"))
# Run: Source in RStudio, or Rscript code/stylized_facts_seasonality.R from the repo folder.

library(data.table)
library(ggplot2)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()   # fall back to the current folder
OUT <- file.path(ROOT, "output", "stylized_facts")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

COUNTRIES <- c("DE", "IT", "NL", "FR", "AT", "ES")
CCOL <- c(DE = "#2c3e50", IT = "#27ae60", NL = "#e67e22", FR = "#2980b9", AT = "#c0392b", ES = "#f1c40f")
SUMMER <- 4:9   # gas summer, April-September

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                  strip.text = element_text(face = "bold")))
save_plot <- function(p, name, w = 11, h = 6.5) {
  if (!is.null(p$labels$subtitle))
    p$labels$subtitle <- paste(strwrap(p$labels$subtitle, width = round(w * 13)), collapse = "\n")
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 150, bg = "white")
  message("  saved ", name, ".png")
}
# linear trend per decade with its standard error (OLS; descriptive)
trend <- function(y, x) {
  ok <- !is.na(y) & !is.na(x)
  if (sum(ok) < 5) return(list(slope_per_decade = NA_real_, se = NA_real_))
  m <- summary(lm(y[ok] ~ x[ok]))$coefficients
  list(slope_per_decade = 10 * m[2, 1], se = 10 * m[2, 2])
}
trends <- list()   # collected into one table at the end
add_trend <- function(d, metric, unit, period) {
  r <- d[, trend(value, year), by = country]
  trends[[length(trends) + 1]] <<- cbind(metric = metric, unit = unit, period = period, r)
}

# ================================================================ A. climate
message("A. climate ...")
tx <- fread(file.path(ROOT, "data", "weather", "tmax_country_daily.csv.gz"))[country %in% COUNTRIES]
tx[, date := as.IDate(date)]
tx[, `:=`(year = year(date), month = month(date), country = factor(country, levels = COUNTRIES))]
tx <- tx[year <= 2025]

# per country-year: heatwave days, events, mean length, first/last heatwave day, hot days, summer Tmax
yr <- tx[, .(heatwave_days = sum(heatwave_day, na.rm = TRUE),
             heatwaves = sum(heatwave_onset, na.rm = TRUE),
             days_tx_ge30 = sum(tx_c >= 30, na.rm = TRUE),
             days_tx_ge35 = sum(tx_c >= 35, na.rm = TRUE),
             tx_jja = mean(tx_c[month %in% 6:8], na.rm = TRUE),
             first_hw = if (any(heatwave_day %in% TRUE)) yday(min(date[heatwave_day %in% TRUE])) else NA_integer_,
             last_hw  = if (any(heatwave_day %in% TRUE)) yday(max(date[heatwave_day %in% TRUE])) else NA_integer_,
             hw_days_outside_jja = sum(heatwave_day & !(month %in% 6:8), na.rm = TRUE),
             jja_coverage = mean(!is.na(tx_c[month %in% 6:8]))), by = .(country, year)]
yr[, tx_jja_anom := tx_jja - mean(tx_jja[year %between% c(1961, 1990)], na.rm = TRUE), by = country]
yr[, decade := paste0(10 * (year %/% 10), "s")]
fwrite(yr, file.path(OUT, "A_climate_country_year.csv"))

dec <- yr[, .(heatwave_days = round(mean(heatwave_days), 1), heatwaves = round(mean(heatwaves), 2),
              days_tx_ge30 = round(mean(days_tx_ge30), 1), tx_jja_anom = round(mean(tx_jja_anom, na.rm = TRUE), 2),
              hw_days_outside_jja = round(mean(hw_days_outside_jja), 1)), by = .(country, decade)]
fwrite(dec, file.path(OUT, "A_climate_by_decade.csv"))

for (v in c("heatwave_days", "heatwaves", "days_tx_ge30", "tx_jja_anom"))
  add_trend(yr[, .(country, year, value = get(v))], v, c(heatwave_days = "days/yr", heatwaves = "events/yr",
            days_tx_ge30 = "days/yr", tx_jja_anom = "C")[[v]], "1961-2025")
add_trend(yr[year >= 1996, .(country, year, value = heatwave_days)], "heatwave_days", "days/yr", "1996-2025")

# figA1: heatwave days per year, with 10-year rolling mean
yr[, hw_roll10 := frollmean(heatwave_days, 10, align = "right"), by = country]
save_plot(ggplot(yr, aes(year, heatwave_days)) +
            geom_col(fill = "#e67e22", alpha = 0.6, width = 0.9) +
            geom_line(aes(y = hw_roll10), colour = "#c0392b", linewidth = 1, na.rm = TRUE) +
            facet_wrap(~country, scales = "free_y") +
            labs(x = NULL, y = "heatwave days per year",
                 title = "A1. Heatwave days per year, 1961-2025",
                 subtitle = "Heatwave: 3+ consecutive days with Tmax above the 1961-1990 98th percentile and above 28 C. Red: 10-year rolling mean"),
          "figA1_heatwave_days")

# figA2: summer Tmax anomaly and days >= 30 C
a2 <- melt(yr[, .(country, year, `JJA mean Tmax, anomaly vs 1961-1990 (C)` = tx_jja_anom, `days with Tmax >= 30 C` = as.numeric(days_tx_ge30))],
           id.vars = c("country", "year"))
save_plot(ggplot(a2, aes(year, value, colour = country)) +
            geom_line(alpha = 0.35) + geom_smooth(method = "loess", span = 0.4, se = FALSE, linewidth = 1) +
            scale_colour_manual(values = CCOL) + facet_wrap(~variable, scales = "free_y") +
            labs(x = NULL, y = NULL, colour = NULL, title = "A2. Hotter summers: mean summer Tmax and hot days, 1961-2025",
                 subtitle = "Thin lines: yearly values; thick: loess smooth"),
          "figA2_summer_tmax", h = 5)

# figA3: the heatwave season gets longer; first and last heatwave day of each year
a3 <- yr[!is.na(first_hw)]
save_plot(ggplot(a3, aes(year)) +
            geom_linerange(aes(ymin = as.Date(first_hw - 1, origin = "2001-01-01"), ymax = as.Date(last_hw - 1, origin = "2001-01-01")),
                           colour = "#e67e22", linewidth = 1.2) +
            scale_y_date(date_labels = "%d %b") + facet_wrap(~country) +
            labs(x = NULL, y = NULL, title = "A3. Heatwave season: first to last heatwave day in each year",
                 subtitle = "Years without a heatwave are blank"),
          "figA3_heatwave_season")

# figA4: CDD vs HDD (country means over stations), 1980-2025
cdd <- fread(file.path(ROOT, "data", "seasonality", "cdd_hdd_country_year.csv"))[country %in% COUNTRIES & year <= 2025]
cdd[, country := factor(country, levels = COUNTRIES)]
cdd[, `:=`(cdd_idx = 100 * cdd / mean(cdd[year %between% c(1981, 1990)]),
           hdd_idx = 100 * hdd_cal / mean(hdd_cal[year %between% c(1981, 1990)])), by = country]
cdd[, cdd_per_1000_hdd := 1000 * cdd / hdd_cal]
add_trend(cdd[, .(country, year, value = cdd)], "cdd_apr_sep", "degree-days", "1980-2025")
add_trend(cdd[, .(country, year, value = hdd_cal)], "hdd_year", "degree-days", "1980-2025")
add_trend(cdd[, .(country, year, value = cdd_per_1000_hdd)], "cdd_per_1000_hdd", "ratio", "1980-2025")
a4 <- melt(cdd[, .(country, year, `CDD (Apr-Sep)` = cdd_idx, `HDD (calendar year)` = hdd_idx)], id.vars = c("country", "year"))
save_plot(ggplot(a4, aes(year, value, colour = variable)) +
            geom_hline(yintercept = 100, colour = "grey60") +
            geom_line(alpha = 0.35) + geom_smooth(method = "loess", span = 0.4, se = FALSE, linewidth = 1) +
            scale_colour_manual(values = c("CDD (Apr-Sep)" = "#c0392b", "HDD (calendar year)" = "#2980b9")) +
            facet_wrap(~country, scales = "free_y") +
            labs(x = NULL, y = "index, 1981-1990 = 100", colour = NULL,
                 title = "A4. Cooling needs up, heating needs down, 1980-2025",
                 subtitle = "Country mean over ECA&D stations. CDD = max(T_mean - 21, 0) summed over April-September; HDD = max(14 - T_mean, 0)"),
          "figA4_cdd_hdd_index")

# ================================================================ B. EU gas demand by sector
message("B. EU gas demand ...")
dm <- fread(file.path(ROOT, "data", "seasonality", "eu_monthly_demand.csv"))
dm[, gas_year := fifelse(month >= 10, year, year - 1L)]   # gas year starting 1 October
dml <- melt(dm[, .(year, month, gas_year, industry, power, household, total)],
            id.vars = c("year", "month", "gas_year"), variable.name = "sector", value.name = "twh")

# figB1: monthly profile by year, total and power
save_plot(ggplot(dml[sector %in% c("total", "power", "household", "industry")], aes(month, twh, colour = factor(year), group = year)) +
            annotate("rect", xmin = 6.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "#fdebd0", alpha = 0.6) +
            geom_line(linewidth = 0.8) + scale_colour_viridis_d(option = "C", end = 0.9) +
            scale_x_continuous(breaks = 1:12, labels = substr(month.abb, 1, 1)) +
            facet_wrap(~sector, scales = "free_y", nrow = 1) +
            labs(x = NULL, y = "TWh per month", colour = NULL,
                 title = "B1. EU gas demand by month and sector, 2019-2026",
                 subtitle = "Household demand carries the winter peak; power demand has its own peak in July-August (shaded)"),
          "figB1_eu_demand_profile", h = 4.8)

# summer/winter ratio by gas year and sector (full gas years only), power share of summer demand,
# and the July-August bump in power demand relative to May-June
gy <- dml[, .(n = .N, summer = sum(twh[month %in% SUMMER]), winter = sum(twh[!month %in% SUMMER])), by = .(gas_year, sector)][n == 12]
gy[, summer_winter_ratio := summer / winter]
sh <- dm[, .(n = .N, power_share_summer = sum(power[month %in% SUMMER]) / sum(total[month %in% SUMMER]),
             power_share_winter = sum(power[!month %in% SUMMER]) / sum(total[!month %in% SUMMER])), by = gas_year][n == 12]
bump <- dm[, .(n = sum(month %in% 5:8), power_bump = sum(power[month %in% 7:8]) / sum(power[month %in% 5:6]),
               total_bump = sum(total[month %in% 7:8]) / sum(total[month %in% 5:6])), by = year][n == 4]
fwrite(dcast(gy, gas_year ~ sector, value.var = "summer_winter_ratio"), file.path(OUT, "B_summer_winter_ratio.csv"))
fwrite(merge(sh, bump, by.x = "gas_year", by.y = "year", all = TRUE), file.path(OUT, "B_power_share_and_bump.csv"))

b2 <- rbind(gy[, .(x = gas_year, value = summer_winter_ratio, series = paste("summer/winter,", sector), panel = "summer / winter demand (gas year)")],
            sh[, .(x = gas_year, value = power_share_summer, series = "power share, summer", panel = "power share of total demand")],
            sh[, .(x = gas_year, value = power_share_winter, series = "power share, winter", panel = "power share of total demand")],
            bump[, .(x = year, value = power_bump, series = "power: Jul-Aug / May-Jun", panel = "July-August bump")],
            bump[, .(x = year, value = total_bump, series = "total: Jul-Aug / May-Jun", panel = "July-August bump")])
save_plot(ggplot(b2, aes(x, value, colour = series)) + geom_line() + geom_point() +
            facet_wrap(~panel, scales = "free_y") +
            labs(x = "gas year (starting 1 October) or calendar year", y = NULL, colour = NULL,
                 title = "B2. Is the summer trough filling in? EU gas demand, 2019-2026",
                 subtitle = "Summer = April-September. A ratio rising towards 1 means a flatter year; a bump above 1 means July-August power demand exceeds May-June"),
          "figB2_eu_seasonality_metrics", h = 5)

# ================================================================ C. storage injections
message("C. storage ...")
st <- fread(file.path(ROOT, "data", "storage_weather_panel.csv.gz"),
            select = c("country", "date", "injection", "withdrawal", "gasInStorage", "workingGasVolume"))
st <- st[country %in% COUNTRIES][, date := as.IDate(date)]
st <- st[date >= as.IDate("2016-04-06")]   # German sites enter AGSI+ on 6 April 2016
sm <- st[, .(inj_twh = sum(injection, na.rm = TRUE) / 1000, wd_twh = sum(withdrawal, na.rm = TRUE) / 1000),
         by = .(year = year(date), month = month(date))]
sm[, net_twh := inj_twh - wd_twh]
cy <- st[, .(storage_twh = sum(gasInStorage, na.rm = TRUE), wgv = sum(workingGasVolume, na.rm = TRUE)), by = date]
cy[, fill_pct := 100 * storage_twh / wgv]
prof <- sm[year >= 2017 & month %in% SUMMER]
prof[, share_of_summer_net := net_twh / sum(net_twh), by = year]
inj_bump <- sm[year >= 2017, .(jul_aug_vs_may_jun = sum(net_twh[month %in% 7:8]) / sum(net_twh[month %in% 5:6]),
                               summer_net_twh = sum(net_twh[month %in% SUMMER])), by = year]
fwrite(inj_bump, file.path(OUT, "C_injection_profile.csv"))
save_plot(ggplot(prof, aes(month, net_twh, colour = factor(year), group = year)) + geom_line(linewidth = 0.8) + geom_point(size = 1) +
            scale_colour_viridis_d(option = "C", end = 0.9) + scale_x_continuous(breaks = SUMMER, labels = month.abb[SUMMER]) +
            labs(x = NULL, y = "net injection, TWh per month", colour = NULL,
                 title = "C1. Summer injection profile, six countries, 2017-2025",
                 subtitle = "DE, IT, NL, FR, AT, ES summed (AGSI+). Does injection slow down in July-August, when power burn peaks?"),
          "figC1_injection_profile", h = 5.5)

# ================================================================ D. power system (needs the panel)
PANEL <- file.path(ROOT, "data", "panel", "country_day_panel.csv.gz")
if (file.exists(PANEL)) {
  message("D. power system ...")
  p <- fread(PANEL)[country %in% COUNTRIES]
  p[, `:=`(date = as.IDate(date), country = factor(country, levels = COUNTRIES))]
  pm <- p[, .(load = mean(load_gwh, na.rm = TRUE), load_peak = max(load_peak_mw, na.rm = TRUE) / 1000,
              gas = mean(gen_gas_gwh, na.rm = TRUE), solar = mean(gen_solar_gwh, na.rm = TRUE),
              nuclear = mean(gen_nuclear_gwh, na.rm = TRUE), hydro = mean(gen_hydro_gwh, na.rm = TRUE),
              cdd = sum(cdd, na.rm = TRUE), heatwave_days = sum(heatwave_day, na.rm = TRUE)),
          by = .(country, year, month)]
  for (v in names(pm)[-(1:3)]) pm[!is.finite(get(v)), (v) := NA]

  # figD1: monthly load and gas generation profiles, one line per year
  d1 <- melt(pm[, .(country, year, month, `load, GWh/day` = load, `gas generation, GWh/day` = gas)], id.vars = c("country", "year", "month"))
  save_plot(ggplot(d1, aes(month, value, colour = year, group = year)) +
              annotate("rect", xmin = 6.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "#fdebd0", alpha = 0.6) +
              geom_line(linewidth = 0.5) +
              scale_colour_viridis_c(option = "C", end = 0.9, breaks = c(2015, 2020, 2025),
                                     guide = guide_colourbar(barwidth = 12)) +
              scale_x_continuous(breaks = 1:12, labels = substr(month.abb, 1, 1)) +
              facet_wrap(variable ~ country, scales = "free_y", nrow = 2) +
              labs(x = NULL, y = NULL, colour = NULL, title = "D1. Electricity load and gas-fired generation by month, 2015-2025",
                   subtitle = "Monthly mean of daily values; one line per year (dark = early, yellow = late). July-August shaded"),
            "figD1_load_gas_profiles", w = 13, h = 6)

  # seasonality metrics per country-year: summer/winter ratios and the July-August bump
  ym <- pm[, .(n = .N,
               load_sw = mean(load[month %in% SUMMER]) / mean(load[!month %in% SUMMER]),
               gas_sw = mean(gas[month %in% SUMMER]) / mean(gas[!month %in% SUMMER]),
               load_jja_vs_djf = mean(load[month %in% 6:8]) / mean(load[month %in% c(12, 1, 2)]),
               gas_bump = mean(gas[month %in% 7:8]) / mean(gas[month %in% 5:6]),
               peak_summer_vs_winter = max(load_peak[month %in% 6:8]) / max(load_peak[month %in% c(12, 1, 2)]),
               solar_summer = mean(solar[month %in% SUMMER]),
               cdd = sum(cdd), heatwave_days = sum(heatwave_days)), by = .(country, year)][n == 12]
  fwrite(ym, file.path(OUT, "D_power_seasonality_country_year.csv"))
  for (v in c("load_sw", "gas_sw", "gas_bump", "peak_summer_vs_winter"))
    add_trend(ym[, .(country, year, value = get(v))], v, "ratio", "2015-2025")

  d2 <- melt(ym[, .(country, year, `load: summer / winter` = load_sw, `gas generation: summer / winter` = gas_sw,
                    `gas generation: Jul-Aug / May-Jun` = gas_bump, `peak load: summer max / winter max` = peak_summer_vs_winter)],
             id.vars = c("country", "year"))
  save_plot(ggplot(d2, aes(year, value, colour = country)) +
              geom_hline(yintercept = 1, colour = "grey60") + geom_line() + geom_point(size = 1) +
              scale_colour_manual(values = CCOL) + facet_wrap(~variable, scales = "free_y") +
              scale_x_continuous(breaks = seq(2015, 2025, 2)) +
              labs(x = NULL, y = "ratio", colour = NULL,
                   title = "D2. Is the power system's summer trough filling in? 2015-2025",
                   subtitle = "Ratios of monthly means (summer = April-September, winter = October-March) and of maximum hourly load. 1 = no seasonality"),
            "figD2_power_seasonality_metrics")

  # figD3: does a hot summer show up in the July-August gas bump? (country-years)
  save_plot(ggplot(ym, aes(heatwave_days, gas_bump, colour = country)) +
              geom_hline(yintercept = 1, colour = "grey60") + geom_point(size = 2) +
              geom_text(aes(label = substr(year, 3, 4)), vjust = -0.8, size = 2.6, show.legend = FALSE) +
              scale_colour_manual(values = CCOL) +
              labs(x = "heatwave days in the year", y = "gas generation, Jul-Aug / May-Jun", colour = NULL,
                   title = "D3. Hot summers and the July-August gas bump, country-years 2015-2025"),
            "figD3_bump_vs_heatwaves", w = 9, h = 6)
} else message("D. skipped: ", PANEL, " not found (run build_country_day_panel.R)")

# ================================================================ trend table
tt <- rbindlist(trends)
tt[, `:=`(slope_per_decade = round(slope_per_decade, 3), se = round(se, 3))]
fwrite(tt, file.path(OUT, "trends_per_decade.csv"))
print(dcast(tt, metric + period ~ country, value.var = "slope_per_decade"))
message("done -> ", OUT)
