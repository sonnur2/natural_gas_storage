# Quick checks and first plots for the ENTSO-E data
#
# Reads the tidy files written by code/entsoe_download.py:
#   data/entsoe/entsoe_<dataset>.csv  (or .csv.gz)
#   columns: datetime_utc, area, dataset, variable, value, unit, resolution
# Writes figures and tables to output/entsoe_check/.
#
# Packages: install.packages(c("data.table", "ggplot2"))
# Run: open in RStudio and Source, or Rscript code/entsoe_check.R from the repo folder.
# Times are UTC. Spain and central Europe are UTC+1 in winter and UTC+2 in summer.

library(data.table)
library(ggplot2)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()   # fall back to the current folder
DATA <- file.path(ROOT, "data", "entsoe")
OUT  <- file.path(ROOT, "output", "entsoe_check")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

STEP_H <- c(PT15M = 0.25, PT30M = 0.5, PT60M = 1)
COUNTRIES <- c("DE", "IT", "NL", "FR", "AT", "ES")
FUEL_GROUP <- c(gas = "Gas", nuclear = "Nuclear", solar = "Solar",
                wind_onshore = "Wind", wind_offshore = "Wind",
                hydro_run_of_river = "Hydro", hydro_reservoir = "Hydro", hydro_pumped = "Hydro",
                lignite = "Coal", hard_coal = "Coal", coal_gas = "Coal", oil = "Oil")
FUEL_COL <- c(Gas = "#d35400", Nuclear = "#8e44ad", Coal = "#2c3e50", Oil = "#7f8c8d",
              Hydro = "#2980b9", Wind = "#16a085", Solar = "#f1c40f", Other = "#bdc3c7")

save_plot <- function(p, name, w = 11, h = 6) {
  ggsave(file.path(OUT, name), p, width = w, height = h, dpi = 150, bg = "white")
  message("  saved ", name)
}

read_ds <- function(ds) {
  f <- file.path(DATA, paste0("entsoe_", ds, c(".csv", ".csv.gz")))
  f <- f[file.exists(f)][1]
  if (is.na(f)) { message("missing: ", ds); return(NULL) }
  message("reading ", basename(f))
  d <- fread(f, colClasses = list(character = "datetime_utc"))
  # cheap date parts from the text, so we avoid parsing millions of timestamps
  d[, `:=`(ym = substr(datetime_utc, 1, 7),
           year = as.integer(substr(datetime_utc, 1, 4)),
           month = as.integer(substr(datetime_utc, 6, 7)),
           hour_utc = as.integer(substr(datetime_utc, 12, 13)))]
  d[, month_date := as.IDate(paste0(ym, "-01"))]
  d[, step_h := STEP_H[resolution]]
  d
}

hours_in_month <- function(ym) {
  start <- as.IDate(paste0(ym, "-01"))
  end <- seq(start, by = "month", length.out = 2)[2]
  as.numeric(end - start) * 24
}

# coverage = hours with data / hours in the month, for sub-daily series
coverage <- function(d, label) {
  cv <- d[!is.na(step_h), .(hours = sum(step_h)), by = .(area, variable, ym, month_date)]
  cv[, share := pmin(hours / hours_in_month(ym[1]), 1), by = ym]
  cv[, dataset := label]
  cv
}

summary_rows <- list()
add_summary <- function(cv) {
  s <- cv[, .(first_month = min(ym), last_month = max(ym), n_months = .N,
              months_95pct = sum(share >= 0.95), mean_coverage = round(mean(share), 3)),
          by = .(dataset, area, variable)]
  summary_rows[[length(summary_rows) + 1]] <<- s
}

# ------------------------------------------------------------------ generation
gen <- read_ds("generation")
if (!is.null(gen)) {
  gen <- gen[!grepl("_consumption$", variable)]
  dups <- gen[, .N, by = .(datetime_utc, area, variable)][N > 1, .N]
  message("  duplicate generation rows: ", dups)
  gen[, fuel := fifelse(variable %in% names(FUEL_GROUP), FUEL_GROUP[variable], "Other")]
  gen[, mwh := value * step_h]

  cv <- coverage(gen, "generation"); add_summary(cv)
  # each tile spans its whole month (default tiles are 28 days wide and leave white gaps)
  cv[, days := hours_in_month(ym[1]) / 24, by = ym]
  p <- ggplot(cv, aes(as.Date(month_date) + days / 2, variable, fill = share)) + geom_tile(aes(width = days)) +
    facet_wrap(~area, ncol = 2, scales = "free_y") +
    scale_fill_gradient(low = "#fdecea", high = "#1e8449", limits = c(0, 1)) +
    labs(title = "Generation: share of hours with data, by fuel and month", x = NULL, y = NULL, fill = "coverage") +
    theme_minimal(base_size = 9)
  save_plot(p, "01_coverage_generation.png", h = 9)

  mix <- gen[, .(twh = sum(mwh, na.rm = TRUE) / 1e6), by = .(area, month_date, fuel)]
  mix[, fuel := factor(fuel, levels = names(FUEL_COL))]
  p <- ggplot(mix, aes(month_date, twh, fill = fuel)) + geom_area() +
    facet_wrap(~area, ncol = 2, scales = "free_y") + scale_fill_manual(values = FUEL_COL) +
    labs(title = "Monthly generation by fuel (TWh)", x = NULL, y = "TWh", fill = NULL) + theme_minimal()
  save_plot(p, "02_generation_mix_monthly.png", h = 8)

  gas <- mix[fuel == "Gas"]
  p <- ggplot(gas, aes(month_date, twh, colour = area)) + geom_line() +
    labs(title = "Gas-fired generation by country (TWh per month)", x = NULL, y = "TWh", colour = NULL) +
    theme_minimal()
  save_plot(p, "03_gas_generation_monthly.png", h = 5)

  # seasonal profile: average gas TWh by calendar month, one line per year
  gm <- gas[, .(twh = sum(twh)), by = .(area, year = year(month_date), m = month(month_date))]
  p <- ggplot(gm, aes(m, twh, group = year, colour = year)) + geom_line() +
    facet_wrap(~area, ncol = 3, scales = "free_y") + scale_x_continuous(breaks = 1:12) +
    scale_colour_viridis_c() +
    labs(title = "Gas-fired generation by calendar month, one line per year (TWh)", x = "month", y = "TWh") +
    theme_minimal()
  save_plot(p, "04_gas_seasonal_profile.png", h = 6)

  # average day, January vs August, last full year
  yr <- max(gen[month == 12, year])
  prof <- gen[year == yr & month %in% c(1, 8),
              .(mw = sum(value)), by = .(area, datetime_utc, month, hour_utc, fuel)][
              , .(mw = mean(mw)), by = .(area, month, hour_utc, fuel)]
  prof[, `:=`(season = factor(fifelse(month == 1, "January", "August"), levels = c("January", "August")), fuel = factor(fuel, levels = names(FUEL_COL)))]
  p <- ggplot(prof, aes(hour_utc, mw / 1000, fill = fuel)) + geom_area() +
    facet_grid(area ~ season, scales = "free_y") + scale_fill_manual(values = FUEL_COL) +
    labs(title = paste("Average day by fuel,", yr, "(GW, hour in UTC)"), x = "hour (UTC)", y = "GW", fill = NULL) +
    theme_minimal(base_size = 9)
  save_plot(p, "05_average_day_jan_aug.png", w = 9, h = 11)
}

# ------------------------------------------------------------------ prices
pr <- read_ds("prices")
if (!is.null(pr)) {
  cv <- coverage(pr, "prices"); add_summary(cv)
  pm <- pr[, .(price = mean(value, na.rm = TRUE), p_min = min(value), p_max = max(value)), by = .(area, month_date)]
  main <- c("DE_AT_LU", "DE_LU", "AT", "NL", "FR", "ES", "IT_NORD")
  p <- ggplot(pm[area %in% main], aes(month_date, price, colour = area)) + geom_line() +
    labs(title = "Monthly average day-ahead power price (EUR/MWh)", x = NULL, y = "EUR/MWh", colour = NULL) +
    theme_minimal()
  save_plot(p, "06_prices_monthly.png", h = 5)
  p <- ggplot(pm[grepl("^IT", area)], aes(month_date, price, colour = area)) + geom_line() +
    labs(title = "Italian zonal prices, monthly average (EUR/MWh)", x = NULL, y = "EUR/MWh", colour = NULL) +
    theme_minimal()
  save_plot(p, "07_prices_italy_zones.png", h = 5)
}

# ------------------------------------------------------------------ load
ld <- read_ds("load")
if (!is.null(ld)) {
  cv <- coverage(ld, "load"); add_summary(cv)
  lm_ <- ld[, .(gw = mean(value, na.rm = TRUE) / 1000), by = .(area, month_date)]
  p <- ggplot(lm_, aes(month_date, gw, colour = area)) + geom_line() +
    labs(title = "Average electricity load by month (GW)", x = NULL, y = "GW", colour = NULL) + theme_minimal()
  save_plot(p, "08_load_monthly.png", h = 5)
}

# ------------------------------------------------------------------ hydro reservoirs (weekly)
hy <- read_ds("hydro")
if (!is.null(hy)) {
  hy[, date := as.IDate(substr(datetime_utc, 1, 10))]
  s <- hy[, .(first_month = min(ym), last_month = max(ym), n_months = uniqueN(ym), months_95pct = NA_integer_,
              mean_coverage = NA_real_), by = .(area, variable)][, dataset := "hydro"]
  summary_rows[[length(summary_rows) + 1]] <- s
  p <- ggplot(hy, aes(date, value / 1e6)) + geom_line(colour = "#2980b9") +
    facet_wrap(~area, scales = "free_y") +
    labs(title = "Hydro reservoir stored energy, weekly (TWh)", x = NULL, y = "TWh") + theme_minimal()
  save_plot(p, "09_hydro_reservoirs.png", h = 5)
}

# ------------------------------------------------------------------ installed capacity (yearly)
cp <- read_ds("capacity")
if (!is.null(cp)) {
  cp[, fuel := fifelse(variable %in% names(FUEL_GROUP), FUEL_GROUP[variable], "Other")]
  cy <- cp[, .(gw = sum(value, na.rm = TRUE) / 1000), by = .(area, year, fuel)]
  cy[, fuel := factor(fuel, levels = names(FUEL_COL))]
  p <- ggplot(cy, aes(year, gw, fill = fuel)) + geom_col() +
    facet_wrap(~area, scales = "free_y") + scale_fill_manual(values = FUEL_COL) +
    labs(title = "Installed capacity by fuel (GW, start of year)", x = NULL, y = "GW", fill = NULL) + theme_minimal()
  save_plot(p, "10_installed_capacity.png", h = 6)
}

# ------------------------------------------------------------------ cross-border flows
fl <- read_ds("flows")
if (!is.null(fl)) {
  cv <- coverage(fl, "flows"); add_summary(cv)
  fm <- fl[, .(mw = mean(value, na.rm = TRUE)), by = .(area, month_date)]
  fm[, c("from", "to") := tstrsplit(area, "_")]
  fm[, pair := fifelse(from < to, paste(from, to, sep = "-"), paste(to, from, sep = "-"))]
  fm[, sign := fifelse(from < to, 1, -1)]
  net <- fm[, .(net_mw = sum(sign * mw)), by = .(pair, month_date)]
  p <- ggplot(net, aes(month_date, net_mw / 1000)) + geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(colour = "#34495e") + facet_wrap(~pair, scales = "free_y") +
    labs(title = "Net physical flow by border, monthly average (GW; positive = first country exports)",
         x = NULL, y = "GW") + theme_minimal()
  save_plot(p, "11_net_flows.png", h = 6)
}

# ------------------------------------------------------------------ summary table
if (length(summary_rows)) {
  smry <- rbindlist(summary_rows, use.names = TRUE, fill = TRUE)
  setcolorder(smry, c("dataset", "area", "variable"))
  fwrite(smry, file.path(OUT, "coverage_summary.csv"))
  message("\nCoverage summary (", file.path(OUT, "coverage_summary.csv"), "):")
  print(smry[order(dataset, area, variable)], nrows = 300)
}
message("\nDone. Figures are in ", OUT)
