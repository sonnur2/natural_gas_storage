# Country x day analysis panel: power system (ENTSO-E), weather, storage, gas prices
#
# Countries: DE, IT, NL, FR, AT, ES. Days: 2015-01-01 to 2025-12-31 (calendar days in
# Central European time, Europe/Brussels, which all six countries share).
#
# Inputs
#   data/entsoe/entsoe_<dataset>.csv[.gz]   from code/entsoe_download.py (local only, git-ignored)
#   data/raw_data/weather/TG_STAID*.txt     mean temperature of the representative station
#   data/weather/country_representative_stations.csv
#   data/weather/tmax_country_daily.csv.gz  max temperature + heatwave flags (build_tmax_heatwave.R)
#   data/storage_weather_panel.csv.gz       AGSI+ facility x day panel
#   data/raw_data/price/ttf/ttf_1month.csv, data/raw_data/price/acer_price.csv   gas prices
#
# Output
#   data/panel/country_day_panel.csv.gz   (about 24,000 rows, small enough to commit)
#   data/panel/codebook.csv               one line per column: share non-missing, first and last date
#
# Conventions
#   energy in GWh per day (MW x hours / 1000); peaks in MW
#   CDD = max(T_mean - 21, 0); HDD = max(14 - T_mean, 0)
#   heatwave: >= 3 days with T_max above the station's 1961-1990 98th percentile and above 28C
#   storage fill = gas in storage / working gas volume (%)
#   prices: DE and AT use the joint DE-AT-LU zone before 1 Oct 2018; IT uses the North zone
#   if a series comes at two resolutions on the same day (e.g. hourly, then 15-min), the finer one is kept
#
# Packages: install.packages(c("data.table", "R.utils"))   # R.utils lets fread/fwrite handle .gz
# Run: Source in RStudio, or Rscript code/build_country_day_panel.R from the repo folder.
# Memory: the generation file is the big one; reading it needs a few GB of RAM.

library(data.table)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()   # fall back to the current folder
ENTSOE  <- Sys.getenv("ENTSOE_DIR", file.path(ROOT, "data", "entsoe"))
WEATHER <- file.path(ROOT, "data", "weather")
OUT     <- file.path(ROOT, "data", "panel")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

COUNTRIES <- c("DE", "IT", "NL", "FR", "AT", "ES")
START <- as.IDate("2015-01-01")
END   <- as.IDate("2025-12-31")
TZ    <- "Europe/Brussels"
STEP_H  <- c(PT15M = 0.25, PT30M = 0.5, PT60M = 1)
RES_ORD <- c(PT15M = 0L, PT30M = 1L, PT60M = 2L)   # finer first

FUEL <- c(gas = "gas", nuclear = "nuclear", solar = "solar",
          wind_onshore = "wind", wind_offshore = "wind",
          hydro_run_of_river = "hydro_ror", hydro_reservoir = "hydro_res", hydro_pumped = "hydro_pump",
          lignite = "coal", hard_coal = "coal", coal_gas = "coal", oil = "oil")   # anything else -> other
PRICE_ZONE <- c(NL = "NL", FR = "FR", ES = "ES", IT = "IT_NORD")

sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)   # NA if nothing reported

ffill_limit <- function(x, k) {   # carry the last value forward at most k steps
  idx <- cummax(seq_along(x) * !is.na(x))
  out <- x[ifelse(idx == 0, NA, idx)]
  out[seq_along(x) - idx > k] <- NA
  out
}

# ---------------------------------------------------------------- ENTSO-E
read_entsoe <- function(ds, cols = c("datetime_utc", "area", "variable", "value", "resolution")) {
  f <- file.path(ENTSOE, paste0("entsoe_", ds, c(".csv", ".csv.gz")))
  f <- f[file.exists(f)][1]
  if (is.na(f)) { message("  missing ENTSO-E file for ", ds, "; its columns will be empty"); return(NULL) }
  d <- fread(f, select = cols, colClasses = list(character = "datetime_utc"))
  # timestamps look like "2015-01-01 00:00:00+00:00"; convert each distinct one once
  u <- data.table(datetime_utc = unique(d$datetime_utc))
  t <- as.POSIXct(substr(u$datetime_utc, 1, 19), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  loc <- format(t, "%Y-%m-%d %H", tz = TZ)
  u[, `:=`(date = as.IDate(substr(loc, 1, 10)), hour = as.integer(substr(loc, 12, 13)))]
  d <- u[d, on = "datetime_utc"][, datetime_utc := NULL]
  d[date >= START & date <= END]
}

finest <- function(d, keys) {   # keep the finest resolution within each group
  d[, r_ := RES_ORD[resolution]]
  d <- d[d[, .I[r_ == min(r_)], by = keys]$V1]
  d[, r_ := NULL][]
}

generation <- function() {
  d <- read_entsoe("generation")
  if (is.null(d)) return(NULL)
  d <- d[area %in% COUNTRIES & resolution %in% names(STEP_H) & !endsWith(variable, "_consumption")]
  d[, fuel := FUEL[variable]][is.na(fuel), fuel := "other"]
  d[, gwh := value * STEP_H[resolution] / 1000]
  g <- d[, .(gwh = sum(gwh), mw_max = max(value)), by = .(area, date, variable, fuel, resolution)]
  ev <- d[fuel == "gas" & hour >= 18 & hour <= 22,             # evening ramp, 18:00-22:59
          .(gwh = sum(gwh), mw_max = NA_real_), by = .(area, date, variable, resolution)][, fuel := "gas_evening"]
  rm(d); gc()
  g <- finest(rbind(g, ev, use.names = TRUE), c("area", "date", "variable", "fuel"))
  w <- dcast(g, area + date ~ fuel, value.var = "gwh", fun.aggregate = sum_na, fill = NA)
  fuels <- setdiff(names(w), c("area", "date"))
  setnames(w, fuels, paste0("gen_", fuels, "_gwh"))
  hyd <- intersect(paste0("gen_", c("hydro_ror", "hydro_res", "hydro_pump"), "_gwh"), names(w))
  w[, gen_hydro_gwh := apply(.SD, 1, sum_na), .SDcols = hyd]
  tot <- intersect(paste0("gen_", c("gas", "nuclear", "solar", "wind", "hydro", "coal", "oil", "other"), "_gwh"), names(w))
  w[, gen_total_gwh := apply(.SD, 1, sum_na), .SDcols = tot]
  w[, gas_share_pct := 100 * gen_gas_gwh / gen_total_gwh]
  peak <- g[fuel == "gas", .(gen_gas_peak_mw = max(mw_max)), by = .(area, date)]
  w <- peak[w, on = .(area, date)]
  setnames(w, "area", "country")
  setcolorder(w, c(setdiff(names(w), "gen_gas_peak_mw"), "gen_gas_peak_mw"))
}

load_gwh <- function() {
  d <- read_entsoe("load")
  if (is.null(d)) return(NULL)
  d <- d[area %in% COUNTRIES & resolution %in% names(STEP_H)]
  l <- d[, .(load_gwh = sum(value * STEP_H[resolution] / 1000), load_peak_mw = max(value)),
         by = .(area, date, resolution)]
  l <- finest(l, c("area", "date"))[, resolution := NULL]
  setnames(l, "area", "country")
}

prices <- function() {
  d <- read_entsoe("prices")
  if (is.null(d)) return(NULL)
  p <- d[, .(mean = mean(value), max = max(value), min = min(value)), by = .(area, date, resolution)]
  p <- finest(p, c("area", "date"))
  out <- rbindlist(lapply(COUNTRIES, function(cc) {
    z <- if (cc %in% c("DE", "AT")) {
      p[(area == "DE_AT_LU" & date < as.IDate("2018-10-01")) |
        (area == (if (cc == "DE") "DE_LU" else "AT") & date >= as.IDate("2018-10-01"))]
    } else p[area == PRICE_ZONE[[cc]]]
    z <- copy(z)[, country := cc]
    z[, .(country, date, power_price_mean_eur_mwh = mean,
          power_price_max_eur_mwh = max, power_price_min_eur_mwh = min)]
  }))
  out
}

flows <- function() {
  d <- read_entsoe("flows")
  if (is.null(d)) return(NULL)
  d <- d[resolution %in% names(STEP_H)]
  f <- d[, .(gwh = sum(value * STEP_H[resolution] / 1000)), by = .(area, date)]
  f[, c("src", "dst") := tstrsplit(area, "_", fixed = TRUE)]
  out <- f[, .(out = sum(gwh)), by = .(country = src, date)]
  inn <- f[, .(inn = sum(gwh)), by = .(country = dst, date)]
  net <- merge(out, inn, by = c("country", "date"), all = TRUE)
  net[, net_export_listed_borders_gwh := fcoalesce(out, 0) - fcoalesce(inn, 0)]
  net[country %in% COUNTRIES, .(country, date, net_export_listed_borders_gwh)]
}

hydro_reservoir <- function() {
  f <- file.path(ENTSOE, paste0("entsoe_hydro", c(".csv", ".csv.gz")))
  f <- f[file.exists(f)][1]
  if (is.na(f)) { message("  missing ENTSO-E file for hydro; its column will be empty"); return(NULL) }
  h <- fread(f, select = c("datetime_utc", "area", "value"), colClasses = list(character = "datetime_utc"))
  t <- as.POSIXct(substr(h$datetime_utc, 1, 19), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  h[, date := as.IDate(format(t, "%Y-%m-%d", tz = TZ))]
  h <- h[area %in% COUNTRIES, .(hydro_reservoir_gwh = value[.N] / 1000), by = .(country = area, date)]  # MWh -> GWh
  grid <- CJ(country = unique(h$country), date = seq(START, END, by = "day"))
  h[grid, on = .(country, date), roll = 6]   # weekly values carried forward to every day of the week
}

# ---------------------------------------------------------------- weather, storage, gas prices
read_tg <- function(sid) {   # daily mean temperature of one ECA&D station, valid days only
  f <- file.path(ROOT, "data", "raw_data", "weather", sprintf("TG_STAID%06d.txt", sid))
  hdr <- grep("^\\s*STAID", readLines(f, n = 60, encoding = "latin1"))[1]
  d <- fread(f, skip = hdr - 1, strip.white = TRUE)
  setnames(d, toupper(trimws(names(d))))
  d[TG != -9999 & Q_TG == 0, .(date = as.IDate(as.character(DATE), "%Y%m%d"), t_mean_c = TG / 10)]
}

weather <- function() {
  rep <- fread(file.path(WEATHER, "country_representative_stations.csv"))
  w <- rbindlist(lapply(COUNTRIES, function(cc) {  # same station as the existing HDD series
    sid <- rep[country == cc, station_id][1]
    read_tg(sid)[date >= START & date <= END][, `:=`(country = cc, tg_station_id = sid)]
  }))
  w[, `:=`(hdd = pmax(14 - t_mean_c, 0), cdd = pmax(t_mean_c - 21, 0))]
  w <- w[, .(country, date, tg_station_id, t_mean_c, hdd, cdd)]
  f <- file.path(WEATHER, "tmax_country_daily.csv.gz")
  if (file.exists(f)) {
    x <- fread(f)[country %in% COUNTRIES]
    x[, date := as.IDate(date)]
    x <- x[, .(country, date, tx_station_id = station_id, t_max_c = tx_c, tx_p98_ref, hot_day,
               heatwave_day, heatwave_onset, heatwave_id)]
    w <- merge(w, x, by = c("country", "date"), all = TRUE)
  } else message("  tmax_country_daily.csv.gz not found: run code/build_tmax_heatwave.R first")
  w
}

storage <- function() {
  s <- fread(file.path(ROOT, "data", "storage_weather_panel.csv.gz"),
             select = c("country", "date", "gasInStorage", "injection", "withdrawal",
                        "workingGasVolume", "injectionCapacity", "withdrawalCapacity"))
  s <- s[country %in% COUNTRIES][, date := as.IDate(date)]
  g <- s[, .(storage_twh = sum_na(gasInStorage), injection_gwh = sum_na(injection),
             withdrawal_gwh = sum_na(withdrawal), wgv_twh = sum_na(workingGasVolume),
             injection_cap_gwh = sum_na(injectionCapacity), withdrawal_cap_gwh = sum_na(withdrawalCapacity)),
         by = .(country, date)]
  g[, `:=`(fill_pct = 100 * storage_twh / wgv_twh,
           injection_util_pct = 100 * injection_gwh / injection_cap_gwh,
           net_injection_gwh = injection_gwh - withdrawal_gwh)][]
}

gas_prices <- function() {
  t <- fread(file.path(ROOT, "data", "raw_data", "price", "ttf", "ttf_1month.csv"), encoding = "UTF-8")
  setnames(t, sub("^\ufeff", "", names(t)))   # drop the byte-order mark if fread kept it
  t <- t[, .(date = as.IDate(Date, "%m/%d/%Y"), ttf_front_month_eur_mwh = as.numeric(gsub(",", "", Price)))]
  a <- fread(file.path(ROOT, "data", "raw_data", "price", "acer_price.csv"))
  a <- a[, .(date = as.IDate(DATE), acer_nwe_eur_mwh = as.numeric(`NORTH-WEST EUROPE PRICE (EUR/MWh)`))]
  p <- merge(t, a, by = "date", all = TRUE)
  p[date >= START & date <= END]
}

# ---------------------------------------------------------------- assemble
panel <- CJ(country = COUNTRIES, date = seq(START, END, by = "day"), sorted = FALSE)
for (step in list(list("generation", generation), list("load", load_gwh), list("prices", prices),
                  list("flows", flows), list("hydro reservoirs", hydro_reservoir),
                  list("weather", weather), list("storage", storage))) {
  message("building ", step[[1]], " ...")
  part <- step[[2]]()
  if (!is.null(part)) panel <- merge(panel, part, by = c("country", "date"), all.x = TRUE, sort = FALSE)
}
panel <- merge(panel, gas_prices(), by = "date", all.x = TRUE, sort = FALSE)
panel[, country := factor(country, levels = COUNTRIES)]
setorder(panel, country, date)
panel[, country := as.character(country)]
panel[, gas_price_eur_mwh := fcoalesce(ttf_front_month_eur_mwh, acer_nwe_eur_mwh)]
for (v in c("ttf_front_month_eur_mwh", "acer_nwe_eur_mwh", "gas_price_eur_mwh"))   # weekends: last trading day
  panel[, (v) := ffill_limit(get(v), 4), by = country]
panel[, `:=`(year = year(date), month = month(date), dow = (wday(date) + 5L) %% 7L)]   # dow: Monday = 0
panel[, `:=`(gas_summer = month >= 4 & month <= 9,
             gas_year = ifelse(month >= 10, year, year - 1L))]   # gas year starting 1 Oct
setcolorder(panel, c("country", "date"))
fwrite(panel, file.path(OUT, "country_day_panel.csv.gz"))

cb <- rbindlist(lapply(names(panel), function(v) {
  ok <- !is.na(panel[[v]])
  data.table(column = v, non_missing_share = round(mean(ok), 3),
             first_date = if (any(ok)) min(panel$date[ok]) else as.IDate(NA),
             last_date  = if (any(ok)) max(panel$date[ok]) else as.IDate(NA))
}))
fwrite(cb, file.path(OUT, "codebook.csv"))
message(sprintf("\npanel: %s rows x %d columns -> %s", format(nrow(panel), big.mark = ","), ncol(panel), OUT))
print(cb, nrows = 200)
