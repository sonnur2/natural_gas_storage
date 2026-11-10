# Daily maximum temperature by country and heatwave flags
#
# Source: ECA&D blended daily maximum temperature (TX) station files,
# data/raw_data/weather_max_temp/TX_STAIDxxxxxx.txt, same format as the TG files
# (TX in 0.1 degC, -9999 missing, Q_TX 0 valid / 1 suspect / 9 missing).
#
# Station per country: the representative station already used for mean temperature
# (data/weather/country_representative_stations.csv), if it has TX data covering the recent
# summers; otherwise the nearest station that covers both periods. The heatwave threshold
# needs 1961-1990, which many recent stations lack. Then the threshold comes from the
# nearest station with a full 1961-1990 record, shifted by the mean summer difference
# between the two stations. Before the daily station starts, the series is filled with that
# reference station (also shifted); see tx_source. Coverage is measured on June-August days,
# because a gap in summer breaks heatwave runs.
#
# Heatwave (definition chosen by s, Nov 2026): at least 3 consecutive days on which the
# daily maximum temperature is
#   - above the 98th percentile of all daily maxima at that station in 1961-1990, and
#   - above 28 degC.
# Only Q_TX == 0 values count; a missing day ends a run.
#
# Output (data/weather/):
#   tmax_stations_used.csv     station per country, threshold, coverage
#   tmax_country_daily.csv.gz  country x day, 1961-2025: tx_c, hot_day, heatwave_day, ...
#   heatwave_events.csv        one row per heatwave: start, end, length, peak tx
#
# Packages: install.packages(c("data.table", "R.utils"))   # R.utils lets fwrite write .gz
# Run: Source in RStudio, or Rscript code/build_tmax_heatwave.R from the repo folder.

library(data.table)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()   # fall back to the current folder
RAW_TX  <- Sys.getenv("RAW_TX", file.path(ROOT, "data", "raw_data", "weather_max_temp"))
RAW_TG  <- file.path(ROOT, "data", "raw_data", "weather")
WEATHER <- file.path(ROOT, "data", "weather")

COUNTRIES  <- c("DE", "IT", "NL", "FR", "AT", "ES")
REF        <- as.IDate(c("1961-01-01", "1990-12-31"))
RECENT     <- as.IDate(c("2015-01-01", "2025-12-31"))
START      <- as.IDate("1961-01-01")
END        <- as.IDate("2025-12-31")
REF_MIN    <- 0.90   # share of valid June-August days required in 1961-1990
RECENT_MIN <- 0.95   # ... and in 2015-2025
PCTL       <- 0.98
ABS_MIN    <- 28
MIN_RUN    <- 3

# ---------------------------------------------------------------- helpers
dms <- function(s) {  # "+52:05:00" -> 52.0833
  s <- trimws(s)
  sgn <- ifelse(substr(s, 1, 1) == "-", -1, 1)
  p <- tstrsplit(substring(s, 2), ":", type.convert = TRUE)
  sgn * (p[[1]] + p[[2]] / 60 + p[[3]] / 3600)
}

read_stations <- function() {
  f <- file.path(RAW_TX, "stations.txt")
  if (!file.exists(f)) f <- file.path(RAW_TG, "stations.txt")
  x <- readLines(f, encoding = "latin1")
  x <- x[grepl("^\\s*[0-9]+\\s*,", x)]
  p <- tstrsplit(x, ",", fixed = TRUE)
  st <- data.table(station_id = as.integer(p[[1]]), station_name = trimws(p[[2]]),
                   country = trimws(p[[3]]), lat = dms(p[[4]]), lon = dms(p[[5]]))
  have <- as.integer(sub("TX_STAID0*([0-9]+)\\.txt", "\\1", list.files(RAW_TX, "^TX_STAID[0-9]+\\.txt$")))
  st[station_id %in% have]
}

read_tx <- function(sid) {
  f <- file.path(RAW_TX, sprintf("TX_STAID%06d.txt", sid))
  hdr <- grep("^\\s*STAID", readLines(f, n = 60, encoding = "latin1"))[1]  # header length varies
  d <- fread(f, skip = hdr - 1, strip.white = TRUE)
  setnames(d, toupper(trimws(names(d))))
  d <- d[TX != -9999 & Q_TX == 0, .(date = as.IDate(as.character(DATE), "%Y%m%d"), tx_c = TX / 10)]
  d[date >= START & date <= END]
}

jja_days <- function(a, b) {
  d <- seq(a, b, by = "day")
  d[month(d) %in% 6:8]
}

coverage <- function(x, period) {  # share of June-August days with a valid value
  j <- jja_days(period[1], period[2])
  sum(x$date %in% j) / length(j)
}

haversine <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  12742 * asin(sqrt(a))
}

# ---------------------------------------------------------------- station choice
# Daily station: the representative TG station if it has TX data with RECENT_MIN summer
# coverage in 2015-2025; otherwise the nearest station meeting both coverage rules.
# Reference station: the daily station itself if it covers 1961-1990; otherwise the nearest
# station that does, shifted by the mean June-August difference between the two over the
# days both report (so the threshold is expressed at the daily station's level).
cache <- new.env()
tx <- function(sid) {
  k <- as.character(sid)
  if (is.null(cache[[k]])) cache[[k]] <- read_tx(sid)
  cache[[k]]
}

choose_station <- function(cc, rep, st) {
  r <- rep[country == cc][1]
  cands <- st[country == cc]
  cands[, dist_km := haversine(r$lat, r$lon, lat, lon)]
  setorder(cands, dist_km)
  daily <- NULL
  if (r$station_id %in% cands$station_id && coverage(tx(r$station_id), RECENT) >= RECENT_MIN) {
    daily <- cands[station_id == r$station_id]
  } else {
    for (i in seq_len(nrow(cands))) {
      x <- tx(cands$station_id[i])
      if (coverage(x, RECENT) >= RECENT_MIN && coverage(x, REF) >= REF_MIN) { daily <- cands[i]; break }
    }
  }
  if (is.null(daily)) return(NULL)
  ds <- tx(daily$station_id)
  if (coverage(ds, REF) >= REF_MIN)
    return(list(daily = daily, ref = daily, ds = ds, rs = ds, shift = 0, rep_id = r$station_id))
  cands[, dist_daily_km := haversine(daily$lat, daily$lon, lat, lon)]
  setorder(cands, dist_daily_km)
  for (i in seq_len(nrow(cands))) {
    if (cands$station_id[i] == daily$station_id) next
    x <- tx(cands$station_id[i])
    if (coverage(x, REF) < REF_MIN) next
    both <- merge(ds, x, by = "date", suffixes = c("_d", "_r"))[month(date) %in% 6:8]
    if (nrow(both) >= 270)  # at least about three summers of overlap
      return(list(daily = daily, ref = cands[i], ds = ds, rs = x,
                  shift = mean(both$tx_c_d - both$tx_c_r), rep_id = r$station_id))
  }
  NULL
}

flag_heatwaves <- function(x, thr) {
  df <- data.table(date = seq(min(x$date), max(x$date), by = "day"))
  df <- x[df, on = "date"]
  df[, hot_day := !is.na(tx_c) & tx_c > thr & tx_c > ABS_MIN]
  df[, run_len := .N, by = rleid(hot_day)]
  df[, heatwave_day := hot_day & run_len >= MIN_RUN]
  df[, heatwave_onset := heatwave_day & !shift(heatwave_day, fill = FALSE)]
  df[, heatwave_id := ifelse(heatwave_day, cumsum(heatwave_onset), NA_integer_)]
  df[, run_len := NULL][]
}

# ---------------------------------------------------------------- main
rep <- fread(file.path(WEATHER, "country_representative_stations.csv"))
st <- read_stations()
used <- list(); daily_out <- list(); events <- list()

for (cc in COUNTRIES) {
  res <- choose_station(cc, rep, st)
  if (is.null(res)) { message(cc, ": no usable TX station"); next }
  ds <- res$ds; rs <- res$rs; sh <- res$shift
  thr <- unname(quantile(rs[date >= REF[1] & date <= REF[2], tx_c], PCTL, type = 7)) + sh
  # full 1961-2025 series: daily station, and before it starts (or in its gaps) the
  # reference station shifted to the daily station's level
  same <- res$ref$station_id == res$daily$station_id
  series <- copy(ds)[, tx_source := "daily_station"]
  if (!same)
    series <- rbind(series, rs[!date %in% ds$date, .(date, tx_c = tx_c + sh, tx_source = "reference_station_shifted")])
  setorder(series, date)
  df <- flag_heatwaves(series, thr)
  df[, `:=`(country = cc, station_id = res$daily$station_id, tx_p98_ref = round(thr, 2))]
  setcolorder(df, c("country", "date", "station_id", "tx_c", "hot_day", "heatwave_day",
                    "heatwave_onset", "heatwave_id", "tx_source", "tx_p98_ref"))
  daily_out[[cc]] <- df
  ev <- df[heatwave_day == TRUE, .(start = min(date), end = max(date), length_days = .N,
                                   peak_tx_c = max(tx_c)), by = heatwave_id][, heatwave_id := NULL]
  events[[cc]] <- cbind(country = cc, ev)
  used[[cc]] <- data.table(
    country = cc, station_id = res$daily$station_id, station_name = res$daily$station_name,
    is_representative_tg_station = res$daily$station_id == res$rep_id,
    dist_from_representative_km = round(res$daily$dist_km, 1),
    summer_coverage_2015_2025 = round(coverage(ds, RECENT), 3),
    reference_station_id = res$ref$station_id, reference_station_name = res$ref$station_name,
    reference_summer_coverage_1961_1990 = round(coverage(rs, REF), 3),
    reference_shift_c = round(sh, 2), tx_p98_1961_1990 = round(thr, 2),
    n_heatwaves_1961_2025 = nrow(ev),
    heatwave_days_1961_1990 = df[date <= REF[2], sum(heatwave_day)],
    heatwave_days_1996_2025 = df[date >= as.IDate("1996-01-01"), sum(heatwave_day)])
  message(sprintf("%s: daily %d %s (rep=%s); reference %d %s shift %+.2fC; p98=%.1fC; heatwaves=%d",
                  cc, res$daily$station_id, res$daily$station_name, res$daily$station_id == res$rep_id,
                  res$ref$station_id, res$ref$station_name, sh, thr, nrow(ev)))
}

fwrite(rbindlist(used), file.path(WEATHER, "tmax_stations_used.csv"))
fwrite(rbindlist(daily_out), file.path(WEATHER, "tmax_country_daily.csv.gz"))
fwrite(rbindlist(events), file.path(WEATHER, "heatwave_events.csv"))
