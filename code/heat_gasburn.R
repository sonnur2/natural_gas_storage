# Q1 and Q2 of the summer-stress analysis, using the country x day panel
#
#   Q1  Does heat raise gas burn for power?  If so, summer is a second demand season for gas,
#       and gas that is burnt in July and August is gas that is not injected for winter.
#   Q2  Is the extra burn peaky?  If heat raises the daily peak (MW) more than the daily
#       volume (GWh), the summer stress is about send-out capacity, not only volume in store.
#
# Sample: gas summer (April-September) 2015-2025, six countries. Temperature is the daily
# maximum (Tmax) at the country's representative station; heatwaves as in build_tmax_heatwave.R.
#
# Specifications (all within country; fixed effects for country x year x month and
# country x day of week, so identification comes from day-to-day weather within a month):
#   S1 total    gas ~ Tmax bins                                   -> total effect of heat
#   S2 + RES    S1 + wind and solar generation                    -> holding renewables fixed
#   S3 + supply S2 + nuclear and hydro generation                 -> net of nuclear/hydro shortfalls
#   S4 + load   S3 + load                                         -> what is left beyond demand
# S1 is the policy-relevant number. S2-S4 are a decomposition, not causal controls: nuclear,
# hydro and load all respond to heat themselves, so the drop from S2 to S3 to S4 shows how
# much of the heat effect runs through each channel. The "channels" regressions run S1 with
# load, nuclear, hydro, solar, wind and net exports as outcomes.
# Standard errors are clustered by calendar week (common across countries).
#
# Output: output/heat_gasburn/  figures fig01-fig08 (png), tables (tex) and coefficient csv.
#
install.packages(c("data.table", "ggplot2", "fixest"))
# etable keep = "%..." matches the original coefficient names (before the dict renaming).
# Run: Source in RStudio, or Rscript code/heat_gasburn.R from the repo folder.

library(data.table)
library(ggplot2)
library(fixest)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()   # fall back to the current folder
PANEL <- file.path(ROOT, "data", "panel", "country_day_panel.csv.gz")
OUT   <- file.path(ROOT, "output", "heat_gasburn")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

COUNTRIES <- c("DE", "IT", "NL", "FR", "AT", "ES")
REF_BIN   <- 20    # reference Tmax bin: 20-22 C
MIN_N     <- 30    # a Tmax bin needs this many summer days in a country, else it is merged into its neighbour
KINK      <- 25    # piecewise-linear summary: slope per degree of Tmax above 25 C
FUEL_COL  <- c(Gas = "#d35400", Nuclear = "#8e44ad", Coal = "#2c3e50", Oil = "#7f8c8d",
               Hydro = "#2980b9", Wind = "#16a085", Solar = "#f1c40f", Other = "#bdc3c7")
SPEC_COL  <- c("S1 total" = "#d35400", "S2 + wind, solar" = "#2980b9",
               "S3 + nuclear, hydro" = "#8e44ad", "S4 + load" = "#7f8c8d")

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                  strip.text = element_text(face = "bold")))
save_plot <- function(p, name, w = 11, h = 6.5) {
  if (!is.null(p$labels$subtitle))   # wrap long subtitles to the figure width
    p$labels$subtitle <- paste(strwrap(p$labels$subtitle, width = round(w * 13)), collapse = "\n")
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 150, bg = "white")
  message("  saved ", name, ".png")
}

# ---------------------------------------------------------------- data
p <- fread(PANEL)
p[, date := as.IDate(date)]
p <- p[country %in% COUNTRIES]
p[, country := factor(country, levels = COUNTRIES)]
setorder(p, country, date)

# Germany closed its last reactors on 15 April 2023; ENTSO-E then stops reporting the series
p[country == "DE" & date >= as.IDate("2023-04-16") & is.na(gen_nuclear_gwh), gen_nuclear_gwh := 0]
# a fuel never reported in a country (IT and AT have no nuclear, NL almost no hydro) is zero there,
# so that those countries stay in the S3/S4 regressions instead of being dropped as missing
for (v in c("gen_nuclear_gwh", "gen_hydro_gwh", "gen_wind_gwh", "gen_solar_gwh")) {
  none <- p[, all(is.na(get(v))), by = country][V1 == TRUE, country]
  if (length(none)) {
    message("  ", v, " never reported in ", paste(none, collapse = ", "), ": set to 0")
    p[country %in% none, (v) := 0]
  }
}

p[, `:=`(summer = month >= 4 & month <= 9,
         week = format(date, "%G-%V"),                       # ISO week: cluster variable
         woy  = as.integer(format(date, "%V")))]             # week of year: seasonal FE in the event study
p[, `:=`(log_gas = log(fifelse(gen_gas_gwh > 0, gen_gas_gwh, NA_real_)),
         log_gas_peak = log(fifelse(gen_gas_peak_mw > 0, gen_gas_peak_mw, NA_real_)),
         log_gas_evening = log(fifelse(gen_gas_evening_gwh > 0, gen_gas_evening_gwh, NA_real_)),
         log_load = log(fifelse(load_gwh > 0, load_gwh, NA_real_)),
         log_load_peak = log(fifelse(load_peak_mw > 0, load_peak_mw, NA_real_)),
         gas_peak_to_mean = gen_gas_peak_mw / (gen_gas_gwh * 1000 / 24),   # 1 = flat profile
         gas_evening_share_pct = 100 * gen_gas_evening_gwh / gen_gas_gwh,  # 18:00-22:59 share of the day
         tx_hot  = pmax(t_max_c - KINK, 0),                                # degrees above 25 C
         tx_mild = pmin(t_max_c, KINK))]

# Tmax bins of 2 C, labelled by their lower edge; thin tails are merged country by country
p[, tmax_b := 2L * as.integer(floor(t_max_c / 2))]
caps <- p[summer & !is.na(tmax_b), .N, by = .(country, tmax_b)][N >= MIN_N,
          .(lo = min(tmax_b), hi = max(tmax_b)), by = country]
p[caps, on = "country", tmax_b := pmin(pmax(tmax_b, i.lo), i.hi)]

# position inside a heatwave (day 1-2, 3-5, 6+), to test persistence beyond same-day heat
p[, hw_n := seq_len(.N), by = .(country, heatwave_id)]
p[, hw_phase := fifelse(heatwave_day %in% TRUE,
                        fifelse(hw_n <= 2, "day 1-2", fifelse(hw_n <= 5, "day 3-5", "day 6+")), "none")]

# event time around heatwave onsets: -7 ... +14 days; other days are the reference (-99)
p[, on_i := fifelse(heatwave_onset %in% TRUE, as.integer(date), NA_integer_)]
p[, `:=`(since = as.integer(date) - nafill(on_i, "locf"), until = nafill(on_i, "nocb") - as.integer(date)), by = country]
p[, event_k := fifelse(!is.na(since) & since <= 14, since, fifelse(!is.na(until) & until <= 7, -until, -99L))]
p[, c("on_i", "since", "until") := NULL]

s <- p[summer == TRUE]
message(sprintf("summer sample: %s country-days; heatwave days: %d", format(nrow(s), big.mark = ","),
                s[, sum(heatwave_day, na.rm = TRUE)]))

# coverage of the variables used, by country (which estimates rest on how much data)
cov <- s[, lapply(.SD, function(x) round(mean(!is.na(x)), 3)), by = country,
         .SDcols = c("gen_gas_gwh", "gen_gas_peak_mw", "gen_gas_evening_gwh", "load_gwh", "gen_nuclear_gwh",
                     "gen_hydro_gwh", "gen_wind_gwh", "gen_solar_gwh", "net_export_listed_borders_gwh", "t_max_c")]
cov <- merge(cov, s[, .(summer_days = .N, heatwave_days = sum(heatwave_day, na.rm = TRUE),
                        heatwaves = sum(heatwave_onset, na.rm = TRUE)), by = country], by = "country")
cov <- merge(cov, caps, by = "country")
fwrite(cov, file.path(OUT, "coverage_by_country.csv"))
print(cov)

# ---------------------------------------------------------------- descriptives
message("descriptives ...")

# fig01: gas generation through the year; each year a thin line, the 2015-2025 mean thick
mo <- p[, .(gas = mean(gen_gas_gwh, na.rm = TRUE)), by = .(country, year, month)]
mo_avg <- mo[, .(gas = mean(gas, na.rm = TRUE)), by = .(country, month)]
save_plot(ggplot(mo, aes(month, gas)) +
            annotate("rect", xmin = 6.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "#fdebd0", alpha = 0.6) +
            geom_line(aes(group = year, colour = year), linewidth = 0.3) +
            geom_line(data = mo_avg, linewidth = 1.2, colour = "black") +
            scale_colour_viridis_c(option = "C", end = 0.9) +
            scale_x_continuous(breaks = 1:12, labels = month.abb) +
            facet_wrap(~country, scales = "free_y") +
            labs(x = NULL, y = "gas-fired generation, GWh per day", colour = "year",
                 title = "Gas-fired generation by month: a second peak in July-August?",
                 subtitle = "Monthly mean of daily generation; thin lines are single years, black is the 2015-2025 mean; July-August shaded"),
          "fig01_gas_by_month")

# fig02: power mix in the gas summer, by month (mean daily GWh, 2015-2025)
fuels <- c(Gas = "gen_gas_gwh", Nuclear = "gen_nuclear_gwh", Coal = "gen_coal_gwh", Oil = "gen_oil_gwh",
           Hydro = "gen_hydro_gwh", Wind = "gen_wind_gwh", Solar = "gen_solar_gwh", Other = "gen_other_gwh")
fuels <- fuels[fuels %in% names(p)]
mix <- melt(cbind(s[, .(country, month)], s[, lapply(.SD, as.numeric), .SDcols = fuels]), id.vars = c("country", "month"),
            variable.name = "col", value.name = "gwh")
mix[, fuel := factor(names(fuels)[match(col, fuels)], levels = rev(names(FUEL_COL)))]
mix <- mix[, .(gwh = mean(gwh, na.rm = TRUE)), by = .(country, month, fuel)][is.finite(gwh)]
save_plot(ggplot(mix, aes(month, gwh, fill = fuel)) + geom_area(colour = "white", linewidth = 0.1) +
            scale_fill_manual(values = FUEL_COL) + scale_x_continuous(breaks = 4:9, labels = month.abb[4:9]) +
            facet_wrap(~country, scales = "free_y") +
            labs(x = NULL, y = "GWh per day", fill = NULL,
                 title = "Summer power mix by month, 2015-2025 average",
                 subtitle = "Gas fills what nuclear, hydro and renewables leave as summer goes on"),
          "fig02_summer_mix")

# fig03: raw relation, mean gas generation by Tmax bin (summer days)
raw <- s[!is.na(tmax_b), .(gas = mean(gen_gas_gwh, na.rm = TRUE), se = sd(gen_gas_gwh, na.rm = TRUE) / sqrt(sum(!is.na(gen_gas_gwh))),
                           peak = mean(gen_gas_peak_mw, na.rm = TRUE) / 1000, n = .N), by = .(country, tmax_b)]
save_plot(ggplot(raw, aes(tmax_b + 1, gas)) +
            geom_pointrange(aes(ymin = gas - 1.96 * se, ymax = gas + 1.96 * se), colour = FUEL_COL[["Gas"]], size = 0.3) +
            geom_line(colour = FUEL_COL[["Gas"]]) +
            geom_text(aes(label = n, y = gas + 1.96 * se), vjust = -0.6, size = 2.4, colour = "grey40") +
            facet_wrap(~country, scales = "free_y") +
            labs(x = "daily maximum temperature, C (2-degree bins; end bins pooled)", y = "gas-fired generation, GWh per day",
                 title = "Gas burn for power by daily maximum temperature, April-September",
                 subtitle = "Raw means with 95% intervals; numbers are days per bin. No controls: season and year effects are still in here"),
          "fig03_gas_by_tmax_raw")

# ---------------------------------------------------------------- regressions
message("regressions ...")
setFixest_notes(FALSE)
FE <- "year^month + dow"           # within a country: year x month and day-of-week effects
CTRL <- list("S1 total" = "", "S2 + wind, solar" = "+ gen_wind_gwh + gen_solar_gwh",
             "S3 + nuclear, hydro" = "+ gen_wind_gwh + gen_solar_gwh + gen_nuclear_gwh + gen_hydro_gwh",
             "S4 + load" = "+ gen_wind_gwh + gen_solar_gwh + gen_nuclear_gwh + gen_hydro_gwh + load_gwh")

tidy <- function(m, ...) {   # coefficients as a data.table, plus whatever labels are passed in ...
  ct <- coeftable(m)
  data.table(term = rownames(ct), est = ct[, 1], se = ct[, 2], p = ct[, 4], nobs = nobs(m), ...)
}
fit <- function(y, rhs, d, fe = FE) {   # NULL if the outcome is missing or constant for this country
  yv <- d[[y]]
  if (sum(!is.na(yv)) < 200) return(NULL)
  if (length(unique(na.omit(yv))) < 10) {   # e.g. a fuel that is always zero in this country
    message("  skipped ", y, " for ", as.character(d$country[1]), ": (almost) constant")
    return(NULL)
  }
  vars <- regmatches(rhs, gregexpr("[a-z_]+_gwh", rhs))[[1]]
  if (length(vars) && any(d[, sapply(.SD, function(x) mean(!is.na(x))), .SDcols = vars] < 0.5)) return(NULL)
  tryCatch(feols(as.formula(paste(y, "~", rhs, "|", fe)), data = d, cluster = ~week),
           error = function(e) {
             message("  skipped ", y, " for ", as.character(d$country[1]), ": ", conditionMessage(e))
             NULL
           })
}
bins <- paste0("i(tmax_b, ref = ", REF_BIN, ")")

# Q1a: gas generation on Tmax bins, by country and specification
co_bins <- list(); m_lin <- list()
for (cc in COUNTRIES) {
  d <- s[country == cc]
  for (sp in names(CTRL)) {
    m <- fit("gen_gas_gwh", paste(bins, CTRL[[sp]]), d)
    if (!is.null(m)) co_bins[[length(co_bins) + 1]] <- tidy(m, country = cc, outcome = "gen_gas_gwh", spec = sp)
    m2 <- fit("gen_gas_gwh", paste("tx_mild + tx_hot", CTRL[[sp]]), d)   # piecewise-linear summary
    if (!is.null(m2)) m_lin[[paste(cc, sp)]] <- m2
  }
}
co_bins <- rbindlist(co_bins)

# Q1b: channels; S1 with load, nuclear, hydro, renewables and net exports as outcomes
CHANNELS <- c(gen_gas_gwh = "Gas", load_gwh = "Load", gen_nuclear_gwh = "Nuclear", gen_hydro_gwh = "Hydro",
              gen_solar_gwh = "Solar", gen_wind_gwh = "Wind", gen_coal_gwh = "Coal",
              net_export_listed_borders_gwh = "Net exports")
CHANNELS <- CHANNELS[names(CHANNELS) %in% names(s)]
co_ch <- rbindlist(lapply(COUNTRIES, function(cc) rbindlist(lapply(names(CHANNELS), function(y) {
  m <- fit(y, bins, s[country == cc])
  if (is.null(m)) NULL else tidy(m, country = cc, outcome = y, spec = "S1 total")
}))))

# Q1c: heatwaves beyond same-day heat; does the effect build up over the days of a heatwave?
m_hw <- list()
for (cc in COUNTRIES) {
  d <- s[country == cc]
  if (d[, sum(hw_phase != "none")] < 10) next   # too few heatwave days to say anything
  m_hw[[cc]] <- fit("gen_gas_gwh", paste(bins, "+ i(hw_phase, ref = 'none')"), d)
}
m_hw <- Filter(Negate(is.null), m_hw)
co_hw <- rbindlist(lapply(names(m_hw), function(cc) tidy(m_hw[[cc]], country = cc, outcome = "gen_gas_gwh", spec = "S1 + heatwave phase")))

# Q1d: pooled, in logs (percent effects comparable across countries)
POOL_FE <- "country^year^month + country^dow"
m_pool <- lapply(names(CTRL), function(sp) {
  ctrl <- gsub("(gen_\\w+_gwh|load_gwh)", "i(country, \\1)", CTRL[[sp]])   # country-specific slopes
  feols(as.formula(paste("log_gas ~", bins, ctrl, "|", POOL_FE)), data = s, cluster = ~week)
})
names(m_pool) <- names(CTRL)

# Q2: peakiness, pooled in logs (S1): daily volume vs daily peak vs evening, and load
PEAK_Y <- c(log_gas = "gas, daily GWh", log_gas_peak = "gas, daily peak MW", log_gas_evening = "gas, 18-23h GWh",
            log_load = "load, daily GWh", log_load_peak = "load, daily peak MW")
m_peak <- lapply(names(PEAK_Y), function(y) feols(as.formula(paste(y, "~", bins, "|", POOL_FE)), data = s, cluster = ~week))
names(m_peak) <- names(PEAK_Y)
co_peak <- rbindlist(lapply(names(PEAK_Y), function(y) tidy(m_peak[[y]], country = "pooled", outcome = y, spec = "S1 total")))

# Q2 by country: peak-to-mean ratio and evening share (levels; higher = peakier gas profile)
co_shape <- rbindlist(lapply(COUNTRIES, function(cc) rbindlist(lapply(c("gas_peak_to_mean", "gas_evening_share_pct"), function(y) {
  m <- fit(y, bins, s[country == cc])
  if (is.null(m)) NULL else tidy(m, country = cc, outcome = y, spec = "S1 total")
}))))

# Heatwave event study, pooled in logs: days around the onset vs other summer days,
# with country x year and country x week-of-year effects (seasonality inside the summer)
EV_Y <- c(log_gas = "gas, daily GWh", log_gas_peak = "gas, daily peak MW", log_load = "load, daily GWh")
m_ev <- lapply(names(EV_Y), function(y)
  feols(as.formula(paste(y, "~ i(event_k, ref = -99) | country^year + country^woy + country^dow")), data = s, cluster = ~week))
names(m_ev) <- names(EV_Y)
co_ev <- rbindlist(lapply(names(EV_Y), function(y) tidy(m_ev[[y]], country = "pooled", outcome = y, spec = "event study")))

co_all <- rbindlist(list(co_bins, co_ch, co_hw, co_peak, co_shape, co_ev,
                         rbindlist(lapply(names(m_pool), function(sp) tidy(m_pool[[sp]], country = "pooled", outcome = "log_gas", spec = sp)))),
                    fill = TRUE)
fwrite(co_all, file.path(OUT, "coefficients_all.csv"))

# ---------------------------------------------------------------- regression figures
message("regression figures ...")
bin_terms <- function(co) {   # keep the Tmax-bin terms, add the reference bin at zero
  b <- co[grepl("^tmax_b::", term)][, tmax_b := as.integer(sub("tmax_b::", "", term))]
  ref <- unique(b[, setdiff(names(b), c("term", "est", "se", "p", "tmax_b")), with = FALSE])[, `:=`(tmax_b = REF_BIN, est = 0, se = 0)]
  rbind(b[, !c("term", "p")], ref, fill = TRUE)
}
pct <- function(x) 100 * (exp(x) - 1)

# fig04: Q1 headline; effect of Tmax on gas generation, by country, S1-S4
b4 <- bin_terms(co_bins)
b4[, spec := factor(spec, levels = names(CTRL))]
save_plot(ggplot(b4, aes(tmax_b + 1, est, colour = spec, fill = spec)) +
            geom_hline(yintercept = 0, colour = "grey50") +
            geom_ribbon(data = b4[spec == "S1 total"], aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.15, colour = NA) +
            geom_line() + geom_point(size = 1) +
            scale_colour_manual(values = SPEC_COL) + scale_fill_manual(values = SPEC_COL, guide = "none") +
            facet_wrap(~country, scales = "free") +
            labs(x = "daily maximum temperature, C (2-degree bins)", y = "gas-fired generation vs a 20-22 C day, GWh per day",
                 colour = NULL, title = "Q1: how much more gas is burnt for power on a hot day?",
                 subtitle = "Within country, year x month and day-of-week effects; 95% band for S1. The gap between lines shows how much runs through renewables, nuclear/hydro and load"),
          "fig04_gas_tmax_bins_by_country")

# fig05: channels (S1): what moves when it gets hot
b5 <- bin_terms(co_ch)
b5[, channel := factor(CHANNELS[outcome], levels = CHANNELS)]
save_plot(ggplot(b5, aes(tmax_b + 1, est, colour = channel)) +
            geom_hline(yintercept = 0, colour = "grey50") + geom_line() + geom_point(size = 1) +
            scale_colour_manual(values = c(Gas = "#d35400", Load = "black", Nuclear = "#8e44ad", Hydro = "#2980b9",
                                           Solar = "#f1c40f", Wind = "#16a085", Coal = "#2c3e50", "Net exports" = "#c0392b")) +
            facet_wrap(~country, scales = "free_y") +
            labs(x = "daily maximum temperature, C", y = "change vs a 20-22 C day, GWh per day", colour = NULL,
                 title = "Q1: channels; who covers the extra load on hot days, and what drops out",
                 subtitle = "Same specification as S1, one regression per series. Net exports: positive = more exported"),
          "fig05_channels_by_country")

# fig06: Q2 headline; percent effect on daily volume vs daily peak (pooled)
b6 <- bin_terms(co_peak)
b6[, series := factor(PEAK_Y[outcome], levels = PEAK_Y)]
save_plot(ggplot(b6, aes(tmax_b + 1, pct(est), colour = series)) +
            geom_hline(yintercept = 0, colour = "grey50") +
            geom_errorbar(aes(ymin = pct(est - 1.96 * se), ymax = pct(est + 1.96 * se)), width = 0.3, alpha = 0.5,
                          position = position_dodge(width = 0.8)) +
            geom_point(position = position_dodge(width = 0.8)) +
            scale_colour_manual(values = c("#d35400", "#e74c3c", "#f39c12", "grey20", "grey55")) +
            labs(x = "daily maximum temperature, C", y = "% change vs a 20-22 C day", colour = NULL,
                 title = "Q2: does heat raise the gas peak more than the gas volume?",
                 subtitle = "Pooled over six countries, logs, country x year x month and country x day-of-week effects; 95% intervals"),
          "fig06_peak_vs_volume_pooled", w = 11, h = 6)

# fig07: shape of the gas profile by country (peak-to-mean ratio, evening share)
b7 <- bin_terms(co_shape)
b7[, series := fifelse(outcome == "gas_peak_to_mean", "peak / mean MW (ratio)", "evening share of daily gas, % points")]
save_plot(ggplot(b7, aes(tmax_b + 1, est, colour = country)) +
            geom_hline(yintercept = 0, colour = "grey50") + geom_line() + geom_point(size = 1) +
            facet_wrap(~series, scales = "free_y") +
            labs(x = "daily maximum temperature, C", y = "change vs a 20-22 C day", colour = NULL,
                 title = "Q2: does the daily gas profile get peakier on hot days?",
                 subtitle = "S1 by country. Peak/mean = 1 means flat output over the day"),
          "fig07_gas_profile_shape", w = 11, h = 5)

# fig08: heatwave event study (pooled)
b8 <- co_ev[grepl("^event_k::", term)][, k := as.integer(sub("event_k::", "", term))]
b8[, series := factor(EV_Y[outcome], levels = EV_Y)]
save_plot(ggplot(b8, aes(k, pct(est), colour = series, fill = series)) +
            annotate("rect", xmin = -0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "#fdebd0", alpha = 0.6) +
            geom_hline(yintercept = 0, colour = "grey50") +
            geom_ribbon(aes(ymin = pct(est - 1.96 * se), ymax = pct(est + 1.96 * se)), alpha = 0.15, colour = NA) +
            geom_line() + geom_point(size = 1) +
            scale_colour_manual(values = c("#d35400", "#e74c3c", "grey20")) +
            scale_fill_manual(values = c("#d35400", "#e74c3c", "grey20"), guide = "none") +
            labs(x = "days since heatwave onset", y = "% vs other summer days", colour = NULL,
                 title = "Heatwave event study: gas burn and load around the start of a heatwave",
                 subtitle = "Pooled, logs; country x year, country x week-of-year and country x day-of-week effects. Shaded: first three days (the minimum heatwave)"),
          "fig08_heatwave_event_study", w = 10, h = 5.5)

# ---------------------------------------------------------------- tables
message("tables ...")
dict <- c(gen_gas_gwh = "Gas generation (GWh/d)", log_gas = "log gas generation", tx_hot = "Tmax above 25C (per C)",
          tx_mild = "Tmax up to 25C (per C)", log_gas_peak = "log gas peak MW", log_gas_evening = "log gas 18-23h",
          log_load = "log load", log_load_peak = "log load peak MW")

# table 1: per degree above 25 C, gas GWh per day, by country (columns) and specification (panels)
tab1 <- rbindlist(lapply(names(m_lin), function(k) {
  ct <- coeftable(m_lin[[k]])
  data.table(lab = k, est = ct["tx_hot", 1], se = ct["tx_hot", 2], nobs = nobs(m_lin[[k]]))
}))
tab1[, `:=`(country = substr(lab, 1, 2), spec = substring(lab, 4))]
tab1[, mean_gas := s[, mean(gen_gas_gwh, na.rm = TRUE), by = country][match(tab1$country, country), V1]]
tab1[, pct_of_mean := round(100 * est / mean_gas, 1)]
fwrite(tab1[, .(country, spec, gwh_per_degree_above_25 = round(est, 2), se = round(se, 2), pct_of_mean, nobs)],
       file.path(OUT, "table1_gas_per_degree_above25.csv"))
etable(m_lin[grepl("S1 total", names(m_lin))], keep = "%tx_", dict = dict, se.below = TRUE,
       headers = sub(" S1 total", "", names(m_lin)[grepl("S1 total", names(m_lin))]),
       title = "Gas-fired generation and daily maximum temperature, April-September 2015-2025 (S1)",
       file = file.path(OUT, "table1_gas_tmax_linear.tex"), replace = TRUE)

# table 2: pooled log specifications S1-S4 (Tmax bins)
etable(m_pool, keep = "%tmax_b", dict = dict, se.below = TRUE, headers = names(m_pool),
       title = "Gas-fired generation (log) on Tmax bins, pooled, S1-S4",
       file = file.path(OUT, "table2_pooled_log_bins.tex"), replace = TRUE)

# table 3: Q2 volume vs peak (pooled)
etable(m_peak, keep = "%tmax_b", dict = dict, se.below = TRUE, headers = unname(PEAK_Y),
       title = "Daily volume vs daily peak on Tmax bins, pooled (S1)",
       file = file.path(OUT, "table3_peak_vs_volume.tex"), replace = TRUE)

# table 4: heatwave phase beyond same-day Tmax, by country
if (length(m_hw))
  etable(m_hw, keep = "%hw_phase", dict = dict, se.below = TRUE, headers = names(m_hw),
         title = "Gas generation in heatwaves beyond same-day Tmax (Tmax bins included)",
         file = file.path(OUT, "table4_heatwave_phase.tex"), replace = TRUE)

# console summary
message("\nQ1, gas GWh per day per degree of Tmax above 25 C (S1 total; in brackets % of mean summer gas burn):")
print(tab1[spec == "S1 total", .(country, gwh_per_degree = round(est, 2), se = round(se, 2), pct_of_mean)])
message("Q2, pooled % effect on a 32-34 C day vs a 20-22 C day:")
top <- b6[tmax_b == 32, .(series, pct = round(pct(est), 1), ci_low = round(pct(est - 1.96 * se), 1), ci_high = round(pct(est + 1.96 * se), 1))]
print(top)
message("done -> ", OUT)
