# Step 1 model: competitive two-season storage benchmark with a fill mandate
#
# Companion to model/step1_competitive_benchmark.tex. Everything here is closed form;
# the script only evaluates the formulas of the note on an ILLUSTRATIVE parameter set
# (EU-sized, round numbers, not a calibration) and draws the figures used in the note.
#
# Units: quantities in TWh per season (summer = Apr-Sep, winter = Oct-Mar),
#        prices in EUR/MWh, so price x quantity is in EUR million (1 TWh = 1e6 MWh).
#
# Notation (as in the note)
#   P_t(y) = P0_t + s_1 y (summer),  P0_2 - s_2 y (winter)   residual price functions
#   s_t  = b_t g_t / (b_t + g_t)    price impact of storage (demand slope b, import-supply slope g)
#   w_t  = g_t / (b_t + g_t)        share of a storage change absorbed by consumption
#   C(y) = c y + kappa/2 y^2        storage cost (kappa = slope of the facility merit order)
#   Delta0 = beta P0_2 - P0_1 - c   no-storage net spread
#   sigma = s_1 + beta s_2,  Lambda = sigma + kappa
#   y*  = min(K, max(0, Delta0 / Lambda))
#
# Output: output/model_step1/  figures (pdf + png) and tables (csv)
# Packages: install.packages(c("data.table", "ggplot2"))
# Run: Source in RStudio, or Rscript code/model_step1_benchmark.R from the repo folder.

library(data.table)
library(ggplot2)

ROOT <- "C:/Users/s-bas-37/Documents/GitHub/natural_gas_storage"
if (!dir.exists(ROOT)) ROOT <- getwd()
OUT <- file.path(ROOT, "output", "model_step1")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                  strip.text = element_text(face = "bold")))
save_plot <- function(p, name, w = 7, h = 4.3) {
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
  message("  saved ", name)
}
COL <- c("#1f4e79", "#c0504d", "#7f7f7f", "#e09b2d", "#4f9a5a", "#7a5195")

# ---------------------------------------------------------------------------
# 1. Illustrative parameters, set from elasticities at a reference point
# ---------------------------------------------------------------------------
# Reference point = the competitive equilibrium y*: summer/winter consumption,
# a reference price, demand and import-supply elasticities. The no-storage
# winter price P0_2 is then backed out so that y* equals y_ref.
ref <- list(
  K      = 1100,     # working gas volume, TWh (EU order of magnitude)
  y_ref  = 850,      # competitive fill, TWh (~77% of K): so a 90% target binds
  D1     = 1300,     # summer consumption at y*, TWh
  D2     = 2000,     # winter consumption at y*, TWh
  P_ref  = 35,       # reference price, EUR/MWh
  eps_d1 = 0.20,     # |demand elasticity|, summer (power sector switching)
  eps_d2 = 0.15,     # |demand elasticity|, winter (heating)
  eta1   = 1.00,     # import (LNG + pipeline) supply elasticity, summer
  eta2   = 0.80,     # import supply elasticity, winter
  c      = 1.5,      # unit injection + withdrawal cost of the cheapest facilities, EUR/MWh
  c_top  = 5.5,      # unit cost of the most expensive facility at full K, EUR/MWh
  beta   = 0.98,     # half-year discount factor (~4% a year)
  P0_1   = 25        # no-storage summer price, EUR/MWh
)

make_par <- function(r = ref) {
  M1 <- r$D1 + r$y_ref; M2 <- r$D2 - r$y_ref            # imports at y*
  b1 <- r$P_ref / (r$eps_d1 * r$D1); b2 <- r$P_ref / (r$eps_d2 * r$D2)
  g1 <- r$P_ref / (r$eta1 * M1);     g2 <- r$P_ref / (r$eta2 * M2)
  p <- list(K = r$K, c = r$c, beta = r$beta, kappa = (r$c_top - r$c) / r$K,
            b1 = b1, b2 = b2, g1 = g1, g2 = g2,
            s1 = b1 * g1 / (b1 + g1), s2 = b2 * g2 / (b2 + g2),
            w1 = g1 / (b1 + g1),      w2 = g2 / (b2 + g2))
  p$sigma  <- p$s1 + p$beta * p$s2
  p$Lambda <- p$sigma + p$kappa
  p$P0_1   <- r$P0_1
  p$Delta0 <- p$Lambda * r$y_ref                         # so that y* = y_ref
  p$P0_2   <- (p$Delta0 + p$P0_1 + p$c) / p$beta
  p$D0_1   <- r$D1 + p$w1 * r$y_ref                      # no-storage quantities
  p$D0_2   <- r$D2 - p$w2 * r$y_ref
  p
}
par0 <- make_par()

# ---------------------------------------------------------------------------
# 2. Closed-form objects
# ---------------------------------------------------------------------------
ystar <- function(p) min(p$K, max(0, p$Delta0 / p$Lambda))

outcome <- function(p, y) {
  data.table(
    y  = y,
    P1 = p$P0_1 + p$s1 * y,
    P2 = p$P0_2 - p$s2 * y,
    D1 = p$D0_1 - p$w1 * y,          M1 = p$D0_1 + (1 - p$w1) * y,
    D2 = p$D0_2 + p$w2 * y,          M2 = p$D0_2 - (1 - p$w2) * y
  )[, `:=`(spread     = p$beta * P2 - P1,
           margin     = p$beta * P2 - P1 - p$c - p$kappa * y,    # marginal storer margin
           profit     = (p$beta * P2 - P1 - p$c) * y - p$kappa / 2 * y^2)][]
}

# Welfare changes from moving fill from y0 to y1 (exact for linear model)
incidence <- function(p, y0, y1) {
  a <- outcome(p, y0); m <- outcome(p, y1)
  dP1 <- m$P1 - a$P1; dP2 <- m$P2 - a$P2
  dt <- data.table(
    group = c("Summer consumers", "Summer importers", "Winter consumers",
              "Winter importers", "Storers"),
    value = c(-dP1 * (a$D1 + m$D1) / 2,
              dP1 * (a$M1 + m$M1) / 2,
              -p$beta * dP2 * (a$D2 + m$D2) / 2,
              p$beta * dP2 * (a$M2 + m$M2) / 2,
              m$profit - a$profit))
  dt[]
}

ys <- ystar(par0)
ybar <- 0.9 * par0$K
G <- ybar - ys
inc <- incidence(par0, ys, ybar)
dwl_formula <- par0$Lambda / 2 * G^2
stopifnot(abs(sum(inc$value) + dwl_formula) < 1e-6)     # incidence adds up to -DWL

key <- data.table(
  item = c("y* (TWh)", "y*/K", "mandated fill (TWh)", "gap G (TWh)",
           "s1", "s2", "kappa", "sigma", "Lambda", "omega1", "omega2",
           "P1 at y*", "P2 at y*", "P1 at mandate", "P2 at mandate",
           "spread beta*P2-P1 at y*", "spread at mandate",
           "shadow price nu = Lambda*G (EUR/MWh)",
           "storer profit at y* (EURm)", "storer profit at mandate (EURm)",
           "DWL (EURm)", "summer consumers (EURm)", "winter consumers (EURm)",
           "summer importers (EURm)", "winter importers (EURm)", "storers (EURm)",
           "per-unit subsidy sigma_s needed (EUR/MWh)", "subsidy bill (EURm)",
           "state fill z (TWh)", "private fill under state filling (TWh)", "state loss (EURm)"),
  value = NA_real_)
a0 <- outcome(par0, ys); m0 <- outcome(par0, ybar)
yp <- max(0, (par0$Delta0 - par0$sigma * ybar) / par0$kappa)  # private fill under state filling
z  <- ybar - yp                                                 # state fill (corner when yp = 0)
state_loss <- (par0$P0_1 + par0$s1 * ybar + par0$c - par0$beta * (par0$P0_2 - par0$s2 * ybar)) * z +
  par0$kappa / 2 * (ybar^2 - yp^2)
key$value <- c(ys, ys / par0$K, ybar, G, par0$s1, par0$s2, par0$kappa, par0$sigma, par0$Lambda,
               par0$w1, par0$w2, a0$P1, a0$P2, m0$P1, m0$P2, a0$spread, m0$spread,
               par0$Lambda * G, a0$profit, m0$profit, dwl_formula,
               inc$value[1], inc$value[3], inc$value[2], inc$value[4], inc$value[5],
               par0$Lambda * G, par0$Lambda * G * ybar, z, yp, state_loss)
key[, value := round(value, 4)]
fwrite(key, file.path(OUT, "key_numbers.csv"))
print(key)

# ---------------------------------------------------------------------------
# 3. Figure 1: prices, spread and storer margin against the fill target theta
# ---------------------------------------------------------------------------
th <- seq(0.5, 1, by = 0.005)
f1 <- rbindlist(lapply(th, function(t) {
  y <- max(ys, t * par0$K); o <- outcome(par0, y); o[, theta := t]
}))
f1l <- melt(f1, id.vars = "theta", measure.vars = c("P1", "P2", "spread", "margin"))
f1l[, variable := factor(variable, levels = c("P1", "P2", "spread", "margin"),
                         labels = c("Summer price", "Winter price",
                                    "Spread (beta P2 - P1)", "Marginal storer margin"))]
p1 <- ggplot(f1l, aes(theta * 100, value, colour = variable)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 100 * ys / par0$K, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 100 * ys / par0$K, y = max(f1l$value), label = " y*/K",
           hjust = 0, vjust = 1, size = 3.2, colour = "grey30") +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = COL[1:4], name = NULL) +
  labs(x = "Fill target theta (% of capacity)", y = "EUR/MWh")
save_plot(p1, "fig1_prices_spread")

# ---------------------------------------------------------------------------
# 4. Figure 2: incidence of the mandate by target theta (unfunded mandate)
# ---------------------------------------------------------------------------
f2 <- rbindlist(lapply(th[th * par0$K >= ys], function(t) {
  d <- incidence(par0, ys, t * par0$K); d[, theta := t]
}))
f2[, group := factor(group, levels = c("Summer consumers", "Summer importers",
                                       "Winter consumers", "Winter importers", "Storers"))]
f2dwl <- f2[, .(dwl = sum(value)), by = theta]
p2 <- ggplot(f2, aes(theta * 100, value / 1000, fill = group)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_line(data = f2dwl, aes(theta * 100, dwl / 1000), inherit.aes = FALSE,
            linewidth = 1, colour = "black") +
  annotate("text", x = 99, y = min(f2dwl$dwl) / 1000, label = "net = -DWL",
           hjust = 1, vjust = 1.4, size = 3.2) +
  scale_fill_manual(values = COL[c(2, 4, 1, 5, 3)], name = NULL) +
  labs(x = "Fill target theta (% of capacity)", y = "Change vs competitive outcome (EUR bn)")
save_plot(p2, "fig2_incidence")

# ---------------------------------------------------------------------------
# 5. Figure 3: distorted seasonality. Higher summer demand -> lower y*, larger DWL
# ---------------------------------------------------------------------------
# A summer demand shift of dD TWh at given prices raises A_1 by b_1 dD and the
# no-storage summer price by w_1 * b_1 dD = s_1 dD. Two winter import elasticities.
f3 <- rbindlist(lapply(c(0.5, 0.8, 1.5), function(e2) {
  r <- ref; r$eta2 <- e2; p <- make_par(r)
  rbindlist(lapply(seq(0, 250, by = 5), function(dD) {
    q <- p; q$P0_1 <- p$P0_1 + p$s1 * dD; q$Delta0 <- q$beta * q$P0_2 - q$P0_1 - q$c
    y <- ystar(q); G <- max(0, 0.9 * q$K - y)
    data.table(eta2 = e2, dD = dD, fill = y / q$K,
               dwl = q$Lambda / 2 * G^2 + ifelse(q$Delta0 < 0, -q$Delta0 * G, 0),
               nu = q$Lambda * G)
  }))
}))
f3[, eta2 := factor(eta2, labels = paste("winter import elasticity", c(0.5, 0.8, 1.5)))]
f3l <- melt(f3, id.vars = c("eta2", "dD"), measure.vars = c("fill", "nu"))
f3l[variable == "fill", value := 100 * value]
f3l[, variable := factor(variable, labels = c("Competitive fill y*/K (%)",
                                              "Shadow cost of 90% target (EUR/MWh)"))]
p3 <- ggplot(f3l, aes(dD, value, colour = eta2)) +
  geom_line(linewidth = 0.9) +
  geom_hline(data = data.table(variable = factor("Competitive fill y*/K (%)",
                                                 levels = levels(f3l$variable)), yint = 90),
             aes(yintercept = yint), linetype = "dashed", colour = "grey40") +
  facet_wrap(~variable, scales = "free_y") +
  scale_colour_manual(values = COL[c(2, 1, 5)], name = NULL) +
  labs(x = "Summer demand shift at given prices (TWh per summer)", y = NULL)
save_plot(p3, "fig3_seasonality_shift", w = 8, h = 4)
# Note: when the summer price shift is equal at every elasticity, the curves differ only
# through Lambda (how fast storage moves prices).

# ---------------------------------------------------------------------------
# 6. Figure 4: state filling and crowding out
# ---------------------------------------------------------------------------
f4 <- rbindlist(lapply(c(0.5, 1, 2), function(mult) {
  r <- ref; r$c_top <- r$c + mult * (ref$c_top - ref$c); p <- make_par(r)
  y0 <- ystar(p)
  rbindlist(lapply(th, function(t) {
    yb <- t * p$K
    if (yb <= y0) return(data.table(theta = t, private = y0, state = 0))
    yp <- max(0, (p$Delta0 - p$sigma * yb) / p$kappa)
    data.table(theta = t, private = yp, state = yb - yp)
  }))[, kappa := sprintf("%s merit-order slope (kappa = %.4f)",
                          c("Flat", "Base", "Steep")[match(mult, c(0.5, 1, 2))], p$kappa)]
}))
f4l <- melt(f4, id.vars = c("theta", "kappa"), variable.name = "who")
f4l[, who := factor(who, levels = c("state", "private"))]   # private fill drawn at the bottom
f4l[, kappa := factor(kappa, levels = unique(kappa))]
p4 <- ggplot(f4l, aes(theta * 100, value, fill = who)) +
  geom_area(alpha = 0.85) +
  facet_wrap(~kappa) +
  scale_fill_manual(values = c(private = COL[1], state = COL[2]), name = NULL,
                    breaks = c("private", "state"), labels = c("Private fill", "State fill")) +
  labs(x = "Fill target theta (% of capacity)", y = "TWh in store on 1 November")
save_plot(p4, "fig4_state_filling", w = 9, h = 3.8)

message("done: ", OUT)
