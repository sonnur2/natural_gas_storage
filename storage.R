rm(list=ls())
library(lubridate)
library(dplyr)
library(ggplot2)
library(broom)
library(foreach)
library(doParallel)


# Upload eu_storage_panel.csv file
eu_storage_panel <- read.csv("eu_storage_panel.csv")
eu_storage_panel$date <- as.Date(eu_storage_panel$gasDayStart, format = "%Y-%m-%d")

eu_storage_panel_2018_2019 <- read.csv("2018_2019_eu_storage_panel.csv")
eu_storage_panel_2018_2019$date <- as.Date(eu_storage_panel_2018_2019$gasDayStart, format = "%Y-%m-%d")

eu_storage_panel <- bind_rows(eu_storage_panel_2018_2019, eu_storage_panel)

# Upload ttf.csv file
ttf_1month <- read.csv("ttf_1month.csv")
#Change the name of the column "Date" to "date" in ttf dataset
colnames(ttf_1month)[colnames(ttf_1month) == "Date"] <- "date"

ttf_3months <- read.csv("ttf_3months.csv")
#Change the name of the column "Date" to "date" in ttf dataset
colnames(ttf_3months)[colnames(ttf_3months) == "Date"] <- "date"
eu_storage_panel$gasDayStart <- ymd(eu_storage_panel$gasDayStart)
eu_storage_panel$gasDayEnd   <- ymd(eu_storage_panel$gasDayEnd)

# Compute the actual gap in days
eu_storage_panel <- eu_storage_panel %>%
  mutate(day_diff = as.numeric(gasDayEnd - gasDayStart))

eu_storage_panel <- eu_storage_panel %>%
  mutate(day_diff = as.numeric(gasDayEnd - gasDayStart)) %>%
  filter(day_diff == 1) %>%
  select(-day_diff)


# Convert all date columns to Date format
eu_storage_panel$date <- as.Date(eu_storage_panel$date)
ttf_1month$date <- mdy(ttf_1month$date)
ttf_3months$date <- mdy(ttf_3months$date)


# For storage: your 'date' column is probably YYYY-MM-DD
eu_storage_panel$date <- ymd(eu_storage_panel$date)

# Verify conversions
class(ttf_1month$date)       # should be "Date"

full_range <- seq(as.Date("2018-01-01"), as.Date("2025-12-31"), by = "day")
full_calendar <- data.frame(date = full_range)

ttf_1month_full <- full_calendar %>%
  left_join(ttf_1month %>% select(date, Price), by = "date") %>%
  arrange(date)

# Function: for each NA, find the nearest non-NA before and after, average them
fill_neighborhood_avg <- function(df, value_col = "Price") {
  x <- df[[value_col]]
  n <- length(x)
  filled <- x
  na_idx <- which(is.na(x))
  
  for (i in na_idx) {
    # look backwards for nearest non-NA
    before <- NA
    j <- i - 1
    while (j >= 1) {
      if (!is.na(x[j])) { before <- x[j]; break }
      j <- j - 1
    }
    # look forwards for nearest non-NA
    after <- NA
    k <- i + 1
    while (k <= n) {
      if (!is.na(x[k])) { after <- x[k]; break }
      k <- k + 1
    }
    filled[i] <- mean(c(before, after), na.rm = TRUE)
  }
  filled
}

ttf_1month_full$Price_neighbor_avg <- fill_neighborhood_avg(ttf_1month_full, "Price")

ttf_3months_full <- full_calendar %>%
  left_join(ttf_3months %>% select(date, Price), by = "date") %>%
  arrange(date)
ttf_3months_full$Price_neighbor_avg <- fill_neighborhood_avg(ttf_3months_full, "Price")
# --- Merge safely ---
merged <- eu_storage_panel %>%
  left_join(ttf_1month_full %>% select(date, spot = Price_neighbor_avg), by = "date") %>%
  left_join(ttf_3months_full %>% select(date, future = Price_neighbor_avg), by = "date")

# Now filter missing values
merged <- merged %>% filter(!is.na(spot), !is.na(future))


# Define parameters
fuel_loss_factor <- 0.01            # 1%
injection_cost_usd_per_mmbtu_2000 <- 0.02
withdrawal_cost_usd_per_mmbtu_2000 <- 0.02
holding_cost_usd_per_mmbtu_per_year_2000 <- 0.40

# --- 2. Conversion factors ---
usd2000_to_usd2023 <- 0.94 # Conversion factor from 2000 USD to 2023 USD
mmbtu_to_mwh <- 0.293071
usd_to_eur <- 0.9   # Use a recent average exchange rate

# --- 3. Convert to EUR/MWh ---
# CPI ratio from 2000 to 2023 (approx)
cpi_ratio <- 304.7 / 172.2  # ≈ 1.77
# Exchange rate (USD to EUR, average 2023)
usd_to_eur <- 0.9

injection_cost_eur_per_mwh <- injection_cost_usd_per_mmbtu_2000 * cpi_ratio * usd_to_eur / mmbtu_to_mwh
withdrawal_cost_eur_per_mwh <- withdrawal_cost_usd_per_mmbtu_2000 * cpi_ratio * usd_to_eur / mmbtu_to_mwh
holding_cost_eur_per_mwh_per_year <- holding_cost_usd_per_mmbtu_per_year_2000 * cpi_ratio * usd_to_eur / mmbtu_to_mwh


# --- 4. Convert holding cost to a daily rate ---
holding_cost_eur_per_mwh_per_day <- holding_cost_eur_per_mwh_per_year / 365

# --- 5. View the results ---
print(paste("Injection Cost (€/MWh):", round(injection_cost_eur_per_mwh, 4)))
print(paste("Withdrawal Cost (€/MWh):", round(withdrawal_cost_eur_per_mwh, 4)))
print(paste("Holding Cost (€/MWh/day):", round(holding_cost_eur_per_mwh_per_day, 6)))

# Select a facility (example: pick one)
facility_eic_example <- "21W000000000093R"  # Change to your chosen facility
df_f <- merged %>% filter(facility_eic == facility_eic_example) %>% arrange(date)

# Check data availability
cat("Number of days for facility", facility_eic_example, ":", nrow(df_f), "\n")

cost_of_carry <- holding_cost_eur_per_mwh_per_day + 0.001  # add a small interest factor

# Compute spread
df_f$spread <- df_f$future - df_f$spot

df_f$injectionCapacity <- as.numeric(df_f$injectionCapacity)
df_f$withdrawalCapacity <- as.numeric(df_f$withdrawalCapacity)

df_f <- df_f %>%
  mutate(
    myopic_net = case_when(
  spread > cost_of_carry ~ -1 * injectionCapacity,
  spread < -cost_of_carry ~ withdrawalCapacity,
  TRUE ~ 0
)
  )

# Check how many days we have injection vs. withdrawal decisions
table(df_f$myopic_net > 0)  # Withdrawal days
table(df_f$myopic_net < 0)  # Injection days

df_f$spot <- as.numeric(as.character(df_f$spot))
df_f$gasInStorage <- as.numeric(as.character(df_f$gasInStorage))

# Define physical constraints for this facility
Vmin <- quantile(df_f$gasInStorage, 0.05, na.rm = TRUE)
Vmax <- quantile(df_f$gasInStorage, 0.95, na.rm = TRUE)
inj_max <- median(df_f$injectionCapacity, na.rm = TRUE)
wdr_max <- median(df_f$withdrawalCapacity, na.rm = TRUE)
compute_optimal_benchmark <- function(df_facility, 
                                       Vmin, Vmax, 
                                       inj_max, wdr_max,
                                       phi = 0.005, 
                                       mu_i = 0.02, 
                                       mu_w = 0.02, 
                                       sigma_s = 0.0001,
                                       beta = 0.999, 
                                       G = 100) {
  
  # ---- 1. Force all inputs to numeric ----
  df_facility$spot <- as.numeric(as.character(df_facility$spot))
  df_facility$gasInStorage <- as.numeric(as.character(df_facility$gasInStorage))
  df_facility$injectionCapacity <- as.numeric(as.character(df_facility$injectionCapacity))
  df_facility$withdrawalCapacity <- as.numeric(as.character(df_facility$withdrawalCapacity))
  
  # Remove rows with missing essential data
  df_facility <- df_facility[!is.na(df_facility$spot) & !is.na(df_facility$gasInStorage), ]
  
  # Make sure cost parameters are numeric scalars
  phi <- as.numeric(phi)[1]
  mu_i <- as.numeric(mu_i)[1]
  mu_w <- as.numeric(mu_w)[1]
  sigma_s <- as.numeric(sigma_s)[1]
  beta <- as.numeric(beta)[1]
  
  T <- nrow(df_facility)
  if (T == 0) {
    warning("No valid rows for facility")
    return(data.frame(date = as.Date(character()), optimal_net = numeric()))
  }
  
  prices <- df_facility$spot
  
  # Discretize storage levels
  V_grid <- seq(Vmin, Vmax, length.out = G)
  V_grid <- as.numeric(V_grid)   # ensure numeric
  
  # Initialize value matrix with a very low number (numeric)
  value <- matrix(-1e9, nrow = G, ncol = T + 1)
  value[, T + 1] <- 0   # terminal value
  
  max_inj_rate <- as.numeric(inj_max)[1]
  max_wdr_rate <- as.numeric(wdr_max)[1]
  
  # ---- 2. Backward induction ----
  for (t in T:1) {
    p <- prices[t]
    for (i in 1:G) {
      V <- V_grid[i]
      best_val <- -Inf
      
      # (a) Withdrawal (positive net)
      max_w <- min(max_wdr_rate, V - Vmin)
      if (max_w > 0 && is.finite(max_w)) {
        dw <- seq(0, max_w, length.out = 21)[-1]
        for (dw_val in dw) {
          V_next <- V - dw_val
          # Interpolate continuation value
          cont <- approx(V_grid, value[, t+1], xout = V_next, rule = 2)$y
          if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
          cost <- phi * dw_val * p + mu_w * dw_val + sigma_s * V
          profit <- dw_val * p - cost
          total <- profit + beta * cont
          if (total > best_val) best_val <- total
        }
      }
      
      # (b) Injection (negative net)
      max_inj <- min(max_inj_rate, Vmax - V)
      if (max_inj > 0 && is.finite(max_inj)) {
        di <- seq(0, max_inj, length.out = 21)[-1]
        for (di_val in di) {
          V_next <- V + di_val
          cont <- approx(V_grid, value[, t+1], xout = V_next, rule = 2)$y
          if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
          cost <- phi * di_val * p + mu_i * di_val + sigma_s * V
          profit <- (-1) * di_val * p - cost   # injection means paying for gas
          total <- profit + beta * cont
          if (total > best_val) best_val <- total
        }
      }
      
      # (c) Do nothing
      cont <- approx(V_grid, value[, t+1], xout = V, rule = 2)$y
      if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
      profit <- -sigma_s * V
      total <- profit + beta * cont
      if (total > best_val) best_val <- total
      
      value[i, t] <- best_val
    }
  }
  
  # ---- 3. Forward simulation ----
  V_actual_start <- df_facility$gasInStorage[1]
  opt_net <- numeric(T)
  V_curr <- V_actual_start
  
  for (t in 1:T) {
    p <- prices[t]
    best_val <- -Inf
    best_dv <- 0
    
    # Withdrawal
    max_w <- min(max_wdr_rate, V_curr - Vmin)
    if (max_w > 0 && is.finite(max_w)) {
      dw <- seq(0, max_w, length.out = 21)[-1]
      for (dw_val in dw) {
        V_next <- V_curr - dw_val
        cont <- approx(V_grid, value[, t+1], xout = V_next, rule = 2)$y
        if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
        cost <- phi * dw_val * p + mu_w * dw_val + sigma_s * V_curr
        profit <- dw_val * p - cost
        total <- profit + beta * cont
        if (total > best_val) {
          best_val <- total
          best_dv <- dw_val
        }
      }
    }
    
    # Injection
    max_inj <- min(max_inj_rate, Vmax - V_curr)
    if (max_inj > 0 && is.finite(max_inj)) {
      di <- seq(0, max_inj, length.out = 21)[-1]
      for (di_val in di) {
        V_next <- V_curr + di_val
        cont <- approx(V_grid, value[, t+1], xout = V_next, rule = 2)$y
        if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
        cost <- phi * di_val * p + mu_i * di_val + sigma_s * V_curr
        profit <- (-1) * di_val * p - cost
        total <- profit + beta * cont
        if (total > best_val) {
          best_val <- total
          best_dv <- (-1) * di_val
        }
      }
    }
    
    # Do nothing
    cont <- approx(V_grid, value[, t+1], xout = V_curr, rule = 2)$y
    if (is.na(cont) || !is.numeric(cont)) cont <- -1e9
    profit <- -sigma_s * V_curr
    total <- profit + beta * cont
    if (total > best_val) {
      best_val <- total
      best_dv <- 0
    }
    
    opt_net[t] <- best_dv
    V_curr <- V_curr - best_dv
    V_curr <- min(max(V_curr, Vmin), Vmax)
  }
  
  return(data.frame(date = df_facility$date, optimal_net = opt_net))
}

benchmark <- compute_optimal_benchmark(
  df_facility = df_f,
  Vmin = Vmin,
  Vmax = Vmax,
  inj_max = inj_max,
  wdr_max = wdr_max,
  phi = fuel_loss_factor,
  mu_i = injection_cost_eur_per_mwh,
  mu_w = withdrawal_cost_eur_per_mwh,
  sigma_s = holding_cost_eur_per_mwh_per_day,
  beta = 0.999,
  G = 100
)



# Merge by date (ensure both date columns are Date type)
df_f <- left_join(df_f, benchmark, by = "date")

# Check if merging worked
summary(df_f$optimal_net)



# Convert columns to numeric (if they are character/factor)
df_f$netWithdrawal <- as.numeric(as.character(df_f$netWithdrawal))
df_f$myopic_net <- as.numeric(as.character(df_f$myopic_net))
df_f$optimal_net <- as.numeric(as.character(df_f$optimal_net))

# Check if any NAs were introduced
sum(is.na(df_f$netWithdrawal))
sum(is.na(df_f$myopic_net))
sum(is.na(df_f$optimal_net))

cor_data <- df_f %>%
  filter(!is.na(netWithdrawal), !is.na(myopic_net), !is.na(optimal_net))


# Check that columns are numeric
class(cor_data$netWithdrawal)
class(cor_data$myopic_net)
class(cor_data$optimal_net)

# If any are not numeric, convert again
cor_data$netWithdrawal <- as.numeric(cor_data$netWithdrawal)
cor_data$myopic_net <- as.numeric(cor_data$myopic_net)
cor_data$optimal_net <- as.numeric(cor_data$optimal_net)

# Now compute correlations
cor_actual_myopic <- cor(cor_data$netWithdrawal, cor_data$myopic_net, use = "complete.obs")
cor_actual_perfect <- cor(cor_data$netWithdrawal, cor_data$optimal_net, use = "complete.obs")
cor_myopic_perfect <- cor(cor_data$myopic_net, cor_data$optimal_net, use = "complete.obs")

cat("Correlation (Actual vs. Myopic)         :", round(cor_actual_myopic, 3), "\n")
cat("Correlation (Actual vs. Perfect-Foresight):", round(cor_actual_perfect, 3), "\n")
cat("Correlation (Myopic vs. Perfect-Foresight):", round(cor_myopic_perfect, 3), "\n")




# Loop over all facilities
# ---------- 3. Get list of unique facilities ----------
unique_facilities <- unique(merged$facility_eic)
cat("Total facilities:", length(unique_facilities), "\n")

# We'll filter out facilities with too few observations (e.g., < 30 days)
# Also ensure they have non-missing injection/withdrawal capacities.

valid_facilities <- merged %>%
  group_by(facility_eic) %>%
  summarise(
    n = n(),
    has_cap = all(!is.na(injectionCapacity) & !is.na(withdrawalCapacity)),
    type = first(type),
    country = first(country)
  ) %>%
  filter(n >= 30, has_cap) %>%
  pull(facility_eic)

cat("Valid facilities (>=30 days, capacity present):", length(valid_facilities), "\n")

# ---------- 4. Loop over facilities and compute correlations ----------
# Initialize results data frame
results_df <- data.frame(
  facility_eic = character(),
  type = character(),
  country = character(),
  cor_actual_myopic = numeric(),
  cor_actual_perfect = numeric(),
  cor_myopic_perfect = numeric(),
  stringsAsFactors = FALSE
)

# Counter for progress
total <- length(valid_facilities)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in seq_along(valid_facilities)) {
  eic <- valid_facilities[i]
  setTxtProgressBar(pb, i)
  
  # Extract facility data
  df_f <- merged %>% filter(facility_eic == eic) %>% arrange(date)
  
  # Ensure numeric conversions (already done in merged, but repeat for safety)
  df_f$spot <- as.numeric(as.character(df_f$spot))
  df_f$future <- as.numeric(as.character(df_f$future))
  df_f$gasInStorage <- as.numeric(as.character(df_f$gasInStorage))
  df_f$injectionCapacity <- as.numeric(as.character(df_f$injectionCapacity))
  df_f$withdrawalCapacity <- as.numeric(as.character(df_f$withdrawalCapacity))
  df_f$netWithdrawal <- as.numeric(as.character(df_f$netWithdrawal))
  
  # Remove rows with NAs in essential columns
  df_f <- df_f %>%
    filter(!is.na(spot), !is.na(future), !is.na(gasInStorage),
           !is.na(injectionCapacity), !is.na(withdrawalCapacity))
  
  if (nrow(df_f) < 10) next
  
  # ---- Compute myopic benchmark ----
  df_f$spread <- df_f$future - df_f$spot
  df_f <- df_f %>%
    mutate(
      myopic_net = case_when(
        spread > cost_of_carry ~ -1 * injectionCapacity,
        spread < -cost_of_carry ~ withdrawalCapacity,
        TRUE ~ 0
      )
    )
  
  # ---- Physical constraints ----
  Vmin <- quantile(df_f$gasInStorage, 0.05, na.rm = TRUE)
  Vmax <- quantile(df_f$gasInStorage, 0.95, na.rm = TRUE)
  inj_max <- median(df_f$injectionCapacity, na.rm = TRUE)
  wdr_max <- median(df_f$withdrawalCapacity, na.rm = TRUE)
  
  # ---- Run DP benchmark (with tryCatch to avoid crashes) ----
  benchmark <- tryCatch({
    compute_optimal_benchmark(
      df_facility = df_f,
      Vmin = Vmin,
      Vmax = Vmax,
      inj_max = inj_max,
      wdr_max = wdr_max,
      phi = fuel_loss_factor,
      mu_i = injection_cost_eur_per_mwh,
      mu_w = withdrawal_cost_eur_per_mwh,
      sigma_s = holding_cost_eur_per_mwh_per_day,
      beta = 0.999,
      G = 50   # reduced G for speed; can increase if needed
    )
  }, error = function(e) {
    warning("DP failed for facility ", eic, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(benchmark) || nrow(benchmark) == 0) next
  
  # ---- Merge and compute correlations ----
  df_f <- left_join(df_f, benchmark, by = "date")
  
  # Ensure numeric
  df_f$optimal_net <- as.numeric(as.character(df_f$optimal_net))
  df_f$myopic_net <- as.numeric(as.character(df_f$myopic_net))
  df_f$netWithdrawal <- as.numeric(as.character(df_f$netWithdrawal))
  
  cor_data <- df_f %>%
    filter(!is.na(netWithdrawal), !is.na(myopic_net), !is.na(optimal_net))
  
  if (nrow(cor_data) < 5) next
  
  # Compute correlations
  cor_actual_myopic <- cor(cor_data$netWithdrawal, cor_data$myopic_net, use = "complete.obs")
  cor_actual_perfect <- cor(cor_data$netWithdrawal, cor_data$optimal_net, use = "complete.obs")
  cor_myopic_perfect <- cor(cor_data$myopic_net, cor_data$optimal_net, use = "complete.obs")
  
  # Store results
  results_df <- rbind(results_df, data.frame(
    facility_eic = eic,
    type = first(df_f$type),
    country = first(df_f$country),
    cor_actual_myopic = cor_actual_myopic,
    cor_actual_perfect = cor_actual_perfect,
    cor_myopic_perfect = cor_myopic_perfect,
    stringsAsFactors = FALSE
  ))
}

close(pb)

# ---------- 5. View and save results ----------
cat("\nNumber of facilities processed successfully:", nrow(results_df), "\n")
print(head(results_df))

# Save to CSV
write.csv(results_df, "facility_correlations.csv", row.names = FALSE)

# Summary statistics
summary(results_df[, c("cor_actual_myopic", "cor_actual_perfect", "cor_myopic_perfect")])

# Optional: plot histogram of actual vs. myopic correlations
if (nrow(results_df) > 0) {
  hist(results_df$cor_actual_myopic, main = "Distribution of Actual vs. Myopic Correlation",
       xlab = "Correlation", col = "steelblue")
}






library(ggplot2)
library(dplyr)
library(tidyr)


library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1. Compute summary statistics for all three correlations ----
summary_stats <- results_df %>%
  summarise(
    n = n(),
    mean_myopic = mean(cor_actual_myopic, na.rm = TRUE),
    se_myopic   = sd(cor_actual_myopic, na.rm = TRUE) / sqrt(n),
    mean_perfect = mean(cor_actual_perfect, na.rm = TRUE),
    se_perfect   = sd(cor_actual_perfect, na.rm = TRUE) / sqrt(n),
    mean_myopic_perfect = mean(cor_myopic_perfect, na.rm = TRUE),
    se_myopic_perfect   = sd(cor_myopic_perfect, na.rm = TRUE) / sqrt(n)
  )

# ---- 2. Reshape to long format for plotting ----
plot_data <- summary_stats %>%
  select(starts_with("mean"), starts_with("se")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("stat", "metric"),
    names_pattern = "(mean|se)_(.*)"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

# ---- 3. Add benchmark references ----
benchmarks <- data.frame(
  metric = c("myopic", "perfect", "myopic_perfect"),
  benchmark = c(0.08, NA, 0.21)   # NA means no reference for actual vs perfect
)

plot_data <- left_join(plot_data, benchmarks, by = "metric")

# ---- 4. Create labels for the x-axis ----
plot_data <- plot_data %>%
  mutate(
    label = case_when(
      metric == "myopic" ~ "Actual vs. Myopic",
      metric == "perfect" ~ "Actual vs.\nPerfect-Foresight",
      metric == "myopic_perfect" ~ "Myopic vs.\nPerfect-Foresight"
    )
  )

# ---- 5. Generate the plot ----
p <- ggplot(plot_data, aes(x = metric, y = mean)) +
  # Error bars: 95% CI
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.2, size = 1, color = "steelblue") +
  # Mean dot
  geom_point(size = 4, color = "steelblue") +
  # Benchmark reference dots (only for metrics with non-NA benchmark)
  geom_point(data = subset(plot_data, !is.na(benchmark)),
             aes(y = benchmark), color = "darkred", size = 4, shape = 17) +
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Labels
  scale_x_discrete(labels = setNames(plot_data$label, plot_data$metric)) +
  labs(
    title = "Correlation of Actual Storage Flows with Competitive Benchmarks",
    subtitle = paste0("Mean across ", summary_stats$n, " facilities; 95% CI; red triangles = Boogert & de Jong (2006) benchmarks"),
    x = "",
    y = "Correlation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11)
  )

print(p)


# Full panel of daily information
# Initialize list to store daily data
panel_list <- list()

# Loop over valid facilities (same as before)
for (eic in valid_facilities) {
  df_f <- merged %>% filter(facility_eic == eic) %>% arrange(date)
  
  # Ensure numeric conversions
  df_f$spot <- as.numeric(as.character(df_f$spot))
  df_f$future <- as.numeric(as.character(df_f$future))
  df_f$gasInStorage <- as.numeric(as.character(df_f$gasInStorage))
  df_f$injectionCapacity <- as.numeric(as.character(df_f$injectionCapacity))
  df_f$withdrawalCapacity <- as.numeric(as.character(df_f$withdrawalCapacity))
  df_f$netWithdrawal <- as.numeric(as.character(df_f$netWithdrawal))
  
  # Remove NAs
  df_f <- df_f %>%
    filter(!is.na(spot), !is.na(future), !is.na(gasInStorage),
           !is.na(injectionCapacity), !is.na(withdrawalCapacity))
  
  if (nrow(df_f) < 10) next
  
  # ---- Myopic benchmark ----
  df_f$spread <- df_f$future - df_f$spot
  df_f <- df_f %>%
    mutate(
      myopic_net = case_when(
        spread > cost_of_carry ~ -1 * injectionCapacity,
        spread < -cost_of_carry ~ withdrawalCapacity,
        TRUE ~ 0
      )
    )
  
  # ---- Physical constraints ----
  Vmin <- quantile(df_f$gasInStorage, 0.05, na.rm = TRUE)
  Vmax <- quantile(df_f$gasInStorage, 0.95, na.rm = TRUE)
  inj_max <- median(df_f$injectionCapacity, na.rm = TRUE)
  wdr_max <- median(df_f$withdrawalCapacity, na.rm = TRUE)
  
  # ---- Dynamic benchmark ----
  benchmark <- tryCatch({
    compute_optimal_benchmark(
      df_facility = df_f,
      Vmin = Vmin,
      Vmax = Vmax,
      inj_max = inj_max,
      wdr_max = wdr_max,
      phi = fuel_loss_factor,
      mu_i = injection_cost_eur_per_mwh,
      mu_w = withdrawal_cost_eur_per_mwh,
      sigma_s = holding_cost_eur_per_mwh_per_day,
      beta = 0.999,
      G = 50
    )
  }, error = function(e) {
    warning("DP failed for facility ", eic)
    return(NULL)
  })
  
  if (is.null(benchmark)) next
  
  # ---- Merge and select columns ----
  df_f <- left_join(df_f, benchmark, by = "date")
  
  # Keep only needed columns for the panel
  df_f_panel <- df_f %>%
    select(
      facility_eic,
      date,
      type,
      country,
      spot,
      netWithdrawal,
      myopic_net,
      optimal_net
    ) %>%
    filter(!is.na(netWithdrawal), !is.na(myopic_net), !is.na(optimal_net))
  
  if (nrow(df_f_panel) > 0) {
    panel_list[[eic]] <- df_f_panel
  }
}

# Combine all facilities into one panel
panel_df <- bind_rows(panel_list)

# Save for later use
write.csv(panel_df, "facility_daily_panel.csv", row.names = FALSE)



write.csv(panel_df, "C:/Users/UX425/Desktop/facility_daily_panel.csv", row.names = FALSE)


# importing panel_df csv file from the desktop
panel_df <- read.csv("C:/Users/UX425/Documents/storage/facility_daily_panel.csv")


panel_df <- panel_df %>%
  mutate(
    deviation_myopic = netWithdrawal - myopic_net,
    deviation_optimal = netWithdrawal - optimal_net,
    month = month(date),
    type_factor = factor(type),
    facility_eic = factor(facility_eic)
  )


library(lfe)   # for high‑dimensional fixed effects

# Without facility fixed effects
model_type <- felm(deviation_myopic ~ spot + type_factor + factor(month) | 0 | 0 | facility_eic,
                   data = panel_df)
summary(model_type)

# Using monthly fixed effects (as you have already done)
model_monthly <- felm(deviation_myopic ~ spot + type_factor + factor(month) | 0 | 0 | facility_eic, data = panel_df)

# Alternatively, using injection/withdrawal season dummies
panel_df <- panel_df %>%
  mutate(
    season = case_when(
      month %in% c(11, 12, 1, 2, 3) ~ "Withdrawal",
      month %in% c(4, 5, 6, 7, 8, 9, 10) ~ "Injection",
      TRUE ~ NA_character_
    )
  )
model_season <- felm(deviation_myopic ~ spot + type_factor + factor(season) | 0 | 0 | facility_eic, data = panel_df)
summary(model_season)


# Full model: type + month + country FE
model_country <- felm(deviation_myopic ~ spot + type_factor | 
                        factor(month) + country | 0 | facility_eic,
                      data = panel_df)
summary(model_country)

# Run the same specification but with deviation_optimal
model_optimal <- felm(deviation_optimal ~ spot + type_factor | factor(month) + country | 0 | facility_eic,
                      data = panel_df)

summary(model_optimal)


# Convert panel_df$date to Date
panel_df$date <- as.Date(panel_df$date)

# Now re-run the join
panel_df <- panel_df %>%
  left_join(merged %>% select(all_of(merge_vars)), 
            by = c("facility_eic", "date"))

# Check again
summary(panel_df[, c("future", "full", "workingGasVolume")])



# Ensure date is Date in both
panel_df$date <- as.Date(panel_df$date)
merged$date <- as.Date(merged$date)

# Remove duplicates in merged
merged_unique <- merged %>%
  distinct(facility_eic, date, .keep_all = TRUE)

# Check if any duplicates remain
dup_check <- merged_unique %>%
  group_by(facility_eic, date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if (nrow(dup_check) > 0) {
  print("Duplicates still exist after distinct() – check data")
}

# List columns in merged_unique
cat("Columns in merged_unique:\n")
print(colnames(merged_unique))

# Check which of your merge_vars are missing
merge_vars <- c("facility_eic", "date", "future", "full", 
                "workingGasVolume", "injectionCapacity", "withdrawalCapacity",
                "gasInStorage")
missing_cols <- setdiff(merge_vars, colnames(merged_unique))
if (length(missing_cols) > 0) {
  cat("Missing columns in merged_unique:\n")
  print(missing_cols)
}

# Select only the columns you need
merged_subset <- merged_unique %>%
  select(all_of(merge_vars))

# Join
panel_df <- panel_df %>%
  left_join(merged_subset, by = c("facility_eic", "date"))

# Check the result
str(panel_df)
summary(panel_df[, c("future", "full", "workingGasVolume")])


# Convert relevant columns to numeric
panel_df$workingGasVolume <- as.numeric(as.character(panel_df$workingGasVolume))
panel_df$full <- as.numeric(as.character(panel_df$full))
panel_df$injectionCapacity <- as.numeric(as.character(panel_df$injectionCapacity))
panel_df$withdrawalCapacity <- as.numeric(as.character(panel_df$withdrawalCapacity))
panel_df$future <- as.numeric(as.character(panel_df$future))

# Now re-run mutate
panel_df <- panel_df %>%
  mutate(
    spread = future - spot,
    post2022 = ifelse(date >= as.Date("2022-06-01"), 1, 0),
    log_capacity = log(workingGasVolume),
    full_ratio = full / 100,   # if full is 0-100
    post2022_spread = post2022 * spread
  )

# Check that new variables are created
summary(panel_df[, c("spread", "log_capacity", "full_ratio")])



# Keep only rows with complete data for regression variables
panel_df_clean <- panel_df %>%
  filter(!is.na(spot), !is.na(future), !is.na(full),
         !is.na(workingGasVolume), !is.na(injectionCapacity),
         !is.na(withdrawalCapacity), !is.na(deviation_myopic),
         !is.na(type_factor))

# Check remaining rows
nrow(panel_df_clean)


panel_df_clean <- panel_df_clean %>%
  mutate(
    spread = future - spot,
    post2022 = ifelse(date >= as.Date("2022-06-01"), 1, 0),
    log_capacity = log(workingGasVolume),  # now workingGasVolume is numeric
    full_ratio = full / 100,   # if full is 0-100
    post2022_spread = post2022 * spread
  )

# Ensure type_factor is a factor and drop unused levels
panel_df_clean$type_factor <- droplevels(as.factor(panel_df_clean$type_factor))

# Exclude SRC from the data (or drop it from the factor levels)
panel_df_clean <- panel_df_clean %>%
  filter(type_factor != "SRC")
library(felm)

# Start simple
model_simple <- felm(deviation_myopic ~ spot + spread + type_factor + type_factor:spread |
                       factor(month) + country | 0 | facility_eic,
                     data = panel_df_clean)
summary(model_simple)

# Then add controls one by one
model_controls <- felm(deviation_myopic ~ spot + spread + full_ratio + log_capacity +
                         injectionCapacity + withdrawalCapacity +
                         type_factor + type_factor:spread |
                         factor(month) + country | 0 | facility_eic,
                       data = panel_df_clean)
summary(model_controls)