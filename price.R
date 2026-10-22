rm(list=ls())
# Load the necessary library 
library(lubridate)
library(dplyr)
library(ggplot2)
library(broom)
library(foreach)
library(doParallel)
library(readxl)
library(tidyr) # for pivot_wider
library(zoo) # for rolling mean

# Define the file paths for the Excel files
file_paths <- list(
  "mibgas/MIBGAS_Data_2018.xlsx",
  "mibgas/MIBGAS_Data_2019.xlsx",
  "mibgas/MIBGAS_Data_2020.xlsx",
  "mibgas/MIBGAS_Data_2021.xlsx",
  "mibgas/MIBGAS_Data_2022.xlsx",
  "mibgas/MIBGAS_Data_2023.xlsx",
  "mibgas/MIBGAS_Data_2024.xlsx",
  "mibgas/MIBGAS_Data_2025.xlsx"
)
# Initialize an empty list to store the datasets
datasets <- list()

# Loop through the file paths and read the specified sheets
for (i in seq_along(file_paths)) {
  if (i <= 3) {
    # For the first three files, read the 'Trading Data PVB' sheet
    datasets[[i]] <- read_excel(file_paths[[i]], sheet = "Trading Data PVB")
  } else {
    # For the remaining files, read the 'Trading Data PVB&VTP' sheet
    datasets[[i]] <- read_excel(file_paths[[i]], sheet = "Trading Data PVB&VTP")
  }
}
# Assign names to the datasets for easier reference
names(datasets) <- c(
  "MIBGAS_Data_2018",
  "MIBGAS_Data_2019",
  "MIBGAS_Data_2020",
  "MIBGAS_Data_2021",
  "MIBGAS_Data_2022",
  "MIBGAS_Data_2023",
  "MIBGAS_Data_2024",
  "MIBGAS_Data_2025"
)

#Give the column names of each dataset
column_names <- lapply(datasets, colnames)


datasets <- lapply(datasets, function(df) {
  # Dynamically find the reference price column name (handles \n and naming shifts)
  price_col <- names(df)[grepl("Reference Price", names(df))][1]
  
  df %>%
    select(
      `Trading day`, 
      Product, 
      `Place of delivery`, 
      `First Day Delivery`, 
      `Last Day Delivery`, 
      all_of(price_col)
    ) %>%
    rename(Price = all_of(price_col))
})

# Check the new standardized column names of each dataset
column_names <- lapply(datasets, colnames)
print(column_names)

# Combine all datasets in the list into a single master dataframe
master_mibgas <- bind_rows(datasets, .id = "Source_File")

# Inspect the structure of the master dataset
glimpse(master_mibgas)

# Filter for the seasonal products (GSES_S and GSES_W) to check availability
seasonal_products <- master_mibgas %>%
  filter(Product %in% c("GSES_S", "GSES_W"))

# 1. Ensure Trading day is a Date type
master_mibgas <- master_mibgas %>%
  mutate(`Trading day` = as.Date(`Trading day`))





# --- Diagnostic: confirm delivery windows for seasonal products look right ---
seasonal_products <- seasonal_products %>%
  mutate(
    `First Day Delivery` = as.Date(`First Day Delivery`),
    `Last Day Delivery`  = as.Date(`Last Day Delivery`)
  )

seasonal_products %>%
  distinct(Product, `First Day Delivery`, `Last Day Delivery`) %>%
  arrange(`First Day Delivery`) %>%
  print(n = 20)
# Check: Summer rows should run ~Apr 1 to ~Sep 30; Winter rows ~Oct 1 to ~Mar 31 (next year)




# Make sure dates are properly formatted
seasonal_tagged <- seasonal_products %>%
  mutate(
    `Trading day` = as.Date(`Trading day`),
    `First Day Delivery` = as.Date(`First Day Delivery`),
    `Last Day Delivery` = as.Date(`Last Day Delivery`),

    season_type = if_else(
      Product == "GSES_S",
      "Summer",
      "Winter"
    ),

    season_start_year = year(`First Day Delivery`)
  )

# Keep only the season that starts in the same calendar year
# and create a clean Summer/Winter pair identifier
spread_df <- seasonal_tagged %>%
  select(
    `Trading day`,
    season_type,
    season_start_year,
    Price
  ) %>%
  filter(
    season_start_year >= 2018,
    season_start_year <= 2027
  ) %>%
  group_by(`Trading day`, season_type, season_start_year) %>%
  summarise(
    Price = first(Price),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(`Trading day`, season_start_year),
    names_from = season_type,
    values_from = Price
  ) %>%
  filter(
    !is.na(Summer),
    !is.na(Winter)
  ) %>%
  mutate(
    Spread = Winter - Summer
  ) %>%
  arrange(`Trading day`)


ggplot(spread_df, aes(x = `Trading day`, y = Spread)) +
  geom_line(linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "PVB Summer–Winter Gas Price Spread",
    x = "Trading day",
    y = "Winter price − Summer price (€ / MWh)"
  ) +
  theme_minimal()