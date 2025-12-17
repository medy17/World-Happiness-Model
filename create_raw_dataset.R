# --- Script 1: Create Raw Master Dataset (Final, Corrected Version) ---

# 1. Load Required Libraries
library(tidyverse)
library(janitor)
library(wbstats)   # For fetching World Bank data
library(countrycode) # For standardizing country names

# 2. Define the Raw Indicators We Want from the World Bank
# The names we give here ("raw_gdp", "raw_health") become the column names.
wb_indicators <- c(
  "raw_gdp" = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 intl $)
  "raw_health" = "SP.DYN.LE00.IN"  # Life expectancy at birth, total (years)
)

cat("Fetching raw indicator data from World Bank API...\n")

# 3. Fetch the Raw Data
# wbstats automatically creates columns named 'raw_gdp' and 'raw_health' for us.
raw_wb_data <- wb_data(
  country = "all",
  indicator = wb_indicators,
  start_date = 2010,
  end_date = 2024
)

# 4. Clean the World Bank Data (No pivot needed!)
wb_data_clean <- raw_wb_data %>%
  # Rename 'date' to 'year' and standardize country names
  rename(year = date) %>%
  mutate(clean_country_name = countrycode(country, "country.name", "country.name")) %>%
  # Select only the columns we need for the merge
  select(year, clean_country_name, raw_gdp, raw_health) %>%
  # Important: Filter out rows for aggregated regions (like 'Africa Eastern and Southern')
  # which don't have happiness scores.
  drop_na(clean_country_name)

cat("World Bank data fetched and cleaned.\n")

# 5. Load and Prepare the World Happiness Report Data
whr_scores <- read_csv("processed_data/full_clean_data.csv", show_col_types = FALSE) %>%
  mutate(clean_country_name = countrycode(country, "country.name", "country.name")) %>%
  select(year, clean_country_name, happiness_score, social_support, freedom, trust, generosity) %>%
  drop_na(clean_country_name)

# 6. Merge the Two Datasets
raw_master_dataset <- left_join(
  whr_scores,
  wb_data_clean,
  by = c("year", "clean_country_name")
) %>%
  # We MUST drop rows where the merge failed (i.e., no WB data for that country/year)
  drop_na(raw_gdp, raw_health) %>%
  # Clean up and reorder
  select(year, country = clean_country_name, happiness_score, raw_gdp, raw_health, everything())

# 7. Save the final, bulletproof dataset
output_dir <- "processed_data"
write_csv(raw_master_dataset, file.path(output_dir, "raw_master_dataset.csv"))

cat("\n--- Bulletproof Dataset Creation Complete ---\n")
cat("Total rows in final merged dataset:", nrow(raw_master_dataset), "\n")
cat("Saved to 'processed_data/raw_master_dataset.csv'.\n")