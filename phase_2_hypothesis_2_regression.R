# --- Phase 2: Hypothesis 2 - Multiple Regression (FINAL - Using RAW Data) ---

# 1. Load Libraries
library(tidyverse)
library(car) # For VIF

# 2. UPDATED: Load the new raw master dataset
raw_data <- read_csv("processed_data/raw_master_dataset.csv", show_col_types = FALSE)

# 3. Create 1-Year Lag Data
lag_data_1yr <- raw_data %>%
  arrange(country, year) %>% group_by(country) %>%
  mutate(happiness_score_future = lead(happiness_score, n = 1)) %>%
  ungroup() %>% drop_na()

# 4. Create 3-Year Lag Data (with validation)
lag_data_3yr <- raw_data %>%
  arrange(country, year) %>% group_by(country) %>%
  mutate(
    happiness_score_future = lead(happiness_score, n = 3),
    year_diff = lead(year, n = 3) - year
  ) %>%
  filter(year_diff == 3) %>% select(-year_diff) %>%
  ungroup() %>% drop_na()

# 5. Fit both models using RAW data
# We should log-transform GDP as it's highly skewed, which helps linear models
model_1yr <- lm(happiness_score_future ~ log(raw_gdp) + raw_health + social_support + freedom + trust + generosity, data = lag_data_1yr)
model_3yr <- lm(happiness_score_future ~ log(raw_gdp) + raw_health + social_support + freedom + trust + generosity, data = lag_data_3yr)

# 6. Present the 3-Year Model Results & VIF
cat("--- Multiple Linear Regression Model Summary (RAW Data, 3-Year Lag) ---\n")
print(summary(model_3yr))
cat("\n--- Variance Inflation Factor (VIF) Test (3-Year Model) ---\n")
print(vif(model_3yr))

# 7. Highlight the Compounding Effects
coef_1yr <- coef(model_1yr)[-1] # Exclude intercept
coef_3yr <- coef(model_3yr)[-1] # Exclude intercept

coefficient_comparison <- tibble(
  Predictor = names(coef_3yr),
  `Effect_at_1_Year` = coef_1yr,
  `Effect_at_3_Years` = coef_3yr,
  `Temporal_Scaling_Ratio` = `Effect_at_3_Years` / `Effect_at_1_Year`
) %>%
  # Make the predictor name for log(raw_gdp) cleaner
  mutate(Predictor = ifelse(Predictor == "log(raw_gdp)", "log(raw_gdp)", Predictor))

cat("\n--- Temporal Scaling Analysis: Comparing 1-Year vs 3-Year Coefficients ---\n\n")
print(coefficient_comparison)
cat("\n*A ratio > 1 suggests the predictor's long-term influence is stronger than its short-term influence.*\n")