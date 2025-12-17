# --- Final Analysis: Naive vs. Pure Raw vs. Hybrid Model ---

# 1. Load All Required Libraries
required_packages <- c("tidyverse", "janitor", "broom", "Metrics", "patchwork")
# Quick install check
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages(lapply(required_packages, require, character.only = TRUE))

# 2. Load Data
raw_data <- read_csv("processed_data/raw_master_dataset.csv", show_col_types = FALSE)

# 3. Create 3-Year Lagged Dataset with Validation
lag_data <- raw_data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    happiness_future = lead(happiness_score, n = 3),
    happiness_present = happiness_score, # For Naive Baseline
    target_year = year + 3,
    year_diff = lead(year, n = 3) - year
  ) %>%
  filter(year_diff == 3) %>%
  select(-year_diff) %>%
  ungroup() %>%
  drop_na()

# 4. Split Eras
training_data <- lag_data %>% filter(target_year <= 2020)
crisis_data   <- lag_data %>% filter(target_year %in% c(2021, 2022))
recovery_data <- lag_data %>% filter(target_year >= 2023)

# --- 5. THE SHOWDOWN: Training the Models ---
cat("\n--- Training Models on 'Training Era' Data ---\n")

# Model A: The "Pure Raw" Model (GDP + Health ONLY)
# This answers the reviewer's critique about data leakage/circularity.
pure_model <- lm(
  happiness_future ~ log(raw_gdp) + raw_health,
  data = training_data
)

# Model B: The "Hybrid" Model (All Predictors)
# This is our previous 'complex' model.
hybrid_model <- lm(
  happiness_future ~ log(raw_gdp) + raw_health + social_support + freedom + trust + generosity,
  data = training_data
)

# --- 6. Evaluate All 3 Approaches ---
evaluate_models <- function(data, era_name) {
  # 1. Naive Baseline
  rmse_naive <- rmse(data$happiness_future, data$happiness_present)

  # 2. Pure Raw Model
  preds_pure <- predict(pure_model, newdata = data)
  rmse_pure <- rmse(data$happiness_future, preds_pure)

  # 3. Hybrid Model
  preds_hybrid <- predict(hybrid_model, newdata = data)
  rmse_hybrid <- rmse(data$happiness_future, preds_hybrid)

  tibble(
    Era = era_name,
    RMSE_Naive = rmse_naive,
    RMSE_Pure_Raw = rmse_pure,
    RMSE_Hybrid = rmse_hybrid
  )
}

# Run evaluations
results <- bind_rows(
  evaluate_models(training_data, "1. Training (<=2020)"),
  evaluate_models(crisis_data,   "2. Crisis (2021-22)"),
  evaluate_models(recovery_data, "3. Recovery (2023+)")
)

cat("\n--- FINAL SCOREBOARD: Root Mean Squared Error (Lower is Better) ---\n")
print(results)

# --- 7. Calculate the "Value of Survey Data" ---
# How much better is Hybrid than Pure?
cat("\n--- Insight: Does Survey Data Add Value? ---\n")
improvement <- results %>%
  mutate(Survey_Data_Improvement = RMSE_Pure_Raw - RMSE_Hybrid) %>%
  select(Era, Survey_Data_Improvement)
print(improvement)

cat("\n* If Improvement is positive, the survey data (freedom, trust, etc.) improved predictions over just GDP/Health.\n")