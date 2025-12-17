# --- Phase 3: Final Model with RAW DATA & Black Swan Test ---

# 1. Load Required Libraries
library(tidyverse)
library(broom)
library(Metrics)
library(patchwork)

# 2. Load the NEW raw master dataset
raw_master_data <- read_csv("processed_data/raw_master_dataset.csv", show_col_types = FALSE)

# 3. Create the complete lagged dataset (3-year lag with validation)
lag_data <- raw_master_data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    happiness_score_future = lead(happiness_score, n = 3),
    target_year = year + 3,
    year_diff = lead(year, n = 3) - year
  ) %>%
  filter(year_diff == 3) %>%
  select(-year_diff) %>%
  ungroup() %>%
  drop_na()

# 4. Split data into the three distinct eras
training_data <- lag_data %>% filter(target_year <= 2020)
crisis_data <- lag_data %>% filter(target_year %in% c(2021, 2022))
recovery_data <- lag_data %>% filter(target_year >= 2023)

# 5. Train ONE model on the Training era data, now using RAW predictors
final_model_raw <- lm(
  # UPDATED FORMULA with new variable names
  happiness_score_future ~ raw_gdp + raw_health + social_support + freedom + trust + generosity,
  data = training_data
)

# 6. Define the evaluation function
evaluate_model <- function(model, data, era_name) {
  predictions_with_intervals <- predict(model, newdata = data, interval = "prediction")
  eval_data <- data %>%
    bind_cols(as_tibble(predictions_with_intervals) %>% rename(predicted_score = fit, pred_lower = lwr, pred_upper = upr))

  metrics <- tibble(
    Era = era_name,
    R_Squared = cor(eval_data$happiness_score_future, eval_data$predicted_score)^2,
    RMSE = rmse(eval_data$happiness_score_future, eval_data$predicted_score),
    MAE = mae(eval_data$happiness_score_future, eval_data$predicted_score)
  )
  return(list(metrics = metrics, data_with_preds = eval_data))
}

# 7. Evaluate the single model across all three eras
training_eval <- evaluate_model(final_model_raw, training_data, "Training (<=2020 Targets)")
crisis_eval <- evaluate_model(final_model_raw, crisis_data, "Crisis (2021-2022 Targets)")
recovery_eval <- evaluate_model(final_model_raw, recovery_data, "Recovery (2023+ Targets)")

# 8. Present the final results
comparison_table <- bind_rows(training_eval$metrics, crisis_eval$metrics, recovery_eval$metrics)
cat("\n--- Quantitative Performance Across Eras (Using RAW Data) ---\n")
print(comparison_table)

plot_data <- bind_rows(
  training_eval$data_with_preds %>% mutate(Era = "A) Training Era"),
  crisis_eval$data_with_preds %>% mutate(Era = "B) Crisis Era"),
  recovery_eval$data_with_preds %>% mutate(Era = "C) Recovery Era")
)

final_plot <- ggplot(plot_data, aes(x = happiness_score_future, y = predicted_score)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "grey50", alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(color = factor(target_year)), alpha = 0.8) +
  facet_wrap(~Era) +
  coord_fixed(xlim = c(2, 8.5), ylim = c(2, 8.5)) +
  labs(
    title = "Model Performance with Raw Data (GDP & Health)",
    subtitle = "Prediction uncertainty (grey ribbon) increases in Crisis and Recovery eras",
    x = "Actual Happiness Score", y = "Predicted Happiness Score", color = "Target Year"
  ) +
  theme_minimal() + theme(legend.position = "bottom")

print(final_plot)
ggsave("final_report_visuals/raw_data_multi_era_plot.png", final_plot, width = 12, height = 5)

cat("\nAnalysis with RAW DATA is complete. The definitive plot has been saved.\n")