# --- Phase 3: Predictive Modelling and Evaluation ---

# 1. Load Required Libraries
library(tidyverse)
library(broom)   # For tidying model output
library(Metrics) # For easily calculating RMSE and MAE

# 2. Load the clean training and testing data from Phase 1
train_data <- read_csv(
  "processed_data/train_set.csv",
  show_col_types = FALSE
)
test_data <- read_csv(
  "processed_data/test_set.csv",
  show_col_types = FALSE
)


# 3. Feature Scaling
# To interpret our final model's coefficients fairly, we need to scale the
# predictor variables. We will standardise them (mean = 0, sd = 1).

# Learn the scaling parameters (mean and sd) from the training
# data ONLY to prevent data leakage from the test set.
scaling_params <- train_data %>%
  summarise(
    across(
      c(
        gdp_per_capita, social_support, health,
        freedom, trust, generosity
      ),
      list(mean = mean, sd = sd)
    )
  )

# Now, apply these learned parameters to both the training and testing sets.
train_scaled <- train_data %>%
  mutate(
    gdp_per_capita = (gdp_per_capita - scaling_params$gdp_per_capita_mean) /
      scaling_params$gdp_per_capita_sd,
    social_support = (social_support - scaling_params$social_support_mean) /
      scaling_params$social_support_sd,
    health = (health - scaling_params$health_mean) / scaling_params$health_sd,
    freedom = (freedom - scaling_params$freedom_mean) / scaling_params$freedom_sd,
    trust = (trust - scaling_params$trust_mean) / scaling_params$trust_sd,
    generosity = (generosity - scaling_params$generosity_mean) /
      scaling_params$generosity_sd
  )

test_scaled <- test_data %>%
  mutate(
    gdp_per_capita = (gdp_per_capita - scaling_params$gdp_per_capita_mean) /
      scaling_params$gdp_per_capita_sd,
    social_support = (social_support - scaling_params$social_support_mean) /
      scaling_params$social_support_sd,
    health = (health - scaling_params$health_mean) / scaling_params$health_sd,
    freedom = (freedom - scaling_params$freedom_mean) / scaling_params$freedom_sd,
    trust = (trust - scaling_params$trust_mean) / scaling_params$trust_sd,
    generosity = (generosity - scaling_params$generosity_mean) /
      scaling_params$generosity_sd
  )


# 4. Train the Final Multiple Linear Regression Model
# We use the scaled training data. The '.' means "use all other columns as predictors".
final_model <- lm(
  happiness_score_next_year ~ .,
  data = select(train_scaled, -year, -country)
)

# 5. Interpret the Final Model's Coefficients
cat("--- Final Model Coefficients (from scaled data) ---\n\n")
# Using broom::tidy() gives a nice, clean data frame of the results
model_summary <- tidy(final_model)
print(model_summary)


# 6. Make Predictions on the Unseen Test Set
predictions <- predict(final_model, newdata = test_scaled)

# Combine predictions with actual values for evaluation
evaluation_df <- test_data %>%
  mutate(predicted_score = predictions)


# 7. Evaluate Model Performance
cat("\n--- Model Performance on the 2018 -> 2019 Test Set ---\n\n")

rmse_val <- rmse(evaluation_df$happiness_score_next_year, evaluation_df$predicted_score)
mae_val <- mae(evaluation_df$happiness_score_next_year, evaluation_df$predicted_score)

cat("Root Mean Squared Error (RMSE):", round(rmse_val, 4), "\n")
cat("Mean Absolute Error (MAE):     ", round(mae_val, 4), "\n")


# 8. Create Visualization for the Final Report
performance_plot <- ggplot(
  evaluation_df,
  aes(x = happiness_score_next_year, y = predicted_score)
) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Model Performance: Actual vs. Predicted Happiness Score",
    subtitle = "Predictions for 2019 based on 2018 data",
    x = "Actual Happiness Score (2019)",
    y = "Predicted Happiness Score (2019)"
  ) +
  coord_fixed(xlim = c(2.5, 8), ylim = c(2.5, 8)) +
  theme_minimal()

# Display the plot
print(performance_plot)

# Save the plot for the report
ggsave("final_report_visuals/performance_plot.png", performance_plot, width = 8, height = 6)