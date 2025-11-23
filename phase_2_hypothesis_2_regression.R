# 1. Load Required Libraries
library(tidyverse)

# 2. Load the clean training data from Phase 1
train_data <- read_csv(
  "processed_data/train_set.csv",
  show_col_types = FALSE
)

# 3. Define and fit the multiple linear regression model
# We are predicting next year's score using all six predictors from the current year.
lm_fit <- lm(
  happiness_score_next_year ~ gdp_per_capita + social_support + health +
    freedom + trust + generosity,
  data = train_data
)

# 4. Present the results of the model
# The F-statistic and its p-value at the bottom of the summary test our hypothesis.
cat("--- Multiple Linear Regression Model Summary ---\n\n")
print(summary(lm_fit))

# 5. Visual Assumption Checks
# Set up a 2x2 plotting area
par(mfrow = c(2, 2))
# Generate the standard diagnostic plots for the model
plot(lm_fit)
# Reset plotting area
par(mfrow = c(1, 1))

cat("\n--- Notes on Diagnostic Plots ---\n")
cat("1. 'Residuals vs Fitted': Check for non-linear patterns. A flat, random cloud is good.\n")
cat("2. 'Normal Q-Q': Check if residuals follow a normal distribution. Points should be close to the dashed line.\n")