# --- Phase 2: Hypothesis 1 - Correlation Test ---

# 1. Load Required Libraries
library(tidyverse)
library(ggpubr) # For adding correlation info to plots

# 2. Load the clean training data from Phase 1
# This script assumes it's being run from the project's root directory
train_data <- read_csv(
  "processed_data/train_set.csv",
  show_col_types = FALSE
)

# 3. Visual Assumption Check: Create a scatter plot
correlation_plot <- ggplot(
  train_data,
  aes(x = gdp_per_capita, y = happiness_score_next_year)
) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(
    method = "pearson",
    label.x = 0.2,
    label.y = 7.8
  ) + # Add correlation coefficient to plot
  labs(
    title = "GDP per Capita (Year t) vs. Happiness Score (Year t+1)",
    subtitle = "Data from 2015-2017 training set",
    x = "GDP per Capita Contribution",
    y = "Next Year's Happiness Score"
  ) +
  theme_minimal()

# Display the plot
print(correlation_plot)

# 4. Perform the Pearson Correlation Test
# We test the alternative hypothesis that the true correlation is positive ("greater")
correlation_test_result <- cor.test(
  ~ gdp_per_capita + happiness_score_next_year,
  data = train_data,
  alternative = "greater",
  conf.level = 0.95
)

# 5. Present the results clearly
cat("--- Pearson's Correlation Test Results ---\n\n")
print(correlation_test_result)