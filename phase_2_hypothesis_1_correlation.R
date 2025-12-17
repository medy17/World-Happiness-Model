# --- Phase 2: Hypothesis 1 - Correlation Test (FINAL - Using RAW Data) ---

# 1. Load Required Libraries
library(tidyverse)
library(ggpubr)

# 2. UPDATED: Load the new raw master dataset
raw_data <- read_csv("processed_data/raw_master_dataset.csv", show_col_types = FALSE)

# 3. Create a 3-YEAR LAG dataset from the raw data
lag_data <- raw_data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    happiness_score_future = lead(happiness_score, n = 3),
    year_diff = lead(year, n = 3) - year
  ) %>%
  filter(year_diff == 3) %>%
  select(-year_diff) %>%
  ungroup() %>%
  drop_na()

cat("Total observations with a valid 3-year lag:", nrow(lag_data), "observations\n")

# 4. Visual Check & Correlation Test using RAW GDP
correlation_plot <- ggplot(
  lag_data,
  # UPDATED: Use raw_gdp in the aesthetic
  aes(x = raw_gdp, y = happiness_score_future)
) +
  geom_point(alpha = 0.6, color = "purple") +
  # Note: GDP is on a large scale, so we'll log-transform for a better linear fit visually
  scale_x_log10(labels = scales::dollar) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  stat_cor(method = "pearson", label.y = 8.0) +
  labs(
    title = "Raw GDP per Capita (Year t) vs. Happiness Score (Year t+3)",
    # UPDATED: Label reflects raw data
    x = "Raw GDP per Capita (Log Scale)",
    y = "Future Happiness Score (3 years later)"
  ) + theme_minimal()

print(correlation_plot)

# Perform the Pearson Correlation Test on raw data
correlation_test_result <- cor.test(
  # UPDATED: Use raw_gdp in the formula
  ~ raw_gdp + happiness_score_future,
  data = lag_data,
  alternative = "greater", conf.level = 0.95
)

cat("\n--- Pearson's Correlation Test (RAW Data, 3-Year Lag) ---\n\n")
print(correlation_test_result)