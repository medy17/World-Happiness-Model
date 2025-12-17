# --- Exploratory Data Analysis (EDA) Visualizations (Final) ---

# 1. Load Required Libraries
library(tidyverse)
library(janitor)
library(ggcorrplot)

# 2. The "Great Unifier" Standardisation Function
standardise_whr_data <- function(filepath) {
  year <- as.integer(str_extract(basename(filepath), "\\d{4}"))
  read_csv(filepath, na = "N/A", show_col_types = FALSE) %>%
    clean_names() %>%
    rename_with(~"country", matches("country_or_region|country")) %>%
    rename_with(~"happiness_score", matches("score|happiness_score")) %>%
    rename_with(~"gdp_per_capita", matches("economy|gdp_per_capita|logged_gdp_per_capita")) %>%
    rename_with(~"social_support", matches("family|social_support")) %>%
    rename_with(~"health", matches("health|healthy_life_expectancy")) %>%
    rename_with(~"freedom", matches("freedom|freedom_to_make_life_choices")) %>%
    rename_with(~"trust", matches("trust|perceptions_of_corruption")) %>%
    rename_with(~"generosity", matches("generosity")) %>%
    mutate(year = year, .before = 1) %>%
    select(
      year, country, happiness_score, gdp_per_capita, social_support,
      health, freedom, trust, generosity
    )
}

# 3. Load ALL data (2015-2024)
data_directory <- "dataset"
all_files <- list.files(data_directory, pattern = "*.csv", full.names = TRUE)

# Create region map from any file that contains the 'region' column
region_map <- map_dfr(all_files, ~{
    df <- read_csv(.x, show_col_types = FALSE, progress = FALSE) %>% clean_names()
    if ("region" %in% names(df)) {
      df %>% select(country, region)
    }
  }) %>%
  distinct(country, .keep_all = TRUE)

# Load, combine, and join with the region map
full_clean_data <- map_dfr(all_files, standardise_whr_data) %>%
  drop_na(happiness_score) %>%
  left_join(region_map, by = "country")

# --- Create & Save ---
output_dir <- "processed_data"
dir.create(output_dir, showWarnings = FALSE)
dir.create("final_report_visuals", showWarnings = FALSE)

# NEW: Save the full clean dataset for reproducibility
write_csv(full_clean_data, file.path(output_dir, "full_clean_data.csv"))

# Visualizations (unchanged, but now run on the full dataset)
hist_plot <- ggplot(full_clean_data, aes(x = happiness_score)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Happiness Scores (2015-2024)", x = "Happiness Score", y = "Frequency") +
  theme_minimal()
ggsave("final_report_visuals/histogram_happiness_score_full.png", hist_plot)

box_plot <- full_clean_data %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x = reorder(region, happiness_score, median), y = happiness_score)) +
  geom_boxplot(fill = "lightcoral") + coord_flip() +
  labs(title = "Happiness Scores by World Region (2015-2024)", x = "Region", y = "Happiness Score") +
  theme_minimal()
ggsave("final_report_visuals/boxplot_by_region_full.png", box_plot)

corr_matrix <- full_clean_data %>%
  select(where(is.numeric), -year) %>%
  cor(use = "complete.obs") %>% round(2)
corr_plot <- ggcorrplot(
  corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726")
) + labs(title = "Correlation Matrix of Happiness Factors (2015-2024)")
ggsave("final_report_visuals/correlation_matrix_full.png", corr_plot)

cat("EDA script complete. 'full_clean_data.csv' created and visuals saved.\n")