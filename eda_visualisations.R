# --- Exploratory Data Analysis (EDA) Visualizations ---

# 1. Load Required Libraries
library(tidyverse)
library(ggcorrplot)

file_paths <- file.path("dataset", c(
  "2015.csv", "2016.csv", "2017.csv", "2018.csv", "2019.csv"
))

# Re-run the standardise function
standardise_whr_data <- function(filepath) {
  # Extract the year from the filename
  year <- as.integer(str_extract(filepath, "\\d{4}"))

  # Read the raw CSV file, treating "N/A" as a missing value
  raw_df <- read_csv(
    filepath,
    na = "N/A",
    show_col_types = FALSE
  )

  # Clean and select relevant columns
  raw_df %>%
    # Use janitor to clean up column names (lowercase, underscores, etc.)
    clean_names() %>%
    # Rename columns to a consistent standard across all years
    rename_with(
      ~ "country",
      matches("country_or_region|country")
    ) %>%
    rename_with(~ "happiness_score", matches("score|happiness_score")) %>%
    rename_with(~ "gdp_per_capita", matches("economy|gdp_per_capita")) %>%
    rename_with(~ "social_support", matches("family|social_support")) %>%
    rename_with(
      ~ "health",
      matches("health|healthy_life_expectancy")
    ) %>%
    rename_with(
      ~ "freedom",
      matches("freedom|freedom_to_make_life_choices")
    ) %>%
    rename_with(
      ~ "trust",
      matches("trust|perceptions_of_corruption")
    ) %>%
    rename_with(~ "generosity", matches("generosity")) %>%
    # Add the year column
    mutate(year = year, .before = 1) %>%
    # Select only the columns we need for the model
    select(
      year,
      country,
      happiness_score,
      gdp_per_capita,
      social_support,
      health,
      freedom,
      trust,
      generosity
    )
}

# Create region map from 2015/2016
region_map <- map_dfr(file_paths[1:2], read_csv) %>%
  clean_names() %>%
  select(country, region) %>%
  distinct(country, .keep_all = TRUE)

# Load and combine all data, then join with the region map
full_clean_data <- map_dfr(file_paths, standardise_whr_data) %>%
  drop_na() %>%
  left_join(region_map, by = "country")


# --- Create Visualizations ---

# Create a directory for the EDA visuals if it doesn't exist
dir.create("final_report_visuals", showWarnings = FALSE)

# 1. Histogram of Happiness Scores
hist_plot <- ggplot(full_clean_data, aes(x = happiness_score)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Happiness Scores (2015-2019)",
    x = "Happiness Score",
    y = "Frequency (Number of Country-Year Observations)"
  ) +
  theme_minimal()

print(hist_plot)
ggsave("final_report_visuals/histogram_happiness_score.png", hist_plot)


# 2. Boxplot of Happiness Score by Region
# Filter out any regions that are NA after the join
boxplot_data <- full_clean_data %>%
  filter(!is.na(region))

box_plot <- ggplot(
  boxplot_data,
  aes(x = reorder(region, happiness_score, median), y = happiness_score)
) +
  geom_boxplot(fill = "lightcoral") +
  coord_flip() + # Flip coordinates to make region labels easier to read
  labs(
    title = "Happiness Scores by World Region",
    x = "Region",
    y = "Happiness Score"
  ) +
  theme_minimal()

print(box_plot)
ggsave("final_report_visuals/boxplot_by_region.png", box_plot)


# 3. Correlation Matrix Heatmap
# Select only the numeric columns for correlation
numeric_data <- full_clean_data %>%
  select(
    happiness_score, gdp_per_capita, social_support,
    health, freedom, trust, generosity
  )

# Calculate the correlation matrix
corr_matrix <- round(cor(numeric_data), 2)

# Create the heatmap plot
corr_plot <- ggcorrplot(
  corr_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726")
) +
  labs(title = "Correlation Matrix of Happiness Factors")

print(corr_plot)
ggsave("final_report_visuals/correlation_matrix.png", corr_plot)