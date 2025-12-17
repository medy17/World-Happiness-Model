# --- World Happiness Report: Definitive Preprocessing ---

# 1. Load Required Libraries
library(tidyverse)
library(janitor)

# 2. The "Great Unifier" Standardisation Function
# This function is built to handle the glorious mess of column names from 2015-2024.
# It's the one function to rule them all.
standardise_whr_data <- function(filepath) {
  # Extract the year from the filename
  year <- as.integer(str_extract(basename(filepath), "\\d{4}"))

  # Read the raw CSV, cleaning up common issues
  raw_df <- read_csv(
    filepath,
    na = "N/A", # Treat "N/A" strings as actual missing values
    show_col_types = FALSE
  )

  # Clean names to snake_case first, which simplifies matching
  # e.g., "Economy..GDP.per.Capita." becomes "economy_gdp_per_capita"
  cleaned_df <- raw_df %>%
    clean_names() %>%
    # Now, we map all the historical variations to our standard names
    rename_with(
      ~ "country",
      matches("country_or_region|country")
    ) %>%
    rename_with(
      ~ "happiness_score",
      matches("score|happiness_score")
    ) %>%
    rename_with(
      ~ "gdp_per_capita",
      matches("economy|gdp_per_capita")
    ) %>%
    rename_with(
      ~ "social_support",
      matches("family|social_support")
    ) %>%
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
    # Add the year and ensure only our target columns remain
    mutate(year = year, .before = 1) %>%
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

  return(cleaned_df)
}

# --- PART I: Original Pipeline (Data 2015-2019) ---

cat("--- Running Original Pipeline for 2015-2019 Data ---\n")

# Define data directory and PRE-2020 file names
data_directory <- "dataset"
file_names_pre_2020 <- c(
  "2015.csv", "2016.csv", "2017.csv", "2018.csv", "2019.csv"
)
pre_2020_paths <- file.path(data_directory, file_names_pre_2020)

# Apply the unifier function and combine the pre-2020 data
world_happiness_pre_2020 <- map_dfr(pre_2020_paths, standardise_whr_data) %>%
  drop_na() # Drop any rows with missing data

# Create the original lagged dataset for predictive modelling
wh_model_data <- world_happiness_pre_2020 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # Create the target variable: the happiness score from the next year
    happiness_score_next_year = lead(happiness_score)
  ) %>%
  ungroup() %>%
  # Remove rows that don't have a 'next year' score (i.e., all 2019 data)
  filter(!is.na(happiness_score_next_year)) %>%
  # Remove the original score column to avoid data leakage
  select(-happiness_score)

# Perform the original time-based split
# Training set: uses 2015-2017 data to predict scores for 2016-2018
train_set <- wh_model_data %>%
  filter(year %in% c(2015, 2016, 2017))

# Testing set: uses 2018 data to predict scores for 2019
test_set <- wh_model_data %>%
  filter(year == 2018)

# Save the original processed data to CSV files
output_dir <- "processed_data"
dir.create(output_dir, showWarnings = FALSE)

write_csv(train_set, file.path(output_dir, "train_set.csv"))
write_csv(test_set, file.path(output_dir, "test_set.csv"))

cat("Original time-based split complete.\n")
cat(" - Training Set (2015-2017):", nrow(train_set), "rows\n")
cat(" - Testing Set (2018):", nrow(test_set), "rows\n")
cat(
  "Files 'train_set.csv' and 'test_set.csv' saved in 'processed_data' directory.\n\n"
)


# --- PART II: Black Swan Pipeline (Data 2020-2024) ---

cat("--- Running Black Swan Pipeline for 2020-2024 Data ---\n")

# Define POST-2020 file names
file_names_post_2020 <- c(
  "2020.csv", "2021.csv", "2022.csv", "2023.csv", "2024.csv"
)
post_2020_paths <- file.path(data_directory, file_names_post_2020)

# Apply the same unifier function to the new data
world_happiness_post_2020 <- map_dfr(post_2020_paths, standardise_whr_data) %>%
  drop_na()

# Save the combined post-2020 data to a single CSV file
write_csv(
  world_happiness_post_2020,
  file.path(output_dir, "post-2020.csv")
)

cat("Post-2020 data processing complete.\n")
cat(
  " - Combined 2020-2024 data:",
  nrow(world_happiness_post_2020),
  "rows\n"
)
cat("File 'post-2020.csv' saved in 'processed_data' directory.\n")