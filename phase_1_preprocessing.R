# 1. Load Required Libraries
library(tidyverse)
library(janitor)

# 2. Define data directory and file names
data_directory <- "dataset"
file_names <- c(
  "2015.csv", "2016.csv", "2017.csv", "2018.csv", "2019.csv"
)

# Create file paths
file_paths <- file.path(data_directory, file_names)

# 3. Function to read and standardise each year's data file
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

# 4. Apply the function to all files and combine them into a single data frame
world_happiness_full <- map_dfr(file_paths, standardise_whr_data) %>%
  # Now that the 'trust' column is numeric, this will correctly remove the row with the NA
  drop_na()

# 5. Create the final lagged dataset for predictive modelling
wh_model_data <- world_happiness_full %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # Create the target variable: the happiness score from the next year
    happiness_score_next_year = lead(happiness_score)
  ) %>%
  ungroup() %>%
  # Remove rows that don't have a 'next year' score (2019 data)
  filter(!is.na(happiness_score_next_year)) %>%
  # Remove the original score column to avoid confusion
  select(-happiness_score)

# 6. Perform the time-based split into training and testing sets
# The training set uses data from 2015-2017 to predict scores for 2016-2018
train_set <- wh_model_data %>%
  filter(year %in% c(2015, 2016, 2017))

# The testing set uses data from 2018 to predict scores for 2019
test_set <- wh_model_data %>%
  filter(year == 2018)


# 7. Final Output and Verification
cat("Preprocessing Complete\n\n")
cat("Final structure of the modelling data:\n")
glimpse(wh_model_data)
cat("\n Data Splitting Results\n")
cat(
  "Total rows in Training Set:",
  nrow(train_set),
  "(Predictor years:",
  paste(unique(train_set$year), collapse = ", "),
  ")\n"
)
cat(
  "Total rows in Testing Set: ",
  nrow(test_set),
  " (Predictor year:",
  unique(test_set$year),
  ")\n"
)

# 8. Save the processed data to CSV files
# Create a new directory to store the clean data
output_dir <- "processed_data"
dir.create(output_dir, showWarnings = FALSE)

# Save the training and testing sets
write_csv(train_set, file.path(output_dir, "train_set.csv"))
write_csv(test_set, file.path(output_dir, "test_set.csv"))

cat(
  "\n--- Data Saved ---\n"
)
cat(
  "Clean training and testing datasets have been saved in the '",
  output_dir,
  "' directory.\n"
)