# --- Column Name Audit Script ---
library(tidyverse)

# Point to the directory containing all CSVs (2015-2024)
data_directory <- "dataset"
all_files <- list.files(data_directory, pattern = "*.csv", full.names = TRUE)

# A function to read only the header of a CSV and the year from its name
get_headers <- function(filepath) {
  year <- as.integer(str_extract(basename(filepath), "\\d{4}"))
  headers <- names(read_csv(filepath, n_max = 0, show_col_types = FALSE))

  # Return a tibble (a modern data frame) for easy handling
  tibble(
    year = year,
    original_column_names = paste(headers, collapse = " | ")
  )
}

# Apply the function to all files and print the result
column_audit <- map_dfr(all_files, get_headers) %>%
  arrange(year)

cat("--- Full Column Name Audit (2015-2024) ---\n\n")
# This will print the full tibble without truncation
print(column_audit, n = Inf)