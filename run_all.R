# --- Master Pipeline Script for the World Happiness Analysis ---
#
# This script executes the entire analysis pipeline from start to finish.
# It ensures all required packages are installed and runs the scripts
# in the correct dependency order.
#
# TO RUN: Just execute this single file.
# --------------------------------------------------------------------

# --- 1. Setup: Package Management ---
# List all required packages for the entire project.
required_packages <- c(
  "tidyverse",
  "janitor",
  "ggcorrplot",
  "ggpubr",
  "car",
  "wbstats",
  "countrycode",
  "Metrics",
  "patchwork"
)

cat("--- Checking for required packages ---\n")

# Check which packages are not installed yet.
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages.
if(length(new_packages)) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
} else {
  cat("All required packages are already installed. Good to go.\n")
}

# Load all packages silently.
suppressPackageStartupMessages({
  lapply(required_packages, require, character.only = TRUE)
})

cat("\n--- All packages loaded. Kicking off the pipeline. ---\n\n")


# --- 2. Execution: Running the Analysis Scripts in Order ---

tryCatch({

  # Step 1: Exploratory Data Analysis
  cat("--------------------------------------------------\n")
  cat("STEP 1: Running EDA and creating 'full_clean_data.csv'...\n")
  cat("--------------------------------------------------\n")
  source("eda_visualisations.R")
  cat("\nSTEP 1 COMPLETE.\n\n")

  # Step 2: Data Engineering - Fetching Raw Data
  cat("--------------------------------------------------\n")
  cat("STEP 2: Fetching World Bank data and creating 'raw_master_dataset.csv'...\n")
  cat("--------------------------------------------------\n")
  source("create_raw_dataset.R")
  cat("\nSTEP 2 COMPLETE.\n\n")

  # Step 3: Hypothesis 1 - Correlation Test
  cat("--------------------------------------------------\n")
  cat("STEP 3: Testing Hypothesis 1 (Correlation) with raw data...\n")
  cat("--------------------------------------------------\n")
  source("phase_2_hypothesis_1_correlation.R")
  cat("\nSTEP 3 COMPLETE.\n\n")

  # Step 4: Hypothesis 2 - Regression & VIF Test
  cat("--------------------------------------------------\n")
  cat("STEP 4: Testing Hypothesis 2 (Regression & VIF) with raw data...\n")
  cat("--------------------------------------------------\n")
  source("phase_2_hypothesis_2_regression.R")
  cat("\nSTEP 4 COMPLETE.\n\n")

  # Step 5: Final Predictive Modelling & Black Swan Test
  cat("--------------------------------------------------\n")
  cat("STEP 5: Running the final multi-era Black Swan analysis...\n")
  cat("--------------------------------------------------\n")
  source("phase_3_predictive_modelling.R")
  cat("\nSTEP 5 COMPLETE.\n\n")

  # Final success message
  cat("==================================================\n")
  cat("         PIPELINE EXECUTED SUCCESSFULLY!          \n")
  cat("==================================================\n")
  cat("All analyses are complete. Check the 'processed_data' and 'final_report_visuals' folders for your outputs.\n")

}, error = function(e) {
  # Error handling: If any script fails, it stops and prints the error.
  cat("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("          PIPELINE FAILED AT SOME POINT.          \n")
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("The error message was:\n")
  print(e)
})