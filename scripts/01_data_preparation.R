# ===============================================================================
# 01_DATA_PREPARATION.R
# Cross-modal Head Nod Study: Data Reshaping and Preparation Utilities
# ===============================================================================
# 
# This script handles data preparation tasks including:
# - Reshaping data from wide to long format
# - Data validation and cleaning
# - Preparing datasets for statistical analysis
#
# Author: Reorganized from reshape_functionturn_long.R
# Date: 2025-01-31
# ===============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
})

cat("===============================================================================\n")
cat("01_DATA_PREPARATION: Cross-modal Head Nod Study\n")
cat("Data Reshaping and Preparation Utilities\n")
cat("===============================================================================\n")

# ===============================================================================
# CONFIGURATION
# ===============================================================================

# Load configuration if not already loaded
if (!exists("CONFIG")) {
  source("../config.R")
}

# Initialize analysis environment
initialize_analysis()

# Configuration constants
DATA_CONFIG <- list(
  # Data files from the data/ directory  
  FUNCTION_DATA = CONFIG$FUNCTION_DATA,
  FORM_DATA = CONFIG$FORM_DATA,
  FUNCTIONTURN_DATA = CONFIG$FUNCTIONTURN_DATA,
  TURN_DATA = CONFIG$TURN_DATA,
  
  # Languages and processing parameters
  LANGUAGES = c("DGS_2.0_2412", "GER_2412", "RSL_2507", "RUS_2503"),
  TIERS = c("form", "function", "turn", "functionturn"),
  
  # Categories to keep for Task 3 (kinematic properties)
  TASK3_CATEGORIES = c("length (seconds)", "extremes amplitude", "velocity")
)

cat("Configuration loaded:\n")
cat("- Languages:", paste(CONFIG$LANGUAGES, collapse = ", "), "\n")
cat("- Tiers:", paste(CONFIG$TIERS, collapse = ", "), "\n")
cat("- Kinematic categories:", paste(CONFIG$TASK3_CATEGORIES, collapse = ", "), "\n\n")

# ===============================================================================
# UTILITY FUNCTIONS
# ===============================================================================

#' Process a single file and return a long-format data frame
#' 
#' @param input_file Path to input CSV file
#' @param language Language identifier
#' @param tier Tier identifier (form, function, turn, functionturn)
#' @return Long-format data frame with language and tier columns
process_file_to_long <- function(input_file, language, tier) {
  
  if (!file.exists(input_file)) {
    warning(paste("File not found:", input_file))
    return(data.frame())
  }
  
  tryCatch({
    # Read raw data
    raw <- read.csv(file = input_file, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    
    # Skip first row (headers) and set column names
    raw <- raw[-1, ]
    names(raw) <- c("Category", "Label", paste0("V", 1:(ncol(raw)-2)))
    
    # Identify value columns
    value_cols <- grep("^V[0-9]+$", names(raw), value = TRUE)
    
    # Convert to long format
    long_df <- raw %>%
      filter(Category %in% CONFIG$TASK3_CATEGORIES) %>%
      pivot_longer(
        cols = all_of(value_cols),
        names_to = "ValueCol",
        values_to = "Value",
        values_drop_na = TRUE
      ) %>%
      filter(Value != "") %>%
      mutate(Value = as.numeric(Value)) %>%
      group_by(Label, Category) %>%
      mutate(ObservationID = row_number()) %>%
      ungroup() %>%
      mutate(
        language = language,
        tier = tier
      ) %>%
      select(Category, Label, ObservationID, Value, language, tier)
    
    return(long_df)
    
  }, error = function(e) {
    warning(paste("Error processing file", input_file, ":", e$message))
    return(data.frame())
  })
}

#' Convert long format to wide format for analysis
#' 
#' @param long_data Long-format data frame
#' @return Wide-format data frame
convert_long_to_wide <- function(long_data) {
  
  wide_df <- long_data %>%
    pivot_wider(
      id_cols = c(Label, ObservationID, language, tier), 
      names_from = Category, 
      values_from = Value
    )
  
  return(wide_df)
}

#' Validate data structure and report issues
#' 
#' @param data Data frame to validate
#' @param data_name Name of dataset for reporting
validate_data_structure <- function(data, data_name) {
  
  cat(sprintf("Validating %s:\n", data_name))
  
  # Check dimensions
  cat(sprintf("- Dimensions: %d rows x %d columns\n", nrow(data), ncol(data)))
  
  # Check for missing values
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  if (any(missing_counts > 0)) {
    cat("- Missing values found:\n")
    for (col in names(missing_counts[missing_counts > 0])) {
      cat(sprintf("  %s: %d missing\n", col, missing_counts[col]))
    }
  } else {
    cat("- No missing values detected\n")
  }
  
  # Check unique labels
  if ("Label" %in% names(data)) {
    unique_labels <- unique(data$Label)
    cat(sprintf("- Unique labels (%d): %s\n", 
                length(unique_labels), 
                paste(head(unique_labels, 10), collapse = ", ")))
    if (length(unique_labels) > 10) {
      cat("  (showing first 10 only)\n")
    }
  }
  
  cat("\n")
}

# ===============================================================================
# MAIN DATA PROCESSING PIPELINE
# ===============================================================================

cat("===============================================================================\n")
cat("PROCESSING DATA FILES\n")
cat("===============================================================================\n")

# Process all tiers and languages
for (tier in CONFIG$TIERS) {
  
  cat(sprintf("Processing tier: %s\n", tier))
  cat(paste(rep("-", 50), collapse=""), "\n")
  
  # Define output files
  output_long_file <- file.path(CONFIG$DATA_DIR, sprintf("%s_long_all_languages.csv", tier))
  output_wide_file <- file.path(CONFIG$DATA_DIR, sprintf("%s_wide_all_languages.csv", tier))
  
  # Storage for all languages
  all_long <- list()
  all_wide <- list()
  
  # Process each language
  for (i in seq_along(CONFIG$LANGUAGES)) {
    language <- CONFIG$LANGUAGES[i]
    input_file <- file.path(CONFIG$DATA_DIR, sprintf("summary.all_files.%s.%s.csv", tier, language))
    
    cat(sprintf("  Processing %s...\n", language))
    
    # Process to long format
    long_df <- process_file_to_long(input_file, language, tier)
    
    if (nrow(long_df) > 0) {
      all_long[[language]] <- long_df
      
      # Convert to wide format
      wide_df <- convert_long_to_wide(long_df)
      all_wide[[language]] <- wide_df
      
      cat(sprintf("    Long format: %d observations\n", nrow(long_df)))
      cat(sprintf("    Wide format: %d observations\n", nrow(wide_df)))
    } else {
      cat(sprintf("    No data processed for %s\n", language))
    }
  }
  
  # Combine all languages
  if (length(all_long) > 0) {
    all_long_df <- bind_rows(all_long)
    all_wide_df <- bind_rows(all_wide)
    
    # Validate combined data
    validate_data_structure(all_long_df, paste(tier, "long format"))
    validate_data_structure(all_wide_df, paste(tier, "wide format"))
    
    # Write output files
    write.csv(all_long_df, file = output_long_file, row.names = FALSE)
    write.csv(all_wide_df, file = output_wide_file, row.names = FALSE)
    
    cat(sprintf("✓ Long-format data written to: %s\n", basename(output_long_file)))
    cat(sprintf("✓ Wide-format data written to: %s\n", basename(output_wide_file)))
  } else {
    cat(sprintf("✗ No data to write for tier: %s\n", tier))
  }
  
  cat("\n")
}

# ===============================================================================
# DATA PREPARATION SUMMARY
# ===============================================================================

cat("===============================================================================\n")
cat("DATA PREPARATION SUMMARY\n")
cat("===============================================================================\n")

# Check which files were created
created_files <- list()
for (tier in CONFIG$TIERS) {
  long_file <- file.path(CONFIG$DATA_DIR, sprintf("%s_long_all_languages.csv", tier))
  wide_file <- file.path(CONFIG$DATA_DIR, sprintf("%s_wide_all_languages.csv", tier))
  
  if (file.exists(long_file) && file.exists(wide_file)) {
    created_files[[tier]] <- list(long = long_file, wide = wide_file)
  }
}

cat("✓ DATA PREPARATION COMPLETED\n")
cat(sprintf("  - Processed %d tiers across %d languages\n", 
            length(CONFIG$TIERS), length(CONFIG$LANGUAGES)))
cat(sprintf("  - Created %d file pairs (long + wide format)\n", length(created_files)))
cat("  - Kinematic variables extracted:", paste(CONFIG$TASK3_CATEGORIES, collapse = ", "), "\n\n")

cat("✓ OUTPUT FILES CREATED\n")
for (tier in names(created_files)) {
  cat(sprintf("  %s tier:\n", tier))
  cat(sprintf("    - Long format: %s\n", basename(created_files[[tier]]$long)))
  cat(sprintf("    - Wide format: %s\n", basename(created_files[[tier]]$wide)))
}

cat("\n✓ READY FOR STATISTICAL ANALYSIS\n")
cat("  Use wide-format files for comprehensive analysis scripts\n")
cat("  Use long-format files for specialized analyses requiring tidy data\n")

cat("\n===============================================================================\n")
cat("01_DATA_PREPARATION COMPLETED SUCCESSFULLY\n")
cat("===============================================================================\n")