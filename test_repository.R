# ===============================================================================
# TEST_REPOSITORY.R
# Cross-Modal Head Nod Analysis Repository Validation
# ===============================================================================
# 
# This script tests the complete repository functionality to ensure
# all components work properly before sharing with collaborators.
#
# Run this script to validate:
# - All required packages are available
# - All data files exist and are readable
# - Scripts can be sourced without errors
# - Output directories can be created
#
# Author: Repository validation script
# Date: 2025-09-01
# ===============================================================================

cat("===============================================================================\n")
cat("TESTING CROSS-MODAL HEAD NOD ANALYSIS REPOSITORY\n")
cat("===============================================================================\n")

# Function to check if packages are installed
check_packages <- function() {
  cat("Checking required R packages...\n")
  
  required_packages <- c(
    "tidyverse", "ggplot2", "reshape2", "rstatix", "lme4", "readxl",
    "viridis", "gridExtra", "car", "emmeans", "effectsize", "boot",
    "lmerTest", "patchwork", "scales", "knitr", "broom"
  )
  
  missing_packages <- c()
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
      cat("  ❌ Missing:", pkg, "\n")
    } else {
      cat("  ✅ Found:", pkg, "\n")
    }
  }
  
  if (length(missing_packages) > 0) {
    cat("\n🚨 MISSING PACKAGES DETECTED!\n")
    cat("Install missing packages with:\n")
    cat("install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    return(FALSE)
  } else {
    cat("\n✅ All required packages are available!\n")
    return(TRUE)
  }
}

# Function to check data files
check_data_files <- function() {
  cat("\nChecking data files...\n")
  
  required_files <- c(
    "data/function_wide_all_languages.csv",
    "data/form_wide_all_languages.csv", 
    "data/functionturn_wide_all_languages.csv",
    "data/turn_wide_all_languages.csv",
    "data/norm.xlsx"
  )
  
  all_files_exist <- TRUE
  
  for (file in required_files) {
    if (file.exists(file)) {
      cat("  ✅ Found:", file, "\n")
    } else {
      cat("  ❌ Missing:", file, "\n")
      all_files_exist <- FALSE
    }
  }
  
  if (all_files_exist) {
    cat("\n✅ All required data files are present!\n")
  } else {
    cat("\n🚨 MISSING DATA FILES DETECTED!\n")
  }
  
  return(all_files_exist)
}

# Function to test data loading
test_data_loading <- function() {
  cat("\nTesting data loading...\n")
  
  tryCatch({
    # Test main dataset
    data <- read.csv("data/function_wide_all_languages.csv")
    cat("  ✅ function_wide_all_languages.csv loaded successfully (", nrow(data), "rows )\n")
    
    # Test normalization file
    norm_data <- readxl::read_excel("data/norm.xlsx")
    cat("  ✅ norm.xlsx loaded successfully (", nrow(norm_data), "rows )\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("  ❌ Error loading data:", e$message, "\n")
    return(FALSE)
  })
}

# Function to check output directories
check_output_dirs <- function() {
  cat("\nChecking output directories...\n")
  
  dirs_to_check <- c("figures", "results")
  
  for (dir in dirs_to_check) {
    if (dir.exists(dir)) {
      cat("  ✅ Directory exists:", dir, "\n")
    } else {
      dir.create(dir, recursive = TRUE)
      cat("  ✅ Created directory:", dir, "\n")
    }
  }
  
  return(TRUE)
}

# Function to test script syntax
test_script_syntax <- function() {
  cat("\nTesting script syntax (parsing only)...\n")
  
  scripts <- list.files("scripts", pattern = "*.R$", full.names = TRUE)
  all_scripts_valid <- TRUE
  
  for (script in scripts) {
    tryCatch({
      parse(script)
      cat("  ✅ Valid syntax:", basename(script), "\n")
    }, error = function(e) {
      cat("  ❌ Syntax error in", basename(script), ":", e$message, "\n")
      all_scripts_valid <- FALSE
    })
  }
  
  return(all_scripts_valid)
}

# Main validation function
main_validation <- function() {
  cat("Starting repository validation...\n\n")
  
  # Run all checks
  packages_ok <- check_packages()
  data_files_ok <- check_data_files()
  data_loading_ok <- test_data_loading()
  output_dirs_ok <- check_output_dirs()
  syntax_ok <- test_script_syntax()
  
  # Summary
  cat("\n===============================================================================\n")
  cat("VALIDATION SUMMARY\n")
  cat("===============================================================================\n")
  
  cat("📦 Packages:     ", if(packages_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📁 Data Files:   ", if(data_files_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📊 Data Loading: ", if(data_loading_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📂 Directories:  ", if(output_dirs_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📝 Script Syntax:", if(syntax_ok) "✅ PASS" else "❌ FAIL", "\n")
  
  overall_status <- packages_ok && data_files_ok && data_loading_ok && output_dirs_ok && syntax_ok
  
  if (overall_status) {
    cat("\n🎉 REPOSITORY VALIDATION SUCCESSFUL!\n")
    cat("Repository is ready for sharing with collaborators.\n")
  } else {
    cat("\n🚨 REPOSITORY VALIDATION FAILED!\n") 
    cat("Please fix the issues above before sharing.\n")
  }
  
  cat("===============================================================================\n")
  
  return(overall_status)
}

# Run validation
if (!interactive()) {
  main_validation()
}
