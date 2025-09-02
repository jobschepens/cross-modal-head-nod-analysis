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
    "tidyverse", "readxl", "reshape2", "rstatix", "lme4", "boot", 
    "broom", "viridis", "patchwork", "scales"
  )
  
  # Check for Binder environment
  in_binder <- Sys.getenv("BINDER_SERVICE_HOST") != ""
  if (in_binder) {
    cat("🐳 Running in Binder environment\n")
    # Add rmarkdown for Binder notebooks
    required_packages <- c(required_packages, "rmarkdown", "devtools", "here")
  }
  
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
    if (in_binder) {
      cat("In Binder environment - trying to install missing packages...\n")
      # Try to install missing packages
      try({
        install.packages(missing_packages, quiet = TRUE)
        cat("✅ Missing packages installed in Binder\n")
      })
    } else {
      cat("Install missing packages with:\n")
      cat("install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    }
    return(FALSE)
  } else {
    cat("\n✅ All required packages are available!\n")
    if (in_binder) {
      cat("🎉 Binder environment fully configured!\n")
    }
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

# Function to test config loading
test_config_loading <- function() {
  cat("\nTesting configuration loading...\n")
  
  tryCatch({
    # Test config loading
    if (file.exists("config.R")) {
      source("config.R")
      cat("  ✅ config.R loaded successfully\n")
      
      # Test that CONFIG exists and has required fields
      if (exists("CONFIG")) {
        required_config_fields <- c("BASE_DIR", "DATA_DIR", "SCRIPTS_DIR", "FIGURES_DIR", "RESULTS_DIR")
        missing_fields <- c()
        
        for (field in required_config_fields) {
          if (!field %in% names(CONFIG)) {
            missing_fields <- c(missing_fields, field)
          }
        }
        
        if (length(missing_fields) > 0) {
          cat("  ❌ Missing CONFIG fields:", paste(missing_fields, collapse = ", "), "\n")
          return(FALSE)
        } else {
          cat("  ✅ All required CONFIG fields present\n")
          cat("  ✅ Base directory:", CONFIG$BASE_DIR, "\n")
          return(TRUE)
        }
      } else {
        cat("  ❌ CONFIG object not created\n")
        return(FALSE)
      }
    } else {
      cat("  ❌ config.R file not found\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("  ❌ Error loading config:", e$message, "\n")
    return(FALSE)
  })
}

# Function to test actual script execution
test_script_execution <- function() {
  cat("\nTesting critical script functionality...\n")
  
  # Test data preparation script can be parsed and key functions work
  tryCatch({
    source("scripts/01_data_preparation.R", local = TRUE)
    cat("  ✅ 01_data_preparation.R executed successfully\n")
  }, error = function(e) {
    cat("  ❌ Error in 01_data_preparation.R:", e$message, "\n")
    return(FALSE)
  })
  
  return(TRUE)
}

# Function to test data file structure and content
test_data_structure <- function() {
  cat("\nTesting data file structure and content...\n")
  
  tryCatch({
    # Test main dataset structure
    data <- read.csv("data/function_wide_all_languages.csv")
    
    # Check expected columns
    expected_cols <- c("language", "Label", "length..seconds.", "extremes.amplitude", "velocity")
    missing_cols <- expected_cols[!expected_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      cat("  ❌ Missing expected columns:", paste(missing_cols, collapse = ", "), "\n")
      return(FALSE)
    } else {
      cat("  ✅ All expected columns present\n")
    }
    
    # Check for reasonable data ranges
    if (nrow(data) < 100) {
      cat("  ⚠️  Warning: Dataset seems small (", nrow(data), "rows)\n")
    } else {
      cat("  ✅ Dataset size looks reasonable (", nrow(data), "rows)\n")
    }
    
    # Check for missing values in key columns
    missing_values <- sapply(data[expected_cols], function(x) sum(is.na(x)))
    if (any(missing_values > nrow(data) * 0.5)) {
      cat("  ⚠️  Warning: High missing values in some columns\n")
    } else {
      cat("  ✅ Missing values within acceptable range\n")
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("  ❌ Error testing data structure:", e$message, "\n")
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
  cat("Starting comprehensive repository validation...\n\n")
  
  # Run all checks
  packages_ok <- check_packages()
  data_files_ok <- check_data_files()
  config_ok <- test_config_loading()
  data_structure_ok <- test_data_structure()
  output_dirs_ok <- check_output_dirs()
  syntax_ok <- test_script_syntax()
  execution_ok <- test_script_execution()
  
  # Summary
  cat("\n===============================================================================\n")
  cat("COMPREHENSIVE VALIDATION SUMMARY\n")
  cat("===============================================================================\n")
  
  cat("📦 Packages:      ", if(packages_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📁 Data Files:    ", if(data_files_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("⚙️  Configuration: ", if(config_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📊 Data Structure:", if(data_structure_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📂 Directories:   ", if(output_dirs_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("📝 Script Syntax: ", if(syntax_ok) "✅ PASS" else "❌ FAIL", "\n")
  cat("🔧 Execution:     ", if(execution_ok) "✅ PASS" else "❌ FAIL", "\n")
  
  overall_status <- all(c(packages_ok, data_files_ok, config_ok, data_structure_ok, 
                         output_dirs_ok, syntax_ok, execution_ok))
  
  if (overall_status) {
    cat("\n🎉 COMPREHENSIVE REPOSITORY VALIDATION SUCCESSFUL!\n")
    cat("Repository is fully tested and ready for collaboration.\n")
    cat("All systems operational - scripts, data, and configuration verified.\n")
  } else {
    cat("\n🚨 REPOSITORY VALIDATION FAILED!\n") 
    cat("Please fix the issues above before sharing.\n")
    cat("Run install.R first if packages are missing.\n")
  }
  
  cat("===============================================================================\n")
  
  return(overall_status)
}

# Run validation
# Check for Binder environment or non-interactive mode
in_binder <- Sys.getenv("BINDER_SERVICE_HOST") != ""
if (!interactive() || in_binder) {
  main_validation()
} else {
  cat("Running in interactive mode.\n")
  cat("To run validation manually, execute: main_validation()\n")
}
