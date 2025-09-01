# ===============================================================================
# INSTALL.R
# Cross-Modal Head Nod Analysis Repository Setup
# ===============================================================================
# 
# This script sets up the complete analysis environment by:
# 1. Installing all required R packages
# 2. Checking data file availability  
# 3. Creating necessary directories
# 4. Running validation tests
#
# Run this script FIRST before using any other analysis scripts.
#
# Author: Repository setup script
# Date: 2025-09-01
# ===============================================================================

cat("===============================================================================\n")
cat("CROSS-MODAL HEAD NOD ANALYSIS REPOSITORY SETUP\n")
cat("===============================================================================\n")

# Set CRAN mirror for package installation
if (length(getOption("repos")) == 1 && getOption("repos") == "@CRAN@") {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
  cat("✅ CRAN mirror set to: https://cran.rstudio.com/\n")
}

# ===============================================================================
# 1. PACKAGE INSTALLATION
# ===============================================================================

cat("1. Installing required R packages...\n")

# Define CORE ESSENTIAL packages (actually used in scripts)
required_packages <- c(
  # Core tidyverse (includes dplyr, ggplot2, readr, tidyr, etc.)
  "tidyverse",
  
  # Data input/output
  "readxl",
  
  # Statistical analysis (core)
  "rstatix", "lme4", "boot", "broom",
  
  # Data reshaping
  "reshape2",
  
  # Visualization (essential only)
  "viridis", "patchwork", "scales"
)

# Function to install packages with progress tracking
install_packages_with_progress <- function(packages) {
  total_packages <- length(packages)
  installed_count <- 0
  failed_packages <- c()
  
  for (i in seq_along(packages)) {
    pkg <- packages[i]
    cat(sprintf("  [%d/%d] Checking %s...", i, total_packages, pkg))
    
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(" installing...")
      tryCatch({
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
        if (requireNamespace(pkg, quietly = TRUE)) {
          cat(" ✅\n")
          installed_count <- installed_count + 1
        } else {
          cat(" ❌ (installation failed)\n")
          failed_packages <- c(failed_packages, pkg)
        }
      }, error = function(e) {
        cat(" ❌ (error:", e$message, ")\n")
        failed_packages <- c(failed_packages, pkg)
      })
    } else {
      cat(" ✅ (already installed)\n")
      installed_count <- installed_count + 1
    }
  }
  
  return(list(
    installed = installed_count,
    failed = failed_packages,
    total = total_packages
  ))
}

# Install packages
package_results <- install_packages_with_progress(required_packages)

# Report results
cat("\nPackage installation summary:\n")
cat(sprintf("  ✅ Successfully available: %d/%d packages\n", 
           package_results$installed, package_results$total))

if (length(package_results$failed) > 0) {
  cat(sprintf("  ❌ Failed installations: %d packages\n", length(package_results$failed)))
  cat("     Failed packages:", paste(package_results$failed, collapse = ", "), "\n")
  cat("\n🚨 SOME PACKAGES FAILED TO INSTALL!\n")
  cat("Please try installing them manually:\n")
  cat("install.packages(c(", paste0("'", package_results$failed, "'", collapse = ", "), "))\n\n")
} else {
  cat("  🎉 All packages installed successfully!\n\n")
}

# ===============================================================================
# 2. CONFIGURATION SETUP
# ===============================================================================

cat("2. Loading configuration...\n")

# Load configuration
tryCatch({
  source("config.R")
  cat("  ✅ Configuration loaded successfully\n")
}, error = function(e) {
  cat("  ❌ Error loading config.R:", e$message, "\n")
  cat("  Creating basic configuration...\n")
  
  # Create minimal configuration if config.R doesn't exist
  CONFIG <<- list(
    REPO_ROOT = getwd(),
    DATA_DIR = file.path(getwd(), "data"),
    FIGURES_DIR = file.path(getwd(), "figures"),
    RESULTS_DIR = file.path(getwd(), "results")
  )
  cat("  ✅ Basic configuration created\n")
})

# ===============================================================================
# 3. DIRECTORY SETUP
# ===============================================================================

cat("3. Creating output directories...\n")

directories_to_create <- c(
  "figures",
  "results", 
  "figures/gardner_altman",
  "results/tables"
)

for (dir in directories_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("  ✅ Created:", dir, "\n")
  } else {
    cat("  ✅ Exists:", dir, "\n")
  }
}

# ===============================================================================
# 4. DATA VALIDATION
# ===============================================================================

cat("4. Validating data files...\n")

required_data_files <- c(
  "data/function_wide_all_languages.csv",
  "data/form_wide_all_languages.csv",
  "data/functionturn_wide_all_languages.csv", 
  "data/turn_wide_all_languages.csv",
  "data/norm.xlsx"
)

missing_files <- c()
for (file in required_data_files) {
  if (file.exists(file)) {
    # Check file size
    file_size <- file.info(file)$size
    cat(sprintf("  ✅ Found: %s (%.1f KB)\n", file, file_size / 1024))
  } else {
    cat("  ❌ Missing:", file, "\n")
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("\n🚨 MISSING DATA FILES!\n")
  cat("Please ensure the following files are in the data/ directory:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
  cat("\nData files are required for the analysis to run.\n")
} else {
  cat("  🎉 All data files found!\n")
}

# ===============================================================================
# 5. QUICK FUNCTIONALITY TEST
# ===============================================================================

cat("5. Testing basic functionality...\n")

# Test data loading
if (length(missing_files) == 0) {
  tryCatch({
    # Load core packages
    suppressPackageStartupMessages({
      library(tidyverse)
      library(readxl)
    })
    
    # Test loading main data file
    test_data <- read.csv("data/function_wide_all_languages.csv", nrows = 10)
    cat(sprintf("  ✅ Data loading test passed (%d columns, %d test rows)\n", 
               ncol(test_data), nrow(test_data)))
    
    # Test normalization file
    norm_data <- read_excel("data/norm.xlsx")
    cat(sprintf("  ✅ Normalization file test passed (%d rows)\n", nrow(norm_data)))
    
  }, error = function(e) {
    cat("  ❌ Data loading test failed:", e$message, "\n")
  })
}

# ===============================================================================
# 6. FINAL SETUP SUMMARY
# ===============================================================================

cat("\n===============================================================================\n")
cat("SETUP SUMMARY\n")
cat("===============================================================================\n")

# Overall status
packages_ok <- length(package_results$failed) == 0
data_ok <- length(missing_files) == 0
overall_ok <- packages_ok && data_ok

cat("📦 Packages:    ", if(packages_ok) "✅ ALL INSTALLED" else "❌ SOME MISSING", "\n")
cat("📁 Data Files:  ", if(data_ok) "✅ ALL PRESENT" else "❌ SOME MISSING", "\n")
cat("📂 Directories: ✅ CREATED\n")
cat("🔧 Config:      ✅ LOADED\n")

if (overall_ok) {
  cat("\n🎉 SETUP COMPLETED SUCCESSFULLY!\n")
  cat("\nYou can now run the analysis scripts:\n")
  cat("  1. source('scripts/01_data_preparation.R')\n")
  cat("  2. source('scripts/02_comprehensive_analysis.R')\n") 
  cat("  3. source('scripts/03_advanced_statistics.R')\n")
  cat("  4. source('scripts/05_publication_figures.R')\n")
  cat("\nOr run the validation test:\n")
  cat("  source('test_repository.R')\n")
} else {
  cat("\n🚨 SETUP INCOMPLETE!\n")
  cat("Please fix the issues above before running the analysis.\n")
}

cat("===============================================================================\n")

# Return status for programmatic use
invisible(overall_ok)
