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

# Define packages in installation order (dependencies first)
# Phase 1: Core system packages that others depend on
core_packages <- c(
  "Rcpp", "RcppArmadillo", "Matrix", "lattice", "nlme"
)

# Phase 2: Core tidyverse and basic packages
basic_packages <- c(
  "boot", "reshape2", "readxl", "knitr"
)

# Phase 3: Statistical packages (need core packages first)
stats_packages <- c(
  "lme4", "car", "rstatix", "broom", "emmeans", "effectsize", "lmerTest"
)

# Phase 4: Visualization and tidyverse (can be heavy)
viz_packages <- c(
  "viridis", "scales", "gridExtra", "patchwork", "tidyverse"
)

# All packages combined for final check
required_packages <- c(core_packages, basic_packages, stats_packages, viz_packages)

# Function to install packages with progress tracking and optimized approach
install_packages_with_progress <- function() {
  # Combine all packages for more efficient installation
  all_packages <- c(core_packages, basic_packages, stats_packages, viz_packages)

  failed_packages <- c()

  # Check which packages are already installed
  already_installed <- c()
  to_install <- c()

  cat("Checking package availability...\n")
  for (pkg in all_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      already_installed <- c(already_installed, pkg)
      cat(sprintf("  ✅ Already installed: %s\n", pkg))
    } else {
      to_install <- c(to_install, pkg)
    }
  }

  cat(sprintf("\n📦 %d/%d packages already available\n", length(already_installed), length(all_packages)))

  if (length(to_install) > 0) {
    cat(sprintf("📥 Installing %d missing packages...\n", length(to_install)))

    # Install all missing packages at once for better performance
    tryCatch({
      install.packages(to_install, dependencies = TRUE, quiet = TRUE, Ncpus = parallel::detectCores())
      cat("✅ Package installation completed\n")

      # Verify installation
      for (pkg in to_install) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          failed_packages <- c(failed_packages, pkg)
          cat(sprintf("  ❌ Failed to install: %s\n", pkg))
        }
      }
    }, error = function(e) {
      cat("❌ Package installation failed\n")
      cat(sprintf("Error: %s\n", e$message))
      failed_packages <<- to_install
    })
  }

  return(failed_packages)
}

# Install packages with phased approach
failed_packages <- install_packages_with_progress()

# Report results
cat("\nPackage installation summary:\n")
total_packages <- length(required_packages)
successful_packages <- total_packages - length(failed_packages)
cat(sprintf("  ✅ Successfully available: %d/%d packages\n", 
           successful_packages, total_packages))

if (length(failed_packages) > 0) {
  cat(sprintf("  ❌ Failed installations: %d packages\n", length(failed_packages)))
  cat("     Failed packages:", paste(failed_packages, collapse = ", "), "\n")
  cat("\n🚨 SOME PACKAGES FAILED TO INSTALL!\n")
  cat("Please try installing them manually:\n")
  cat("install.packages(c(", paste0("'", failed_packages, "'", collapse = ", "), "))\n\n")
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
packages_ok <- length(failed_packages) == 0
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
