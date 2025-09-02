#!/usr/bin/env Rscript
# ===============================================================================
# QUICK_TEST.R
# Fast validation for CI/CD pipelines
# ===============================================================================
# This script provides minimal validation for CI environments
# focusing on critical functionality without comprehensive testing
#
# Author: Quick test script
# Date: 2025-09-01
# ===============================================================================

cat("🚀 Running quick validation...\n")

# Set error handling
options(warn = 1)

# Test 1: Load configuration
tryCatch({
  source("config.R")
  cat("✅ Config loaded\n")
}, error = function(e) {
  cat("❌ Config failed:", e$message, "\n")
  quit(status = 1)
})

# Test 2: Check core packages
core_pkgs <- c("tidyverse", "readxl", "lme4", "rstatix")
missing_pkgs <- c()
for (pkg in core_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, pkg)
  }
}
if (length(missing_pkgs) > 0) {
  cat("❌ Missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✅ Core packages available\n")
}

# Test 3: Data file existence
data_files <- c(
  "data/function_wide_all_languages.csv",
  "data/form_wide_all_languages.csv",
  "data/norm.xlsx"
)
missing_data <- c()
for (file in data_files) {
  if (!file.exists(file)) {
    missing_data <- c(missing_data, file)
  }
}
if (length(missing_data) > 0) {
  cat("❌ Missing data files:", paste(missing_data, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✅ Data files present\n")
}

# Test 4: Basic data loading
tryCatch({
  data <- read.csv("data/function_wide_all_languages.csv", nrows = 5)
  if (nrow(data) > 0 && ncol(data) > 0) {
    cat("✅ Data loading works\n")
  } else {
    cat("❌ Data loading failed: empty dataset\n")
    quit(status = 1)
  }
}, error = function(e) {
  cat("❌ Data loading failed:", e$message, "\n")
  quit(status = 1)
})

# Test 5: Script syntax
scripts <- list.files("scripts", pattern = "*.R$", full.names = TRUE)
syntax_errors <- c()
for (script in scripts) {
  tryCatch({
    parse(script)
  }, error = function(e) {
    syntax_errors <- c(syntax_errors, basename(script))
  })
}
if (length(syntax_errors) > 0) {
  cat("❌ Syntax errors in:", paste(syntax_errors, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("✅ All scripts parse correctly\n")
}

cat("🎉 Quick validation passed!\n")
quit(status = 0)
