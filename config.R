# ===============================================================================
# CONFIG.R
# Cross-Modal Head Nod Analysis Repository Configuration
# ===============================================================================
# 
# This script sets up all path configurations and constants for the analysis.
# Edit this file to customize paths for your local environment.
#
# Author: Repository configuration
# Date: 2025-09-01
# ===============================================================================

# ===============================================================================
# PATH CONFIGURATION
# ===============================================================================

# Get the repository root directory (where this script is located)
get_repo_root <- function() {
  # Try to find the root directory containing the 'data' folder
  current_dir <- getwd()
  
  # Check if we're already in the repo root
  if (dir.exists("data") && dir.exists("scripts")) {
    return(current_dir)
  }
  
  # Check if we're in the scripts directory
  if (basename(current_dir) == "scripts") {
    return(dirname(current_dir))
  }
  
  # Try parent directory
  parent_dir <- dirname(current_dir)
  if (dir.exists(file.path(parent_dir, "data")) && dir.exists(file.path(parent_dir, "scripts"))) {
    return(parent_dir)
  }
  
  # If not found, return current directory and let user know
  cat("Warning: Could not automatically detect repository root.\n")
  cat("Current directory:", current_dir, "\n")
  cat("Please ensure you're running scripts from the repository root or scripts/ directory.\n")
  return(current_dir)
}

# Set repository root
REPO_ROOT <- get_repo_root()
cat("Repository root:", REPO_ROOT, "\n")

# ===============================================================================
# ANALYSIS CONFIGURATION
# ===============================================================================

CONFIG <- list(
  # === PATH CONFIGURATION ===
  REPO_ROOT = REPO_ROOT,
  BASE_DIR = REPO_ROOT,  # Alias for compatibility with test scripts
  OUTPUT_DIR = REPO_ROOT,  # For script compatibility
  DATA_DIR = file.path(REPO_ROOT, "data"),
  SCRIPTS_DIR = file.path(REPO_ROOT, "scripts"),
  FIGURES_DIR = file.path(REPO_ROOT, "figures"),
  RESULTS_DIR = file.path(REPO_ROOT, "results"),
  
  # === DATA FILE PATHS ===
  FUNCTION_DATA = file.path(REPO_ROOT, "data", "function_wide_all_languages.csv"),
  FORM_DATA = file.path(REPO_ROOT, "data", "form_wide_all_languages.csv"),
  FUNCTIONTURN_DATA = file.path(REPO_ROOT, "data", "functionturn_wide_all_languages.csv"),
  TURN_DATA = file.path(REPO_ROOT, "data", "turn_wide_all_languages.csv"),
  NORMALIZATION_FILE = file.path(REPO_ROOT, "data", "norm.xlsx"),
  
  # === ANALYSIS PARAMETERS ===
  HEAD_NOD_FORMS = c("sn", "hnn", "mn", "ln", "lnn"),
  HEAD_NOD_FUNCTIONS = c("affirmation", "feedback", "other"),
  KINEMATIC_VARS = c("length..seconds.", "extremes.amplitude", "velocity"),
  LANGUAGES = c("DGS_2.0_2412", "GER_2412", "RSL_2507", "RUS_2503"),
  TIERS = c("form", "function", "turn", "functionturn"),
  TASK3_CATEGORIES = c("length (seconds)", "extremes amplitude", "velocity"),
  
  # === FUNCTION-TURN LABELS ===
  AFFIRM_LABELS = c("affirm PR", "affirm mid-turn", "affirmation turn_initialization"),
  FEEDB_LABELS = c("feedb PR", "feedb mid-turn", "feedback turn_initialization"),
  
  # === PLOT SETTINGS ===
  PLOT_WIDTH = 7,
  PLOT_HEIGHT = 5,
  WIDE_PLOT_WIDTH = 9,
  PLOT_DPI = 300,
  
  # === STATISTICAL SETTINGS ===
  OUTLIER_THRESHOLD = 5,  # IQR multiplier for outlier removal
  BOOTSTRAP_ITERATIONS = 2000,
  CONFIDENCE_LEVEL = 0.95,
  
  # === OUTPUT SETTINGS ===
  SAVE_PLOTS = TRUE,
  SAVE_TABLES = TRUE,
  VERBOSE = TRUE
)

# ===============================================================================
# UTILITY FUNCTIONS
# ===============================================================================

# Function to ensure directories exist
ensure_directories <- function() {
  dirs_to_create <- c(
    CONFIG$FIGURES_DIR,
    CONFIG$RESULTS_DIR
  )

  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      if (!is.null(CONFIG$VERBOSE) && CONFIG$VERBOSE) {
        cat("Created directory:", dir, "\n")
      }
    }
  }
}

# Function to check if all data files exist
check_data_files <- function() {
  data_files <- c(
    CONFIG$FUNCTION_DATA,
    CONFIG$FORM_DATA,
    CONFIG$FUNCTIONTURN_DATA,
    CONFIG$TURN_DATA,
    CONFIG$NORMALIZATION_FILE
  )
  
  missing_files <- c()
  
  for (file in data_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }
  
  if (length(missing_files) > 0) {
    cat("❌ Missing data files:\n")
    for (file in missing_files) {
      cat("  -", file, "\n")
    }
    stop("Please ensure all data files are present before running analysis.")
  } else {
    if (!is.null(CONFIG$VERBOSE) && CONFIG$VERBOSE) {
      cat("✅ All data files found\n")
    }
  }
}

# Function to set working directory to repository root
set_repo_working_directory <- function() {
  # Ensure we have a valid REPO_ROOT
  if (exists("REPO_ROOT") && is.character(REPO_ROOT) && length(REPO_ROOT) > 0) {
    setwd(REPO_ROOT)
  } else if (exists("CONFIG") && !is.null(CONFIG$REPO_ROOT) && is.character(CONFIG$REPO_ROOT) && length(CONFIG$REPO_ROOT) > 0) {
    setwd(CONFIG$REPO_ROOT)
  } else {
    # Fallback: try to determine repo root
    current_dir <- getwd()
    if (dir.exists("data") && dir.exists("scripts")) {
      # Already in repo root
      cat("Already in repository root:", current_dir, "\n")
    } else {
      cat("Warning: Could not determine repository root. Staying in current directory:", current_dir, "\n")
    }
  }
  if (!is.null(CONFIG$VERBOSE) && CONFIG$VERBOSE) {
    cat("Working directory set to:", getwd(), "\n")
  }
}

# ===============================================================================
# PACKAGE MANAGEMENT
# ===============================================================================

# Function to install required packages if missing
install_required_packages <- function(interactive = TRUE) {
  required_packages <- c(
    "tidyverse", "ggplot2", "reshape2", "rstatix", "lme4", "readxl",
    "viridis", "gridExtra", "car", "emmeans", "effectsize", "boot",
    "lmerTest", "patchwork", "scales", "knitr", "broom", "dplyr"
  )
  
  missing_packages <- c()
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    if (interactive) {
      cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
      install.packages(missing_packages, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    } else {
      cat("Missing packages detected (skipping installation in non-interactive mode):", 
          paste(missing_packages, collapse = ", "), "\n")
      cat("Please install these packages manually if needed.\n")
    }
  }
  
  # Load core packages (suppress warnings in case some are missing)
  suppressPackageStartupMessages({
    tryCatch(library(tidyverse), error = function(e) cat("Note: tidyverse not available\n"))
    library(ggplot2)
    library(readxl)
  })

  if (!is.null(CONFIG$VERBOSE) && CONFIG$VERBOSE) {
    cat("✅ Package setup complete\n")
  }
}

# ===============================================================================
# INITIALIZATION
# ===============================================================================

# Function to initialize the analysis environment
initialize_analysis <- function(interactive = TRUE, install_packages = TRUE) {
  cat("===============================================================================\n")
  cat("CROSS-MODAL HEAD NOD ANALYSIS - INITIALIZATION\n")
  cat("===============================================================================\n")
  
  # Install packages (with option to skip)
  if (install_packages) {
    install_required_packages(interactive = interactive)
  }
  
  # Set working directory
  set_repo_working_directory()
  
  # Check data files
  check_data_files()
  
  # Create output directories
  ensure_directories()
  
  cat("✅ Analysis environment initialized successfully\n")
  cat("Repository root:", CONFIG$REPO_ROOT, "\n")
  cat("===============================================================================\n")
}

# ===============================================================================
# EXPORT CONFIGURATION
# ===============================================================================

# Make CONFIG available globally
assign("CONFIG", CONFIG, envir = .GlobalEnv)

# Print configuration summary if verbose
if (!is.null(CONFIG$VERBOSE) && CONFIG$VERBOSE && interactive()) {
  cat("\n=== CONFIGURATION SUMMARY ===\n")
  cat("Repository root:", CONFIG$REPO_ROOT, "\n")
  cat("Data directory:", CONFIG$DATA_DIR, "\n")
  cat("Figures directory:", CONFIG$FIGURES_DIR, "\n")
  cat("Results directory:", CONFIG$RESULTS_DIR, "\n")
  cat("Languages:", paste(CONFIG$LANGUAGES, collapse = ", "), "\n")
  cat("Kinematic variables:", paste(CONFIG$KINEMATIC_VARS, collapse = ", "), "\n")
  cat("============================\n\n")
}
