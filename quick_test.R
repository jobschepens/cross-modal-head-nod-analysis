# Quick test for Binder environment
cat("=== BINDER ENVIRONMENT TEST ===\n")

# Check if we're in Binder
in_binder <- Sys.getenv("BINDER_SERVICE_HOST") != ""
cat("In Binder environment:", in_binder, "\n")

# Check R version
cat("R version:", R.version.string, "\n")

# Check CRAN mirror
cat("CRAN mirror:", getOption("repos")["CRAN"], "\n")

# Test basic package loading
tryCatch({
  library(tidyverse)
  cat("✅ tidyverse loaded successfully\n")
}, error = function(e) {
  cat("❌ tidyverse failed:", e$message, "\n")
})

# Test data file access
if (file.exists("data/function_wide_all_languages.csv")) {
  cat("✅ Data files accessible\n")
} else {
  cat("❌ Data files not accessible\n")
}

cat("=== TEST COMPLETE ===\n")
