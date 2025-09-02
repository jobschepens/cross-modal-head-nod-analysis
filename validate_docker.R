#!/usr/bin/env Rscript
# ===============================================================================
# DOCKER VALIDATION SCRIPT
# Cross-Modal Head Nod Analysis - Docker Environment Validation
# ===============================================================================
# 
# This script validates that the optimized Docker environment works correctly
# by testing both install2.r and install.R package installation approaches.
#
# Run this script to verify the hybrid package installation is working:
# docker run --rm -v $(pwd):/analysis cross-modal-head-nod-analysis Rscript validate_docker.R
#
# ===============================================================================

cat("===============================================================================\n")
cat("DOCKER ENVIRONMENT VALIDATION\n")
cat("Hybrid Package Installation Testing (install2.r + install.R)\n")
cat("===============================================================================\n")

# ===============================================================================
# 1. TEST CRITICAL PACKAGES (INSTALLED BY install2.r)
# ===============================================================================

cat("1. Testing packages installed by install2.r...\n")

critical_packages <- c(
  "lme4", "boot", "ggplot2", "dplyr", "tidyr", "readr", "readxl", "knitr", "rmarkdown"
)

critical_success <- 0
critical_total <- length(critical_packages)

for (pkg in critical_packages) {
  result <- tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat(sprintf("  ✅ %s: Successfully loaded\n", pkg))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ❌ %s: FAILED - %s\n", pkg, e$message))
    FALSE
  })
  if (result) critical_success <- critical_success + 1
}

cat(sprintf("\nCritical packages: %d/%d successful\n", critical_success, critical_total))

# ===============================================================================
# 2. TEST ADDITIONAL PACKAGES (INSTALLED BY install.R)
# ===============================================================================

cat("\n2. Testing additional packages installed by install.R...\n")

additional_packages <- c(
  "car", "rstatix", "broom", "emmeans", "effectsize", "lmerTest",
  "viridis", "scales", "gridExtra", "patchwork"
)

additional_success <- 0
additional_total <- length(additional_packages)

for (pkg in additional_packages) {
  result <- tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat(sprintf("  ✅ %s: Successfully loaded\n", pkg))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ❌ %s: FAILED - %s\n", pkg, e$message))
    FALSE
  })
  if (result) additional_success <- additional_success + 1
}

cat(sprintf("\nAdditional packages: %d/%d successful\n", additional_success, additional_total))

# ===============================================================================
# 3. TEST CORE FUNCTIONALITY
# ===============================================================================

cat("\n3. Testing core functionality...\n")

# Test lme4 (critical for analysis)
test_lme4 <- tryCatch({
  library(lme4, quietly = TRUE)
  # Simple test model
  data(sleepstudy)
  model <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
  cat("  ✅ lme4: Mixed-effects modeling works\n")
  TRUE
}, error = function(e) {
  cat(sprintf("  ❌ lme4: FAILED - %s\n", e$message))
  FALSE
})

# Test tidyverse functionality
test_tidyverse <- tryCatch({
  library(dplyr, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  # Simple test
  result <- mtcars %>% 
    filter(cyl == 4) %>%
    summarise(mean_mpg = mean(mpg))
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  cat("  ✅ tidyverse: Data manipulation and plotting works\n")
  TRUE
}, error = function(e) {
  cat(sprintf("  ❌ tidyverse: FAILED - %s\n", e$message))
  FALSE
})

# ===============================================================================
# 4. SUMMARY REPORT
# ===============================================================================

cat("\n===============================================================================\n")
cat("VALIDATION SUMMARY\n")
cat("===============================================================================\n")

total_success <- critical_success + additional_success
total_packages <- critical_total + additional_total

cat(sprintf("Package Installation Results:\n"))
cat(sprintf("  install2.r packages: %d/%d (%.1f%%)\n", 
           critical_success, critical_total, 
           100 * critical_success / critical_total))
cat(sprintf("  install.R packages:  %d/%d (%.1f%%)\n", 
           additional_success, additional_total, 
           100 * additional_success / additional_total))
cat(sprintf("  Total packages:      %d/%d (%.1f%%)\n", 
           total_success, total_packages, 
           100 * total_success / total_packages))

cat(sprintf("\nFunctionality Tests:\n"))
cat(sprintf("  lme4 mixed models:   %s\n", if(test_lme4) "✅ PASS" else "❌ FAIL"))
cat(sprintf("  tidyverse pipeline:  %s\n", if(test_tidyverse) "✅ PASS" else "❌ FAIL"))

# Overall assessment
if (total_success == total_packages && test_lme4 && test_tidyverse) {
  cat("\n🎉 DOCKER ENVIRONMENT: FULLY FUNCTIONAL\n")
  cat("The hybrid install2.r + install.R approach is working perfectly!\n")
  quit(status = 0)
} else if (critical_success == critical_total && test_lme4) {
  cat("\n⚠️  DOCKER ENVIRONMENT: MOSTLY FUNCTIONAL\n")
  cat("Critical packages work, but some additional packages may have issues.\n")
  quit(status = 1)
} else {
  cat("\n❌ DOCKER ENVIRONMENT: MAJOR ISSUES\n")
  cat("Critical packages or core functionality is not working.\n")
  quit(status = 2)
}
