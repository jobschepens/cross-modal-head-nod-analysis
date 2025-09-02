# Test script to validate lme4 and dependencies are working
# Run this in Docker container to test package availability

cat("Testing lme4 and dependencies...\n")

# Test core packages
test_packages <- c("lme4", "rstatix", "Matrix", "nlme", "nloptr")

results <- data.frame(
  package = character(),
  available = logical(),
  version = character(),
  stringsAsFactors = FALSE
)

for (pkg in test_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    version <- packageVersion(pkg)
    results <- rbind(results, data.frame(
      package = pkg,
      available = TRUE, 
      version = as.character(version)
    ))
    cat(sprintf("✅ %s v%s - OK\n", pkg, version))
  }, error = function(e) {
    results <<- rbind(results, data.frame(
      package = pkg,
      available = FALSE,
      version = "NOT AVAILABLE"
    ))
    cat(sprintf("❌ %s - FAILED: %s\n", pkg, e$message))
  })
}

# Test lme4 functionality with a simple model
cat("\nTesting lme4 functionality...\n")
tryCatch({
  library(lme4)
  # Simple test with built-in data
  model <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
  cat("✅ lme4 mixed-effects model test - SUCCESS\n")
  cat("Model summary:\n")
  print(summary(model))
}, error = function(e) {
  cat(sprintf("❌ lme4 functionality test - FAILED: %s\n", e$message))
})

cat("\nPackage test summary:\n")
print(results)

# Check R version
cat(sprintf("\nR version: %s\n", R.version.string))

# Check available system libraries that might affect compilation
cat("\nSystem libraries check:\n")
system("dpkg -l | grep -E '(cmake|gsl|blas|lapack)' || echo 'No relevant system packages found'")
