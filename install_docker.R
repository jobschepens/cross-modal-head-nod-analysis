# Docker-specific package installation with system dependency handling
# This script handles the complex dependency chain for lme4 and related packages

cat("🐳 Docker-specific package installation starting...\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install critical system-level packages first
cat("Installing critical system packages...\n")
critical_deps <- c("Rcpp", "RcppArmadillo", "Matrix", "lattice", "nlme")
install.packages(critical_deps, dependencies = TRUE)

# Check if critical packages loaded
for (pkg in critical_deps) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Critical dependency failed: ", pkg)
  }
}

cat("✅ Critical dependencies installed\n")

# Install lme4 specifically with verbose output
cat("Installing lme4 (this may take time)...\n")
install.packages("lme4", dependencies = TRUE)

if (requireNamespace("lme4", quietly = TRUE)) {
  cat("✅ lme4 installed successfully\n")
} else {
  cat("❌ lme4 installation failed\n")
  # Try alternative installation
  cat("Trying alternative lme4 installation...\n")
  install.packages("lme4", type = "source")
}

# Continue with remaining packages
remaining_packages <- c(
  "readxl", "rstatix", "boot", "broom", "reshape2",
  "viridis", "patchwork", "scales", "gridExtra", 
  "car", "emmeans", "effectsize", "lmerTest", "knitr"
)

cat("Installing remaining packages...\n")
for (pkg in remaining_packages) {
  cat("Installing", pkg, "...")
  tryCatch({
    install.packages(pkg, dependencies = TRUE)
    cat(" ✅\n")
  }, error = function(e) {
    cat(" ❌\n")
    cat("Error:", e$message, "\n")
  })
}

# Finally install tidyverse (can be heavy)
cat("Installing tidyverse...\n")
install.packages("tidyverse", dependencies = TRUE)

cat("🐳 Docker package installation complete!\n")
