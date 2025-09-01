# Binder R package installation script
# Installs CORE ESSENTIAL packages for cross-modal head nod analysis

cat("🐳 Installing CORE R packages for Binder environment...\n")

# Set CRAN mirror for reliable installation
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# CORE essential packages only (9 packages)
required_packages <- c(
  "tidyverse", "readxl", "rstatix", "lme4", "boot", "broom",
  "reshape2", "viridis", "patchwork", "scales"
)

# Add Binder-specific packages
binder_packages <- c("rmarkdown", "devtools", "here")
all_packages <- c(required_packages, binder_packages)

# Install packages with progress tracking
for (i in seq_along(all_packages)) {
  pkg <- all_packages[i]
  cat(sprintf("[%d/%d] Installing %s...\n", i, length(all_packages), pkg))
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
}

cat("✅ CORE packages installed successfully!\n")
cat("🚀 Environment ready for cross-modal head nod analysis!\n")
