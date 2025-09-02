# Binder R package installation script (using rocker/binder base)
# The rocker/binder image comes with many packages pre-installed

cat("🐳 Setting up packages for rocker/binder environment...\n")

# Set CRAN mirror for reliable installation
if (length(getOption("repos")) == 1 && getOption("repos") == "@CRAN@") {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
  cat("✅ CRAN mirror set to: https://cran.rstudio.com/\n")
}

# Check which packages are already available in rocker/binder
essential_packages <- c(
  "tidyverse", "readxl", "rstatix", "lme4", "boot", "broom", 
  "reshape2", "viridis", "patchwork", "scales", "lmerTest", 
  "car", "emmeans", "effectsize", "gridExtra", "knitr"
)

# Install any missing packages
missing_packages <- c()
cat("🔍 Checking package availability...\n")

for (pkg in essential_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat(sprintf("  📦 Need to install: %s\n", pkg))
  } else {
    cat(sprintf("  ✅ Available: %s\n", pkg))
  }
}

if (length(missing_packages) > 0) {
  cat(sprintf("📥 Installing %d missing packages...\n", length(missing_packages)))
  install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
  cat("✅ Package installation completed\n")
} else {
  cat("🎉 All essential packages already available!\n")
}
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
}

cat("✅ CORE packages installed successfully!\n")
cat("🚀 Environment ready for cross-modal head nod analysis!\n")
