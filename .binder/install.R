# Binder R package installation script
# Installs all required packages for cross-modal head nod analysis

cat("🐳 Installing R packages for Binder environment...\n")

# Set CRAN mirror for reliable installation
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Essential packages for head nod analysis
required_packages <- c(
  "tidyverse", "readxl", "rstatix", "lme4", "boot", "broom",
  "car", "emmeans", "effectsize", "pwr", "reshape2", "viridis", 
  "patchwork", "scales", "RColorBrewer", "gridExtra", "knitr", 
  "rmarkdown", "devtools", "here"
)

# Install packages with progress tracking
for (i in seq_along(required_packages)) {
  pkg <- required_packages[i]
  cat(sprintf("[%d/%d] Installing %s...\n", i, length(required_packages), pkg))
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
}

cat("✅ All Binder packages installed successfully!\n")
cat("🚀 Environment ready for cross-modal head nod analysis!\n")
