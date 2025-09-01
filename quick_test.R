#!/usr/bin/env R

# Quick validation test
cat("Testing repository setup...\n")

# Test if key files exist
key_files <- c("config.R", "install.R", "data", "scripts")
for (file in key_files) {
  if (file.exists(file) || dir.exists(file)) {
    cat("✅", file, "exists\n")
  } else {
    cat("❌", file, "missing\n")
  }
}

# Test if we can load config
tryCatch({
  source("config.R")
  cat("✅ config.R loads successfully\n")
  cat("✅ REPO_ROOT =", REPO_ROOT, "\n")
}, error = function(e) {
  cat("❌ Error loading config.R:", e$message, "\n")
})

cat("Basic validation complete.\n")
