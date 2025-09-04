cat('=== COMPREHENSIVE DOCKER TEST ===\n')

# Test 1: Package availability
cat('\n1. TESTING PACKAGE AVAILABILITY\n')
packages_to_test <- c('ggeffects', 'broom.mixed', 'effsize', 'kableExtra', 'tidyverse', 'lme4', 'ggplot2')
for(pkg in packages_to_test) {
  if(requireNamespace(pkg, quietly=TRUE)) {
    cat('✅', pkg, 'available\n')
  } else {
    cat('❌', pkg, 'MISSING\n')
  }
}

# Test 2: Configuration loading
cat('\n2. TESTING CONFIGURATION\n')
try({
  source('config.R')
  cat('✅ config.R loaded successfully\n')
  cat('✅ CONFIG$VERBOSE =', CONFIG$VERBOSE, '\n')
  cat('✅ CONFIG$REPO_ROOT =', CONFIG$REPO_ROOT, '\n')
}, error=function(e) {
  cat('❌ Config error:', e$message, '\n')
})

# Test 3: Data file access
cat('\n3. TESTING DATA FILES\n')
data_files <- c('data/function_wide_all_languages.csv', 'data/form_wide_all_languages.csv')
for(file in data_files) {
  if(file.exists(file)) {
    size <- file.info(file)$size
    cat('✅', file, '(', round(size/1024), 'KB)\n')
  } else {
    cat('❌', file, 'MISSING\n')
  }
}

# Test 4: Script execution
cat('\n4. TESTING SCRIPT EXECUTION\n')
try({
  source('scripts/01_data_preparation.R')
  cat('✅ 01_data_preparation.R executed\n')
}, error=function(e) {
  cat('❌ 01_data_preparation.R failed:', e$message, '\n')
})

# Test 5: Analysis script
cat('\n5. TESTING ANALYSIS SCRIPTS\n')
try({
  source('scripts/04_refined_analyses.R')
  cat('✅ 04_refined_analyses.R executed\n')
}, error=function(e) {
  cat('❌ 04_refined_analyses.R failed:', e$message, '\n')
})

# Test 6: Output verification
cat('\n6. TESTING OUTPUT GENERATION\n')
output_files <- c('results/refined_analyses_output.txt', 'figures/gardner_altman_refined/')
for(file in output_files) {
  if(file.exists(file) || dir.exists(file)) {
    cat('✅', file, 'created\n')
  } else {
    cat('❌', file, 'NOT FOUND\n')
  }
}

cat('\n=== DOCKER TEST COMPLETED ===\n')