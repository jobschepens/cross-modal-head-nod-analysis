# ===============================================================================
# 08_REPRODUCE_ORIGINAL_PLOS.R  
# Reproduction Script: Original PLOS DGS Head Nod Analysis
# ===============================================================================

# Load configuration if not already loaded
if (!exists("CONFIG")) {
  source("../config.R")
}

# Initialize analysis environment
initialize_analysis()

# Load libraries
required_packages <- c("broom", "scales", "xtable", "tidyverse", "lme4", "readr", "performance")
optional_packages <- c("introdataviz", "see", "ggeffects", "patchwork")

# Install required packages if missing
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing required package:", pkg, "\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load optional packages with error handling
for (pkg in optional_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Optional package not available:", pkg, "(continuing without it)\n")
  }
}

# Set working directory
setwd("c:/Users/Job/sciebo6/vicomdata")

# REPRODUCTION ATTEMPT 1: Using final cleaned approach
# Based on current state of archive/oldcode/code.R

cat("=== REPRODUCTION ATTEMPT 1: Final Cleaned Approach ===\n")

# Data loading (adjusted path)
name <- "archive/output_2024-12-21/summary.all_files.function.DGS_2.0_2412.csv"

cat("Loading data from:", name, "\n")
if (!file.exists(name)) {
  stop("Data file not found: ", name)
}

raw <- read.csv(file=name, header=F, sep=",", stringsAsFactors = FALSE)

cat("Raw data dimensions:", dim(raw), "\n")
if (ncol(raw) >= 10) {
  preview_cols <- paste(as.character(unlist(raw[1,1:min(10, ncol(raw))])), collapse = ", ")
  cat("First row preview:", preview_cols, "\n")
} else {
  cat("First row preview: [less than 10 columns]\n")
}

# Data processing (following original exactly)
raw <- raw[-1,]
names(raw) <- c("Category","Label","Number_of_labels","Mean","Median","SD","Variance","Min","Max")

for (i in 10:length(colnames(raw))) {colnames(raw)[i] <- paste0(i-10)}

cat("Label distribution:\n")
print(table(raw$Label))

# Split by nod function
fee <- raw[raw$Label == "feedback", ]
aff <- raw[raw$Label == "affirmation", ]

cat("Feedback rows:", nrow(fee), "\n")
cat("Affirmation rows:", nrow(aff), "\n")

# Transpose and restructure
fee <- t(fee) %>% `colnames<-`(fee[[1]]) %>% as.data.frame()
aff <- t(aff) %>% `colnames<-`(aff[[1]]) %>% as.data.frame()

fee <- fee[-c(1:9), ]
aff <- aff[-c(1:9), ]

# Add nod function labels
fee$nod <- "feedback"
aff$nod <- "affirmation"

# Combine datasets
raw <- rbind(fee, aff)

cat("Combined data dimensions:", dim(raw), "\n")
cat("Nod function distribution:\n")
print(table(raw$nod))

# Save intermediate file
write.csv(raw, file = "reproduction_raw_function_dgs.csv", row.names = FALSE)

# Read back and continue analysis
df <- read_csv("reproduction_raw_function_dgs.csv") 
cat("Loaded processed data dimensions:", dim(df), "\n")

# Data cleaning and preprocessing
df <- df[complete.cases(df),]

for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]] <- as.factor(df[[col]])
  }
}

# Check key variables
cat("Key variable summaries:\n")
cat("Length (seconds):", summary(as.numeric(df$`length (seconds)`)), "\n")
cat("Velocity:", summary(as.numeric(df$velocity)), "\n") 
cat("Extremes amplitude:", summary(as.numeric(df$`extremes amplitude`)), "\n")

# Scale numeric variables (following original)
num_cols <- sapply(df, is.numeric)
df[num_cols] <- as.vector(lapply(df[num_cols], function(x) as.vector(scale(x))))

# Remove outliers (following original approach)
remove_outliers <- function(df, cols) {
  for (col in cols) {
    qnt <- quantile(df[[col]], probs=c(.25, .75), na.rm = T)
    H <- 5 * IQR(df[[col]], na.rm = T) # only remove extreme outliers
    df <- df[df[[col]] > (qnt[1] - H), ]
    df <- df[df[[col]] < (qnt[2] + H), ]
  }
  return(df)
}

# CRITICAL: Test different outlier removal strategies
cat("\n=== OUTLIER REMOVAL SENSITIVITY ANALYSIS ===\n")

# Strategy 1: Final approach (current cleaned code)
df2_final <- remove_outliers(df, c("length (seconds)" , "extremes amplitude" , "velocity"))
cat("Final approach - outliers removed:", nrow(df) - nrow(df2_final), "\n")
cat("Final approach - sample size:", nrow(df2_final), "\n")

# Strategy 2: Earlier approach (from commented code)
df2_earlier <- remove_outliers(df, c("length (seconds)" , "velocity"))  # simplified - no peak max prom available
cat("Earlier approach - outliers removed:", nrow(df) - nrow(df2_earlier), "\n") 
cat("Earlier approach - sample size:", nrow(df2_earlier), "\n")

# Strategy 3: No outlier removal
df2_none <- df
cat("No outlier removal - sample size:", nrow(df2_none), "\n")

# Use final approach for main analysis
df2 <- df2_final

# STATISTICAL ANALYSIS REPRODUCTION

cat("\n=== STATISTICAL ANALYSIS ===\n")

# Function to run analysis on different datasets
run_statistical_analysis <- function(data, label) {
  cat("\n--- Analysis for:", label, "---\n")
  cat("Sample size:", nrow(data), "\n")
  
  # T-tests
  var_names <- c("velocity", "`length (seconds)`", "`extremes amplitude`")
  results_list <- list()
  
  for (var in var_names) {
    ttest <- tidy(t.test(as.formula(paste(var, "~ nod")), data = data))
    results_list[[paste(var, "ttest", sep="_")]] <- ttest
    cat(var, "t-test p-value:", round(ttest$p.value, 4), 
        ifelse(ttest$p.value < 0.05, "*", ""), "\n")
  }
  
  # Try mixed-effects models
  f1 <- nod ~ velocity + (1|speaker)
  f2 <- nod ~ `length (seconds)` + (1|speaker)
  f3 <- nod ~ `extremes amplitude` + (1|speaker)
  
  models <- list()
  aic_values <- c()
  
  for (i in 1:3) {
    formula <- list(f1, f2, f3)[[i]]
    var_name <- c("velocity", "duration", "amplitude")[i]
    
    tryCatch({
      model <- glmer(formula, data = data, family = binomial, 
                     control = glmerControl(optimizer="bobyqa"))
      models[[var_name]] <- model
      aic_values[var_name] <- AIC(model)
      cat(var_name, "model AIC:", round(AIC(model), 2), "\n")
    }, error = function(e) {
      cat(var_name, "model failed:", e$message, "\n")
    })
  }
  
  if (length(aic_values) > 0) {
    best_var <- names(aic_values)[which.min(aic_values)]
    cat("Best single predictor by AIC:", best_var, "\n")
  }
  
  return(list(results = results_list, models = models, aic = aic_values))
}

# Run analysis on different outlier removal strategies
analysis_final <- run_statistical_analysis(df2_final, "Final outlier approach")
analysis_earlier <- run_statistical_analysis(df2_earlier, "Earlier outlier approach") 
analysis_none <- run_statistical_analysis(df2_none, "No outlier removal")

# Main analysis continues with final approach
df2 <- df2_final

# Mixed-effects models (following original)
cat("\n=== MIXED-EFFECTS MODEL COMPARISON ===\n")

# Model formulations from original code
f0 <- nod ~ (1|speaker)
f1 <- nod ~ velocity + (1|speaker)
f2 <- nod ~ `length (seconds)` + (1|speaker)
f3 <- nod ~ `extremes amplitude` + (1|speaker)
f12 <- nod ~ velocity + `length (seconds)` + (1|speaker)
f13 <- nod ~ velocity + `extremes amplitude` + (1|speaker)
f23 <- nod ~ `length (seconds)` + `extremes amplitude` + (1|speaker)
f <- nod ~ velocity + `length (seconds)` + `extremes amplitude` + (1|speaker)

# Fit models with error handling
fit_model_safely <- function(formula, data) {
  tryCatch({
    glmer(formula, data = data, family = binomial, 
          control = glmerControl(optimizer="bobyqa"))
  }, error = function(e) {
    cat("Error fitting model:", deparse(formula), "\nError:", e$message, "\n")
    return(NULL)
  })
}

a0 <- fit_model_safely(f0, df2)
a1 <- fit_model_safely(f1, df2)
a2 <- fit_model_safely(f2, df2)
a3 <- fit_model_safely(f3, df2)
a12 <- fit_model_safely(f12, df2)
a13 <- fit_model_safely(f13, df2)
a23 <- fit_model_safely(f23, df2)
a <- fit_model_safely(f, df2)

# Model comparison (AIC)
models <- list(
  "Baseline" = a0,
  "Velocity only" = a1,
  "Duration only" = a2, 
  "Amplitude only" = a3,
  "Velocity + Duration" = a12,
  "Velocity + Amplitude" = a13,
  "Duration + Amplitude" = a23,
  "Full model" = a
)

# Remove NULL models
models <- models[!sapply(models, is.null)]

if (length(models) > 0) {
  # Compare models
  aic_values <- sapply(models, AIC)
  aic_comparison <- data.frame(
    Model = names(aic_values),
    AIC = aic_values,
    Delta_AIC = aic_values - min(aic_values)
  )
  aic_comparison <- aic_comparison[order(aic_comparison$AIC), ]
  
  cat("Model comparison (AIC):\n")
  print(aic_comparison)
  
  cat("\nBest model:", aic_comparison$Model[1], "\n")
  
  # Summary of best model
  best_model <- models[[aic_comparison$Model[1]]]
  cat("\nBest model summary:\n")
  print(summary(best_model))
  
} else {
  cat("ERROR: No models fitted successfully\n")
}

# Correlation analysis
cat("\n=== CORRELATION ANALYSIS ===\n")
cor_velocity_amplitude <- cor.test(df2$`extremes amplitude`, df2$velocity)
cor_velocity_duration <- cor.test(df2$`length (seconds)`, df2$velocity)
cor_amplitude_duration <- cor.test(df2$`extremes amplitude`, df2$`length (seconds)`)

cat("Velocity vs Amplitude correlation:", 
    round(cor_velocity_amplitude$estimate, 3), 
    "(p =", round(cor_velocity_amplitude$p.value, 4), ")\n")
cat("Velocity vs Duration correlation:", 
    round(cor_velocity_duration$estimate, 3), 
    "(p =", round(cor_velocity_duration$p.value, 4), ")\n")
cat("Amplitude vs Duration correlation:", 
    round(cor_amplitude_duration$estimate, 3), 
    "(p =", round(cor_amplitude_duration$p.value, 4), ")\n")

# SUMMARY REPORT
cat("\n", rep("=", 60), "\n")
cat("REPRODUCTION SUMMARY\n")
cat(rep("=", 60), "\n")
cat("Data source:", name, "\n")

cat("\nOUTLIER REMOVAL SENSITIVITY:\n")
cat("Final approach sample size:", nrow(df2_final), "\n")
cat("Earlier approach sample size:", nrow(df2_earlier), "\n") 
cat("No outlier removal sample size:", nrow(df2_none), "\n")

cat("\nPRIMARY DIFFERENTIATOR BY OUTLIER STRATEGY:\n")
if (exists("analysis_final") && length(analysis_final$aic) > 0) {
  cat("Final approach best predictor:", names(analysis_final$aic)[which.min(analysis_final$aic)], "\n")
}
if (exists("analysis_earlier") && length(analysis_earlier$aic) > 0) {
  cat("Earlier approach best predictor:", names(analysis_earlier$aic)[which.min(analysis_earlier$aic)], "\n")
}
if (exists("analysis_none") && length(analysis_none$aic) > 0) {
  cat("No outlier removal best predictor:", names(analysis_none$aic)[which.min(analysis_none$aic)], "\n")
}

cat("\nFINAL ANALYSIS (using final outlier approach):\n")
cat("Final sample size:", nrow(df2), "\n")
cat("Feedback nods:", sum(df2$nod == "feedback"), "\n")
cat("Affirmation nods:", sum(df2$nod == "affirmation"), "\n")

if (exists("aic_comparison")) {
  cat("Primary differentiator (by AIC):", aic_comparison$Model[1], "\n")
  cat("Velocity model rank:", which(grepl("Velocity", aic_comparison$Model))[1], "\n")
  cat("Duration model rank:", which(grepl("Duration.*only", aic_comparison$Model))[1], "\n")
}

cat("Strong correlations found:", 
    any(c(abs(cor_velocity_amplitude$estimate), 
          abs(cor_velocity_duration$estimate), 
          abs(cor_amplitude_duration$estimate)) > 0.5), "\n")

cat("\nCRITICAL FINDING: Outlier removal strategy affects results:", 
    length(unique(c(
      if(exists("analysis_final") && length(analysis_final$aic) > 0) names(analysis_final$aic)[which.min(analysis_final$aic)],
      if(exists("analysis_earlier") && length(analysis_earlier$aic) > 0) names(analysis_earlier$aic)[which.min(analysis_earlier$aic)],
      if(exists("analysis_none") && length(analysis_none$aic) > 0) names(analysis_none$aic)[which.min(analysis_none$aic)]
    ))) > 1, "\n")

cat("\nReproduction completed at:", Sys.time(), "\n")

# Save results
save(df2_final, df2_earlier, df2_none, 
     analysis_final, analysis_earlier, analysis_none,
     aic_comparison, models, 
     cor_velocity_amplitude, cor_velocity_duration, cor_amplitude_duration,
     file = "reproduction_results_sensitivity.RData")

cat("Results saved to: reproduction_results_sensitivity.RData\n")
