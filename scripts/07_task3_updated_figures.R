# ===============================================================================
# 07_TASK3_FIGURES.R
# Task 3 Kinematic Properties Analysis and Visualization
# ===============================================================================

cat("=== TASK 3 KINEMATIC ANALYSIS ===\n")

# Load configuration if not already loaded
if (!exists("CONFIG")) {
  # Try different paths for flexibility
  config_paths <- c("../config.R", "config.R")
  config_loaded <- FALSE
  
  for (config_path in config_paths) {
    if (file.exists(config_path)) {
      source(config_path)
      config_loaded <- TRUE
      break
    }
  }
  
  if (!config_loaded) {
    stop("config.R not found. Please ensure you're running from the repository root or scripts/ directory.")
  }
}

# Initialize analysis environment (optional - only if function exists)
if (exists("initialize_analysis") && is.function(initialize_analysis)) {
  # Use non-interactive mode for Binder compatibility
  initialize_analysis(interactive = FALSE, install_packages = FALSE)
}

library(tidyverse)
library(ggplot2)
library(scales)

cat("Loading data...\n")

# Load the data
df_raw <- read_csv(CONFIG$FUNCTION_DATA, show_col_types = FALSE)

# Define clean language mapping (matching original collaborator package)
language_clean <- c(
  "DGS_2.0_2412" = "DGS",
  "GER_2412" = "German", 
  "RSL_2507" = "RSL",
  "RUS_2503" = "Russian"
)

cat("Original languages:", paste(unique(df_raw$language), collapse = ", "), "\n")

# Clean and prepare data
df <- df_raw %>%
  # Apply language cleaning
  mutate(
    language_clean = recode(language, !!!language_clean),
    language_clean = factor(language_clean, levels = c("DGS", "German", "RSL", "Russian"))
  ) %>%
  # Rename kinematic variables for consistency
  rename(
    velocity = velocity,
    duration = `length (seconds)`,
    amplitude = `extremes amplitude`
  ) %>%
  # Ensure consistent factor levels
  mutate(
    Label = factor(Label)
  )

cat("Cleaned languages:", paste(levels(df$language_clean), collapse = ", "), "\n")

# Define kinematic variables for analysis
kinematic_vars <- c("velocity", "duration", "amplitude")

# Check available functions
available_functions <- names(table(df$Label))[table(df$Label) > 0]
target_functions <- c("affirmation", "feedback", "other")
available_target_functions <- intersect(target_functions, available_functions)

cat("Available functions:", paste(available_functions, collapse = ", "), "\n")
cat("Target functions:", paste(available_target_functions, collapse = ", "), "\n")

# Filter for target functions and complete cases
df_clean <- df %>%
  filter(Label %in% available_target_functions) %>%
  select(language_clean, Label, all_of(kinematic_vars)) %>%
  filter(complete.cases(.))

cat("Clean dataset BEFORE outlier removal:", nrow(df_clean), "observations\n")

# CRITICAL: Apply 5 IQR outlier removal (as in original script)
remove_outliers_5iqr <- function(data, variable_name) {
  data %>%
    filter(
      .data[[variable_name]] <= quantile(.data[[variable_name]], 0.75, na.rm = TRUE) + 5 * IQR(.data[[variable_name]], na.rm = TRUE),
      .data[[variable_name]] >= quantile(.data[[variable_name]], 0.25, na.rm = TRUE) - 5 * IQR(.data[[variable_name]], na.rm = TRUE)
    )
}

# Apply outlier removal to each kinematic variable
df_no_outliers <- df_clean
for(var in kinematic_vars) {
  df_no_outliers <- remove_outliers_5iqr(df_no_outliers, var)
}

cat("Clean dataset AFTER outlier removal:", nrow(df_no_outliers), "observations\n")
cat("Outliers removed:", nrow(df_clean) - nrow(df_no_outliers), "observations\n")

# =============================================================================
# TASK 3: CROSS-MODAL COMPARISON PLOTS - CORRECT JITTER PATTERN
# =============================================================================

cat("\n=== GENERATING TASK 3 PLOTS (CORRECT JITTER PATTERN) ===\n")

# Create plots with correct jitter pattern:
# - Grey dots: No jitter (fixed positions at category centers)
# - Colored dots: Heavy jitter to show distribution
create_correct_jitter_plot <- function(property, property_label) {
  
  cat(sprintf("Creating correct jitter pattern plot for %s\n", property))
  
  # Define dodge width
  dodge_width <- 0.75
  
  ggplot(
    df_no_outliers,
    aes(x = language_clean, y = .data[[property]], fill = Label)
  ) +
    # 1. Boxplots with colored fill
    geom_boxplot(position = position_dodge(width = dodge_width), alpha = 0.7) +
    
    # 2. Grey dots - NO JITTER (fixed positions)
    geom_point(
      color = "grey50",  # Fixed grey color
      position = position_dodge(width = dodge_width),  # NO jitter, just dodge
      alpha = 0.6, 
      size = 0.8
    ) +
    
    # 3. Colored dots - HEAVY JITTER to show distribution
    geom_point(
      aes(color = Label),
      position = position_jitterdodge(
        dodge.width = dodge_width,
        jitter.width = 0.4,  # Heavy jitter to show distribution
        seed = 123
      ),
      alpha = 0.7, 
      size = 0.6
    ) +
    
    facet_wrap(~Label, scales = "free_y") +
    labs(
      title = sprintf("Cross-Modal Comparison: %s", property_label),
      subtitle = "Head Nod Kinematic Properties by Function and Language",
      x = "Language",
      y = property_label,
      fill = "Function",
      color = "Function"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      strip.text = element_text(size = 11, face = "bold")
    ) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    scale_color_brewer(type = "qual", palette = "Set2")
}

# Alternative approach: Simpler version with clear separation
create_simple_jitter_plot <- function(property, property_label) {
  
  cat(sprintf("Creating simple jitter pattern plot for %s\n", property))
  
  ggplot(
    df_no_outliers,
    aes(x = language_clean, y = .data[[property]], fill = Label)
  ) +
    # Boxplots
    geom_boxplot(position = "dodge", alpha = 0.7) +
    
    # Jittered colored points to show distribution
    geom_point(
      aes(color = Label),
      position = position_jitterdodge(
        dodge.width = 0.75,
        jitter.width = 0.3,  # Strong jitter for distribution
        seed = 42
      ),
      alpha = 0.6, 
      size = 0.7
    ) +
    
    facet_wrap(~Label, scales = "free_y") +
    labs(
      title = sprintf("Cross-Modal Comparison: %s", property_label),
      subtitle = "Head Nod Kinematic Properties by Function and Language",
      x = "Language",
      y = property_label,
      fill = "Function",
      color = "Function"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      strip.text = element_text(size = 11, face = "bold")
    ) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    scale_color_brewer(type = "qual", palette = "Set2")
}

# Generate plots with correct jitter patterns
cat("=== Creating plots with correct jitter pattern ===\n")

# Method 1: Grey dots (no jitter) + colored dots (heavy jitter)
p3_velocity_correct <- create_correct_jitter_plot("velocity", "Velocity")
p3_duration_correct <- create_correct_jitter_plot("duration", "Duration (seconds)")
p3_amplitude_correct <- create_correct_jitter_plot("amplitude", "Extremes Amplitude")

# Method 2: Simple version (only colored jittered dots)
p3_velocity_simple <- create_simple_jitter_plot("velocity", "Velocity")
p3_duration_simple <- create_simple_jitter_plot("duration", "Duration (seconds)")
p3_amplitude_simple <- create_simple_jitter_plot("amplitude", "Extremes Amplitude")

# Create combined plot with correct jitter pattern
df_plot_long <- df_no_outliers %>%
  pivot_longer(
    cols = all_of(kinematic_vars),
    names_to = "property",
    values_to = "value"
  ) %>%
  mutate(
    property_label = case_when(
      property == "velocity" ~ "Velocity",
      property == "duration" ~ "Duration (seconds)",
      property == "amplitude" ~ "Extremes Amplitude"
    ),
    property_label = factor(property_label, levels = c("Velocity", "Duration (seconds)", "Extremes Amplitude"))
  )

# Combined plot with correct jitter pattern
p3_combined_correct <- ggplot(
  df_plot_long,
  aes(x = language_clean, y = value, fill = Label)
) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7) +
  
  # Grey dots - NO jitter
  geom_point(
    color = "grey50",
    position = position_dodge(width = 0.75),  # No jitter
    alpha = 0.5, 
    size = 0.4
  ) +
  
  # Colored dots - HEAVY jitter
  geom_point(
    aes(color = Label),
    position = position_jitterdodge(
      dodge.width = 0.75,
      jitter.width = 0.35,
      seed = 123
    ),
    alpha = 0.6, 
    size = 0.5
  ) +
  
  facet_grid(property_label ~ Label, scales = "free_y") +
  labs(
    title = "Cross-Modal Comparison: Head Nod Kinematic Properties",
    subtitle = "Grey dots: fixed positions, Colored dots: jittered for distribution",
    x = "Language",
    y = "Value",
    fill = "Function",
    color = "Function"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_color_brewer(type = "qual", palette = "Set2")

# Save plots with correct jitter pattern
cat("Saving plots with correct jitter pattern...\n")

# Method 1: Grey (no jitter) + colored (jitter)
ggsave(file.path(CONFIG$FIGURES_DIR, "task3_velocity_correct_jitter.pdf"), p3_velocity_correct,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_duration_correct_jitter.pdf"), p3_duration_correct,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_amplitude_correct_jitter.pdf"), p3_amplitude_correct,
       width = 12, height = 8, dpi = 300)

# Method 2: Simple version (only colored jitter)
ggsave(file.path(CONFIG$FIGURES_DIR, "task3_velocity_simple_jitter.pdf"), p3_velocity_simple,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_duration_simple_jitter.pdf"), p3_duration_simple,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_amplitude_simple_jitter.pdf"), p3_amplitude_simple,
       width = 12, height = 8, dpi = 300)

# Combined plot
ggsave(file.path(CONFIG$FIGURES_DIR, "task3_kinematic_distributions_correct_jitter.pdf"), p3_combined_correct,
       width = 16, height = 12, dpi = 300)

# Save with collaborator package naming (using simple jitter approach)
ggsave(file.path(CONFIG$FIGURES_DIR, "task3_cross_modal_comparison_velocity_correct_jitter.pdf"), p3_velocity_simple,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_cross_modal_comparison_length__seconds__correct_jitter.pdf"), p3_duration_simple,
       width = 12, height = 8, dpi = 300)

ggsave(file.path(CONFIG$FIGURES_DIR, "task3_cross_modal_comparison_extremes_amplitude_correct_jitter.pdf"), p3_amplitude_simple,
       width = 12, height = 8, dpi = 300)

cat("\\nCorrect jitter pattern Task 3 plots generated successfully!\\n")
cat("Files created:\\n")
cat("- task3_kinematic_distributions_correct_jitter.pdf (combined plot)\\n")
cat("\\nMethod 1 (grey no-jitter + colored jitter):\\n")
cat("- task3_velocity_correct_jitter.pdf\\n")
cat("- task3_duration_correct_jitter.pdf\\n")
cat("- task3_amplitude_correct_jitter.pdf\\n")
cat("\\nMethod 2 (simple - only colored jitter):\\n")
cat("- task3_velocity_simple_jitter.pdf\\n")
cat("- task3_duration_simple_jitter.pdf\\n")
cat("- task3_amplitude_simple_jitter.pdf\\n")
cat("\\nCollaborator package naming (simple jitter):\\n")
cat("- task3_cross_modal_comparison_velocity_correct_jitter.pdf\\n")
cat("- task3_cross_modal_comparison_length__seconds__correct_jitter.pdf\\n")
cat("- task3_cross_modal_comparison_extremes_amplitude_correct_jitter.pdf\\n")

cat("\\n=== JITTER PATTERN FIXES APPLIED ===\\n")
cat("1. Grey dots: Fixed positions (position_dodge only, no jitter)\\n")
cat("2. Colored dots: Heavy jitter (jitter.width = 0.3-0.4) to show distribution\\n")
cat("3. Clear visual separation between fixed and jittered points\\n")
cat("4. Maintained original Set2 color scheme\\n")

cat("\\n=== TASK 3 CORRECT JITTER PATTERN COMPLETED ===\\n")
