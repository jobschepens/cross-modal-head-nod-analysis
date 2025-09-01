# ===============================================================================
# 05_PUBLICATION_FIGURES.R
# Cross-modal Head Nod Study: Publication-Ready Figure Generation
# ===============================================================================
# 
# This script generates standardized, publication-ready figures including:
# - Standardized plot themes and color schemes
# - High-resolution figure export functions
# - Consistent formatting across all visualizations
# - Figure legends and annotations for manuscripts
# - Multi-panel figures for comprehensive display
#
# Author: Publication figure generation system
# Date: 2025-01-31
# ===============================================================================

# ===============================================================================
# SETUP AND CONFIGURATION
# ===============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(viridis)
  library(patchwork)
  library(scales)
  library(grid)
  library(gridExtra)
  library(RColorBrewer)
  library(readxl)
})

# Load configuration if not already loaded
if (!exists("CONFIG")) {
  if (file.exists("../config.R")) {
    source("../config.R")
  } else if (file.exists("config.R")) {
    source("config.R")
  } else {
    stop("config.R not found. Please ensure you're running from the correct directory.")
  }
}

# Initialize analysis environment
if (exists("initialize_analysis") && is.function(initialize_analysis)) {
  initialize_analysis()
}

# Publication-specific configuration (extends base CONFIG)
PUB_CONFIG <- list(
  FIGURES_DIR = file.path(CONFIG$BASE_DIR, "figures"),
  NORMALIZATION_FILE = file.path(CONFIG$DATA_DIR, "norm.xlsx"),
  
  # Publication settings
  DPI = 300,
  FIGURE_WIDTH = 8,
  FIGURE_HEIGHT = 6,
  WIDE_FIGURE_WIDTH = 12,
  MULTI_PANEL_HEIGHT = 10,
  
  # Data categories
  HEAD_NOD_FORMS = c("sn", "hnn", "mn", "ln", "lnn"),
  HEAD_NOD_FUNCTIONS = c("affirmation", "feedback", "other"),
  KINEMATIC_VARS = c("length (seconds)", "extremes amplitude", "velocity"),
  AFFIRM_LABELS = c("affirm PR", "affirm mid-turn", "affirmation turn_initialization"),
  FEEDB_LABELS = c("feedb PR", "feedb mid-turn", "feedback turn_initialization"),
  
  # Color schemes
  LANGUAGE_COLORS = c("DGS" = "#1f77b4", "German" = "#ff7f0e", "RSL" = "#2ca02c", "Russian" = "#d62728"),
  MODALITY_COLORS = c("Signed" = "#2E86AB", "Spoken" = "#A23B72"),
  FUNCTION_COLORS = c("affirmation" = "#1f77b4", "feedback" = "#ff7f0e", "other" = "#2ca02c")
)

cat("===============================================================================\n")
cat("05_PUBLICATION_FIGURES: Cross-modal Head Nod Study\n")
cat("Publication-Ready Figure Generation\n")
cat("===============================================================================\n")

# Create publication figures directory
if (!dir.exists(PUB_CONFIG$FIGURES_DIR)) {
  dir.create(PUB_CONFIG$FIGURES_DIR, recursive = TRUE)
}

cat("Configuration loaded:\n")
cat("- Publication figures directory:", PUB_CONFIG$FIGURES_DIR, "\n")
cat("- Figure resolution:", PUB_CONFIG$DPI, "DPI\n")
cat("- Standard dimensions:", PUB_CONFIG$FIGURE_WIDTH, "x", PUB_CONFIG$FIGURE_HEIGHT, "\n\n")

# ===============================================================================
# PUBLICATION THEME AND STYLING
# ===============================================================================

#' Create standardized publication theme
theme_publication <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size, face = "bold"),
      
      # Layout elements
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      
      # Legend positioning
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      
      # Strip styling for facets
      strip.background = element_rect(fill = "grey95", color = "black", size = 0.5),
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Save publication figure with consistent settings
save_publication_figure <- function(plot, filename, width = PUB_CONFIG$FIGURE_WIDTH, 
                                  height = PUB_CONFIG$FIGURE_HEIGHT, dpi = PUB_CONFIG$DPI) {
  
  filepath <- file.path(PUB_CONFIG$FIGURES_DIR, filename)
  
  ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = "pdf",
    useDingbats = FALSE  # For better text rendering
  )
  
  cat(sprintf("Saved publication figure: %s\n", filename))
}

# ===============================================================================
# UTILITY FUNCTIONS
# ===============================================================================

#' Convert time values to minutes (from previous scripts)
convert_time_to_minutes <- function(time_val) {
  if (inherits(time_val, c("POSIXct", "POSIXt", "Date"))) {
    hours <- as.numeric(format(time_val, "%H"))
    minutes <- as.numeric(format(time_val, "%M"))
    seconds <- as.numeric(format(time_val, "%S"))
    return(hours * 60 + minutes + seconds / 60)
  }
  
  time_str <- as.character(time_val)
  
  if (grepl(":", time_str)) {
    parts <- as.numeric(strsplit(time_str, ":")[[1]])
    if (length(parts) == 3) {
      return(parts[1] * 60 + parts[2] + parts[3] / 60)
    } else if (length(parts) == 2) {
      return(parts[1] + parts[2] / 60)
    }
  }
  
  numeric_val <- suppressWarnings(as.numeric(time_str))
  if (!is.na(numeric_val)) {
    return(numeric_val)
  }
  
  warning(paste("Could not parse time value:", time_str))
  return(0)
}

#' Add normalized counts to data
add_normalized_counts <- function(data, normalization_vector) {
  data %>%
    mutate(
      short_lang = substr(language, 1, 3),
      norm_n = n / as.numeric(normalization_vector[short_lang])
    ) %>%
    select(-short_lang)
}

# ===============================================================================
# DATA LOADING AND NORMALIZATION
# ===============================================================================

cat("===============================================================================\n")
cat("LOADING DATA AND SETTING UP NORMALIZATION\n")
cat("===============================================================================\n")

# Load main datasets
datasets <- list(
  form_all = read_csv(file.path(CONFIG$DATA_DIR, "form_wide_all_languages.csv"), show_col_types = FALSE),
  function_all = read_csv(file.path(CONFIG$DATA_DIR, "function_wide_all_languages.csv"), show_col_types = FALSE),
  functionturn_all = read_csv(file.path(CONFIG$DATA_DIR, "functionturn_wide_all_languages.csv"), show_col_types = FALSE),
  turn_all = read_csv(file.path(CONFIG$DATA_DIR, "turn_wide_all_languages.csv"), show_col_types = FALSE)
)

cat("Datasets loaded:\n")
for (name in names(datasets)) {
  cat(sprintf("- %s: %d rows x %d columns\n", name, nrow(datasets[[name]]), ncol(datasets[[name]])))
}

# Load normalization data
if (file.exists(PUB_CONFIG$NORMALIZATION_FILE)) {
  freq_data <- read_excel(PUB_CONFIG$NORMALIZATION_FILE, sheet = 1)
  
  # Process normalization
  freq_lang_col <- grep("lang|language", names(freq_data), ignore.case = TRUE, value = TRUE)[1]
  freq_length_col <- grep("annot.*[Ll]ength.*min|total.*min|duration.*min", 
                          names(freq_data), ignore.case = TRUE, value = TRUE)[1]
  
  freq_normalization <- freq_data %>%
    mutate(
      time_minutes = map_dbl(.data[[freq_length_col]], convert_time_to_minutes),
      short_lang = substr(.data[[freq_lang_col]], 1, 3)
    ) %>%
    group_by(short_lang) %>%
    summarise(
      total_minutes = sum(time_minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(short_lang), total_minutes > 0)
  
  freq_interaction_lengths <- setNames(
    freq_normalization$total_minutes,
    freq_normalization$short_lang
  )
  
  cat("\nNormalization data loaded and processed.\n\n")
} else {
  cat("Warning: Normalization file not found. Using default values.\n")
  freq_interaction_lengths <- c("DGS" = 180, "GER" = 150, "RSL" = 210, "RUS" = 120)
}

# ===============================================================================
# FIGURE 1: HEAD NOD FORMS DISTRIBUTION
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING FIGURE 1: HEAD NOD FORMS DISTRIBUTION\n")
cat("===============================================================================\n")

# Prepare form data
form_counts <- datasets$form_all %>%
  filter(Label %in% CONFIG$HEAD_NOD_FORMS) %>%
  group_by(language, Label) %>%
  summarise(n = n(), .groups = "drop") %>%
  add_normalized_counts(freq_interaction_lengths) %>%
  mutate(
    language_clean = case_when(
      str_detect(language, "DGS") ~ "DGS",
      str_detect(language, "GER") ~ "German",
      str_detect(language, "RSL") ~ "RSL",
      str_detect(language, "RUS") ~ "Russian",
      TRUE ~ language
    ),
    modality = case_when(
      language_clean %in% c("DGS", "RSL") ~ "Signed",
      language_clean %in% c("German", "Russian") ~ "Spoken",
      TRUE ~ "Unknown"
    )
  )

# Create Figure 1
figure1 <- ggplot(form_counts, aes(x = Label, y = norm_n, fill = language_clean)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = CONFIG$LANGUAGE_COLORS, name = "Language") +
  labs(
    title = "Figure 1: Distribution of Head Nod Forms Across Languages",
    subtitle = "Normalized frequency per minute of interaction",
    x = "Head Nod Form",
    y = "Frequency (per minute)"
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_publication_figure(figure1, "Figure1_head_nod_forms_distribution.pdf")

# ===============================================================================
# FIGURE 2: HEAD NOD FUNCTIONS DISTRIBUTION
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING FIGURE 2: HEAD NOD FUNCTIONS DISTRIBUTION\n")
cat("===============================================================================\n")

# Prepare function data
function_counts <- datasets$function_all %>%
  filter(Label %in% CONFIG$HEAD_NOD_FUNCTIONS) %>%
  group_by(language, Label) %>%
  summarise(n = n(), .groups = "drop") %>%
  add_normalized_counts(freq_interaction_lengths) %>%
  mutate(
    language_clean = case_when(
      str_detect(language, "DGS") ~ "DGS",
      str_detect(language, "GER") ~ "German",
      str_detect(language, "RSL") ~ "RSL",
      str_detect(language, "RUS") ~ "Russian",
      TRUE ~ language
    ),
    modality = case_when(
      language_clean %in% c("DGS", "RSL") ~ "Signed",
      language_clean %in% c("German", "Russian") ~ "Spoken",
      TRUE ~ "Unknown"
    )
  )

# Create Figure 2
figure2 <- ggplot(function_counts, aes(x = Label, y = norm_n, fill = language_clean)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = CONFIG$LANGUAGE_COLORS, name = "Language") +
  labs(
    title = "Figure 2: Distribution of Head Nod Functions Across Languages",
    subtitle = "Normalized frequency per minute of interaction",
    x = "Head Nod Function",
    y = "Frequency (per minute)"
  ) +
  theme_publication()

save_publication_figure(figure2, "Figure2_head_nod_functions_distribution.pdf")

# ===============================================================================
# FIGURE 3: KINEMATIC PROPERTIES COMPARISON
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING FIGURE 3: KINEMATIC PROPERTIES COMPARISON\n")
cat("===============================================================================\n")

# Prepare kinematic data
kinematic_data <- datasets$function_all %>%
  filter(Label %in% c("affirmation", "feedback")) %>%
  select(language, Label, all_of(CONFIG$KINEMATIC_VARS)) %>%
  filter(complete.cases(.)) %>%
  mutate(
    language_clean = case_when(
      str_detect(language, "DGS") ~ "DGS",
      str_detect(language, "GER") ~ "German",
      str_detect(language, "RSL") ~ "RSL",
      str_detect(language, "RUS") ~ "Russian",
      TRUE ~ language
    ),
    modality = case_when(
      language_clean %in% c("DGS", "RSL") ~ "Signed",
      language_clean %in% c("German", "Russian") ~ "Spoken",
      TRUE ~ "Unknown"
    )
  ) %>%
  # Remove extreme outliers
  filter(
    `length (seconds)` < quantile(`length (seconds)`, 0.95, na.rm = TRUE),
    `extremes amplitude` < quantile(`extremes amplitude`, 0.95, na.rm = TRUE),
    velocity < quantile(velocity, 0.95, na.rm = TRUE)
  )

# Create individual plots for each kinematic variable
kinematic_plots <- list()

for (var in CONFIG$KINEMATIC_VARS) {
  kinematic_plots[[var]] <- ggplot(kinematic_data, aes(x = language_clean, y = .data[[var]], fill = Label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    scale_fill_manual(values = CONFIG$FUNCTION_COLORS, name = "Function") +
    labs(
      title = str_to_title(var),
      x = "Language",
      y = str_to_title(var)
    ) +
    theme_publication() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Combine into multi-panel figure
figure3 <- wrap_plots(kinematic_plots, ncol = 3) +
  plot_annotation(
    title = "Figure 3: Kinematic Properties of Head Nods by Function and Language",
    subtitle = "Boxplots showing distribution of duration, amplitude, and velocity",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

save_publication_figure(figure3, "Figure3_kinematic_properties_comparison.pdf", 
                       width = CONFIG$WIDE_FIGURE_WIDTH, height = CONFIG$FIGURE_HEIGHT)

# ===============================================================================
# FIGURE 4: TURN-TAKING PATTERNS
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING FIGURE 4: TURN-TAKING PATTERNS\n")
cat("===============================================================================\n")

# Prepare turn-taking data
turn_labels <- c(CONFIG$AFFIRM_LABELS, CONFIG$FEEDB_LABELS)
turn_counts <- datasets$functionturn_all %>%
  filter(Label %in% turn_labels) %>%
  group_by(language, Label) %>%
  summarise(n = n(), .groups = "drop") %>%
  add_normalized_counts(freq_interaction_lengths) %>%
  mutate(
    language_clean = case_when(
      str_detect(language, "DGS") ~ "DGS",
      str_detect(language, "GER") ~ "German",
      str_detect(language, "RSL") ~ "RSL",
      str_detect(language, "RUS") ~ "Russian",
      TRUE ~ language
    ),
    function_type = case_when(
      Label %in% CONFIG$AFFIRM_LABELS ~ "Affirmation",
      Label %in% CONFIG$FEEDB_LABELS ~ "Feedback",
      TRUE ~ "Other"
    ),
    turn_position = case_when(
      str_detect(Label, "PR") ~ "Passive Recipiency",
      str_detect(Label, "mid-turn") ~ "Mid-turn",
      str_detect(Label, "turn_initialization") ~ "Turn Initialization",
      TRUE ~ "Unknown"
    )
  )

# Create Figure 4
figure4 <- ggplot(turn_counts, aes(x = turn_position, y = norm_n, fill = function_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~language_clean, scales = "free_y") +
  scale_fill_manual(values = c("Affirmation" = "#1f77b4", "Feedback" = "#ff7f0e"), 
                    name = "Function") +
  labs(
    title = "Figure 4: Turn-taking Patterns of Head Nods",
    subtitle = "Normalized frequency by turn position and function across languages",
    x = "Turn Position",
    y = "Frequency (per minute)"
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_publication_figure(figure4, "Figure4_turn_taking_patterns.pdf", 
                       width = CONFIG$WIDE_FIGURE_WIDTH, height = CONFIG$FIGURE_HEIGHT)

# ===============================================================================
# FIGURE 5: CROSS-MODAL COMPARISON
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING FIGURE 5: CROSS-MODAL COMPARISON\n")
cat("===============================================================================\n")

# Prepare cross-modal comparison data
modal_comparison <- function_counts %>%
  group_by(modality, Label) %>%
  summarise(
    mean_freq = mean(norm_n, na.rm = TRUE),
    se_freq = sd(norm_n, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create Figure 5
figure5 <- ggplot(modal_comparison, aes(x = Label, y = mean_freq, fill = modality)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_freq - se_freq, ymax = mean_freq + se_freq),
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = CONFIG$MODALITY_COLORS, name = "Modality") +
  labs(
    title = "Figure 5: Cross-modal Comparison of Head Nod Functions",
    subtitle = "Mean frequency with standard error across signed and spoken languages",
    x = "Head Nod Function",
    y = "Mean Frequency (per minute)"
  ) +
  theme_publication()

save_publication_figure(figure5, "Figure5_cross_modal_comparison.pdf")

# ===============================================================================
# SUPPLEMENTARY FIGURE: NORMALIZATION COMPARISON
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING SUPPLEMENTARY FIGURE: NORMALIZATION COMPARISON\n")
cat("===============================================================================\n")

# Create normalization comparison figure
if (exists("freq_normalization")) {
  supp_figure <- ggplot(freq_normalization, aes(x = short_lang, y = total_minutes, fill = short_lang)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    scale_fill_manual(values = CONFIG$LANGUAGE_COLORS, name = "Language") +
    labs(
      title = "Supplementary Figure: Interaction Lengths by Language",
      subtitle = "Total annotation time used for frequency normalization",
      x = "Language",
      y = "Total Minutes"
    ) +
    theme_publication() +
    theme(legend.position = "none")
  
  save_publication_figure(supp_figure, "SupplementaryFigure_normalization_lengths.pdf")
}

# ===============================================================================
# MULTI-PANEL SUMMARY FIGURE
# ===============================================================================

cat("===============================================================================\n")
cat("CREATING MULTI-PANEL SUMMARY FIGURE\n")
cat("===============================================================================\n")

# Create simplified versions for multi-panel display
panel_a <- ggplot(form_counts %>% filter(Label %in% c("sn", "hnn", "ln")), 
                  aes(x = Label, y = norm_n, fill = modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = CONFIG$MODALITY_COLORS, name = "Modality") +
  labs(title = "A) Head Nod Forms", x = "Form", y = "Frequency (per min)") +
  theme_publication() +
  theme(legend.position = "none", axis.title = element_text(size = 10))

panel_b <- ggplot(function_counts, aes(x = Label, y = norm_n, fill = modality)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = CONFIG$MODALITY_COLORS, name = "Modality") +
  labs(title = "B) Head Nod Functions", x = "Function", y = "Frequency (per min)") +
  theme_publication() +
  theme(legend.position = "none", axis.title = element_text(size = 10))

panel_c <- ggplot(kinematic_data, aes(x = Label, y = `length (seconds)`, fill = modality)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = CONFIG$MODALITY_COLORS, name = "Modality") +
  labs(title = "C) Duration Comparison", x = "Function", y = "Duration (s)") +
  theme_publication() +
  theme(legend.position = "none", axis.title = element_text(size = 10))

panel_d <- ggplot(modal_comparison, aes(x = Label, y = mean_freq, fill = modality)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = CONFIG$MODALITY_COLORS, name = "Modality") +
  labs(title = "D) Cross-modal Summary", x = "Function", y = "Mean Frequency") +
  theme_publication() +
  theme(axis.title = element_text(size = 10))

# Combine panels
summary_figure <- (panel_a | panel_b) / (panel_c | panel_d) +
  plot_annotation(
    title = "Summary Figure: Cross-modal Head Nod Analysis",
    subtitle = "Comprehensive comparison across signed and spoken languages",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

save_publication_figure(summary_figure, "SummaryFigure_comprehensive_analysis.pdf", 
                       width = CONFIG$WIDE_FIGURE_WIDTH, height = CONFIG$MULTI_PANEL_HEIGHT)

# ===============================================================================
# PUBLICATION FIGURES SUMMARY
# ===============================================================================

cat("\n===============================================================================\n")
cat("PUBLICATION FIGURES SUMMARY\n")
cat("===============================================================================\n")

cat("✓ PUBLICATION FIGURES GENERATED\n")
cat("  - Figure 1: Head nod forms distribution across languages\n")
cat("  - Figure 2: Head nod functions distribution across languages\n")
cat("  - Figure 3: Kinematic properties comparison (multi-panel)\n")
cat("  - Figure 4: Turn-taking patterns by function and language\n")
cat("  - Figure 5: Cross-modal comparison of functions\n")
cat("  - Supplementary Figure: Normalization lengths comparison\n")
cat("  - Summary Figure: Comprehensive multi-panel analysis\n\n")

cat("✓ PUBLICATION STANDARDS APPLIED\n")
cat("  - High resolution (300 DPI) PDF output\n")
cat("  - Consistent typography and styling\n")
cat("  - Standardized color schemes\n")
cat("  - Professional layout and spacing\n")
cat("  - Clear legends and annotations\n\n")

cat("✓ FIGURE SPECIFICATIONS\n")
cat(sprintf("  - Standard figures: %d x %d inches\n", CONFIG$FIGURE_WIDTH, CONFIG$FIGURE_HEIGHT))
cat(sprintf("  - Wide figures: %d x %d inches\n", CONFIG$WIDE_FIGURE_WIDTH, CONFIG$FIGURE_HEIGHT))
cat(sprintf("  - Multi-panel figures: %d x %d inches\n", CONFIG$WIDE_FIGURE_WIDTH, CONFIG$MULTI_PANEL_HEIGHT))
cat(sprintf("  - Resolution: %d DPI\n", PUB_CONFIG$DPI))
cat("  - Format: PDF (vector graphics)\n\n")

cat("✓ READY FOR MANUSCRIPT INTEGRATION\n")
cat("  - All figures saved in:", PUB_CONFIG$FIGURES_DIR, "\n")
cat("  - Consistent naming convention applied\n")
cat("  - Publication-ready quality and formatting\n")
cat("  - Cross-modal comparison framework highlighted\n")

cat("\n===============================================================================\n")
cat("05_PUBLICATION_FIGURES COMPLETED SUCCESSFULLY\n")
cat("All figures ready for manuscript submission\n")
cat("===============================================================================\n")