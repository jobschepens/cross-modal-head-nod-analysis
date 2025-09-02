# Cross-Modal Head Nod Analysis: A Linguistic Study

[![DOI](https://img.shields.io/badge/DOI-paper%20reference-blue)](link-to-paper)
[![R](https://img.shields.io/badge/R-4.4%2B-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jobschepens/cross-modal-head-nod-analysis/HEAD?urlpath=rstudio)

## 🚀 Quick Start Options

### Option 1: Run Online with Binder (No Installation Required!)
Click the Binder badge above or this link to run the analysis in your browser:

**🔗 [Launch Interactive Analysis](https://mybinder.org/v2/gh/jobschepens/cross-modal-head-nod-analysis/HEAD?urlpath=rstudio)**

- ✅ No software installation needed
- ✅ Full RStudio environment in browser
- ✅ All packages pre-installed
- ✅ Interactive analysis and visualization
- ✅ Works on any device with web browser

### Option 2: Local Installation
```bash
git clone https://github.com/jobschepens/cross-modal-head-nod-analysis.git
cd cross-modal-head-nod-analysis
Rscript install.R        # Install required packages
Rscript test_repository.R  # Validate everything works
```

### Option 3: Docker Container (Optimized)
```bash
git clone https://github.com/jobschepens/cross-modal-head-nod-analysis.git
cd cross-modal-head-nod-analysis
docker-compose up head-nod-analysis
```

**Docker Optimization Features:**
- ✅ **Hybrid package installation**: Uses `install2.r` (littler) for critical packages, ensuring reliable dependency resolution
- ✅ **Binary packages**: Leverages `rocker/tidyverse:latest` for pre-compiled CRAN binaries (no source compilation)
- ✅ **Fast builds**: Critical packages (lme4, tidyverse components) installed via optimized `install2.r`
- ✅ **Fallback support**: Custom `install.R` handles any additional packages not covered by `install2.r`
- ✅ **Reproducible environment**: Consistent package versions across different systems

## Overview

This repository contains the analysis code and data for the cross-modal head nod study comparing kinematic properties across four languages: German Sign Language (DGS), German, Russian Sign Language (RSL), and Russian. The study investigates head nod forms, functions, and kinematic patterns using computer vision analysis and statistical methods.

## Repository Structure

```
├── data/                           # Datasets and supporting files
│   ├── function_wide_all_languages.csv    # Main analysis dataset
│   ├── form_wide_all_languages.csv        # Form distribution data
│   ├── functionturn_wide_all_languages.csv # Turn-taking analysis data
│   ├── turn_wide_all_languages.csv        # Turn position data
│   ├── norm.xlsx                          # Normalization data
│   ├── abstract.txt                       # Study abstract
│   └── task*.txt                          # Analysis task descriptions
├── scripts/                        # R analysis scripts
│   ├── 01_data_preparation.R              # Data loading and cleaning
│   ├── 02_comprehensive_analysis.R        # Main analysis pipeline
│   ├── 03_advanced_statistics.R           # Statistical modeling
│   ├── 04_refined_analyses.R              # Focused analyses
│   ├── 05_publication_figures.R           # Publication-ready plots
│   ├── 06_corrected_analysis.R            # Data corrections
│   ├── 07_task3_figures.R                 # Task 3 kinematic visualizations
│   └── 08_reproduce_original_plos.R       # Reproduction of original PLOS study
├── figures/                        # Generated plots and visualizations
│   ├── task1_*.pdf                        # Head nod forms analysis
│   ├── task2_*.pdf                        # Functions distribution
│   ├── task3_*.pdf                        # Kinematic properties
│   ├── task4_*.pdf                        # Turn-taking analysis
│   └── gardner_altman_*.pdf               # Effect size visualizations
├── results/                        # Statistical outputs and summaries
├── narrative/                      # Analysis documentation
├── DEVELOPMENT_NOTES.md            # Technical documentation
└── README.md                       # This file
```

## Study Design

### Research Questions
1. **Forms Analysis**: Distribution of head nod forms across languages
2. **Functions Analysis**: Head nod functions (affirmation, feedback, other)
3. **Kinematic Analysis**: Cross-modal comparison of velocity, duration, and amplitude
4. **Turn-taking Analysis**: Head nods in conversational contexts
5. **Statistical Testing**: Significance testing and effect sizes

### Languages Studied
- **German Sign Language (DGS)** - Signed modality
- **German** - Spoken modality  
- **Russian Sign Language (RSL)** - Signed modality
- **Russian** - Spoken modality

### Data Collection
- **Computer vision**: OpenPose keypoint extraction for kinematic analysis
- **Manual annotation**: ELAN-based functional annotation
- **Cross-modal design**: Signed vs. spoken language comparison

## Key Findings

### Kinematic Properties (Task 3)
The kinematic analysis examines cross-modal head nod patterns with:
- **Velocity**: Primary differentiator across functions and languages
- **Duration**: Secondary kinematic property showing functional differences  
- **Amplitude**: Extremes of head movement range

### Statistical Analysis
- **Non-parametric testing**: Kruskal-Wallis tests (data non-normality)
- **Effect size estimation**: Cohen's d with bootstrap confidence intervals
- **Mixed-effects modeling**: Language and function interactions
- **Gardner-Altman plots**: Modern effect size visualization

## Requirements

### Software
- **R 4.4+**
- **RStudio** (recommended)

### R Packages

```r
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "ggplot2",      # Plotting
  "rstatix",      # Statistical tests
  "lme4",         # Mixed-effects models
  "readxl",       # Excel file reading
  "scales",       # Plot scaling
  "reshape2"      # Data reshaping
))
```

## 🐳 Binder Integration

This repository is fully configured for **Binder**, allowing you to run the complete analysis in your browser without any local installation:

### Features
- ✅ **Zero installation**: Click and run immediately
- ✅ **Full RStudio environment**: Complete IDE with all features
- ✅ **Pre-installed packages**: All 16 essential R packages ready
- ✅ **Interactive analysis**: Modify code, generate plots, explore data
- ✅ **Reproducible environment**: Same setup for all users

### Getting Started with Binder

1. **Click the Binder badge** at the top of this README
2. **Wait for environment setup** (2-3 minutes first time)
3. **RStudio opens** in your browser automatically
4. **Start analyzing**: All scripts and data ready to use

### What's Available in Binder

- **Interactive Notebook**: `analysis_notebook.Rmd` - Guided analysis tutorial
- **Full Script Access**: All 8 analysis scripts in `scripts/` directory  
- **Data Exploration**: Complete datasets in `data/` directory
- **Results Generation**: Create figures and statistical outputs
- **Repository Testing**: Run `test_repository.R` to validate everything

### Binder Configuration Files

The repository includes specialized Binder configuration:

- `.binder/runtime.txt` - Specifies R version (4.4)
- `.binder/install.R` - Installs all required R packages
- `.binder/postBuild` - Sets up analysis environment
- `analysis_notebook.Rmd` - Interactive tutorial notebook

## Usage

### Quick Start

1. **Clone the repository**:
   ```bash
   git clone https://github.com/username/cross-modal-head-nod-analysis.git
   cd cross-modal-head-nod-analysis
   ```

2. **Setup and install dependencies**:
   ```r
   # Run the installation script (REQUIRED FIRST STEP)
   source("install.R")
   
   # Validate installation
   source("test_repository.R")
   ```

3. **Run the analysis pipeline**:
   ```r
   # Main analysis (generates all figures and results)
   source("scripts/02_comprehensive_analysis.R")
   
   # Advanced statistics
   source("scripts/03_advanced_statistics.R")
   
   # Publication figures
   source("scripts/05_publication_figures.R")
   ```

### Prerequisites

- **R version**: 4.0+ (recommended 4.3+)
- **System**: Windows, macOS, or Linux
- **Memory**: At least 4GB RAM recommended for large datasets

### Individual Analyses
- **Task 1 - Forms**: `scripts/02_comprehensive_analysis.R`
- **Task 2 - Functions**: `scripts/02_comprehensive_analysis.R`  
- **Task 3 - Kinematics**: `scripts/07_task3_figures.R`
- **Task 4 - Turn-taking**: `scripts/04_refined_analyses.R`
- **Task 5 - Statistics**: `scripts/03_advanced_statistics.R`
- **Reproduction Analysis**: `scripts/08_reproduce_original_plos.R`

## Kinematic Analysis

The kinematic analysis examines three key properties:
- **Velocity**: Angular velocity of head movement
- **Duration**: Length of head nod sequences  
- **Amplitude**: Extremes of head movement range

**Analysis script**: `scripts/07_task3_figures.R`
**Figures**: `figures/task3_*.pdf`

## Reproduction Analysis

### PLOS Study Validation
The repository includes reproduction of the original PLOS DGS study to validate methodological consistency.

**Script**: `scripts/08_reproduce_original_plos.R`

**Key Findings**:
- ✅ Velocity: p < 0.001 (highly significant) - primary differentiator
- ✅ Duration: p < 0.001 (highly significant) - secondary differentiator  
- ✅ Amplitude: p > 0.25 (non-significant)
- ✅ Results stable across all outlier removal approaches

**Documentation**:
- `results/reproduction_output.txt` - Complete statistical output

## Reproducibility

### Data Preprocessing
All data preprocessing steps are documented in:
- `scripts/01_data_preparation.R` - Data loading and cleaning
- `scripts/06_corrected_analysis.R` - Label corrections and fixes

### Statistical Methods
- **Outlier removal**: 5 IQR rule consistently applied
- **Effect sizes**: Bootstrap confidence intervals (2000 iterations)
- **Multiple comparisons**: Appropriate corrections applied
- **Non-parametric methods**: Used due to data non-normality

## Citation

If you use this code or data, please cite:

```bibtex
@article{yourname2025headnod,
  title={Cross-Modal Head Nod Analysis: A Linguistic Study},
  author={Your Name and Collaborators},
  journal={Journal Name},
  year={2025},
  publisher={Publisher}
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## AI Assistance Disclosure

This project used AI assistance (GitHub Copilot, Claude) for code organization, documentation, and workflow optimization. All scientific decisions and interpretations remain fully human-directed. See [DEVELOPMENT_NOTES.md](DEVELOPMENT_NOTES.md) for complete transparency details.

## Contact

- **Lead Author**: [Your Name] - your.email@institution.edu
- **Repository**: https://github.com/username/cross-modal-head-nod-analysis
- **Issues**: Please use GitHub Issues for questions and bug reports

## Acknowledgments

- OpenPose computer vision framework for kinematic analysis
- ELAN software for annotation
- R statistical computing environment
- Study participants and collaborating institutions

#### **01_data_preparation.R** (679+ lines)
- **Purpose**: Data reshaping and preparation utilities
- **Function**: Convert wide to long format, data validation, prepare datasets for analysis
- **Replaces**: `reshape_functionturn_long.R`
- **Output**: Prepared data files for all subsequent analyses

#### **02_comprehensive_analysis.R** (679+ lines)
- **Purpose**: Main analysis covering all 5 concrete tasks
- **Function**: 
  - Task 1: Head nod forms distribution analysis
  - Task 2: Head nod functions distribution analysis  
  - Task 3: Three-variable kinematic comparison
  - Task 4: Turn-taking analysis
  - Task 5: Basic statistical significance testing
- **Output**: Core figures and basic statistical results

#### **03_advanced_statistics.R** (800+ lines)
- **Purpose**: Advanced statistical methods and estimation statistics
- **Function**: 
  - Mixed-effects modeling with random effects
  - Gardner-Altman estimation plots with bootstrap confidence intervals
  - Comprehensive power analysis
  - Raincloud plots and sophisticated visualizations
  - Model comparison and selection
- **Consolidates**: `task5_statistical_analysis.R` + `task5_complete_statistical_analysis.R` + `task5_estimation_statistics_corrected.R`
- **Output**: Advanced statistical results, Gardner-Altman plots, model comparisons

#### **04_refined_analyses.R** (485 lines)
- **Purpose**: Focused analyses on core categories
- **Function**: 
  - Core head nod forms (sn, hnn, mn, ln, lnn)
  - Key function-turn combinations (6 specific categories)
  - Targeted statistical comparisons for cleaner results
- **Consolidates**: `task5_forms_refined.R` + `task5_functionturn_corrected.R` + `task5_functionturn_refined.R`
- **Output**: Refined statistical analyses and focused Gardner-Altman plots

#### **05_publication_figures.R** (434 lines)
- **Purpose**: Generate standardized, publication-ready figures
- **Function**: 
  - Standardized plot themes and color schemes
  - High-resolution figure export (300 DPI PDF)
  - Multi-panel figures for comprehensive display
  - Consistent formatting for manuscript submission
- **New**: Extracted figure generation functionality
- **Output**: Publication-ready figures with professional formatting

## Research Framework

### Five Concrete Tasks Addressed

1. **Task 1**: Head nod forms distribution analysis per language
2. **Task 2**: Head nod functions distribution analysis per language
3. **Task 3**: Three-variable kinematic comparison (duration, amplitude, velocity)
4. **Task 4**: Turn-taking analysis (passive recipiency, turn-taking, mid-turn)
5. **Task 5**: Statistical significance testing with advanced methods

### Cross-modal Comparison Framework

- **Languages**: DGS, German, RSL, Russian
- **Modalities**: Signed (DGS, RSL) vs Spoken (German, Russian)
- **Functions**: Affirmation, Feedback, Other
- **Forms**: Single nod (sn), Head nod sequence (hnn), Large nod (ln), Large nod sequence (lnn), Mixed nod (mn)
- **Turn-taking**: Passive recipiency (PR), Turn initialization, Mid-turn

## Key Features

### Statistical Methods
- **Traditional**: ANOVA, post-hoc tests, effect sizes (eta-squared)
- **Advanced**: Mixed-effects modeling, bootstrap confidence intervals
- **Estimation Statistics**: Gardner-Altman plots emphasizing effect sizes over p-values
- **Power Analysis**: Comprehensive power calculations for all comparisons
- **Cross-modal**: Signed vs spoken language comparisons

### Data Processing
- **Normalization**: Proper frequency normalization using interaction lengths
- **Outlier Handling**: 5×IQR rule for extreme outlier removal
- **Data Validation**: Comprehensive checks and error handling
- **Multiple Formats**: Both long and wide format data preparation

### Visualization
- **Gardner-Altman Plots**: 19 estimation statistics plots showing effect sizes with confidence intervals
- **Publication Figures**: High-resolution (300 DPI) PDF outputs
- **Comprehensive Coverage**: All 5 tasks visualized with consistent styling
- **Cross-modal Emphasis**: Signed vs spoken comparisons highlighted

## Usage Instructions

### Quick Start (Recommended Workflow)

```r
# 1. Prepare data (if needed)
source("scripts/01_data_preparation.R")

# 2. Run comprehensive analysis (all 5 tasks)
source("scripts/02_comprehensive_analysis.R")

# 3. Generate advanced statistics
source("scripts/03_advanced_statistics.R")

# 4. Create refined analyses (optional)
source("scripts/04_refined_analyses.R")

# 5. Generate publication figures
source("scripts/05_publication_figures.R")
```

### Individual Script Usage

Each script can be run independently:

- **Data Preparation**: Run `01_data_preparation.R` to reshape and validate data
- **Core Analysis**: Run `02_comprehensive_analysis.R` for all 5 concrete tasks
- **Advanced Stats**: Run `03_advanced_statistics.R` for sophisticated statistical methods
- **Focused Analysis**: Run `04_refined_analyses.R` for core category comparisons
- **Publication**: Run `05_publication_figures.R` for manuscript-ready figures


## Requirements

### R Packages
```r
# Core packages
library(tidyverse)
library(ggplot2)
library(readxl)

# Statistical analysis
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(effectsize)
library(pwr)
library(boot)

# Visualization
library(viridis)
library(patchwork)
library(gridExtra)
library(scales)

# Output formatting
library(knitr)
library(kableExtra)
library(broom)
library(broom.mixed)
```

### Data Requirements
- `norm.xlsx`: Normalization data (interaction lengths)
- Wide-format CSV files for each tier (form, function, turn, functionturn)
- Proper column naming: `extremes.amplitude` (with dot, not underscore)

## Output Files

### Statistical Results
- **CSV files**: Comprehensive statistical results, effect sizes, power analysis
- **LaTeX tables**: Publication-ready statistical tables
- **Model comparisons**: AIC/BIC model selection results

### Figures
- **19 Gardner-Altman plots**: Proper estimation statistics (no outliers, no simple error bars)
- **Core analysis figures**: Distribution plots, boxplots, turn-taking patterns
- **Publication figures**: High-resolution, professionally formatted
- **Multi-panel summaries**: Comprehensive overview figures

### Documentation
- **Statistical narrative**: Comprehensive analysis interpretation
- **Log files**: Detailed analysis output and diagnostics
- **README**: Complete usage documentation (this file)


## Citation and Usage

This repository provides a complete analysis framework for cross-modal head nod studies.

**Recommended Citation**: Cross-modal Head Nod Study Analysis, 2025.

## Contact and Support

For questions about the analysis, refer to the documentation in each script.

---

**Analysis Scripts**: 8 R scripts covering all tasks
**Statistical Methods**: Non-parametric testing, mixed-effects modeling, effect size estimation