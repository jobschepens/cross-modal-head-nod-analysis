# Binder Integration Guide

## Overview
This repository is now fully integrated with Binder, allowing users to run the complete cross-modal head nod analysis in their browser without any local installation.

## What is Binder?
Binder (mybinder.org) is a free service that creates reproducible, interactive computational environments from Git repositories. It:
- Builds a Docker container from your repository
- Provides access via web browser
- Includes RStudio, Jupyter, or other interfaces
- Requires no software installation from users

## Repository Configuration

### Core Binder Files
- `.binder/runtime.txt` - Specifies R version (4.4)
- `.binder/install.R` - Installs all required R packages
- `.binder/postBuild` - Post-build setup script
- `analysis_notebook.Rmd` - Interactive tutorial notebook

### Supporting Files
- `requirements.txt` - R package dependencies
- `install.R.binder` - Binder-specific package installation
- Updated `README.md` with Binder badge and instructions
- Enhanced `test_repository.R` with Binder environment detection

## Binder Environment Features

### Pre-installed Components
✅ R 4.4 with RStudio interface
✅ All 20 essential R packages (including extras for Binder)
✅ Complete repository with data and scripts
✅ Output directories pre-created
✅ Repository validation on startup

### Package List
Essential analysis packages:
- tidyverse, readxl, rstatix, lme4, boot, broom
- car, emmeans, effectsize, pwr, reshape2
- viridis, patchwork, scales, RColorBrewer, gridExtra
- knitr, rmarkdown, devtools, here

### User Experience
1. **Click Binder badge** in README
2. **Wait 2-3 minutes** for environment build (first time)
3. **RStudio opens** automatically in browser
4. **All scripts ready** to run immediately
5. **Interactive analysis** with full R capabilities

## Access Methods

### Primary Access
**Binder Badge**: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jobschepens/cross-modal-head-nod-analysis/HEAD?urlpath=rstudio)

### Direct URL
```
https://mybinder.org/v2/gh/jobschepens/cross-modal-head-nod-analysis/HEAD?urlpath=rstudio
```

### Alternative Interfaces
- **RStudio**: `?urlpath=rstudio` (default)
- **Jupyter Lab**: `?urlpath=lab`
- **R Notebook**: `?filepath=analysis_notebook.Rmd`

## Usage Scenarios

### For Collaborators
- ✅ Immediate access to analysis environment
- ✅ No R/RStudio installation needed
- ✅ Consistent environment across all users
- ✅ Can modify code and rerun analyses

### For Reviewers
- ✅ Verify reproducibility without setup
- ✅ Explore data and methods interactively
- ✅ Test modifications to analyses
- ✅ Generate alternative visualizations

### For Students/Learners
- ✅ Learn R and statistical analysis
- ✅ Follow guided tutorial notebook
- ✅ Experiment with real research data
- ✅ No technical barriers to entry

## Technical Details

### Build Process
1. Binder reads `.binder/runtime.txt` for R version
2. Installs base R environment
3. Runs `.binder/install.R` to install packages
4. Executes `.binder/postBuild` for setup
5. Launches RStudio interface

### Environment Validation
The `test_repository.R` script automatically:
- Detects Binder environment
- Validates all packages installed
- Tests data file accessibility
- Confirms script functionality
- Reports environment status

### Data Persistence
⚠️ **Important**: Binder environments are temporary
- Changes are NOT saved when session ends
- Download important results before closing
- Sessions timeout after ~10 minutes of inactivity
- Use for exploration and analysis, not long-term storage

## Troubleshooting

### Common Issues
1. **Build fails**: Check `.binder/install.R` for package conflicts
2. **Slow startup**: First build takes longer; subsequent faster
3. **Missing packages**: Update `.binder/install.R`
4. **Data not found**: Ensure files in repository match script paths

### Performance
- **Memory**: 2GB RAM limit in Binder
- **Compute**: Limited CPU resources
- **Storage**: Temporary filesystem only
- **Sessions**: Auto-timeout after inactivity

## Benefits for Research

### Reproducibility
- ✅ Exact environment specification
- ✅ Version-controlled configuration
- ✅ Platform-independent access
- ✅ Eliminates "works on my machine" issues

### Accessibility
- ✅ No technical setup barriers
- ✅ Works on any device with browser
- ✅ Immediate availability
- ✅ Free to use

### Collaboration
- ✅ Shared computational environment
- ✅ Easy result sharing and verification
- ✅ Interactive code exploration
- ✅ Real-time analysis capabilities

The repository is now fully Binder-enabled and ready for online collaborative analysis! 🚀
