# Binder Integration Guide - Rocker Project Edition

## Overview
This repository uses **rocker/binder** for a reliable, consistent Binder environment that matches our Docker setup. This provides better package compatibility and eliminates the lme4 compilation issues.

## What is Binder?
Binder (mybinder.org) is a free service that creates reproducible, interactive computational environments from Git repositories. Our setup uses the rocker project for enhanced reliability.

## Repository Configuration

### Core Binder Files (Updated for Rocker)
- `.binder/Dockerfile` - **NEW**: Uses rocker/binder base image
- `.binder/runtime.txt` - Updated to document Dockerfile usage
- `.binder/install.R` - Streamlined for rocker environment
- `.binder/postBuild` - Simplified post-build verification
- `analysis_notebook.Rmd` - Interactive tutorial notebook

### Rocker Integration Benefits
✅ **Consistent Environment**: Same base as our main Docker setup
✅ **Pre-installed Packages**: rocker/binder includes many R packages
✅ **Reliable lme4**: No compilation issues (same fix as Docker)
✅ **Better Performance**: Faster builds, fewer installation failures
✅ **System Libraries**: All necessary dependencies included

## Binder Environment Features

### Pre-installed Components
✅ R 4.4+ with RStudio interface (via rocker/binder)
✅ Essential R packages pre-installed in base image
✅ Complete repository with data and scripts
✅ Output directories pre-created
✅ Repository validation on startup
✅ Same system libraries as main Docker environment

### Package Management
**Rocker/Binder Approach**: 
- Most packages come pre-installed with rocker/binder base image
- Only missing packages are installed during build
- Same reliable lme4 installation as our Docker setup
- Faster build times due to fewer compilation requirements

**Essential Analysis Packages**:
- tidyverse, readxl, rstatix, lme4, boot, broom
- car, emmeans, effectsize, reshape2, viridis
- patchwork, scales, gridExtra, knitr, lmerTest

### User Experience
1. **Click Binder badge** in README
2. **Wait 1-2 minutes** for environment build (faster with rocker)
3. **RStudio opens** automatically in browser
4. **All scripts ready** to run immediately
5. **Reliable lme4** - no compilation failures

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
