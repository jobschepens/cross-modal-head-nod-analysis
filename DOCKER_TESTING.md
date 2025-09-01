# Cross-Modal Head Nod Analysis - Docker Testing

This directory contains Docker configuration for reproducible testing and development.

## Quick Start

### Prerequisites
- Docker installed on your system
- Docker Compose (usually included with Docker Desktop)

### Run Tests
```bash
# Build and run comprehensive tests
docker-compose up head-nod-analysis

# Or build and run manually
docker build -t cross-modal-analysis .
docker run --rm -v ${PWD}/figures:/analysis/figures -v ${PWD}/results:/analysis/results cross-modal-analysis
```

### Interactive Development
```bash
# Start interactive R session
docker-compose up dev-environment

# Run specific scripts
docker-compose run --rm dev-environment R -e "source('scripts/01_data_preparation.R')"
```

### Run Individual Analysis Scripts
```bash
# Data preparation
docker-compose run --rm dev-environment R -e "source('scripts/01_data_preparation.R')"

# Comprehensive analysis
docker-compose run --rm dev-environment R -e "source('scripts/02_comprehensive_analysis.R')"

# Generate publication figures
docker-compose run --rm dev-environment R -e "source('scripts/05_publication_figures.R')"
```

## What Gets Tested

1. **Package Installation**: All required R packages
2. **Data Loading**: CSV files and Excel normalization data
3. **Configuration**: config.R and path management
4. **Data Structure**: Expected columns and data quality
5. **Script Syntax**: All R scripts parse correctly
6. **Script Execution**: Critical functionality actually works
7. **Output Generation**: Figures and results directories

## Volumes

- `./figures:/analysis/figures` - Generated plots and visualizations
- `./results:/analysis/results` - Statistical outputs and summaries

## Environment

- **Base Image**: rocker/tidyverse:4.3.0 (R 4.3.0 with tidyverse pre-installed)
- **Additional Packages**: All analysis dependencies installed
- **Working Directory**: `/analysis`
- **User**: Non-root user with proper permissions

## Troubleshooting

### Build Issues
```bash
# Clean build (no cache)
docker-compose build --no-cache

# Check build logs
docker-compose build --progress=plain
```

### Permission Issues
```bash
# Fix output directory permissions
sudo chown -R $USER:$USER figures results
```

### View Logs
```bash
# See container output
docker-compose logs head-nod-analysis

# Follow logs in real-time
docker-compose logs -f dev-environment
```

This provides a completely isolated, reproducible environment for testing and development.
