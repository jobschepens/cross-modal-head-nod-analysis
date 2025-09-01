# Cross-Modal Head Nod Analysis Docker Environment
FROM rocker/tidyverse:4.3.0

# Install system dependencies needed for R packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libgit2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /analysis

# Copy repository contents first (needed for install.R)
COPY . /analysis/

# Install packages using our dependency-aware install script
RUN R --vanilla -e "source('install.R')"

# Set proper permissions
RUN chmod +x /analysis/*.R /analysis/scripts/*.R

# Create output directories
RUN mkdir -p /analysis/figures /analysis/results

# Default command: run repository tests
CMD ["R", "--vanilla", "-e", "source('test_repository.R')"]
