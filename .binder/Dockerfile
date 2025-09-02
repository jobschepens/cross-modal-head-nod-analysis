# Binder Dockerfile using Rocker Project
# This provides the same reliable environment as our main Docker setup

FROM rocker/binder:latest

# Install system dependencies for R packages (including lme4 dependencies)
RUN apt-get update && apt-get install -y \
    cmake \
    libgsl-dev \
    libnlopt-dev \
    libglpk-dev \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libgit2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy repository contents
COPY --chown=${NB_USER}:${NB_USER} . ${HOME}

# Use install2.r for critical packages (more reliable than install.packages)
# This leverages littler for better dependency resolution
RUN install2.r --deps TRUE --error \
    lme4 \
    boot \
    ggplot2 \
    dplyr \
    tidyr \
    readr \
    readxl \
    knitr \
    rmarkdown

# Install any remaining packages using our proven installation script
RUN R --vanilla -e "source('install.R')"

# Create output directories
RUN mkdir -p ${HOME}/figures ${HOME}/results

# Set up Binder environment
USER ${NB_USER}
WORKDIR ${HOME}
