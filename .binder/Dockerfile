# Binder Dockerfile using Rocker Project
# This provides the same reliable environment as our main Docker setup

FROM rocker/binder:latest

# Install system dependencies for R packages
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

# Copy repository contents
COPY --chown=${NB_USER}:${NB_USER} . ${HOME}

# Install R packages using our proven installation script
RUN R --vanilla -e "source('install.R')"

# Create output directories
RUN mkdir -p ${HOME}/figures ${HOME}/results

# Set up Binder environment
USER ${NB_USER}
WORKDIR ${HOME}
