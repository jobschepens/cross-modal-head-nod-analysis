# Binder Dockerfile using Rocker Project
# This provides the same reliable environment as our main Docker setup

FROM rocker/binder:latest

# Ensure we're running as root for system package installation
USER root

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

# Install R packages as binary packages (avoiding compilation issues)
RUN apt-get update && apt-get install -y \
    r-cran-lme4 \
    r-cran-boot \
    r-cran-car \
    r-cran-rstatix \
    r-cran-broom \
    r-cran-viridis \
    r-cran-scales \
    && rm -rf /var/lib/apt/lists/*

# Copy repository contents
COPY --chown=${NB_USER}:${NB_USER} . ${HOME}

# Install any remaining packages using our proven installation script
# (This will skip packages already installed via apt)
RUN R --vanilla -e "source('install.R')"

# Create output directories
RUN mkdir -p ${HOME}/figures ${HOME}/results

# Switch back to the notebook user for Binder
USER ${NB_USER}
WORKDIR ${HOME}
