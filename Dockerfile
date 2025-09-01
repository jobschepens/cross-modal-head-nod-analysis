# Cross-Modal Head Nod Analysis Docker Environment
FROM rocker/tidyverse:4.3.0

# Set working directory
WORKDIR /analysis

# Install additional R packages
RUN R -e "install.packages(c('reshape2', 'rstatix', 'lme4', 'readxl', 'viridis', 'gridExtra', 'car', 'emmeans', 'effectsize', 'boot', 'lmerTest', 'patchwork', 'scales', 'knitr', 'broom'), repos='https://cran.rstudio.com/')"

# Copy repository contents
COPY . /analysis/

# Set proper permissions
RUN chmod +x /analysis/*.R /analysis/scripts/*.R

# Create output directories
RUN mkdir -p /analysis/figures /analysis/results

# Default command: run repository tests
CMD ["R", "--vanilla", "-e", "source('test_repository.R')"]
