# Dockerfile for 2DNMR-Analyst Shiny Application
# Based on R 4.2.2 as specified in renv.lock
FROM rocker/shiny:4.2.2

# Maintainer
LABEL maintainer="Julien Guibert"
LABEL description="2DNMR-Analyst - Sharp Peak Identification for 2D NMR"

# Install system dependencies required for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    # General build tools
    build-essential \
    # For curl, httr, RCurl
    libcurl4-openssl-dev \
    # For openssl
    libssl-dev \
    # For xml2
    libxml2-dev \
    # For Cairo, ragg
    libcairo2-dev \
    libxt-dev \
    # For rgl (3D visualization)
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    # For image processing (magick, png, jpeg)
    libmagick++-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    # For text rendering
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    # For git2r
    libgit2-dev \
    # For sodium
    libsodium-dev \
    # For units, sf
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    # For V8
    libv8-dev \
    # For audio (if needed)
    libasound2-dev \
    # Cleanup
    && rm -rf /var/lib/apt/lists/*

# Install Python 3.10 (to match user's environment)
RUN apt-get update && apt-get install -y --no-install-recommends \
    software-properties-common \
    && add-apt-repository -y ppa:deadsnakes/ppa \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
    python3.10 \
    python3.10-venv \
    python3.10-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up Python 3.10 virtual environment
ENV VIRTUAL_ENV=/opt/venv
RUN python3.10 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

# Install Python packages for TensorFlow/Keras
# Matching user's versions: Python 3.10.11, TensorFlow 2.15.1, NumPy 1.26.4
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir \
    numpy==1.26.4 \
    scipy \
    pandas \
    tensorflow==2.15.1 \
    h5py \
    pillow

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Create app directory
WORKDIR /srv/shiny-server/2dnmr-analyst

# Copy renv infrastructure first (for Docker layer caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R

# Configure R to use binary packages when possible (faster install)
RUN mkdir -p /root/.config/R && \
    echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))' > /root/.Rprofile

# Restore R packages from renv.lock
# This may take 10-20 minutes
RUN R -e "renv::restore(prompt = FALSE)"

# Configure reticulate to use our Python
ENV RETICULATE_PYTHON=/opt/venv/bin/python
RUN R -e "library(reticulate); use_virtualenv('/opt/venv', required = TRUE)"

# Copy the rest of the application
COPY . .

# Set permissions for shiny user
RUN chown -R shiny:shiny /srv/shiny-server/2dnmr-analyst

# Expose Shiny port
EXPOSE 3838

# Configure Shiny Server
RUN echo 'run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    site_dir /srv/shiny-server/2dnmr-analyst;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
