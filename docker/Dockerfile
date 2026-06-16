# zhncommandR -- Shiny dashboard image
#
# Build:
#   docker build -t cttir/zhncommandr:latest -f docker/Dockerfile .
# Run:
#   docker run --rm -p 3838:3838 cttir/zhncommandr:latest
#
# Behind Caddy + TLS see docker/Caddyfile.

FROM rocker/r-ver:4.4.1

# System libraries needed for readxl, curl, ssl, fonts.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libpng-dev \
      libjpeg-dev \
      libtiff5-dev \
      libcairo2-dev \
      libxt-dev \
      pandoc \
      git \
    && rm -rf /var/lib/apt/lists/*

# Use Posit Public Package Manager for fast binary installs.
ENV RENV_CONFIG_PPM_ENABLED=TRUE \
    RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/latest \
    R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

WORKDIR /srv/zhncommandR
COPY DESCRIPTION /srv/zhncommandR/DESCRIPTION

# Install package dependencies (Imports + Suggests that the app uses).
RUN R -q -e "install.packages(c('pak'), repos = 'https://cloud.r-project.org')" \
    && R -q -e "pak::local_install_deps('.', dependencies = c('Imports','LinkingTo'))" \
    && R -q -e "pak::pak(c('survminer','shiny.i18n','thematic','sass','openxlsx'))"

COPY . /srv/zhncommandR
RUN R CMD INSTALL --no-html --no-help /srv/zhncommandR \
    && rm -rf /tmp/Rtmp*

EXPOSE 3838
CMD ["R", "-e", "zhncommandR::zhn_run_app(host = '0.0.0.0', port = 3838)"]
