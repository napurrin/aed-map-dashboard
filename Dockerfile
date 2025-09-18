# Dockerfile (최종 버전)
FROM rocker/shiny-verse:latest
RUN apt-get update && apt-get install -y \
    libgdal-dev libxml2-dev libproj-dev libgeos-dev libudunits2-dev cmake curl \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /srv/shiny-server/aed_app
COPY install_packages.R .
RUN Rscript install_packages.R
COPY . .
EXPOSE 3838
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s \
  CMD curl --fail http://localhost:3838 || exit 1
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
