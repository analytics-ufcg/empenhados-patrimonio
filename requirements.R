#! /usr/local/bin/Rscript

# Alguns pacotes precisam da instalação de dependências para o SO utilizado.
# Algumas dependências para o ubuntu:
# * sudo apt-get install -y libgdal-dev libproj-dev

pkgs <- c("rgdal", "gdalUtils", "dplyr", "readr", "htmlwidgets", "plotly", "DT", "stringr", "here", "leaflet", "ggrepel")
install.packages(pkgs, dependencies = TRUE, repos = "http://cran.rstudio.com/")