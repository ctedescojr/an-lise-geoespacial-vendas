# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", "rgdal", "raster", "rgeos",
             "tidyverse", "gridExtra", "dplyr",
             "ggrepel", "png", "tmap", "maptools",
             "knitr", "kableExtra", "sf", "sp",
             "sjPlot", "grid", "broom","profvis",
             "FactoMineR", "magick", "RColorBrewer",
             "amap", "rgl", "devtools",
             "ade4", "GISTools", "adehabitatHR",
             "readxl", "writexl", "rayshader")

library("ggplot2")
library(htmlwidgets)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# O pacote rayshader que está no CRAN, no momento, possui alguns bugs. A versão
# que está no GitHub do autor do pacote já é mais funcional. Para instalá-la:
devtools::install_github("tylermorganwall/rayshader")

# Para carregar o rayshader
library(rayshader)
