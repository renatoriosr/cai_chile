# 00_setup.R — Instalación y carga de librerías del proyecto CAI
# Ejecutar una sola vez para instalar; después solo library() al inicio de cada script.

paquetes <- c(
  # Manipulación de datos
  "tidyverse",   # dplyr, tidyr, readr, ggplot2, purrr, stringr, forcats
  "lubridate",   # fechas
  "janitor",     # clean_names(), limpieza rápida
  "readxl",      # leer .xlsx (Casen viene en xlsx frecuentemente)

  # Espacial
  "sf",          # objetos espaciales vectoriales
  "terra",       # objetos raster (WorldPop)
  "geosphere",   # distancias geodésicas en metros
  "chilemapas",  # shapefiles de comunas de Chile (incluye Ñuble)
  "leaflet",     # mapas interactivos

  # Visualización
  "plotly",      # interactividad sobre ggplot
  "scales",      # formateo de ejes
  "viridis",     # paletas amigables

  # Shiny + deploy
  "shiny",
  "shinydashboard",
  "DT",          # tablas searchable
  "rsconnect",   # deploy a shinyapps.io

  # Reporte
  "knitr",
  "rmarkdown"
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) {
  message("Instalando: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes)
}

invisible(lapply(paquetes, library, character.only = TRUE))
message("Setup OK — ", length(paquetes), " paquetes cargados.")
