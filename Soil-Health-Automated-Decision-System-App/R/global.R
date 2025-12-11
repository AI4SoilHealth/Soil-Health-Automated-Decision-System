packages_needed <- c(
  "shiny",
  "leaflet", "leaflet.extras",
  "terra", "sf",
  "rnaturalearth", "rnaturalearthdata",
  "ggplot2", "dplyr",
  "jsonlite", "geojsonsf",
  "here",
  "readxl",
  "tibble", "tidyr"
)

installed <- rownames(installed.packages())
missing_packages <- packages_needed[!packages_needed %in% installed]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

