# ===============================================================
# global.R
# ---------------------------------------------------------------
# Purpose:
#   - Define global package dependencies.
#   - Install missing packages if needed.
#   - Loaded BEFORE ui.R and server.R by Shiny.
# ===============================================================


# ----------------------------------------------------------------
# List of required packages for the application
# ----------------------------------------------------------------
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


# ----------------------------------------------------------------
# Identify packages that still need installation
# ----------------------------------------------------------------
installed <- rownames(installed.packages())
missing_packages <- packages_needed[!packages_needed %in% installed]


# ----------------------------------------------------------------
# Install missing packages (only if required)
# ----------------------------------------------------------------
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

