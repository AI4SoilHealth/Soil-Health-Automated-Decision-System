# ===============================================================
# tmp.R
# ---------------------------------------------------------------
# Purpose:
#   Standalone utility script (NOT used by the Shiny app).
#
#   • Reads the list of soil layers from Excel.
#   • Builds /vsicurl paths to Wasabi-hosted GeoTIFFs.
#   • Loads rasters as a SpatRaster stack.
#   • Generates 10 random sample AOIs within the EU region.
#   • Extracts soil data for these AOIs.
#
# This script is useful for:
#   - Testing data access
#   - Verifying COG performance
#   - Debugging raster layer issues
#
# IMPORTANT:
#   - No functionality changed.
#   - All values, paths, and operations remain identical.
# ===============================================================

library(terra)
library(sf)
library(here)
library(dplyr)
library(readxl)
library(rnaturalearth)

# ---------------------------------------------------------------
# 1) Load soil layer list and build /vsicurl URLs
# ---------------------------------------------------------------
lyrs <- readxl::read_xlsx(here("R/dat/in/lyrs.xlsx")) %>%
  mutate(
    lyr_full = paste(
      "/vsicurl/https://s3.eu-central-1.wasabisys.com/eumap",
      folder,
      lyr,
      sep = "/"
    )
  )

# Load soil raster stack
soil <- rast(lyrs$lyr_full)
names(soil) <- lyrs$name


# ---------------------------------------------------------------
# 2) Build random AOIs inside extended European region
# ---------------------------------------------------------------
set.seed(123)

aoi <- rnaturalearth::ne_countries(scale = "small") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(3035) %>%
  st_crop(
    xmin = 2600000, xmax = 7300000,
    ymin = 1400000, ymax = 5500000
  ) %>%
  mutate(
    cat = ifelse(
      name %in% c(
        "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
        "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
        "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
        "United Kingdom"
      ),
      "yes", "no"
    )
  ) %>%
  filter(cat == "yes") %>%
  select(name) %>%
  summarise() %>%
  st_sample(10)


# ---------------------------------------------------------------
# 3) Extract soil data for test AOIs
# ---------------------------------------------------------------
soil <- extract(
  soil,
  vect(aoi)
)

