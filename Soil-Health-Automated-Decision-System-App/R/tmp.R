library(terra)
library(sf)
library(here)
library(dplyr)
library(readxl)
library(rnaturalearth)

lyrs <- readxl::read_xlsx(here("R/dat/in/lyrs.xlsx")) %>%
  mutate(
    lyr_full = paste(
      "/vsicurl/https://s3.eu-central-1.wasabisys.com/eumap",
      folder,
      lyr,
      sep = "/"
    )
  )

soil <- rast(lyrs$lyr_full)
names(soil) <- lyrs$name

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

soil <- extract(
  soil,
  vect(aoi)
)

