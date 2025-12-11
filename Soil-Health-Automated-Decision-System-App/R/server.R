library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(terra)
library(sf)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(tibble)
library(tidyr)
source(here("R/functions.R"))


server <- function(input, output, session) {

  THRESH_LOW  <- 0.2
  THRESH_HIGH <- 0.8
  lu_class_map <- data.frame(
    value = c(
      12, 13, 19, 20,           # Cropland
      14, 15, 16, 17,           # Cropland (special crops treated as Cropland)
      18, 21, 26,               # Grassland
      22, 23, 24, 25, 27, 28, 29  # Forestry
    ),
    group = c(
      rep("Cropland", 8),
      rep("Grassland", 3),
      rep("Forestry", 7)
    ),
    stringsAsFactors = FALSE
  )
  
  lu_levels <- c("Cropland", "Grassland", "Forestry")
  advice_path <- "*********/eumap_resample/management_advice.xlsx"
  
  advice_df <- if (file.exists(advice_path)) {
    readxl::read_xlsx(advice_path) %>%
      mutate(
        region = as.character(region),
        use    = as.character(use),
        threat = as.character(threat),
        url    = as.character(url)
      )
  } else {
    warning("management_advice.xlsx not found at: ", advice_path)
    NULL
  }
  
  norm_txt <- function(x) {
    tolower(trimws(gsub("\\s+", " ", x)))
  }
  
  if (!is.null(advice_df)) {
    advice_df <- advice_df %>%
      mutate(
        region_key = norm_txt(region),
        use_key    = norm_txt(use),
        threat_key = norm_txt(threat)
      )
  }

  vals <- reactiveValues(
    metrics      = NULL,
    country      = NULL,
    monitor      = NULL,
    lu_table     = NULL,
    dominant_use = NULL
  )

  countries <- rnaturalearth::ne_countries(
    scale = "medium", returnclass = "sf"
  ) %>%
    st_make_valid() %>%
    st_transform(3035)

  lu_candidates <- c(
    "*********/eumap_resample/LU.tif",
    "*********/eumap_resample/LU.tif"
  )
  
  landuse_path <- lu_candidates[file.exists(lu_candidates)][1]
  
  if (is.na(landuse_path)) {
    warning("Land-use raster not found at any candidate path.")
    landuse_rast <- NULL
  } else {
    landuse_rast <- tryCatch(
      terra::rast(landuse_path),
      error = function(e) {
        warning("Could not read land-use raster at ", landuse_path,
                ": ", conditionMessage(e))
        NULL
      }
    )
  }

  output$map <- renderLeaflet({
    
    leaflet() %>%
    addProviderTiles(
      providers$Esri.WorldImagery,
      group   = "Esri.WorldImagery",
      layerId = "baseid"
    ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group   = "Esri.WorldTopoMap",
        layerId = "cartoid"
      ) %>%
      addLayersControl(
        baseGroups = c("Esri.WorldImagery", "Esri.WorldTopoMap"),
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
    addDrawToolbar(
      targetGroup = "draw",
      editOptions = editToolbarOptions(
        selectedPathOptions = selectedPathOptions()
      ),
      polylineOptions      = FALSE,
      circleOptions        = FALSE,
      rectangleOptions     = FALSE,
      circleMarkerOptions  = FALSE,
      polygonOptions = drawPolygonOptions(
        shapeOptions = drawShapeOptions(
          color       = "#D9A04B",
          fillColor   = "#D9A04B",
          fillOpacity = 0.3
        )
      ),
      markerOptions = drawMarkerOptions(),
      singleFeature = TRUE
    ) %>%
    setView(
      lng  = 10.445,
      lat  = 52.285,
      zoom = 12
    )
  })

  observeEvent(input$map_draw_new_feature, {
    
    feature <- input$map_draw_new_feature
    req(feature$geometry)

    leafletProxy("map") %>%
      clearGroup("draw") %>%
      addGeoJSON(feature, group = "draw")

    aoi <- list(
      type       = "Feature",
      properties = list(),
      geometry   = feature$geometry
    ) %>%
      toJSON(auto_unbox = TRUE) %>%
      geojson_sf() %>%
      st_make_valid() %>%
      st_transform(3035)

    hit <- st_intersects(aoi, countries)
    idx <- unique(unlist(hit))
    
    vals$country <- if (length(idx) > 0) {
      paste(sort(unique(countries$name[idx])), collapse = ", ")
    } else {
      countries$name[
        st_nearest_feature(st_centroid(st_geometry(aoi)), countries)
      ]
    }

    local_data_dir <- "*********/eumap_resample"
    
    lyrs <- readxl::read_xlsx(here("R/dat/in/lyrs.xlsx")) %>%
      mutate(lyr_full = file.path(local_data_dir, lyr))
    
    existing_files <- lyrs$lyr_full[file.exists(lyrs$lyr_full)]
    
    if (length(existing_files) < nrow(lyrs)) {
      warning("Some raster files were not found in the local directory.")
    }
    
    soil_lyrs <- rast(existing_files)
    names(soil_lyrs) <- lyrs$name[file.exists(lyrs$lyr_full)]

    withProgress(
      message = "Calculation in progress",
      detail  = "This may take a while...",
      value   = NULL,
      {
        soil <- terra::extract(
          soil_lyrs,
          vect(aoi),
          fun = function(z) mean(z, na.rm = TRUE)
        ) %>%
          mutate(
            oc = oc / 10,
            bd = bd / 100,
            ec = ec / 10,
            ph = ph / 10
          )
      }
    )

    vals$metrics <- soil %>%
      mutate(
        oc_to_clay = oc / (clay * 10),
        soc_loss = rescale(
          oc_to_clay,
          lower_bound  = 1 / 13,
          upper_bound  = 1 / 8,
          lower_target = 0.1,
          upper_target = 1,
          invert       = TRUE
        ),
        pd = bd + clay * 0.005,
        compact = rescale(
          pd,
          lower_bound  = 1.60,
          upper_bound  = 1.82,
          lower_target = 0.1,
          upper_target = 1,
          invert       = FALSE
        ),
        salty = rescale(
          ec,
          lower_bound  = 10,
          upper_bound  = 100,
          lower_target = 0.1,
          upper_target = 1,
          invert       = FALSE
        ),
        excess = rescale(
          p_ex,
          lower_bound  = 30,
          upper_bound  = 50,
          lower_target = 0.1,
          upper_target = 1,
          invert       = FALSE
        ),
        acid = rescale(
          ph,
          lower_bound  = 5,
          upper_bound  = 6.5,
          lower_target = 0.1,
          upper_target = 1,
          invert       = TRUE
        )
      ) %>%
      select(soc_loss, compact, salty, excess, acid) %>%
      pivot_longer(
        everything(),
        names_to  = "metric",
        values_to = "value"
      ) %>%
      mutate(
        nice_name = case_when(
          metric == "soc_loss" ~ "SOC\nloss",
          metric == "compact"  ~ "Subsoil\ncompaction",
          metric == "salty"    ~ "Salinization",
          metric == "excess"   ~ "Excess\nnutrients",
          metric == "acid"     ~ "Acidification"
        ),
        trend = ifelse(value > .1, sample(c(NA, "\u25C4", "\u25BA")), NA)
      )

    vals$monitor <- vals$metrics %>%
      filter(value >= THRESH_LOW) %>%
      arrange(desc(value)) %>%
      mutate(nice_name_clean = gsub("\n", " ", nice_name)) %>%
      pull(nice_name_clean)

    if (!is.null(landuse_rast)) {
      aoi_for_lu <- tryCatch({
        if (!identical(terra::crs(landuse_rast), terra::crs(vect(aoi)))) {
          st_transform(aoi, terra::crs(landuse_rast))
        } else {
          aoi
        }
      }, error = function(e) aoi)
      lu_crop <- tryCatch(
        terra::crop(landuse_rast, vect(aoi_for_lu), snap = "out"),
        error = function(e) NULL
      )
      if (!is.null(lu_crop)) {
        lu_mask <- tryCatch(
          terra::mask(lu_crop, vect(aoi_for_lu)),
          error = function(e) NULL
        )
        if (!is.null(lu_mask)) {
          v <- tryCatch(terra::values(lu_mask, na.rm = TRUE), error = function(e) NULL)
          if (!is.null(v)) {
            if (is.matrix(v)) v <- v[, 1, drop = TRUE]
            v <- v[!is.na(v)]
            if (length(v) > 0) {
              tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
              names(tab) <- c("value", "count")
              tab$value <- suppressWarnings(as.integer(as.character(tab$value)))
              total <- sum(tab$count)
              vals$lu_table <- tab %>%
                mutate(percent = 100 * count / total) %>%
                left_join(lu_class_map, by = "value") %>%
                filter(!is.na(group)) %>%
                group_by(group) %>%
                summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") %>%
                rename(label = group) %>%
                complete(label = lu_levels, fill = list(percent = 0)) %>%
                mutate(label = factor(label, levels = lu_levels)) %>%
                arrange(label)
              vals$dominant_use <- as.character(
                vals$lu_table$label[which.max(vals$lu_table$percent)]
              )
            } else {
              vals$lu_table <- NULL
              vals$dominant_use <- NULL
            }
          } else {
            vals$lu_table <- NULL
            vals$dominant_use <- NULL
          }
        } else {
          vals$lu_table <- NULL
          vals$dominant_use <- NULL
        }
      } else {
        vals$lu_table <- NULL
        vals$dominant_use <- NULL
      }
    } else {
      vals$lu_table <- NULL
      vals$dominant_use <- NULL
    }
  })

  output$countryName <- renderText({
    if (is.null(vals$country) || vals$country == "") {
      "—"
    } else {
      vals$country
    }
  })

  output$dominantLandUse <- renderText({
    if (is.null(vals$dominant_use)) "—" else vals$dominant_use
  })

  output$monitorText <- renderUI({
    if (is.null(vals$metrics)) {
      return("—")
    }
    threats <- vals$monitor
    if (is.null(threats) || length(threats) == 0) {
      return("None (all < 0.2)")
    }
    country  <- ifelse(is.null(vals$country), "", vals$country)
    landuse  <- ifelse(is.null(vals$dominant_use), "", vals$dominant_use)
    country_key <- norm_txt(strsplit(country, ",")[[1]][1])
    use_key     <- norm_txt(landuse)
    ui_links <- lapply(threats, function(thr) {
      thr_key <- norm_txt(thr)
      hit <- NULL
      if (!is.null(advice_df)) {
        hit <- advice_df %>%
          dplyr::filter(
            region_key == country_key,
            use_key    == use_key,
            threat_key == thr_key
          )
      }
      if (!is.null(hit) && nrow(hit) > 0) {
        rec_url <- hit$url[1]
        return(
          tags$div(
            style = "margin-bottom:6px;",
            tags$a(
              href   = rec_url,
              target = "_blank",
              tags$b(thr),
              ": recommended action"
            )
          )
        )
      }
      q <- paste(
        "practical advice on how to remedy",
        thr,
        "in", landuse, "soil in", country
      )
      url <- paste0(
        "https://www.google.com/search?q=",
        utils::URLencode(q, reserved = TRUE),
        "&udm=50"
      )
      tags$div(
        style = "margin-bottom:6px;",
        tags$a(
          href   = url,
          target = "_blank",
          tags$b(thr),
          ": recommended action"
        )
      )
    })
    tagList(ui_links)
  })

  output$polarPlot <- renderPlot(width = 900, height = 900, {
    req(vals$metrics)
    data <- vals$metrics %>%
      tibble::add_row(value = c(0, 0), .before = 1) %>%  
      mutate(
        id          = dplyr::row_number(),
        angle_trend = 90 - 360 * (id - 0.5) / max(id),
        
        hjust = dplyr::case_when(
          angle_trend <= -90 ~ 1,
          TRUE               ~ 0
        ),
        angle = dplyr::case_when(
          angle_trend <= -90 ~ angle_trend + 180,
          TRUE               ~ angle_trend
        )
      )
    plot_max_value <- 1.0
    grid_manual <- data.frame(
      x    = c(1.5, 1.5),
      xend = c(2.4, 2.4),
      y    = c(THRESH_LOW, THRESH_HIGH)
    )
    ggplot(data, aes(x = id, y = value)) +
      geom_bar(
        stat  = "identity",
        width = 1,
        fill  = "#667651",
        color = "white"
      ) +
      ylim(-plot_max_value / 3, plot_max_value * 2) +
      coord_polar(start = 0, clip = "off") +
      geom_text(
        aes(
          x     = id,
          y     = value + 0.1,
          label = nice_name,
          angle = angle,
          hjust = hjust
        ),
        size = 7
      ) +
      theme_minimal() +
      theme(
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      ) +
      geom_segment(
        data = grid_manual,
        aes(x = x, xend = xend, y = y, yend = y),
        col = "grey50"
      ) +
      geom_text(
        data = grid_manual,
        aes(x = 1, y = y, label = c("low", "high")),
        size = 7,
        col  = "grey50",
        hjust = 0
      ) +
      annotate(
        geom  = 'text',
        x     = 1.9,
        y     = plot_max_value * 1.4,
        label = "Threats\nto\n                soil health",
        size  = 8,
        col   = "grey50",
        hjust = 0.5
      )
  })
} 
  
  