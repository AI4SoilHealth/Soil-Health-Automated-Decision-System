
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

# Load helper functions
source(here("R/functions.R"))


server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  # Thresholds for threat classification
  # ---------------------------------------------------------------
  THRESH_LOW  <- 0.2
  THRESH_HIGH <- 0.8
  
  
  # ---------------------------------------------------------------
  # Land-use class mapping
  #
  # These LU.tif numeric codes are grouped into Cropland / Grassland / Forestry.
  # This mapping MUST remain unchanged because:
  #   - Server logic depends on these labels.
  #   - UI displays dominant land use using this lookup.
  #   - Polar plot uses this fixed ordering.
  # ---------------------------------------------------------------
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
  
  # Fixed factor order used everywhere (plots, tables, display).
  lu_levels <- c("Cropland", "Grassland", "Forestry")
  
  
  # ---------------------------------------------------------------
  # Management advice table
  #
  # Excel sheet contains per-country, per-land-use, per-threat URLs.
  # We normalize text so matching is case-insensitive and whitespace-robust.
  #
  # If the file is missing, app continues gracefully (using Google fallback).
  # ---------------------------------------------------------------
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
  
  # Helper to normalize text for safe matching
  norm_txt <- function(x) {
    tolower(trimws(gsub("\\s+", " ", x)))
  }
  
  # Add normalized matching keys when table exists
  if (!is.null(advice_df)) {
    advice_df <- advice_df %>%
      mutate(
        region_key = norm_txt(region),
        use_key    = norm_txt(use),
        threat_key = norm_txt(threat)
      )
  }
  
  
  # ---------------------------------------------------------------
  # Reactive values storage
  #
  # These hold computation results after a user draws an AOI:
  #   - metrics: 5 soil health threat metrics (0–1 scaled)
  #   - country: intersecting country name(s)
  #   - monitor: threats ≥ threshold
  #   - lu_table: land-use percentages for Cropland/Grassland/Forestry
  #   - dominant_use: main land use within AOI
  #
  # These values are updated in the AOI event observer.
  # ---------------------------------------------------------------
  vals <- reactiveValues(
    metrics      = NULL,
    country      = NULL,
    monitor      = NULL,
    lu_table     = NULL,
    dominant_use = NULL
  )
  
  
  # ---------------------------------------------------------------
  # Load country boundaries (vector)
  #
  # - Using rnaturalearth's medium scale.
  # - Reprojected to EPSG:3035 (same CRS as soil rasters).
  # - st_make_valid ensures geometries are clean for intersections.
  # ---------------------------------------------------------------
  countries <- rnaturalearth::ne_countries(
    scale = "medium", returnclass = "sf"
  ) %>%
    st_make_valid() %>%
    st_transform(3035)
  
  
  # ---------------------------------------------------------------
  # Load Land-use raster (LU.tif)
  #
  # Two possible hard-coded paths exist; we try both.
  # If none found, LU computation is skipped gracefully.
  # ---------------------------------------------------------------
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
  
  # ===============================================================
  # server.R — SECTION B: Map Initialization
  # ---------------------------------------------------------------
  # Purpose:
  #   - Create the base Leaflet map rendered in the UI.
  #   - Add imagery & topo basemaps.
  #   - Add drawing tools (polygon + marker) for user-defined AOI.
  #   - Set initial map extent.
  # ===============================================================
  
  
  # ---------------------------------------------------------------
  # Render Leaflet map
  # ---------------------------------------------------------------
  output$map <- renderLeaflet({
    
    leaflet() %>%
      
      # Base layers -------------------------------------------------
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
      
      # Draw toolbar ------------------------------------------------
    addDrawToolbar(
      targetGroup = "draw",
      editOptions = editToolbarOptions(
        selectedPathOptions = selectedPathOptions()
      ),
      
      # Enabled / disabled drawing modes:
      polylineOptions      = FALSE,
      circleOptions        = FALSE,
      rectangleOptions     = FALSE,
      circleMarkerOptions  = FALSE,
      
      # Polygon (AOI) drawing style:
      polygonOptions = drawPolygonOptions(
        shapeOptions = drawShapeOptions(
          color       = "#D9A04B",
          fillColor   = "#D9A04B",
          fillOpacity = 0.3
        )
      ),
      
      # Allow marker placement (unused but retained for feature parity)
      markerOptions = drawMarkerOptions(),
      
      # Enforce single AOI
      singleFeature = TRUE
    ) %>%
      
      # Initial map view -------------------------------------------
    setView(
      lng  = 10.445,
      lat  = 52.285,
      zoom = 12
    )
  })
  
  # ===============================================================
  # server.R — SECTION C: AOI Handling & Calculations
  # ---------------------------------------------------------------
  # This section runs every time the user draws a new feature on the
  # map. It performs:
  #   1) AOI geometry extraction & reprojection
  #   2) Country identification
  #   3) Soil raster extraction + metric computation
  #   4) Threat scoring (0–1)
  #   5) Land-use zonal statistics & dominant land use
  # ===============================================================
  
  
  observeEvent(input$map_draw_new_feature, {
    
    feature <- input$map_draw_new_feature
    req(feature$geometry)       # Require valid GeoJSON geometry
    
    
    # ---------------------------------------------------------------
    # 1) Display AOI on map (clear previous polygon)
    # ---------------------------------------------------------------
    leafletProxy("map") %>%
      clearGroup("draw") %>%
      addGeoJSON(feature, group = "draw")
    
    
    # ---------------------------------------------------------------
    # 2) Convert AOI from GeoJSON → sf → EPSG:3035
    # ---------------------------------------------------------------
    aoi <- list(
      type       = "Feature",
      properties = list(),
      geometry   = feature$geometry
    ) %>%
      toJSON(auto_unbox = TRUE) %>%
      geojson_sf() %>%
      st_make_valid() %>%
      st_transform(3035)
    
    
    # ---------------------------------------------------------------
    # 3) Determine country name(s) intersecting AOI
    #
    # Rules:
    #   - If AOI intersects multiple countries → show comma-separated list
    #   - If none intersect → use nearest country to AOI centroid
    # ---------------------------------------------------------------
    hit <- st_intersects(aoi, countries)
    idx <- unique(unlist(hit))
    
    vals$country <- if (length(idx) > 0) {
      paste(sort(unique(countries$name[idx])), collapse = ", ")
    } else {
      countries$name[
        st_nearest_feature(st_centroid(st_geometry(aoi)), countries)
      ]
    }
    
    
    # ===============================================================
    # 4) SOIL METRIC EXTRACTION
    # ===============================================================
    
    # ---------------------------------------------------------------
    # 4.1 Load soil raster list from Excel
    # ---------------------------------------------------------------
    local_data_dir <- "*********/eumap_resample"
    
    lyrs <- readxl::read_xlsx(here("R/dat/in/lyrs.xlsx")) %>%
      mutate(lyr_full = file.path(local_data_dir, lyr))
    
    existing_files <- lyrs$lyr_full[file.exists(lyrs$lyr_full)]
    
    if (length(existing_files) < nrow(lyrs)) {
      warning("Some raster files were not found in the local directory.")
    }
    
    soil_lyrs <- rast(existing_files)
    names(soil_lyrs) <- lyrs$name[file.exists(lyrs$lyr_full)]
    
    
    # ---------------------------------------------------------------
    # 4.2 Extract mean soil values over AOI
    # ---------------------------------------------------------------
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
    
    
    # ---------------------------------------------------------------
    # 4.3 Compute soil health metrics (all original formulas preserved)
    # ---------------------------------------------------------------
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
        # Random left/right arrow; unchanged
        trend = ifelse(value > .1, sample(c(NA, "\u25C4", "\u25BA")), NA)
      )
    
    
    # ---------------------------------------------------------------
    # 4.4 Determine threats ≥ threshold for monitoring output
    # ---------------------------------------------------------------
    vals$monitor <- vals$metrics %>%
      filter(value >= THRESH_LOW) %>%
      arrange(desc(value)) %>%
      mutate(nice_name_clean = gsub("\n", " ", nice_name)) %>%
      pull(nice_name_clean)
    
    
    # ===============================================================
    # 5) LAND-USE ZONAL STATISTICS
    # ===============================================================
    
    if (!is.null(landuse_rast)) {
      
      # -------------------------------------------------------------
      # 5.1 Reproject AOI if required
      # -------------------------------------------------------------
      aoi_for_lu <- tryCatch({
        if (!identical(terra::crs(landuse_rast), terra::crs(vect(aoi)))) {
          st_transform(aoi, terra::crs(landuse_rast))
        } else {
          aoi
        }
      }, error = function(e) aoi)
      
      # -------------------------------------------------------------
      # 5.2 Crop + mask LU raster to AOI
      # -------------------------------------------------------------
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
            
            # Convert matrix → vector just like original code
            if (is.matrix(v)) v <- v[, 1, drop = TRUE]
            v <- v[!is.na(v)]
            
            if (length(v) > 0) {
              
              # -----------------------------------------------------
              # 5.3 Frequency table of LU codes
              # -----------------------------------------------------
              tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
              names(tab) <- c("value", "count")
              tab$value <- suppressWarnings(as.integer(as.character(tab$value)))
              total <- sum(tab$count)
              
              # -----------------------------------------------------
              # 5.4 Map LU codes → Cropland / Grassland / Forestry
              # -----------------------------------------------------
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
              
              # Dominant land use
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
      # Land-use raster missing → skip gracefully
      vals$lu_table <- NULL
      vals$dominant_use <- NULL
    }
    
  })
  
  # ===============================================================
  # server.R — SECTION D: Text Outputs
  # ---------------------------------------------------------------
  # This section renders:
  #   • Country name
  #   • Dominant land use
  #   • Detected threats with clickable advice links
  # ===============================================================
  
  
  # ---------------------------------------------------------------
  # 1) Country name
  # ---------------------------------------------------------------
  output$countryName <- renderText({
    if (is.null(vals$country) || vals$country == "") {
      "—"
    } else {
      vals$country
    }
  })
  
  
  # ---------------------------------------------------------------
  # 2) Dominant land use
  # ---------------------------------------------------------------
  output$dominantLandUse <- renderText({
    if (is.null(vals$dominant_use)) "—" else vals$dominant_use
  })
  
  
  # ---------------------------------------------------------------
  # 3) Threats: clickable advice links
  #
  # NOTES:
  #   • Formatting (bold threat names) is preserved exactly.
  #   • Behavior:
  #       - If Excel advice exists → use recommended link
  #       - Else → Google fallback search link
  #   • The UI displays one entry per threat above the threshold.
  # ---------------------------------------------------------------
  output$monitorText <- renderUI({
    
    # No metric results yet
    if (is.null(vals$metrics)) {
      return("—")
    }
    
    threats <- vals$monitor
    
    # If all metrics below threshold
    if (is.null(threats) || length(threats) == 0) {
      return("None (all < 0.2)")
    }
    
    # Extract context for generating recommendations
    country  <- ifelse(is.null(vals$country), "", vals$country)
    landuse  <- ifelse(is.null(vals$dominant_use), "", vals$dominant_use)
    
    # Normalize country + landuse (matching logic unchanged)
    country_key <- norm_txt(strsplit(country, ",")[[1]][1])  # first country only
    use_key     <- norm_txt(landuse)
    
    
    # -----------------------------------------------------------
    # Build the output list: one <div> per threat
    # -----------------------------------------------------------
    ui_links <- lapply(threats, function(thr) {
      
      thr_key <- norm_txt(thr)
      
      # ---------------------------------------------------------
      # 3.1 Attempt to find curated advice in Excel
      # ---------------------------------------------------------
      hit <- NULL
      if (!is.null(advice_df)) {
        hit <- advice_df %>%
          dplyr::filter(
            region_key == country_key,
            use_key    == use_key,
            threat_key == thr_key
          )
      }
      
      # If curated recommendation exists → use its URL
      if (!is.null(hit) && nrow(hit) > 0) {
        rec_url <- hit$url[1]
        
        return(
          tags$div(
            style = "margin-bottom:6px;",
            tags$a(
              href   = rec_url,
              target = "_blank",
              tags$b(thr),    # BOLD THREAT NAME (unchanged)
              ": recommended action"
            )
          )
        )
      }
      
      # ---------------------------------------------------------
      # 3.2 Fallback to Google search
      # ---------------------------------------------------------
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
          tags$b(thr),   # BOLD THREAT NAME
          ": recommended action"
        )
      )
    })
    
    
    # Return list of link divs to UI
    tagList(ui_links)
  })
  
  # ===============================================================
  # server.R — SECTION E: Plot Outputs
  # ---------------------------------------------------------------
  # Contains:
  #   • Threats polar plot (active)
  #   • Land-use polar plot (commented out, unchanged)
  # ===============================================================
  
  
  # ---------------------------------------------------------------
  # Threats Polar Plot
  #
  # Notes:
  #   • Polar bar plot showing the five scaled threat scores.
  #   • Two zero-value spacer entries included exactly as original.
  #   • Threshold lines (low = 0.2, high = 0.8) unchanged.
  #   • Angles & label positioning identical to your final version.
  # ---------------------------------------------------------------
  output$polarPlot <- renderPlot(width = 900, height = 900, {
    req(vals$metrics)
    
    # -------------------------------------------
    # Prepare data for polar plotting
    # -------------------------------------------
    data <- vals$metrics %>%
      tibble::add_row(value = c(0, 0), .before = 1) %>%  # spacer bars
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
    
    # Threshold guide lines
    grid_manual <- data.frame(
      x    = c(1.5, 1.5),
      xend = c(2.4, 2.4),
      y    = c(THRESH_LOW, THRESH_HIGH)
    )
    
    
    # -------------------------------------------
    # Build the plot (all settings unchanged)
    # -------------------------------------------
    ggplot(data, aes(x = id, y = value)) +
      geom_bar(
        stat  = "identity",
        width = 1,
        fill  = "#667651",
        color = "white"
      ) +
      ylim(-plot_max_value / 3, plot_max_value * 2) +
      coord_polar(start = 0, clip = "off") +
      
      # Text labels
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
      
      # Theme
      theme_minimal() +
      theme(
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      ) +
      
      # Threshold lines
      geom_segment(
        data = grid_manual,
        aes(x = x, xend = xend, y = y, yend = y),
        col = "grey50"
      ) +
      
      # Threshold labels
      geom_text(
        data = grid_manual,
        aes(x = 1, y = y, label = c("low", "high")),
        size = 7,
        col  = "grey50",
        hjust = 0
      ) +
      
      # Title annotation (unchanged)
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
  
  