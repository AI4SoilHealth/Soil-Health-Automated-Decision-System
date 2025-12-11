library(shiny)
library(leaflet)
library(leaflet.extras)
library(terra)
library(sf)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      
      /* ---------------------------------------------- */
      /* Plot stacking container (right panel)          */
      /* ---------------------------------------------- */
      #plot-stack {
        margin-top: 120px;
        padding-right: 8px;
      }

      /* Force the plot height inside container */
      #polar-plot {
        height: 700px !important;
        width: 100%;
      }

      /* ---------------------------------------------- */
      /* Floating information panel                     */
      /* ---------------------------------------------- */
      #info-panel {
        position: absolute;
        top: 15px;
        left: 15px;
        z-index: 1000;

        background: rgba(255,255,255,0.92);
        padding: 14px 16px;
        border-radius: 12px;

        box-shadow: 0 3px 10px rgba(0,0,0,0.15);
        backdrop-filter: blur(3px);

        max-width: 380px;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }

      /* Grid layout for label/value pairs */
      .info-line {
        display: grid;
        grid-template-columns: auto 1fr;
        gap: 6px;
        margin-bottom: 6px;
        font-size: 15px;
        color: #333333;
      }

      .info-label  { 
        font-weight: 600; 
        color: #333333; 
        white-space: nowrap;
      }
      
      .info-value  { 
        font-weight: 400; 
        color: #333333; 
        word-wrap: break-word;
        min-width: 0;
      }

      /* ---------------------------------------------- */
      /* Logos (bottom-right of right panel)            */
      /* ---------------------------------------------- */
      #logo-container {
        position: absolute;
        bottom: -150px;
        right: 20px;
        display: flex;
        align-items: center;
        gap: 15px;
      }

      .logo-img {
        height: 60px;
      }

    "))
  ),
  fluidRow(

    column(
      width = 7,
      leafletOutput("map", width = "100%", height = "100vh")
    ),
    column(
      width = 5, id = "plot-column",
      tags$div(
        id = "info-panel",
        
        tags$div(
          "Soil health dashboard",
          style = "
            font-size: 20px;
            font-weight: 700;
            margin-bottom: 12px;
            color: #2c3e50;
            font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"
        ),
        div(
          class = "info-line",
          span(class = "info-label", "Country:"),
          span(class = "info-value", textOutput("countryName", inline = TRUE))
        ),
        div(
          class = "info-line",
          span(class = "info-label", "Dominant land use:"),
          span(class = "info-value", textOutput("dominantLandUse", inline = TRUE))
        ),
        div(
          class = "info-line",
          span(class = "info-label", "Detected soil threats:"),
          span(class = "info-value", uiOutput("monitorText"))
        )
      ),
      div(
        id = "plot-stack",
        div(
          id = "polar-plot",
          plotOutput("polarPlot", width = "100%", height = "700px")
        )
      ),
      tags$div(
        id = "logo-container",
        tags$img(src = "ai4soilhealth_4f.png", class = "logo-img"),
        tags$img(src = "european-commission.PNG", class = "logo-img")
      )
    )
  )
)


