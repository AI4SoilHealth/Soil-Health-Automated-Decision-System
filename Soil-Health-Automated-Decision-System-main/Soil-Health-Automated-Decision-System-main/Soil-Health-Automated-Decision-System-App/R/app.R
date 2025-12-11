# ===============================================================
# app.R
# ---------------------------------------------------------------
# Entry point for the Shiny application.
#
# Loads:
#   • ui.R   (defines user interface)
#   • server.R (defines server-side logic)
#
# Then launches the app via shinyApp().
# ===============================================================

source("R/ui.R")
source("R/server.R")

shinyApp(
  ui     = ui,
  server = server
)
