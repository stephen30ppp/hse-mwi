# R/modules/map_module.R

library(shiny)
library(leaflet)

# Define the map moudle UI
mapModuleUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = 850)
}

# define the service for map moudle
mapModule <- function(input, output, session, data, fill_var, additional_params = list()) {
  output$map <- renderLeaflet({
    # Draw a map using the incoming data and fill variables
    plot_map(fill_var, data, additional_params)
  })

  # other map related reaction logic can added here
}
