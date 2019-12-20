library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

source("database/MongoDB.R")

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet(dfAll) %>%
      setView(lng=-20, lat=45, zoom=4) %>%
      addTiles() %>%
      addCircles(data = dfAll, lat = ~lat, lng = ~lng, weight = 1)
  })
}