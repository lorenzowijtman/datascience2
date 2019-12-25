source("common/libs.R")
source("database/MongoDB.R")

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet(dfAll) %>%
      setView(lng=-0, lat=50, zoom=5) %>%
      addTiles() %>%
      addCircles(data = dfAll, lat = ~lat, lng = ~lng, weight = 1)
  })
}