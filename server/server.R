source("database/MongoDB.R")

server <- function(input, output, session) {
  city_locations <- data.frame(city="Amsterdam", lat=52.370216, long=4.895168) %>%
    rbind(data.frame(city="London", lat = 51.509865, long = -0.118092)) %>%
    rbind(data.frame(city="Paris", lat = 48.864716, long = 2.349014)) %>%
    rbind(data.frame(city="Barcelona", lat = 41.390205, long = 2.154007)) %>%
    rbind(data.frame(city="Milan", lat = 45.464664, long = 9.188540)) %>%
    rbind(data.frame(city="Vienna", lat = 	48.210033, long = 16.363449))
  
  #Update the country select with available countries from the data set 
  updateSelectInput(session, "countrySelect", label = "Select Country", choices = comboDf$country, selected = "")
  updateSelectInput(session, "citySelect", label = "Select City", choices = comboDf$city, selected = "")
  
  observeEvent(input$countrySelect, {
    cities_in_country <- list()
    for(i in 1:nrow(comboDf)) {
      if(comboDf$country[i] == input$countrySelect) {
        city <- as.character(comboDf$city[i])
        cities_in_country[city] <- city
      }
    }
    updateSelectInput(session, "citySelect", label = "Select City", choices = cities_in_country , selected = "")
  }, ignoreInit = TRUE)
  
  
  
  observeEvent(input$citySelect, {
    # place in paris has amsterdam in the street name, thus I search for country AND city
    selectedCountry <- input$countrySelect
    selectedCity <- input$citySelect

    hotels <<- subset(dfAll, str_detect(Hotel_Address, as.character(selectedCountry)) & str_detect(Hotel_Address, as.character(selectedCity)),
                     select = c(Hotel_Name, Hotel_Address, lat, lng, Average_Score, Reviewer_Nationality, Total_Number_of_Reviews))
    
    zoom <- 10
    i <- 0
    for(i in 1:nrow(city_locations)) {
      if(city_locations$city[i] == input$citySelect) {
        lat <- city_locations$lat[i]
        long <- city_locations$long[i]
        
        renderMap(lat, long, zoom, hotels)
      } 
    }
    
    hotelList <- c("All",sort(unique(hotels$Hotel_Name)))
    
    updateSelectInput(session, "hotelSelect", label = "Select Hotel", choices = hotelList, selected = "")
    
   
  }, ignoreInit = TRUE)
  
  observeEvent(input$hotelSelect, {
    selectedHotel <- input$hotelSelect
    if(selectedHotel == "All") {
      for(i in 1:nrow(city_locations)) {
        if(city_locations$city[i] == input$citySelect) {
          lat <- city_locations$lat[i]
          long <- city_locations$long[i]
          renderMap(lat, long, 10, hotels)
        } 
      }
    } else {
      sub <- subset(hotels, Hotel_Name == as.character(selectedHotel), select = c(Hotel_Name, Hotel_Address, lat, lng, Average_Score, Reviewer_Nationality, Total_Number_of_Reviews))
     
      renderMap(unique(sub$lat), unique(sub$lng), 15, sub)
    }
    
  }, ignoreInit = TRUE)
  
  renderMap <- function(lat, long, zoom, hotels) {
    if(!is.null(hotels)) {
      output$mymap <- renderLeaflet({
        leaflet(dfAll) %>%
          setView(lng=long, lat=lat, zoom=zoom) %>%
          addTiles()  %>%
          addMarkers(lat = unique(hotels$lat), unique(hotels$lng), popup = paste("Name", unique(hotels$Hotel_Name), "<br>",
                                                                                 "Address", unique(hotels$Hotel_Address), "<br>",
                                                                                 "Average Score", unique(hotels$Average_Score), "<br>",
                                                                                 "Amount of Reviews", unique(hotels$Total_Number_of_Reviews)))
      })} else {
        output$mymap <- renderLeaflet({
          leaflet(dfAll) %>%
            setView(lng=long, lat=lat, zoom=zoom) %>%
            addTiles()
        })
      }
  }
  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click
    print(p)
  })
}