source("database/MongoDB.R")
source("algorithms/NBTests.R")

server <- function(input, output, session) {
  
  # city and country long and latitude, hardcoded because no other cities are present in the dataset
  # hard coding can be avoided by selecting the first hotel in the list for the city and -
  #  - zoom out since zoom in provided to the method that renders the map
  
  country_locations <- data.frame(country="France", lat=47.824905, long=2.618787) %>%
    rbind(data.frame(country="United Kingdom", lat = 51.509865, long = -0.118092)) %>%
    rbind(data.frame(country="Netherlands", lat=52.370216, long=4.895168)) %>%
    rbind(data.frame(country="Barcelona", lat = 41.390205, long = 2.154007)) %>%
    rbind(data.frame(country="Milan", lat = 45.464664, long = 9.188540)) %>%
    rbind(data.frame(country="Vienna", lat = 	48.210033, long = 16.363449))
  
  city_locations <- data.frame(city="Amsterdam", lat=52.370216, long=4.895168) %>%
    rbind(data.frame(city="London", lat = 51.509865, long = -0.118092)) %>%
    rbind(data.frame(city="Paris", lat = 48.864716, long = 2.349014)) %>%
    rbind(data.frame(city="Barcelona", lat = 41.390205, long = 2.154007)) %>%
    rbind(data.frame(city="Milan", lat = 45.464664, long = 9.188540)) %>%
    rbind(data.frame(city="Vienna", lat = 	48.210033, long = 16.363449))
  
  #Update the country select with available countries from the data set 
  updateSelectInput(session, "countrySelect", label = "Select Country", choices = comboDf$country, selected = "")
  updateSelectInput(session, "citySelect", label = "Select City", choices = comboDf$city, selected = "")
  
  # observer for the selection of country in the menu, zooms in on teh specific country but doesnt load anything until city is selected
  observeEvent(input$countrySelect, {
    cities_in_country <- list()
    for(i in 1:nrow(comboDf)) {
      if(comboDf$country[i] == input$countrySelect) {
        city <- as.character(comboDf$city[i])
        cities_in_country[city] <- city
      }
    }
    updateSelectInput(session, "citySelect", label = "Select City", choices = cities_in_country , selected = "")
    enable(input$citySelect)
  }, ignoreInit = TRUE)
  
  
  
  observeEvent(input$citySelect, {
    # place in paris has amsterdam in the street name, thus I search for country AND city
    selectedCountry <- input$countrySelect
    selectedCity <- input$citySelect

    hotels <<- getHotels(selectedCountry, selectedCity)
    head(hotels)
    
    zoom <- 12
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
  
  # observer for when a hotel is selected, zooms in on the specific selected hotel and only shows that hotel with a marker
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
  
  #observer for clicks on markers of the map, shows reviews for the specific hotels in the menu
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click
    print(p)
  }, ignoreInit = T)
  
  
  renderMap <- function(lat, long, zoom, hotels) {
    if(!is.null(hotels)) {
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng=long, lat=lat, zoom=zoom) %>%
          addTiles()  %>%
          addMarkers(lat = unique(hotels$lat), unique(hotels$lng), 
                     popup = paste("Name", unique(hotels$Hotel_Name), "<br>",
                                  "Address", unique(hotels$Hotel_Address), "<br>",
                                  "Average Score", unique(hotels$Average_Score), "<br>",
                                  "Amount of Reviews", unique(hotels$Total_Number_of_Reviews)))
      })} else {
        output$mymap <- renderLeaflet({
          leaflet() %>%
            setView(lng=long, lat=lat, zoom=zoom) %>%
            addTiles()
        })
      }
  }
  
  # Render the map when the application starts
  renderMap(48.864716, 2.349014, 5, NULL)
  
  
  # observers for algorithms page
  observeEvent(input$trainBtn, {
      output$status <- renderText({"Model being trained"})
      neg <- input$maxRevNeg
      pos <- input$minRevPos
      amount <- input$NBAmount
      trainNB(pos, neg, amount)
      output$status <- renderText({"Model Trained"})
    })
  
  observeEvent(input$testBtn, {
    pred <- testNBWithData() 
    predTbl <- pred %>%
      group_by(sentiment, prediction) %>%
      tally()
    output$testOut <- renderTable({predTbl})
    
    accuracy <- ml_multiclass_classification_evaluator(pred) *100
    accString <- paste0("accuracy: ", round(accuracy, 2), "%")
    
    output$testAccuracy <- renderText({accString})
  })
  
  observeEvent(input$selfRevBtn, {
    review <- input$writtenRev
    sentiment <- testNBWithOwnReview(review)
    output$selfSentiment <- renderText({paste0("Sentiment of review: ",sentiment)})
  })
  
  
  session$onSessionEnded(function() {
    if(exists("sc") && spark_connection_is_open(sc)) {
      spark_disconnect(sc)
    }
  })
  
}