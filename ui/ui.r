ui <- fluidPage(
  
  # Application title
  titlePanel("Some Hotel Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("countrySelect", "Select Country", choices = c("All"), multiple = F),
      selectInput("citySelect", "Select City", choices = c("All","Amsterdam", "London", "Paris"), multiple = F),
      selectInput("hotelSelect", "Select Hotel", choices = c("All"), multiple = F),
    ),
    
    # Show a map
    mainPanel(
      leafletOutput(outputId = "mymap"),
      fluidRow(
        dataTableOutput("Table")
      )
    )
  )
)