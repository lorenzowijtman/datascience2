ui <- navbarPage("Data Science 2", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("ui/styles.css")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              tagList(tags$head(tags$script(type="text/javascript", src = "busy.js"))),
                              div(class = "busy", p('Loading'),img(src="loading.gif")),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Data Science 2"),
                                            
                                            selectInput("countrySelect", "Country", c("")),
                                            selectInput("citySelect", "City", c("")),
                                            selectInput("hotelSelect", "Hotel", c(""))
                              )
                          )
                 ),
                 
                 tabPanel("Algorithms",
                          div(class="algorithmsOuter",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("ui/styles.css")
                              ),
                              
                              mainPanel(
                                fluidRow(
                                  h3("Naive Bayes"),
                                  numericInput("NBAmount", "Amount of reviews to use (total)", min = 1, max = 551000, value = 10000),
                                  sliderInput("maxRevNeg", "maximum reviewer score for negative reviews", min = 1, max = 8, value = 8, round = F),
                                  sliderInput("minRevPos", "minimum reviewer score for positive reviews", min = 8, max = 10, value = 9, round = F, step = 0.1),
                                ),
                                fluidRow(
                                  actionButton("trainBtn", "Train model"),
                                  textOutput("status"),
                                  actionButton("testBtn", "Test model"),
                                  tableOutput("testOut"),
                                  textOutput("testAccuracy")
                                ),
                                fluidRow(
                                  textAreaInput("writtenRev", "Write your own review"),
                                  actionButton("selfRevBtn", "Test with review"),
                                  textOutput("selfSentiment")
                                )
                                
                              )
                          )
                          ),
                 
                 conditionalPanel("false", icon("crosshair"))
)