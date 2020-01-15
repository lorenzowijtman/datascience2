library(shiny)

# Define UI for application 
source('common/libs.R')
source('ui/ui.R')
source('server/server.R')

# Run the application 
shinyApp(ui = ui, server = server)
