# Laura Goyeneche
# September 27, 2018
# HW1 R Shiny 
# -------------------------------------------------------------------

# Libraries
library(shiny)
library(shinydashboard)

# User interface
# -------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

# Server function
# -------------------------------------------------------------------
server <- function(input, output) {

}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------