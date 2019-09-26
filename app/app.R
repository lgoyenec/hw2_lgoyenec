# Laura Goyeneche
# September 27, 2018
# HW1 R Shiny 
# -------------------------------------------------------------------

# Libraries
library(shiny)
library(shinydashboard)
library(shinythemes)

library(readxl)

# Working directory
cd = "C:/Users/lgoye/OneDrive/Documents/GitHub/project1_lgoyenec/data_xls"
setwd(cd)

# Data 
crops       = read_excel("obs_drug_colombia.xlsx", sheet = "illicit_crops")
eradication = read_excel("obs_drug_colombia.xlsx", sheet = "manual_eradication")
seizures    = read_excel("obs_drug_colombia.xlsx", sheet = "seizures")

# User interface
# -------------------------------------------------------------------
ui = 
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = HTML("<a href=http://www.odc.gov.co/><font color=white>Observatory of drugs</font></a>")),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Supply", 
                 tabName = "supply", 
                 icon = icon("seedling"),
                 selectInput("input1", 
                             "Variable of interest",
                             choices  = c("Illicit crops" = "crops", "Manual eradication" = "eradication", "Seizures" = "seizures"), 
                             selected = "crops"),
                 sliderInput("slider1", 
                             "Time period", 
                             1999, 2018, 2018, 
                             sep = "")
                 ),
        menuItem("Consumption", 
                 tabName = "demand", 
                 icon = icon("user-astronaut"),
                 selectInput("input2",
                             "Substance",
                             choices  = c("Marihuana","Cocaine","Heroin"),
                             selected = "Cocaine"),
                 radioButtons("button1",
                              "Year of epidemiological study",
                              choices  = c(2008, 2011),
                              selected = 2011)
                 ),
        menuItem('Criminality', 
                 tabName = "criminality", 
                 icon = icon("skull"),
                 radioButtons("button2",
                              "Gender",
                              choices  = c("Total","Male","Female"),
                              selected = "Total"),
                 radioButtons("button3",
                              "Judicial situation",
                              choices = c("Convicted or Accused","Convicted", "Accused"),
                              selected = "All prisoners")
                 )
      )
    ),
    dashboardBody(
      
    )
)

# Server function
# -------------------------------------------------------------------
server <- function(input, output) {

}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------