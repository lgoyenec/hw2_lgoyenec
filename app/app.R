# Laura Goyeneche
# September 27, 2018
# HW1 R Shiny 
# -------------------------------------------------------------------

# Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)

library(readxl)

# Working directory
cd = "C:/Users/lgoye/OneDrive/Documents/GitHub/project1_lgoyenec/data_xls"
setwd(cd)

# Data 
crops       = read_excel("obs_drug_colombia.xlsx", sheet = "illicit_crops")
eradication = read_excel("obs_drug_colombia.xlsx", sheet = "manual_eradication")
seizures    = read_excel("obs_drug_colombia.xlsx", sheet = "seizures")


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# User interface
# -------------------------------------------------------------------
ui = 
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = HTML("<a href=http://www.odc.gov.co/><font color=white>Observatory of drugs</font></a>")),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        convertMenuItem(
          menuItem("Supply", 
                   tabName = "supply", 
                   icon = icon("seedling"),
                   startExpanded = T,
                   selectInput("input1", 
                               "Variable of interest",
                               choices  = c("Illicit crops" = "crops", "Manual eradication" = "eradication", "Seizures" = "seizures"), 
                               selected = "crops"),
                   sliderInput("slider1", 
                               "Time period", 
                               1999, 2018, value = c(2015,2018), 
                               sep = "")
          ), "supply"
        ),
        convertMenuItem(
          menuItem("Consumption", 
                   tabName = "demand", 
                   icon = icon("user-astronaut"),
                   checkboxGroupInput("input2",
                                      "Substance",
                                      choices  = c("Marihuana","Cocaine","Heroin"),
                                      selected = "Cocaine"),
                   radioButtons("button1",
                                "Year of epidemiological study",
                                choices  = c(2008, 2013),
                                selected = 2013)
          ), "demand"
        ),
        convertMenuItem(
          menuItem('Criminality', 
                   tabName = "criminality", 
                   icon = icon("skull"),
                   radioButtons("button2",
                                "Gender",
                                choices  = c("Total","Male","Female")),
                   radioButtons("button3",
                                "Judicial situation",
                                choices = c("Convicted or Accused","Convicted", "Accused"))
          ), "criminality"
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "supply",p("Laura")),
        tabItem(tabName = "demand",h2("Y")),
        tabItem(tabName = "criminality",h2("Z"))
      )
    )
)




# Server function
# -------------------------------------------------------------------
server = function(input, output) {

}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------