# Laura Goyeneche
# September 27, 2018
# HW1 R Shiny 
# -------------------------------------------------------------------

# Libraries
# -------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinydashboard)

library(readxl)

library(DT)
library(plotly)

# Working directory
# -------------------------------------------------------------------
cd = "C:/Users/lgoye/OneDrive/Documents/GitHub/project1_lgoyenec"
setwd(cd)

# Data 
# -------------------------------------------------------------------
cd_data     = paste0(cd,"/data_xls/obs_drug_colombia.xlsx")

crops       = read_excel(cd_data, sheet = "illicit_crops")
eradication = read_excel(cd_data, sheet = "manual_eradication")
seizures    = read_excel(cd_data, sheet = "seizures")
demand      = c()
crime       = read_excel(cd_data, sheet = "criminality")

# Function for conditional menu item
# -------------------------------------------------------------------
source("convertMenuItem.R")

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
                               choices  = c("Illicit crops" = "A", "Manual eradication" = "B", "Seizures" = "C"), 
                               selected = "A"),
                   sliderInput("slider1", 
                               "Time period", 
                               1999, 2018, value = c(2010,2018), 
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
        ),
        menuItem('Data set',
                 tabName = "data",
                 icon = icon("table"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "supply", 
          fluidRow(
            valueBoxOutput("vB1", width = 6),
            valueBoxOutput("vB2", width = 6)
          ),
          fluidRow(
            box(plotlyOutput("plot1"), width = 12)
          )
        ),
        tabItem(tabName = "demand",h2("Y")),
        tabItem(tabName = "criminality",h2("Z")),
        tabItem(
          tabName = "data",
          tabsetPanel(
            type = "tabs",
            tabPanel("Illicit Crops"     , br(), DT::dataTableOutput("table1")),
            tabPanel("Manual Eradication", br(), DT::dataTableOutput("table2")),
            tabPanel("Seizures"          , br(), DT::dataTableOutput("table3")),
            tabPanel("Consumption"       , br(), DT::dataTableOutput("table4")),
            tabPanel("Criminality"       , br(), DT::dataTableOutput("table5"))
          )
        )
      )
    )
)

# Server function
# -------------------------------------------------------------------
server = function(input, output) {
  
  # Select variables of datasets as reactive objects
  # -----------------------------------------------------------------
  dt_crops = reactive({data = crops %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2])})
  dt_eradi = reactive({data = eradication %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2])})
  dt_seizu = reactive({data = seizures %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2])} %>% select(-Sustancia))
  dt_deman = reactive({})
  dt_crime = reactive({})
  
  # Supply
  # -----------------------------------------------------------------
  
  # Select values for valueBox
  
  observe({
    if (input$input1 == "A") {
      # Illicit crops of cocaine (hectares)
      # -------------------------------------------------------------
      output$vB1 = renderValueBox({
        dt_crops() %>%
          filter(Total == max(Total)) %>%
          valueBox(~Year,
                   p(strong(
                     paste("presented the highest number of illicit crops of cocaine", ~Total))),
                   icon = icon("globe-americas"),
                   color = "teal"
                   )
      })
      
      output$plot1 = 
        renderPlotly({
          dt_crops() %>% 
            plot_ly(x =~ Year, y =~ Total, type = "scatter", mode = "lines+markers")
        })
    } else if (input$input1 == "B") {
      # Manual eradication (hectares)
      # -------------------------------------------------------------
      output$plot1 = 
        renderPlotly({
          dt_eradi() %>% 
            plot_ly(x =~ Year, y =~ Total, type = "scatter", mode = "lines+markers")
        })
    } else {
      # Seizures (Kg)
      # -------------------------------------------------------------
      output$plot1 = 
        renderPlotly({
          dt_seizu() %>% 
            plot_ly(x =~ Year, y =~ Total, type = "scatter", mode = "lines+markers", linetype =~ Substance)
        })
    }
  })
  

  # Data tables based on filters
  # -----------------------------------------------------------------
  output$table1 = DT::renderDataTable({DT::datatable(dt_crops(), options = list(pageLength = 10), rownames = F)})
  output$table2 = DT::renderDataTable({DT::datatable(dt_eradi(), options = list(pageLength = 10), rownames = F)})
  output$table3 = DT::renderDataTable({DT::datatable(dt_seizu(), options = list(pageLength = 10), rownames = F)})
  output$table4 = DT::renderDataTable({DT::datatable(dt_deman(), options = list(pageLength = 10), rownames = F)})
  output$table5 = DT::renderDataTable({DT::datatable(dt_crime(), options = list(pageLength = 10), rownames = F)})
  
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------