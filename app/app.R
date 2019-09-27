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
#cd = "C:/Users/lgoye/OneDrive/Documents/GitHub/project1_lgoyenec/app"
#setwd(cd)

# Data 
# -------------------------------------------------------------------
cd_data     = "obs_drug_colombia.xlsx"

crops       = read_excel(cd_data, sheet = "illicit_crops")
eradication = read_excel(cd_data, sheet = "manual_eradication")
seizures    = read_excel(cd_data, sheet = "seizures")
crime       = read_excel(cd_data, sheet = "criminality")

# Function for conditional menu item
# -------------------------------------------------------------------
source("convertMenuItem.R")

# User interface
# -------------------------------------------------------------------
ui = 
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = HTML("<a href=http://www.odc.gov.co/><font color=white>Observatory of Drugs</font></a>")),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        convertMenuItem(
          menuItem("Colombian Supply", 
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
          menuItem('Colombian Crime', 
                   tabName = "criminality", 
                   icon = icon("skull"),
                   radioButtons("button2",
                                "Analysis by Gender or Judicial situation",
                                choices  = c("Gender","Judicial situation"),
                                selected = "Gender")
          ), "criminality"
        ),
        menuItem('Data',
                 tabName = "data",
                 icon = icon("table")),
        br(),
        br(),
        p(strong("Laura Goyeneche"), br(),
          em("MSPPM-Data Analytics '20", style = "font-size:11px"), br(),
          em("Carnegie Mellon University", style = "font-size:11px"), br(),
          strong(em("lgoyenec@andrew.cmu.edu", style = "font-size:10px")), br(),
          a("https://github.com/lgoyenec/hw1_lgoyenec", 
            href = "https://github.com/lgoyenec/project1_lgoyenec",
            style = "font-size:10px"),
          style = "font-size:12px"),
        br(),
        p(strong("Source:"), "Observatory of Drugs in Colombia", style = "font-size:11px"),
        br()
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
        tabItem(
          tabName = "criminality",
          fluidRow(
            valueBoxOutput("vB3", width = 6),
            valueBoxOutput("vB4", width = 6)
          ),
          fluidRow(
            box(plotlyOutput("plot2"), width = 12)
          )
        ),
        tabItem(
          tabName = "data",
          tabsetPanel(
            type = "tabs",
            tabPanel("Illicit Crops"     , br(), DT::dataTableOutput("table1")),
            tabPanel("Manual Eradication", br(), DT::dataTableOutput("table2")),
            tabPanel("Seizures"          , br(), DT::dataTableOutput("table3")),
            tabPanel("Criminality"       , br(), DT::dataTableOutput("table4"))
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
  dt_crops = reactive({data = crops       %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2])})
  dt_eradi = reactive({data = eradication %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2])})
  dt_seizu = reactive({data = seizures    %>% filter(Year >= input$slider1[1] & Year <= input$slider1[2]) %>% select(-Sustancia)})
  dt_deman = reactive({})
  dt_crime = reactive({data = crime       %>% filter(if (input$button2 == "Gender") Group == 2 else Group == 1 ) %>%
                                              mutate(Percent = round(Total*100/sum(Total),2)) %>%
                                              select(-Tema, - Desagregacion)})
  
  # Supply
  # -----------------------------------------------------------------
  observe({
    if (input$input1 == "A"){
      # Illicit crops of cocaine (hectares)
      # -------------------------------------------------------------
      data    = dt_crops()
      textvb1 = "presented the highest number of illicit crops of cocaine"
      textvb2 = "increased the illicit crops of coca between"
      titleP  = "Illicit crops of cocaine (hectares)"
    } else if (input$input1 == "B") {
      # Manual eradication (hectares)
      # -------------------------------------------------------------
      data = dt_eradi()
      textvb1 = "presented the highest number of hectares of coca eradicated"
      textvb2 = "increased the hectares of coca eradicated between"
      titleP  = "Manual eradication (hectares)"
    } else {
      # Seizures (Kg)
      # -------------------------------------------------------------
      data = dt_seizu()
      textvb1 = "presented the highest number of seizures"
      textvb2 = "increased the kg of seizures"
      titleP  = "Seizures (Kg)"
    }
    
    output$vB1 = renderValueBox({
      value = data %>% filter(Total == max(Total)) %>% as.numeric()
      valueBox(value[1], textvb1, color = "yellow")
    })
    
    output$vB2 = renderValueBox({
      value = round((data[nrow(data),2]/data[1,2])-1,2)
      years = c(min(data[,1]), max(data[,1]))
      valueBox(paste(value,"%"), paste(textvb2, years[1], "and", years[2]), color = "yellow")
    })
    
    observe({
      if(nrow(data) == (input$slider1[2]-input$slider1[1] + 1)){
        value  = data %>% filter(Total == max(Total)) %>% as.numeric()
        output$plot1 = renderPlotly({
          data %>%
            filter(Total != max(Total)) %>%
            plot_ly(x =~ Year, y =~ Total, type = "bar") %>%
            add_bars(x = value[1], y = value[2], showlegend = F) %>%
            layout(title = titleP)
        })
      } else {
        output$plot1 = renderPlotly({
          data %>% 
            plot_ly(x =~ Year, y =~ Total, type = "scatter", mode = "lines+markers", linetype =~ Substance) %>%
            layout(title = titleP,
                   yaxis = list(range = c(0, max(data[,2])*1.5)), 
                   legend = list(x = 0.3, y = 1, orientation = "h"))
        })
      }
    })
  })
  
  # Criminality
  # -----------------------------------------------------------------
  output$vB3 = renderValueBox({
    value = dt_crime() %>% filter(g_cat == 1) %>% summarise(n = sum(Percent)) %>% as.numeric()
    text  = dt_crime() %>% filter(g_cat == 1) %>% select(Category) %>% unique() %>% as.character()
    valueBox(paste(value,"%"), text, color = "teal")
  })
  
  output$vB4 = renderValueBox({
    value = dt_crime() %>% filter(g_cat == 2) %>% summarise(n = sum(Percent)) %>% as.numeric()
    text  = dt_crime() %>% filter(g_cat == 2) %>% select(Category) %>% unique() %>% as.character()
    valueBox(paste(value,"%"), text, color = "teal")
  })
  
  output$plot2 = renderPlotly({
    text  = ifelse(input$button2 == "Gender","Gender","Judicial situation")
    dt_crime() %>%
      plot_ly() %>%
      add_bars(y =~ Percent, x =~ Category, color =~ Crime, type = "bar", width = 0.4) %>%
      layout(title = paste("Crimes related with illicit drug (2018) by",text),
             yaxis = list(range = c(0,140)),
             legend = list(x = 0.1, y = 1), 
             barmode = 'stack')
  })
  
  # Data tables based on filters
  # -----------------------------------------------------------------
  output$table1 = DT::renderDataTable({DT::datatable(dt_crops(), options = list(pageLength = 10), rownames = F)})
  output$table2 = DT::renderDataTable({DT::datatable(dt_eradi(), options = list(pageLength = 10), rownames = F)})
  output$table3 = DT::renderDataTable({DT::datatable(dt_seizu(), options = list(pageLength = 10), rownames = F)})
  output$table4 = DT::renderDataTable({DT::datatable(dt_crime(), options = list(pageLength = 10), rownames = F)})
  
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------