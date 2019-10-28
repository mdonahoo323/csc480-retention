#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://rstudio.github.io/shinydashboard/structure.html

library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(rgdal)
library(DT)
library(tidyverse)

library(readxl)
# retention <- read_excel("../data/Retention_withXref.xlsx")
retention <- read_excel("data/Retention_withXref.xlsx")
# retention <- load("data/retention.RData")

# DataDictionary <- read_excel("../data/DataDictionary.xlsx")
DataDictionary <- read_excel("data/DataDictionary.xlsx")
# DataDictionary <- load("data/DataDictionary.RData")


states <- readOGR("data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")

us <- retention %>%
    mutate(retained = ifelse(RETAINED == 'Y',1,0))

us <- subset(us,is.element(us$STATE, states$NAME))
# is.element(us$STATE, states$NAME)

states <- subset(states,is.element(states$NAME, us$STATE))
# is.element(states$NAME, us$STATE)

us <- us[order(match(us$STATE, states$NAME)),]

#create bins based on the retained rate in us
bins <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
#give pal a number, it returns a color, see RColorBrewer package "RdYlBu"
pal <- colorBin("BrBG", domain = c(0,1), bins = bins)

r4 <- retention %>%
    filter (YEAR >= 2010) %>%
    filter (YEAR <= 2014) %>%
    group_by(MAJOR, RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() %>%
    filter (RETAINED == 'Y')


# UI
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Retention Toolkit"),
    dashboardSidebar(
        sliderInput("date_range", label = "Date Range",
                    min = min(us$YEAR),
                    max = max(us$YEAR),
                    value = c(min(us$YEAR), max(us$YEAR)),
                    sep = "",
                    step = 1),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Charts",icon = icon("line-chart"),
                     menuSubItem("State at Admit", tabName = "stateAtAdmit", icon = icon("map")),
                     menuSubItem("Race & Ethnicity", tabName = "raceEthnicity",icon = icon("pie-chart")),
                     menuSubItem("Gender", tabName = "gender", icon = icon("bar-chart")),
                     menuSubItem("Major", tabName = "major",icon = icon("cog",lib="glyphicon")),
                     menuSubItem("AcaIndex", tabName = "acaIndex",icon = icon("heart",lib="font-awesome")),
                     menuSubItem("GPA", tabName = "gpa",icon = icon("crown"))
                     ),
            menuItem("Data Dictionary", tabName = "datadict", icon = icon("book")),
            menuItem("Raw Data", tabName = "rawdata", icon = icon("database")),
            menuItem("Help", tabName = "helpdoc", icon = icon("flag"))
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    tags$img(style = "height:15%; width:90%;", src = "SMU_ColorLogo_CMYK.png"),
                    HTML("<h1><center>Institutional Research Retention Toolkit</center></h1>"),
                    HTML("<h3><center>by Kim Van Vleet and Mary Donahoo</center></h3>")
            ),
            tabItem(tabName = "stateAtAdmit",fluidRow(box(width = 12,leafletOutput(outputId = "mymap"))),
                    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table" )))
                    ),
            tabItem(tabName = "raceEthnicity",
                    h2("Race & Ethnicity visualization goes here")
                    ),
            tabItem(tabName = "gender",
                    h2("Gender visualization goes here")
            ),
            tabItem(tabName = "major",
                    h2("Percent of Admits Retained by Major"),
                    fluidRow(box(width = 12,plotOutput(outputId = "major_plot")))
            ),
            tabItem(tabName = "acaIndex",
                    h2("AcaIndex visualization goes here")
            ),
            tabItem(tabName = "gpa",
                    h2("GPA visualization goes here")
            ),
            tabItem(tabName = "datadict",
                    fluidRow(box(width = 12, dataTableOutput(outputId = "data_dict" )))
            ),
            tabItem(tabName = "rawdata",
                    fluidRow(box(width = 12, dataTableOutput(outputId = "raw_data" )))
            ),
            tabItem(tabName = "helpdoc",
                    fluidPage(box(width = 12,uiOutput( outputId = "help_pdf")))
            )
        )
        
    )
)

# Server 15:00
server <- function(input, output) {
    
    data_input <- reactive({
        us %>%
            filter(YEAR >= 2004) %>%
            filter(YEAR <= 2018) %>%
            group_by(STATE) %>%
            summarize(numAdmits = n(),
                      numRetained = sum(retained)) %>%
            mutate(numNotRetained = numAdmits - numRetained,
                   retainedRate = numRetained/numAdmits)
    })
    
    data_input_ordered <- reactive({
        data_input()[order(match(data_input()$STATE, states$NAME)),]
    })    
    
    labels <- reactive({
        paste("<p>",data_input_ordered()$STATE, "</p>",
              "<p>","Retained Rate: ", round(data_input_ordered()$retainedRate,digits = 3),
              "</p>",sep="")
    })  
    
# output$mymap <- renderLeaflet(
    
    output$mymap <- renderLeaflet(
        leaflet() %>%
            setView(-96,37.8,4) %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            addPolygons(data = states,
                        weight = 1,
                        smoothFactor = 0.5,
                        color = "white",
                        fillOpacity = 0.8,
                        fillColor = pal(data_input_ordered()$retainedRate),
                        label = lapply(labels(),HTML)) %>%
            addLegend(pal = pal,
                      values = data_input_ordered()$retainedRate,
                      opacity = 0.7,
                      position = "topright",
                      title = "Retention Rate")
    )
    output$summary_table <- renderDataTable(data_input())
    output$major_plot <- renderPlot(ggplot(r4, aes(x = pct, y = reorder(MAJOR,pct))) + 
                                        geom_point() +
                                        labs(x="Percent Retained",y="Major"))
    output$data_dict <- renderDataTable(DataDictionary)
    output$raw_data <- renderDataTable(retention)
    output$help_pdf <- renderUI(tags$iframe(style = "height:800px; width:100%; scrolling = yes;",src = "help.pdf"))
}


# Run the application 
shinyApp(ui = ui, server = server)
