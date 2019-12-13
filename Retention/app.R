# CSC480 Fall 2019 
# Mary Donahoo and Kim Van Vleet
# Student Retention Toolkit using R
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://rstudio.github.io/shinydashboard/structure.html
#    https://leaflet-extras.github.io/leaflet-providers/preview/

library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(rgdal)
library(DT)
library(tidyverse)

library(readxl)
retention <- read_excel("data/Retention_withXref.xlsx")
DataDictionary <- read_excel("data/DataDictionary.xlsx")

# Shapefile downloaded from US Census Bureau
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
states <- readOGR("data/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
# prepare and match shapefile data to retention data for display in Leaflet map
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


# Summarize data for use in ggplot visualizations
dataREcurrent <- retention %>%
    filter (YEAR >= 2010) %>%
    group_by(RE_CURRENT, YEAR, RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() %>%
    filter (RETAINED == 'Y') 
listREcurrent <- sort(unique(dataREcurrent$RE_CURRENT))

dataREformer <- retention %>%
    filter (YEAR < 2010) %>%
    filter (RE_FORMER != 'n/a') %>%
    group_by(RE_FORMER, YEAR, RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() %>%
    filter (RETAINED == 'Y') 
listREformer <- sort(unique(dataREformer$RE_FORMER))

dataGender <- retention %>%
    group_by(GENDER,YEAR,RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() 

r4 <- retention %>%
    filter (YEAR >= 2004) %>%
    filter (YEAR <= 2018) %>%
    group_by(MAJOR, RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() %>%
    filter (RETAINED == 'Y')

# Get unique list of majors for Select list on Major tab
list_of_majors <- sort(unique(retention$MAJOR))

dataMajor <- retention %>%
    group_by(MAJOR, YEAR, RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit() %>%
    filter (RETAINED == 'Y') 


dataAcaindex <- retention %>%
    filter(ACAINDEX != "n/a") %>%
    group_by(ACAINDEX,YEAR,RETAINED) %>% 
    summarize(N = n()) %>%
    mutate(freq = N/sum(N), 
           pct = round((freq*100),0)) %>%
    na.omit()

dataGPA <- retention %>%
    filter(TermGPA != 'WD') %>%
    mutate(gpa = as.numeric(TermGPA))

# UI
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Retention Toolkit"),
    dashboardSidebar(

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
            menuItem("Glossary", tabName = "glossarydoc", icon = icon("book")),
            menuItem("Raw Data", tabName = "rawdata", icon = icon("database")),
            menuItem("Help", tabName = "helpdoc", icon = icon("flag"))
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    tags$img(style = "height:15%; width:90%;", src = "SMU_ColorLogo_CMYK.png"),
                    HTML("<h1><center>Student Retention Toolkit</center></h1>"),
                    HTML("<h3><center>by Kim Van Vleet and Mary Donahoo</center></h3>")
            ),
            tabItem(tabName = "stateAtAdmit",
                    fluidRow(box(width = 12,leafletOutput(outputId = "mymap"))),
                    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table" )))
                    ),
            tabItem(tabName = "raceEthnicity",
                    h2("Retention by Race & Ethnicity"),
                    
                    fluidRow(box(width = 6,
                                 plotOutput(outputId = "REcurrent_plot", height = 300)),
                             box(width = 6, 
                                 plotOutput(outputId = "REformer_plot", height = 300))),
                    fluidRow(box(width = 6,
                                 checkboxGroupInput(inputId = "selected_REcurrent",
                                                    label = "Current Race & Ethnicity Categories:",
                                                    choices = listREcurrent,
                                                    selected = c("Black/African Amer",
                                                                 "White",
                                                                 "Asian",
                                                                 "Hispanic/Latino"))),
                             box(width = 6,
                                 checkboxGroupInput(inputId = "selected_REformer", 
                                                    label = "Former Race & Ethnicity Categories:",
                                                    choices = listREformer,
                                                    selected = c("Black",
                                                                 "Caucasian",
                                                                 "Asian",
                                                                 "Latino"))))
                    ),
            tabItem(tabName = "gender",
                    h2("Retention by Gender"),
                    fluidRow(box(width = 2, 
                                 radioButtons(inputId = "gender_rb",
                                              label = "Select one: ",
                                              c("Retained" = "Y",
                                                "Not Retained" = "N")))),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "genderCount_plot",height = 275))),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "genderPct_plot", height = 275)))
                    ),
            tabItem(tabName = "major",
                    h2("Retention by Major"),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "major_plot3", height = 250))),
                    fluidRow(box(width = 6,
                                 selectInput(inputId = "selected_major",
                                             label = "Select one or more Major(s): ",
                                             choices = list_of_majors,
                                             selected = "Computer Science",
                                             multiple = TRUE,
                                             selectize = TRUE
                                             ),
                                 sliderInput(inputId = "major_year_range",label = "Select Year Range:",
                                             min = 2004,max = 2018,
                                             value = c(2010,2018),
                                             sep="",
                                             step = 1),
                                 HTML("<i>Inputs control plot above</i>")
                                 ),
                             box(width = 6,plotOutput(outputId = "major_plot", height = 400))
                             )
            ),
            tabItem(tabName = "acaIndex",
                    h2("Retention by Academic Index"),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "acaindexCount_plot",height = 275))),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "acaindexPct_plot", height = 275))),
                    h4("Academic Index is a number between 1 and 5 assigned to domestic students for the purpose of awarding scholarships.")
                    ),
            tabItem(tabName = "gpa",
                    h2("Retention by Term GPA"),
                    fluidRow(box(width = 12,
                                 plotOutput(outputId = "gpa_plot",height = 400)))
            ),
            tabItem(tabName = "datadict",
                    fluidRow(box(width = 12, dataTableOutput(outputId = "data_dict" )))
            ),
            tabItem(tabName = "glossarydoc",
                    fluidRow(box(width = 12, uiOutput( outputId = "Glossary_pdf")))
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
                      title = "<p>Retention Rate</p><p>by State</p>")
    )
    output$summary_table <- renderDataTable(data_input())
    
    output$REcurrent_plot <- renderPlot({
        selected_dataREcurrent <- dataREcurrent %>%
            filter(RE_CURRENT %in% input$selected_REcurrent)
        ggplot(selected_dataREcurrent, aes(x = YEAR, y = pct,color = RE_CURRENT )) + 
            geom_line(size = 1) +
            scale_x_continuous("Year", breaks = selected_dataREcurrent$YEAR) +
            labs(x="Year",y="Percent",color="Current") +
            ggtitle("Percent Retained, 2010 to Present")
    })
    
    output$REformer_plot <- renderPlot({
        selected_dataREformer <- dataREformer %>%
            filter(RE_FORMER %in% input$selected_REformer)
        ggplot(selected_dataREformer, aes(x = YEAR, y = pct,color = RE_FORMER )) + 
            geom_line(size = 1) +
            scale_x_continuous("Year", breaks = selected_dataREformer$YEAR) +
            labs(x="Year",y="Percent",color="Former") +
            ggtitle("Percent Retained, Prior to 2010")
    })
    
    output$genderCount_plot <- renderPlot({
        selected_dataGender <- dataGender %>%
            filter(RETAINED == input$gender_rb) 
        genderPlotTitle <- if(input$gender_rb == "Y") "Retained" else "Not Retained"
        ggplot(selected_dataGender, aes(x = YEAR, y = N, fill=GENDER)) + 
            geom_bar(stat = "identity", position = 'dodge') +
            scale_x_continuous("Year", breaks = selected_dataGender$YEAR) +
            labs(x="Year",y="Head Count") +
            ggtitle(paste("Number",genderPlotTitle))})
    
    output$genderPct_plot <- renderPlot({
        selected_dataGender <- dataGender %>%
            filter(RETAINED == input$gender_rb) 
        genderPlotTitle <- if(input$gender_rb == "Y") "Retained" else "Not Retained"
        ggplot(selected_dataGender, aes(x = YEAR, y = pct, fill=GENDER)) + 
            geom_bar(stat = "identity", position = 'dodge') +
            scale_x_continuous("Year", breaks = selected_dataGender$YEAR) +
            labs(x="Year",y="Percent") +
            ggtitle(paste("Percent",genderPlotTitle))})
    
    output$major_plot <- renderPlot(ggplot(r4, aes(x = pct, y = reorder(MAJOR,pct))) + 
                                        geom_point(color = "steel blue", size = 2) +
                                        labs(x="Percent Retained",y="Major") +
                                        ggtitle("Retention Percent by Major (for all available years)"))
    
    output$major_plot3 <- renderPlot({
        selected_dataMajor <- dataMajor %>%
            filter(MAJOR %in% input$selected_major) %>%
            filter(YEAR >= input$major_year_range[1]) %>%
            filter(YEAR <= input$major_year_range[2])
        ggplot(selected_dataMajor, aes(x = YEAR, y = pct,color = MAJOR )) + 
            geom_line(size = 1) +
            scale_x_continuous("Year", breaks = selected_dataMajor$YEAR) +
            labs(x="Year",y="Pct Retained") +
            ggtitle("Percent Retained for Selected Major(s)")})
    
    output$acaindexCount_plot <- renderPlot(ggplot(dataAcaindex, aes(x = YEAR, y=N)) +
                                                geom_col(aes(fill = RETAINED)) +
                                                facet_wrap(~ACAINDEX, ncol = 5) +
                                                labs(x="Year",y="Count") +
                                                ggtitle("Retention Count by Academic Index"))
    
    output$acaindexPct_plot <- renderPlot(ggplot(dataAcaindex, aes(x = YEAR, y=pct)) +
                                              geom_col(aes(fill = RETAINED)) +
                                              facet_wrap(~ACAINDEX, ncol = 5) +
                                              labs(x="Year",y="Percent") +
                                              ggtitle("Retention Percent by Academic Index"))
    
    output$gpa_plot <- renderPlot(ggplot(dataGPA, aes(x = YEAR, y=gpa,color = RETAINED)) +
                                      geom_point(alpha = 0.1, shape = 16) +
                                      geom_jitter() +
                                      facet_wrap(~RETAINED) +
                                      labs(x="Year",y="Term GPA"))
    
    output$data_dict <- renderDataTable(DataDictionary,options = list(pageLength = 15))
    output$Glossary_pdf <- renderUI(tags$iframe(style = "height:800px; width:100%; scrolling = yes;",src = "Glossary.pdf"))
    output$raw_data <- renderDataTable(retention)
    output$help_pdf <- renderUI(tags$iframe(style = "height:800px; width:100%; scrolling = yes;",src = "help.pdf"))
}


# Run the application 
shinyApp(ui = ui, server = server)
