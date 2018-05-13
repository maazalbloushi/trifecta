library(rgdal)
library(leaflet)
library(RColorBrewer)
library(DT)
library(shiny)
library(shinythemes)
library(shinydashboard)

# --------------- #
# USER INTERFACE
# --------------- #
ui <- fluidPage(
                tags$style(type="text/css", "body {padding-top: 50px;}"),
                theme = shinytheme("united"),
                navbarPage("Trifecta Group", id = "selector",
                           position = "fixed-top", header = NULL,
                           tabPanel("Map Analysis", value = 1, 
                                    fluidRow(
                                            titlePanel(HTML("<font size='5'>&nbsp;&nbsp;&nbsp;Analysis of Talent Shortage & Career Opportunity in Malaysia</font>")),
                                            
                                            # Left Side
                                            column(width = 6,
                                                   
                                                  
                                                   fluidRow(
                                                        
                                                       # Map of Malaysia:
                                                       box(width = 12, title = HTML("<font size='4'>Map of Malaysia</font>"), 
                                                           leafletOutput("tableMalaysiaMap", height = 400),
                                                           br()
                                                           ),
                                                      
                                                   # Show / Hide checkboxes:
                                                   box(height = 65,width = 3,status = "warning",
                                                       uiOutput("routeSelect"),
                                                       checkboxGroupInput("supplydemand", "Legend",
                                                                          choiceNames = list(HTML("<font size='2' color='blue'>Talent availability</font>"), HTML("<font size='2' color='red'>Job opportunity</font>")),
                                                                          choiceValues = list("talents", "jobs"),
                                                                          selected = c('talents', 'jobs')
                                                       )
                                                   ),
                                                   
                                                   box(height = 65,width = 3,status = "warning",
                                                       selectInput("states", "States",
                                                                   choices = c(
                                                                       "Kuala Lumpur" = 1,
                                                                       "Selangor" = 2,
                                                                       "Pulau Pinang" = 3,
                                                                       "Johor" = 4,
                                                                       "Perak" = 5,
                                                                       "Kelantan" = 6,
                                                                       "Sarawak" = 7
                                                                   ),
                                                                   selected = "2"
                                                       )
                                                   ),
                                                   
                                                   box(height = 65,width = 6, sliderInput("obs", "Map zoom:", min = 1, max = 20, value = 10)),

                                                   
                                                   # State Demography
                                                   fluidRow(
                                                       box(width = 8, title = HTML("<font size='4'>&nbsp;&nbsp;&nbsp;State Demography</font>"), tableOutput('table'))
                                                   )
                                                )
                                            ),
                                            
                                            # Right Side - Student Enrollment:
                                            column(width = 5, 
                                                   fluidRow(
                                                       box(width = 12, title = HTML("<font size='4'>&nbsp;&nbsp;&nbsp;Student Enrollment and Career Opportunity</font>"), tableOutput('tableEnrollmentCareer'))
                                                   )
                                            )
                                    )
                            ),
                           tabPanel("Documentation", value = 2, 
                                    fluidRow(
                                        titlePanel(HTML("<font size='5'>&nbsp;&nbsp;&nbsp;Documentation</font>"))
                                    )
                            ),
                           tabPanel("About", value = 3, 
                                    fluidRow(
                                        titlePanel(HTML("<font size='5'>&nbsp;&nbsp;&nbsp;About the Shiny application</font>")),
                                        
                                        p(
                                            "    This application is a group project for the Principles of Data Science course (WQD7001)"   
                                        )
                                    )
                           )
                )
        )

# --------------- #
# SERVER SIDE
# --------------- #
server <- function(input, output) {
  
    # Display the State Demography
    datdisplay <- read.csv(file = "dataset/datasetdisplay.csv")
    output$table <- renderTable(datdisplay)
  
    # Read the full state demography data (with longitude and latitude)
    dat <- read.csv(file = "dataset/dataset.csv")
  
    # Talent vs Availability Dataset selector
    values <- reactiveValues(dat2 = NULL) #for reset dataframe value when new state selected.
    observeEvent(input$states, {
        csvSelected <- paste("stateData/",input$states,".csv",sep = "") #filepath creation with selectedInput
        values$dat2 <- read.csv(csvSelected) #load the csv
    })
  
    output$tableEnrollmentCareer <- renderTable(values$dat2) #render the table
    
    # Malaysian Map
    output$tableMalaysiaMap <- renderLeaflet({
    zoomV <- input$obs
    
    dat_Long = dat[input$states,"Longitude"]
    dat_Lat = dat[input$states,"Latitude"]
    
    if (is.element('jobs', input$supplydemand)) {
      demandC <- 'red'
    } else {
      demandC <- 'none'
    }
    
    if (is.element('talents', input$supplydemand)) {
      supplyC <- 'blue'
    } else {
      supplyC <- 'none'
    }
    
    geoData <- readLines("json/malaysia.geojson") %>% paste(collapse = "\n")
    
    leaflet(dat) %>% addTiles() %>%
      setView(lng = dat_Long, lat = dat_Lat, zoom = zoomV) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
      addGeoJSON(geoData, weight = 2, color = "#000", fill = FALSE) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 2,
                 radius = ~sqrt(Demand) * 50, popup = ~States, color = demandC
      ) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 2,
                 radius = ~sqrt(Supply) * 50, popup = ~States, color = supplyC
      )
  })
}

# --------------- #
# APP BINDING
# --------------- #
shinyApp(ui = ui, server = server)