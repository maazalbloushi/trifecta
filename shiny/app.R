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
                navbarPage("The Trifecta", id = "selector",
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
                                        titlePanel(HTML("<font size='5'>&nbsp;&nbsp;&nbsp;Documentation</font>")),
                                        
                                        column(width = 12, 
                                               p(
                                                   HTML("This Shiny application presents several information about the availability of talents and career prospects for several states in Malaysia.")
                                               ),
                                               br(),
                                               p(
                                                   HTML("<h4>The Map Widget</h4>"),
                                                   p(
                                                       HTML("The main widget is the Map, which shows the number of students currently enrolled in vocational college for a particular state and is designated by a blue circle. Within the same map, the number of available jobs for each selected state are also presented by a red circle. The size of the circle indicates how large or small the availability of either talent or career opportunity. If the talent circle (blue) is smaller than the job opportunity circle (red), it means that there is a shortage of skilled workers to fill in those job demands. If the situation is reversed, it shows that there is an excess of talents but no jobs to fulfil them. This relationship shows how skilled workers in Malaysia could make the decision to move to a different state in order to find jobs available for their sector.")
                                                   )
                                                        
                                               ),
                                               br(),
                                               p(
                                                   HTML("<h4>The Legend Checkbox Widget</h4>"),
                                                   p(
                                                       HTML("Two main types of data are presented within the map and can be controlled by the Legend checkboxes. To show only the available talents designated by blue circles inside the map, you may check the “Talent availability” checkbox and uncheck the “Job opportunity” checkbox. To show only the job opportunity designated by red circles inside the map, check the “Job opportunity” checkbox and uncheck the “Talent availability” checkbox. Checking or unchecking both checkboxes will show or hide both types of data on the map.")
                                                   )
                                               ),
                                               br(),
                                               p(
                                                   HTML("<h4>The States Dropdown Widget</h4>"),
                                                   p(
                                                       HTML("You can use the State dropdown widget to see the information for other states in Malaysia. By selecting a state using this widget, the map will move to that particular state and display the circles representing both talent availability and job opportunity according to their designated colours.")
                                                   )
                                               ),
                                               br(),
                                               p(
                                                   HTML("<h4>The Map Zoom Widget</h4>"),
                                                   p(
                                                       HTML("In order to zoom in or out of the map, you may drag the slider on the Map Zoom widget. The maximum zoom out value is 1 and zoom in value is 20.")
                                                   )
                                               )
                                        )
                                    )
                            ),
                           tabPanel("About", value = 3, 
                                    fluidRow(
                                        titlePanel(HTML("<font size='5'>&nbsp;&nbsp;&nbsp;About the Shiny application</font>")),

                                        column(width = 12,
                                            p(
                                                HTML("This Shiny application is developed to show the relationship between the availability of talents and career opportunities in Malaysia by looking at the datasets from The Malaysian Administrative Modernisation and Management Planning Unit (MAMPU). The link between talents and job prospects are made by combining both the data of Vacancies for Non-graduates by State and Occupational Category (Sector) with the data of Vocational College Enrollments by State.")
                                            )
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