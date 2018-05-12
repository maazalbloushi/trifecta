library(rgdal)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(shinydashboard)

# -------------------------- #

ui <- dashboardPage(
        skin = "purple",
        
        dashboardHeader(
            title = "Trifecta Group"
        ),
        
        dashboardSidebar(
            disable = TRUE
        ),
    
        dashboardBody(
            fluidRow(
                
                # This is the map view:
                column(width = 9,
                       box(title = "Map of job opportunities vs available talents in Malaysia", width = NULL, solidHeader = TRUE,
                           leafletOutput("themap", height = 500)
                       )
                       #,
                       #column(12,
                      #        tableOutput('table')
                       #)
                ),
                
                # This is the column selection:
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("routeSelect"),
                           checkboxGroupInput("supplydemand", "Show",
                                choices = c(
                                    "Talents" = 'talents',
                                    "Job Opportunities" = 'jobs'
                                ),
                                selected = c('talents', 'jobs')
                           ),
                           p(
                               class = "text-muted",
                               paste("Select to show either talents or job opportunities per state or select both."
                               )
                           )
                       ),
                       box(width = NULL, status = "warning",
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
                       box(width = NULL,
                           sliderInput("obs", "Map zoom:", min = 1, max = 20, value = 10)
                       )
                )
            ),
            fluidRow(
              column(width=12,
                     box(width=5,tableOutput('table')),
                     #box(width=7,DT::dataTableOutput('diffTable'))#table to show state individual data
                     box(width=7,tableOutput('table2'))
              )
            )
        )
)

# -------------------------- #

server <- function(input, output) {
    
    dat <- read.csv(file = "dataset/dataset.csv")
    output$table <- renderTable(dat)
  
    values <- reactiveValues(dat2 = NULL) #for reset dataframe value when new state selected.
    
    observeEvent(input$states, {
      csvSelected <- paste("stateData/",input$states,".csv",sep = "") #filepath creation with selectedInput
      values$dat2 <-read.csv(csvSelected) #load the csv
     })
     
    output$table2 <- renderTable(values$dat2) #render the table
    
    output$themap <- renderLeaflet({
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
            #setView(lng = 101.654390, lat = 3.120111, zoom = zoomV) %>%
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

# -------------------------- #

shinyApp(ui = ui, server = server)