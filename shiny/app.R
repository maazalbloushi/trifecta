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
                       ),
                       column(12,
                              tableOutput('table')
                       )
                       
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
                                           "Pulau Pinang" = 3,
                                           "Kelantan" = 6,
                                           "Perak" = 5,
                                           "Selangor" = 2,
                                           "Kuala Lumpur" = 1,
                                           "Johor" = 4,
                                           "Sarawak" = 7
                                       ),
                                       selected = "2"
                           )
                       ),
                       box(width = NULL,
                           sliderInput("obs", "Map zoom:", min = 1, max = 20, value = 10)
                       )
                )
            )
        )
)

# -------------------------- #

server <- function(input, output) {
    
    dat <- read.csv(file = "dataset/dataset.csv")
    output$table <- renderTable(dat)
    
    
    
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