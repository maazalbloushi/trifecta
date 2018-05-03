library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(geojsonio)
library(sp)
library(sf)

# USER INTERFACE:
ui <- dashboardPage(
        skin = "purple",
        
        dashboardHeader(
            #disable = TRUE
            title = "Trifecta Group"
        ),
        
        dashboardSidebar(
            disable = TRUE
            #sidebarMenuOutput("menu")
        ),
    
        dashboardBody(
            fluidRow(
                
                # This is the map view:
                column(width = 9,
                       box(title = "Map of job opportunities vs available talents in Malaysia", width = NULL, solidHeader = TRUE,
                           leafletOutput("themap", height = 400),
                           p(),
                           actionButton("recalc", "Reset view")
                       )
                       
                ),
                
                # This is the column selection:
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("routeSelect"),
                           checkboxGroupInput("directions", "Show",
                                choices = c(
                                    Talents = 1,
                                    Job_Opportunities = 2
                                ),
                                selected = c(1, 2)
                           ),
                           p(
                               class = "text-muted",
                               paste("Select to show either talents or job opportunities per state or select both."
                               )
                           ),
                           actionButton("zoomButton", "Zoom to Area")
                       ),
                       box(width = NULL, status = "warning",
                           selectInput("interval", "States",
                                       choices = c(
                                           "Perlis" = 10,
                                           "Pulau Pinang" = 20,
                                           "Kedah" = 30,
                                           "Kelantan" = 40,
                                           "Terengganu" = 50,
                                           "Pahang" = 60,
                                           "Perak" = 70,
                                           "Selangor" = 80,
                                           "Kuala Lumpur" = 90,
                                           "Negeri Sembilan" = 100,
                                           "Melaka" = 110,
                                           "Johor" = 120,
                                           "Sabah" = 130,
                                           "Sarawak" = 140
                                       ),
                                       selected = "40"
                           )
                       ),
                       box(width = NULL,
                           sliderInput("obs", "Map zoom:", min = 1, max = 15, value = 8)
                       )
                )
            )
        )
)

# SERVER:
server <- function(input, output) {
    
    output$themap <- renderLeaflet({
        zoomV <- input$obs
        geoData <- readLines("json/malaysia.geojson") %>% paste(collapse = "\n")
        leaflet() %>%
            setView(lng = 101.654390, lat = 3.120111, zoom = zoomV) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(noWrap = TRUE))%>%
            addGeoJSON(geoData, weight = 2, color = "#000", fill = FALSE)
    })
    
}

shinyApp(ui = ui, server = server)