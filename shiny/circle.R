library(rgdal)
library(leaflet)
library(shiny)

# -------------------------- #

ui <- fluidPage(

    mainPanel(
        
        column(6,
               box(width=NULL,
                tableOutput('table')
               )
        ),
        
        leafletOutput("themap", height = 500, width = 900),
        box(width = NULL,
            sliderInput("obs", "Map zoom:", min = 1, max = 15, value = 8)
        )
        
    )
)


# -------------------------- #

server <- function(input, output) {
    
    dat <- read.csv(file = "dataset/dataset.csv")
    output$table <- renderTable(dat)
    
    output$themap <- renderLeaflet({
        zoomV <- input$obs
        geoData <- readLines("json/malaysia.geojson") %>% paste(collapse = "\n")
        leaflet(dat) %>% addTiles() %>%
            #setView(lng = 101.654390, lat = 3.120111, zoom = 10) %>%
            setView(lng = 101.654390, lat = 3.120111, zoom = zoomV) %>%
            addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
            addGeoJSON(geoData, weight = 2, color = "#000", fill = FALSE) %>%
            addCircles(lng = ~Longitude, lat = ~Latitude, weight = 2,
                       radius = ~sqrt(Demand) * 50, popup = ~States, color = 'red'
            ) %>%
            addCircles(lng = ~Longitude, lat = ~Latitude, weight = 2,
                       radius = ~sqrt(Supply) * 50, popup = ~States, color = 'blue'
            )
            # addCircleMarkers(
            #     radius = 6,
            #     color = 'red',
            #     stroke = FALSE, fillOpacity = 0.5
            # )
    })
}

# -------------------------- #

shinyApp(ui, server)