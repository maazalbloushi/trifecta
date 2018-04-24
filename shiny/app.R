library(shiny)
library(shinyCustom)
library(leaflet)
library(RColorBrewer)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Matching talents with job vacancies in Malaysia"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      useShinyCustom(slider_delay = "0"),
      customSliderInput("bins", #Use customSliderInput instead of sliderInput
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
      
      selectInput("var", 
                  label = "Please select the state:",
                  choices = c("Perak", 
                              "Pulau Pinang", 
                              "Kedah", 
                              "Perlis", 
                              "Johor", 
                              "Kelantan", 
                              "Melaka", 
                              "Negeri Sembilan", 
                              "Pahang", 
                              "Selangor", 
                              "Trengganu", 
                              "Sabah", 
                              "Sarawak", 
                              "Kuala Lumpur", 
                              "Putrajaya", 
                              "Labuan"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(leafletOutput("map"))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 101.654390, lat = 3.120111, zoom = 10) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(noWrap = TRUE))
  })
  
}

shinyApp(ui = ui, server = server)