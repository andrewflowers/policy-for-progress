# Example Shiny dashboard with leaflet map of MA cities and towns
#### FIRST RUN example-ma-map.R first
# Click the 'Run App' button above 

library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

# Load data

# Dashboard UI created from shinydashboard::dashboardPage() function
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Massachusetts population quantiles"),
    dashboardSidebar(), # TODO: specify inputs
    dashboardBody(
        fluidRow(box = 12, leafletOutput(outputId = "ma_map")) #,
        #fluidRow(box = 12, dataTableOutput(outputId = "ma_table"))
        )
)

# Define server logic required to interact with map
server <- function(input, output) {
    
    ## add reactive() function to update
    # data_input <- reactive({crs_cities_towns %>% 
    #     filter(NAME == input$muni_name) # TODO: link UI sidebar inputs to "muni_name"
    # })
    
    output$ma_map <- renderLeaflet(
        crs_cities_towns %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ color_pal(estimate)) %>%
            addLegend("bottomleft", 
                      pal = color_pal, 
                      values = ~ estimate,
                      title = "Population quantiles",
                      opacity = 1) %>%
            setView(lat = 42.08, lng = -71.72, zoom = 8)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
