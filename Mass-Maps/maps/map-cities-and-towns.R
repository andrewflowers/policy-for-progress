library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)

filtered_data <- reactive({ # call this reactive variable "data_for_map"
  crs_cities_towns %>%
    filter(Asian >= input$asianRange[1]) %>% filter(Asian <= input$asianRange[2]) %>% 
    filter(Black >= input$blackRange[1]) %>% filter(Black <= input$blackRange[2]) %>% 
    filter(Hispanic >= input$hispanicRange[1]) %>% filter(Hispanic <= input$hispanicRange[2]) %>% 
    filter(Native >= input$nativeRange[1]) %>% filter(Native <= input$nativeRange[2]) %>% 
    filter(White >= input$whiteRange[1]) %>% filter(White <= input$whiteRange[2]) 
})

map_cities_and_towns <- renderLeaflet(
  filtered_data() %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(label = ~ CityTown, #str_extract(NAME, "^([^,]*)"),
                stroke = TRUE, opacity = 1, weight = 0.25, color= 'black',
                smoothFactor = 0,
                fillOpacity = 0.7,
                fillColor = ~ color_pal(filtered_data()[[input$selectedRace]]) 
    ) %>%
    addLegend("bottomleft", 
              pal = color_pal, 
              values = ~ crs_cities_towns[[input$selectedRace]], #asian_share,
              bins = 5,
              title = paste(input$selectedRace, " population %:"), #"Asian population %:",
              opacity = 1) %>%
    setView(lat = 42.08, lng = -71.72, zoom = 8)
)