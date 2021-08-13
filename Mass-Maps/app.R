# Mass Maps
library(tidyverse)
library(stringr)
library(DT)

library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

library(sf)
library(leaflet)
# library(mapview)
# library(tmap)
# library(webshot)

# library(htmlwidgets)
library(shiny)
# library(shinyjs)
library(shinydashboard)

# remotes::install_github("walkerke/crsuggest")
# library(crsuggest)

# Load ui and map files
source("./ui/ui-cities-and-towns.R")
source("./ui/ui-school-districts.R")
source("./maps/map-cities-and-towns.R")

# Load data -- statistics and geometry objects
crs_cities_towns <- st_read("./data/cities_and_towns.shp")
crs_school_districts <- st_read("./data/school_districts.shp") %>% filter(Type !='secondary')

color_pal <- colorNumeric(palette = "viridis", domain = NULL) 

# Dashboard UI created from shinydashboard::dashboardPage() function
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Mass Maps"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cities and Towns", tabName = "cities_and_towns", icon = icon("city")),
      menuItem("School Districts", tabName = "school_districts", icon = icon("school"))
    )
    
  ), # TODO: specify inputs, if any
  dashboardBody(
    
      tabItems(
        cities_and_towns_ui,
        school_districts_ui
        )
  )
  )

# Define server logic required to interact with map
server <- function(input, output, session) {
  
  filtered_data <- reactive({ # call this reactive variable "data_for_map"
    crs_cities_towns %>%
      filter(Asian >= input$asianRange[1]) %>% filter(Asian <= input$asianRange[2]) %>%
      filter(Black >= input$blackRange[1]) %>% filter(Black <= input$blackRange[2]) %>%
      filter(Hispanic >= input$hispanicRange[1]) %>% filter(Hispanic <= input$hispanicRange[2]) %>%
      filter(Native >= input$nativeRange[1]) %>% filter(Native <= input$nativeRange[2]) %>%
      filter(White >= input$whiteRange[1]) %>% filter(White <= input$whiteRange[2])
  })
  
  filtered_school_districts <- reactive({ # call this reactive variable "data_for_map"
    crs_school_districts %>%
      filter(Asian >= input$asianRangeSchools[1]) %>% filter(Asian <= input$asianRangeSchools[2]) %>%
      filter(Black >= input$blackRangeSchools[1]) %>% filter(Black <= input$blackRangeSchools[2]) %>%
      filter(Hispanic >= input$hispanicRangeSchools[1]) %>% filter(Hispanic <= input$hispanicRangeSchools[2]) %>%
      filter(Native >= input$nativeRangeSchools[1]) %>% filter(Native <= input$nativeRangeSchools[2]) %>%
      filter(White >= input$whiteRangeSchools[1]) %>% filter(White <= input$whiteRangeSchools[2])
  })
  
  data_displayed <- reactive({
    filtered_data() %>% 
      st_set_geometry(NULL) %>% 
      arrange(desc(input$selectedRace)) %>% 
      rename(`City/Town` = CityTown
             )
  })
  
  school_districts_displayed <- reactive({
    filtered_school_districts() %>% 
      st_set_geometry(NULL) %>% 
      arrange(desc(input$selectedRace)) %>% 
      select(-no_future, -sc_19_20, -gr_19_20) %>% 
      rename(`School choice?` = sc_20_21,
             `Grades:` = gr_20_21)
  })

  output$ma_map <- renderLeaflet(
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
  
  output$school_districts_map <- renderLeaflet(
    filtered_school_districts() %>%
      leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(label = ~ School, #str_extract(NAME, "^([^,]*)"),
                  stroke = TRUE, opacity = 1, weight = 0.25, color= 'black',
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  fillColor = ~ color_pal(filtered_school_districts()[[input$selectedRace]])
      ) %>%
      addLegend("bottomleft",
                pal = color_pal,
                values = ~ crs_school_districts[[input$selectedRace]], #asian_share,
                bins = 5,
                title = paste(input$selectedRace, " population %:"), #"Asian population %:",
                opacity = 1) %>%
      setView(lat = 42.08, lng = -71.72, zoom = 8)
  )
  
  observeEvent(input$selectedRace,
               {
                 
                 # Update palette color function
                 color_pal <-  colorNumeric(palette = "viridis", domain = crs_cities_towns[[input$selectedRace]])

                 # Redraw Map
                 leafletProxy("ma_map", session=session, data=filtered_data()) %>%
                   clearShapes() %>%
                   clearControls() %>% 
                   addPolygons(label = ~ CityTown, #str_extract(NAME, "^([^,]*)"),
                               stroke = TRUE, opacity = 1, weight = 0.25, color= 'black',
                               smoothFactor = 0,
                               fillOpacity = 0.7,
                               fillColor = ~ color_pal(filtered_data()[[input$selectedRace]])
                   ) %>% 
                   addLegend("bottomleft", 
                             pal = color_pal, 
                             values = ~ crs_cities_towns[[input$selectedRace]],
                             bins = 5,
                             title = paste(input$selectedRace, " population %:"),
                             opacity = 1)

               })
  
  observeEvent(input$selectedRaceSchools,
               {
                 
                 # Update palette color function
                 color_pal <-  colorNumeric(palette = "viridis", domain = crs_school_districts[[input$selectedRaceSchools]])
                 
                 # Redraw Map
                 leafletProxy("school_districts_map", session=session, data=filtered_school_districts()) %>%
                   clearShapes() %>%
                   clearControls() %>% 
                   addPolygons(label = ~ School, #str_extract(NAME, "^([^,]*)"),
                               stroke = TRUE, opacity = 1, weight = 0.25, color= 'black',
                               smoothFactor = 0,
                               fillOpacity = 0.7,
                               fillColor = ~ color_pal(filtered_school_districts()[[input$selectedRaceSchools]])
                   ) %>% 
                   addLegend("bottomleft", 
                             pal = color_pal, 
                             values = ~ crs_school_districts[[input$selectedRaceSchools]],
                             bins = 5,
                             title = paste(input$selectedRaceSchools, " population %:"),
                             opacity = 1)
                 
               })

  output$ma_table <- renderDataTable(data_displayed())
  
  output$school_districts_table <- renderDataTable(school_districts_displayed())

  
}

# Run the application 
shinyApp(ui = ui, server = server)
