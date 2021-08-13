library(tidyverse)
library(stringr)
library(DT)

library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

library(sf)
library(leaflet)
library(mapview)
library(tmap)
library(webshot)

library(htmlwidgets)
library(shiny)
library(shinyjs)
library(shinydashboard)

remotes::install_github("walkerke/crsuggest")
library(crsuggest)

# simple map of cities and towns, using Census data and geometries from tidycensus (city & town, school district, block group)
ma_cities_towns <- get_acs(geography = "county subdivision", # cities and towns
                  variables = "B01003_001", # population
                  state = "MA",
                  geometry = TRUE) 

color_pal <- colorQuantile(palette = "viridis", domain = ma_cities_towns$estimate, n = 5)

crs_cities_towns <- ma_cities_towns %>% 
  st_transform(crs = "+init=epsg:4326") # TODO: change crs syntax

ex_ma_map <- crs_cities_towns %>%
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

ex_ma_map

# save static map
mapshot(ex_ma_map, file = "example_massachusetts_map.png")

# save dynamic map
saveWidget(ex_ma_map, file = "example_dynamic_massachusetts_map.html")

