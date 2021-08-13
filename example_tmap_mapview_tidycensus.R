library(tidyverse)
library(tidycensus)

library(shiny)
library(shinyjs)

library(tigris)
library(sf)
library(mapview)
library(tmap)
library(htmlwidgets)

options(tigris_use_cache = TRUE)

remotes::install_github("walkerke/crsuggest")
library(crsuggest)

# Download shape files for MA cities/towns and tracts
ma_cities_towns <- county_subdivisions(state = "MA", cb = TRUE)
ma_tracts <- tracts(state = "MA", cb = TRUE)

# Interactively browse MA cities/towns and tracts with mapview
mapview(ma_cities_towns)
mapview(ma_tracts)

# Suggest CRS for MA cities and towns

glimpse(suggest_crs(ma_cities_towns)) # same for MA Census tracts

# recommended projected CRS is "NAD83 / Massachusetts Mainland (ftUS)" with code = 2249

ma_projeced_crs <- st_transform(ma_cities_towns, crs = 2249)

mapview(ma_projeced_crs) # Recommended projection for MA cities and towns, interactive in MapBox

ma_crs <- glimpse(suggest_crs(ma_cities_towns))$crs_code[1] %>% as.numeric()

# Layer Census tracts on Cities/Towns
mapview(ma_projeced_crs) + st_transform(ma_tracts, crs = 2249)

### Get ACS data on MA cities and towns

# Example: Median household income

ma_med_hh_income <- get_acs(geography = "county subdivision",
                            variables = c(hhincome = "B19013_001"),
                            state = "MA",
                            geometry = TRUE)

ggplot(ma_med_hh_income, aes(fill = estimate)) + 
  geom_sf()

mapview(st_transform(ma_med_hh_income, crs = 2249), zcol = 'estimate') # Simple interactive choropleth map by median HH income

## Pull race data by MA city/town

ma_race_cities_towns <- get_acs(
  geography = "county subdivision",
  variables = c(
    White = "B03002_003",
    Black = "B03002_004",
    # Native = "B03002_005",
    Asian = "B03002_006",
    Hispanic = "B03002_012"
  ),
  summary_var = "B03002_001",
  state = "MA",
  geometry = TRUE,
  output = "wide"
) %>%
  mutate(non_white_pct = 1- (WhiteE / summary_est))

## tmap example map

tm_shape(ma_race_cities_towns,
         projection = sf::st_crs(ma_crs)) + 
  tm_polygons(col = "non_white_pct", 
              style = "jenks",
              n = 5, 
              palette = "viridis",
              title = "Non-white population share", 
              legend.hist = TRUE) +
  tm_layout(title = "Percent non-white\nby City or Town",
            frame = FALSE,
            legend.outside = TRUE) 

# Interactive map example
ma_interactive_map <- mapview(ma_race_cities_towns, zcol = "non_white_pct") # interactive map of MA cities/towns by percent non-white

ma_interactive_map

htmlwidgets(ma_interactive_map, "ma_interactive_map.html")

# Explore color palettes
# tmaptools::palette_explorer()



####################### OTHER NOTES

# Example
tm_shape(ma_race_cities_towns) + 
  tm_polygons() + 
  tm_bubbles(size = "estimate", alpha = 0.5, 
             col = "navy")
 
# facet mapping
facet_map <- tm_shape(ma_race,
                      projection = sf::st_crs(2249)) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 7,
          palette = "Blues")
 
facet_map

# dot-density mapping

groups <- unique(ma_race_cities_towns$variable)


ma_dots <- map_df(groups, ~ {
  ma_race_cities_towns %>%
    filter(variable == .x) %>%
    st_transform(2249) %>%
    mutate(est50 = as.integer(estimate / 50)) %>%
    mutate(est50 = as.integer(estimate / 50)) %>%
    st_sample(size = .$est50, exact = TRUE) %>%
    st_sf() %>%
    mutate(group = .x)
}) %>%
  slice_sample(prop = 1)

