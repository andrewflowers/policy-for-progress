# Data for cities and towns

library(tidyverse)
library(tidycensus)
library(sf)
library(stringr)
library(DT)

# Fetch MA data and geometries
ma_cities_towns <- get_acs(
  year = 2019,
  geography = "county subdivision", # cities and towns
  variables = c(
    White = "B03002_003",
    Black = "B03002_004",
    Native = "B03002_005",
    Asian = "B03002_006",
    Hispanic = "B03002_012"
  ),
  summary_var = "B03002_001",
  state = "MA",
  geometry = TRUE,
  output = "wide"
) %>% 
  filter(summary_est > 0) %>%  # filter out county subdivisions with no population
  mutate(white_share = round(100*WhiteE / summary_est, 1),
         black_share = round(100*BlackE / summary_est, 1),
         asian_share = round(100*AsianE / summary_est, 1),
         hispanic_share = round(100* HispanicE / summary_est, 1),
         native_share = round(100* NativeE / summary_est, 1),
  )

crs_cities_towns <- ma_cities_towns %>% 
  st_transform(crs = 'WGS84') %>% 
  select(NAME, asian_share, black_share, hispanic_share, native_share, white_share, geometry) %>% 
  mutate(shorter_name = str_extract(NAME, "^([^,]*)"),
         display_name = str_trim(str_sub(shorter_name, end = -5)),
         cityTown = str_trim(str_sub(shorter_name, start = (nchar(shorter_name)-4)))) %>% 
  select(CityTown = display_name,
         Asian = asian_share,
         Black = black_share,
         Hispanic = hispanic_share,
         Native = native_share,
         White = white_share)

crs_cities_towns %>% st_write("./data/cities_and_towns.shp")
