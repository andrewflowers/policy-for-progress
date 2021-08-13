library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

library(mapview)
library(stringr)
library(tmap)
library(sf)

remotes::install_github("walkerke/crsuggest")
library(crsuggest)


# Inter-district school choice program participation
raw_sc_data <- read_csv('~/repos/policy-for-progress/school-choice/raw_school_choice_data.csv') # 232 unified districts?

# Map of MA school districts
elementary_geo <- school_districts(state = 'MA', cb = TRUE, type = 'elementary')
secondary_geo <- school_districts(state = 'MA', cb = TRUE, type = 'secondary')
unified_geo <- school_districts(state = 'MA', cb = TRUE)

mapview(unified_geo) + mapview(elementary_geo)

# Demographic data on school districts from the 2015-2019 5-year ACS

race_unified <- get_acs(
  geography = 'school district (unified)',
  variables = c(
    White = 'B03002_003',
    Black = 'B03002_004',
    # Native = 'B03002_005',
    Asian = 'B03002_006',
    Hispanic = 'B03002_012'
  ),
  summary_var = 'B03002_001',
  state = 'MA',
  output = 'wide',
  geometry = TRUE
) %>%
  mutate(district_type = 'unified',
         non_white_pct = (1- (WhiteE / summary_est))*100)

race_elem <- get_acs(
  geography = 'school district (elementary)',
  variables = c(
    White = 'B03002_003',
    Black = 'B03002_004',
    # Native = 'B03002_005',
    Asian = 'B03002_006',
    Hispanic = 'B03002_012'
  ),
  summary_var = 'B03002_001',
  state = 'MA',
  output = 'wide',
  geometry = TRUE
) %>%
  mutate(district_type = 'elementary',
         non_white_pct = (1- (WhiteE / summary_est))*100)

race_sec <- get_acs(
  geography = 'school district (secondary)',
  variables = c(
    White = 'B03002_003',
    Black = 'B03002_004',
    # Native = 'B03002_005',
    Asian = 'B03002_006',
    Hispanic = 'B03002_012'
  ),
  summary_var = 'B03002_001',
  state = 'MA',
  output = 'wide',
  geometry = TRUE
) %>%
  mutate(district_type = 'secondary',
         non_white_pct = (1- (WhiteE / summary_est))*100)

# Interactive map with two layers
mapview(race_unified) + mapview(race_elem)


# Merged school data
race_all <- bind_rows(race_unified, race_elem, race_sec)

# Thematic map of unified districts
glimpse(suggest_crs(race_unified))

tm_shape(bind_rows(race_unified, race_elem),
         projection = sf::st_crs(2249)) + 
  tm_polygons(col = "non_white_pct", 
              style = "jenks",
              n = 5, 
              palette = "viridis",
              title = "Non-white students share (%)"
              ) +
  tm_layout(title = "Massachusetts Public Schools",
            frame = FALSE,
            legend.outside = FALSE) 

# Import DESE enrollment data
raw_dese_enrollment <- read_csv("dese_district_grade_race.csv")

race_sc_data <- race_all %>% 
  mutate(short_name = str_trim(str_extract(NAME, pattern = '^.*(?=(\\School))'))) %>% 
  mutate(stripped_short_name = str_replace_all(short_name, pattern = 'Regional', replacement = '')) %>% 
  mutate(stripped_short_name = str_trim(str_replace_all(stripped_short_name, pattern = '-', replacement = ' '))) %>% 
  left_join(raw_sc_data, by = c("stripped_short_name" = "District"))


tm_shape(race_sc_data,  #%>% filter(district_type != 'secondary'),
         projection = sf::st_crs(2249)) + 
  tm_polygons(col = "status_20_21", 
              style = "jenks",
              n = 3, 
              palette = "viridis",
              title = "School choice district?"
  ) +
  tm_layout(title = "MA School Choice Districts",
            frame = TRUE,
            legend.outside = FALSE) 


