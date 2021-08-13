# Data for school districts

library(tidyverse)
library(tidycensus)
library(stringr)
library(sf)

##### TIDYCENSUS #####

# Fetch MA school districts data and geometries
unified_school_districts <- get_acs(
  year = 2019,
  geography = "school district (unified)", 
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
  filter(summary_est > 0) %>%  # filter out school districts with no population
  mutate(district_type = 'unified',
         white_share = round(100*WhiteE / summary_est, 1),
         black_share = round(100*BlackE / summary_est, 1),
         asian_share = round(100*AsianE / summary_est, 1),
         hispanic_share = round(100* HispanicE / summary_est, 1),
         native_share = round(100* NativeE / summary_est, 1),
  )

secondary_school_districts <- get_acs(
  year = 2019,
  geography = "school district (secondary)", 
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
  filter(summary_est > 0) %>%  # filter out school districts with no population
  mutate(district_type = 'secondary',
         white_share = round(100*WhiteE / summary_est, 1),
         black_share = round(100*BlackE / summary_est, 1),
         asian_share = round(100*AsianE / summary_est, 1),
         hispanic_share = round(100* HispanicE / summary_est, 1),
         native_share = round(100* NativeE / summary_est, 1),
  )

elementary_school_districts <- get_acs(
  year = 2019,
  geography = "school district (elementary)", 
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
  filter(summary_est > 0) %>%  # filter out school districts with no population
  mutate(district_type = 'elementary',
         white_share = round(100*WhiteE / summary_est, 1),
         black_share = round(100*BlackE / summary_est, 1),
         asian_share = round(100*AsianE / summary_est, 1),
         hispanic_share = round(100* HispanicE / summary_est, 1),
         native_share = round(100* NativeE / summary_est, 1),
  )

# Merged school data
all_school_districts <- bind_rows(unified_school_districts, secondary_school_districts, elementary_school_districts)

# Clean school data
clean_all_school_districts <- all_school_districts %>% 
  st_transform(crs = 'WGS84') %>% 
  select(NAME, asian_share, black_share, hispanic_share, native_share, white_share, district_type, geometry) %>% 
  mutate(shorter_name = str_extract(NAME, "^([^,]*)"),
         display_name = str_trim(str_sub(shorter_name, end = -16))) %>% 
  select(School = display_name,
         Asian = asian_share,
         Black = black_share,
         Hispanic = hispanic_share,
         Native = native_share,
         White = white_share,
         Type = district_type)

##### DESE #####

# Inter-district school choice program participation
raw_sc_data <- read_csv('./data/raw_school_choice_data.csv') # 232 unified districts?

# Enrollment data
#raw_dese_enrollment <- read_csv("~/repos/policy-for-progress/school-choice/dese_district_grade_race.csv")

##### EXPORT DATA
clean_all_school_districts %>% 
  left_join(raw_sc_data, by = c("School" = "District")) %>% 
  rename(no_future = prior_years_not_future,
         sc_19_20 = status_19_20,
         gr_19_20 = grades_19_20,
         sc_20_21 = status_20_21,
         gr_20_21 = grades_20_21
         ) %>% 
  st_write("./data/school_districts.shp")
