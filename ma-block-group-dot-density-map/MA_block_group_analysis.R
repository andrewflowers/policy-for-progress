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

# Download race data for MA block groups
ma_race_block_group <- get_acs(
  geography = "block group",
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

# Interactive map for block group non-white share

ma_interactive_map <- mapview(ma_race_block_group, zcol = "non_white_pct") 
ma_interactive_map

# MA block group Census data on race

ma_race_block_group_long <- get_acs(
  geography = "block group",
  variables = c(
    White = "B03002_003",
    Black = "B03002_004",
    # Native = "B03002_005",
    Asian = "B03002_006",
    Hispanic = "B03002_012"
  ),
  summary_var = "B03002_001",
  state = "MA",
  geometry = TRUE
) 

# dot density map
groups <- unique(ma_race_block_group_long$variable)

# ma_race_block_group_dots <- map_df(groups, ~ {
#   ma_race_block_group_long %>%
#     filter(estimate > 0, variable == .x) %>%
#     st_transform(2249) %>%
#     mutate(est50 = as.integer(estimate / 50)) %>% 
#     st_sample(size = .$est50, exact = TRUE) %>%
#     st_sf() %>%
#     mutate(group = .x)
# }) %>%
#   slice_sample(prop = 1)
# 
# # save as RDS file
# saveRDS(ma_race_block_group_dots, file = "ma_race_block_group_dots.rds")

# save dot density map
dot_density_2019_map <- tm_shape(ma_race_block_group_dots %>% rename("Race" = "group")) +
  tm_dots(col = "Race", palette = "Set1", size = 0.002) +
  tm_layout(frame = FALSE) 

# layer cities and towns on top
ma_cities_towns <- st_transform(county_subdivisions(state = "MA", cb = TRUE), crs = 2249)

tmap_mode("plot")
# tmap_mode("view")

dot_density_2019_map +
  tm_shape(ma_cities_towns) + 
  tm_polygons(alpha = 0, legend.show = c(FALSE, TRUE)) +
  tm_add_legend(type = "fill",
                col = c(tmaptools::get_brewer_pal("Set1", 4, plot = FALSE)),
                labels = c("Asian", "Black", "Hispanic", "White"),
                title = "Race") +
  tm_layout(title = "Massachusetts racial segregation in 2019",
            title.size = 1.5,
            frame = FALSE, 
            legend.position = c('center', 'bottom'))


## Replicate for 2010
decennial_variables <- load_variables(year = 2010, dataset = 'sf1', cache = TRUE)

ma_race_block_group_long_2010 <- get_decennial(
  year = 2010,
  geography = "block group",
  variables = c(
    White = "P005003",
    Black = "P005004",
    # Native = "P005005",
    Asian = "P005006",
    Hispanic = "P005010"
  ),
  summary_var = "P003001",
  state = "MA",
  geometry = TRUE
) 

# create dot density map
# ma_race_block_group_dots_2010 <- map_df(groups, ~ {
#   ma_race_block_group_long_2010 %>%
#     filter(value > 0, variable == .x) %>%
#     st_transform(2249) %>%
#     mutate(est50 = as.integer(value / 50)) %>% 
#     st_sample(size = .$est50, exact = TRUE) %>%
#     st_sf() %>%
#     mutate(group = .x)
# }) %>%
#   slice_sample(prop = 1)
# 
# # save as RDS file
# saveRDS(ma_race_block_group_dots_2010, file = "mma_race_block_group_dots_2010.rds")

# 2010 dot density map

dot_density_2010_map <- tm_shape(ma_race_block_group_dots_2010 %>% rename("Race" = "group")) +
  tm_dots(col = "Race", palette = "Set1", size = 0.002) +
  tm_layout(frame = FALSE) 

dot_density_2010_map +
  tm_shape(ma_cities_towns) + 
  tm_polygons(alpha = 0, legend.show = c(FALSE, TRUE)) +
  tm_add_legend(type = "fill",
                col = c(tmaptools::get_brewer_pal("Set1", 4, plot = FALSE)),
                labels = c("Asian", "Black", "Hispanic", "White"),
                title = "Race") +
  tm_layout(title = "Massachusetts racial segregation in 2010",
            title.size = 1.5,
            frame = FALSE, 
            legend.position = c('center', 'bottom'))


# facet mapping with 2019 and 2010
combined_dot_data <- rbind(ma_race_block_group_dots_2010 %>% mutate(year = 2010),
                           ma_race_block_group_dots %>% mutate(year = 2019))


combined_map <- tm_shape(combined_dot_data %>% rename("Race" = "group")) +
  tm_facets(by = "year", scale.factor = 4, by = c(1,2)) +
  tm_dots(col = "Race", palette = "Set1", size = 0.002) +
  tm_layout(frame = FALSE) 

combined_map +
  tm_shape(ma_cities_towns) + 
  tm_polygons(alpha = 0, legend.show = c(FALSE, TRUE)) +
  tm_add_legend(type = "fill",
                col = c(tmaptools::get_brewer_pal("Set1", 4, plot = FALSE)),
                labels = c("Asian", "Black", "Hispanic", "White"),
                title = "Race") +
  tm_layout(title = "",
            title.size = 1.5,
            frame = FALSE, 
            legend.position = c('center', 'bottom'))

# ## simple facet example
# facet_map <- tm_shape(ma_race,
#                       projection = sf::st_crs(2249)) + 
#   tm_facets(by = "variable", scale.factor = 4) + 
#   tm_fill(col = "percent",
#           style = "quantile",
#           n = 7,
#           palette = "Blues")
# 
# facet_map
# ## simple facet example

## Replicate for 2000 -- NOT WORKING, NEED TO FIX
# What are the correct codes?
ma_race_block_group_long_2000 <- get_decennial(
  year = 2000,
  geography = "block group",
  variables = c(
    White = "P005003",
    Black = "P005004",
    # Native = "P005005",
    Asian = "P005006",
    Hispanic = "P005010"
  ),
  summary_var = "P003001",
  state = "MA",
  geometry = TRUE
) 
