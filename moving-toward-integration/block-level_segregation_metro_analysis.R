setwd("./moving-toward-integration/")

library(tidyverse)

raw_metro_data <- read_csv("Block dissimilarity for the 60 SMSAs.csv")

raw_metro_data %>% 
  filter(`SMSA name` == 'Boston, MA SMSA')