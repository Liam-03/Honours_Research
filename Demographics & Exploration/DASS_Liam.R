# Import relevant libraries
library(dplyr)
library(ggplot2)
library(tidyr)

DASS_File <- read.csv("DASS.csv") # Read the csv file and store 

# DASS categories
DASS_sums <- DASS_File %>%
  select(PIN, item, response) %>%
  group_by(PIN) %>%
  summarise(sum = sum(as.numeric(response[item %in% c(1:21)], na.rm = TRUE)))

print(DASS_sums)
