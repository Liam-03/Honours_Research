library(dplyr)
library(tidyr)

# 1) Save data files including only patients who completed the task
mouse_data_csv <- read.csv("mouse_data.csv")
mouse_data_completed <- mouse_data_csv %>%
  filter(complete == "y") %>%
  group_by(PIN, stage) %>%
  mutate(click_count = cumsum(event_type == "Click")) %>%
  ungroup()

complete_csv <- read.csv("complete.csv")
complete_completed <- complete_csv %>%
  filter(complete == "y")

mouse_data_completed_cumsum <- mouse_data_completed %>%
  group_by(PIN, stage) %>%
  mutate(click_count = cumsum(event_type == "Click"))

# 2) Calculate number of clicks per participant
clicks_per_participant <- aggregate(event_type ~ PIN, data = subset(mouse_data_completed, event_type == "Click"), FUN = length)
