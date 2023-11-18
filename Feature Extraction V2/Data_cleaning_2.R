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

# 3) Inferentials prep
PHQ9_clicks_PIN_merged <- merge(PHQ9_sums, clicks_per_participant, by = "PIN")
PHQ9_classification <- PHQ9_clicks_PIN_merged$Illness_status
PHQ9_clicks <- PHQ9_clicks_PIN_merged$event_type
data_PHQ9_mean_click_duration <- data.frame(Classification = PHQ9_clicks_PIN_merged$Illness_status, Ratio = PHQ9_clicks_PIN_merged$event_type)

# 4) Wilcoxon test
PHQ9_group_clicks <- PHQ9_clicks_PIN_merged$event_type[PHQ9_clicks_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_clicks <- PHQ9_clicks_PIN_merged$event_type[PHQ9_clicks_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_clicks, PHQ9_subclinical_group_clicks)
