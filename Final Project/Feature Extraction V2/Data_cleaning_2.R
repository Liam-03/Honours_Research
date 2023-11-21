library(dplyr)
library(tidyr)

# 1) Save data files including only patients who completed the task
psychiatric_inventory_stages <- c(
  "demographics", "PC-PTSD-5", "GAD-7", "ISI", "DASS", 
  "MOVES", "RAADS-14", "LSAS", "AUDIT", "ASRM", "OCI-R", 
  "YIAT", "PRIME-R", "PHQ-9", "EAT-26", "ASRS-5", "PID-5-BF", 
  "PGSI", "smoking", "SDS", "ICAR")

mouse_data_csv <- read.csv("mouse_data.csv")
mouse_data_completed <- mouse_data_csv %>%
  filter(complete == "y") %>%
  filter(stage %in% psychiatric_inventory_stages) %>%
  group_by(PIN, stage) %>%
  mutate(click_count = cumsum(event_type == "Click")) %>%
  ungroup()

complete_csv <- read.csv("complete.csv")
complete_completed <- complete_csv %>%
  filter(complete == "y") %>%
  filter(stage %in% psychiatric_inventory_stages)

mouse_data_completed_cumsum <- mouse_data_completed %>%
  group_by(PIN) %>%
  mutate(click_count = cumsum(event_type == "Click")) %>%
  filter(stage %in% psychiatric_inventory_stages)

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
