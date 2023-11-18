library(dplyr)
library(tidyr)

# Feature extraction
# 1. Avg velocity in bin 1 per click
timestamp_binned_df <- mouse_data_completed_cumsum %>%
  group_by(PIN, stage) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>%
  mutate(distance = if_else(event_type == "Click", 0, distance(x, y, lag(x), lag(y))))%>%
  filter((scroll_position_x == lag(scroll_position_x) & scroll_position_y == lag(scroll_position_y)) | event_type == "Click") %>% # Filter distance changes from scrolling
  group_by(PIN, stage, click_count) %>%
  mutate(time = if_else(event_type == "Click", 0, timestamp-lag(timestamp))) %>%
  mutate(time_cumsum = cumsum(time)) %>% 
  mutate(time_bins = cut(time_cumsum, breaks = seq(0, max(time_cumsum) + 100, by = 100), include.lowest = TRUE, labels = FALSE))

velocity_bin_1 <- timestamp_binned_df %>% 
  filter(time_bins == 1) %>%
  group_by(PIN, stage, click_count) %>%
  summarize(
    sm_distance = sum(distance),
    sm_time = sum(time),
    ratio = round(sm_distance / sm_time, 2)
  ) %>%
  filter(is.finite(ratio))

timestamp_binned_df <- mouse_data_completed_cumsum %>%
  group_by(PIN, stage) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>%
  mutate(distance = if_else(event_type == "Click", 0, distance(x, y, lag(x), lag(y))))%>%
  filter((scroll_position_x == lag(scroll_position_x) & scroll_position_y == lag(scroll_position_y)) | event_type == "Click") %>% # Filter distance changes from scrolling
  group_by(PIN, stage, click_count) %>%
  mutate(time = if_else(event_type == "Click", 0, timestamp-lag(timestamp))) %>%
  mutate(time_cumsum = cumsum(time)) %>% 
  mutate(time_bins = cut(time_cumsum, breaks = seq(0, max(time_cumsum) + 100, by = 100), include.lowest = TRUE, labels = FALSE))

velocity_bin_1_PIN <- timestamp_binned_df %>% 
  filter(time_bins == 1) %>%
  group_by(PIN) %>%
  summarize(
    sm_distance = sum(distance),
    sm_time = sum(time),
    ratio = round(sm_distance / sm_time, 2)
  ) %>%
  filter(is.finite(ratio))

# 2. Median acceleration in bin 1 per click
timestamp_binned_acc_df <- mouse_data_completed_cumsum %>%
  group_by(PIN, stage, click_count) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>%
  mutate(distance = if_else(event_type == "Click", 0, distance(x, y, lag(x), lag(y))))%>%
  mutate(time = if_else(event_type == "Click", 0, timestamp-lag(timestamp))) %>%
  mutate(time_cumsum = cumsum(time)) %>% 
  mutate(time_bins = cut(time_cumsum, breaks = seq(0, max(time_cumsum) + 100, by = 100), include.lowest = TRUE, labels = FALSE)) %>%
  mutate(velocity = distance/time) %>%
  mutate(velocity_diff = velocity - lag(velocity)) %>%
  mutate(acceleration = velocity_diff / (timestamp - lag(timestamp))) %>%
  filter((scroll_position_x == lag(scroll_position_x) & scroll_position_y == lag(scroll_position_y)) | event_type == "Click") #Filter distance changes from scrolling

median_acceleration_bin_1and2 <- timestamp_binned_acc_df %>% 
  filter(time_bins == 1 | time_bins == 2) %>%
  group_by(PIN, stage, click_count) %>%
  summarize(median_acc = median(acceleration, na.rm = TRUE)) %>%
  filter(!is.na(median_acc))

median_acceleration_bin_1and2_PIN <- median_acceleration_bin_1and2 %>%
  group_by(PIN) %>%
  summarise(median_acc_bin1_2 = median(median_acc))

# Inferential statistics preparation
PHQ9_velocity_bin1_merged <- merge(PHQ9_sums, velocity_bin_1_PIN, by = "PIN")
PHQ9_median_acceleration_bin_1and2_merged <- merge(PHQ9_sums, median_acceleration_bin_1and2_PIN, by = "PIN")

# Wilcoxon test
PHQ9_group_velocity_bin1 <- PHQ9_velocity_bin1_merged$ratio[PHQ9_velocity_bin1_merged$Illness_status == "MD"]
PHQ9_subclinical_velocity_bin1 <- PHQ9_velocity_bin1_merged$ratio[PHQ9_velocity_bin1_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_velocity_bin1, PHQ9_subclinical_velocity_bin1)

PHQ9_group_acceleration_bin_1and2 <- PHQ9_median_acceleration_bin_1and2_merged$median_acc_bin1_2[PHQ9_median_acceleration_bin_1and2_merged$Illness_status == "MD"]
PHQ9_subclinical_group_acceleration_bin_1and2 <- PHQ9_median_acceleration_bin_1and2_merged$median_acc_bin1_2[PHQ9_median_acceleration_bin_1and2_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_acceleration_bin_1and2, PHQ9_subclinical_group_acceleration_bin_1and2)
