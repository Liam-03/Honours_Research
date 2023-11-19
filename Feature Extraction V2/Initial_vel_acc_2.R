library(dplyr)
library(tidyr)

# Feature extraction
# 1. Avg velocity in bin 1 per click
timestamp_binned_df <- length_differences_df %>%
  group_by(PIN, stage, movement_count) %>%
  mutate(time = if_else(!is.na(timestamp-lag(timestamp)), timestamp-lag(timestamp), 0)) %>%
  mutate(time_cumsum = cumsum(time)) %>%
  mutate(time_bins = cut(time_cumsum, breaks = seq(0, max(time_cumsum) + 100, by = 100), include.lowest = TRUE, labels = FALSE)) %>%
  mutate(velocity = distance/time) %>%
  mutate(velocity_diff = velocity - lag(velocity)) %>%
  mutate(acceleration = velocity_diff / (timestamp - lag(timestamp)))

velocity_bin_2_df <- timestamp_binned_df %>% 
  filter(time_bins %in% c(1,2)) %>%
  group_by(PIN, stage, movement_count) %>%
  summarize(
    sm_distance = sum(distance, na.rm = TRUE),
    sm_time = sum(time, na.rm = TRUE),
    velocity_bin_1_per_click = sm_distance / sm_time
  ) %>%
  filter(is.finite(velocity_bin_1_per_click))

velocity_200ms <- velocity_bin_2_df %>%
  group_by(PIN) %>%
  summarise(
    median_velocity_200ms = median(velocity_bin_1_per_click, na.rm = TRUE),
    mean_velocity_200ms = mean(velocity_bin_1_per_click, na.rm = TRUE),
    max_velocity_200ms = max(velocity_bin_1_per_click, na.rm = TRUE))

acceleration_bin_2_df <- timestamp_binned_df %>% 
  filter(time_bins %in% c(1,2)) %>%
  group_by(PIN, stage, movement_count) %>%
  filter(is.finite(acceleration)) %>%
  summarize(
    median_acc_200ms_per_click = median(acceleration, na.rm = TRUE),
    max_acc_200ms_per_click = max(acceleration, na.rm = TRUE))

acceleration_200ms <- acceleration_bin_2_df %>%
  group_by(PIN) %>%
  summarise(
    median_median_acceleration_200ms = median(median_acc_200ms_per_click, na.rm = TRUE),
    median_max_acceleration_200ms = median(max_acc_200ms_per_click, na.rm = TRUE))

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
