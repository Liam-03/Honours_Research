library(dplyr)
library(tidyr)

# Feature extraction
acceleration_df <- velocity_df %>%
  mutate(acceleration = velocity_diff / (timestamp - lag(timestamp)))

max_acceleration_overall_PIN <- acceleration_df %>%
  group_by(PIN) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(max_acceleration = max(acceleration))

max_acceleration_overall_PIN_stage <- acceleration_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(max_acceleration = max(acceleration))

mean_max_acceleration <- acceleration_df %>%
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(max_acceleration = max(acceleration)) %>%
  group_by(PIN) %>%
  summarise(mean_max_acceleration = mean(max_acceleration))

min_acceleration_overall_PIN <- acceleration_df %>% # min acceleration = deceleration
  group_by(PIN) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(min_acceleration = min(acceleration))

min_acceleration_overall_PIN_stage <- acceleration_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(min_acceleration = min(acceleration))

mean_min_acceleration <- acceleration_df %>% # should probably filter out 0 acceleration
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(min_acceleration = min(acceleration)) %>%
  group_by(PIN) %>%
  summarise(mean_min_acceleration = mean(min_acceleration))

median_acceleration_PIN <- acceleration_df %>%
  group_by(PIN) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(median_acceleration = median(acceleration))

median_acceleration_PIN_stage <- acceleration_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(median_acceleration = median(acceleration))

q1_acceleration_PIN <- acceleration_df %>%
  group_by(PIN) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(Q1_acceleration = quantile(acceleration, probs = 0.25))

q3_acceleration_PIN <- acceleration_df %>%
  group_by(PIN) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(Q3_acceleration = quantile(acceleration, probs = 0.75))

# Inferential statistics preparation
PHQ9_max_acc_merged <- merge(PHQ9_sums, max_acceleration_overall_PIN, by = "PIN")
PHQ9_mean_max_acc_merged <- merge(PHQ9_sums, mean_max_acceleration, by = "PIN")
PHQ9_min_acc_merged <- merge(PHQ9_sums, min_acceleration_overall_PIN, by = "PIN")
PHQ9_mean_min_acc_merged <- merge(PHQ9_sums, mean_min_acceleration, by = "PIN")
PHQ9_median_acc_merged <- merge(PHQ9_sums, median_acceleration_PIN, by = "PIN")
PHQ9_q1_acc_merged <- merge(PHQ9_sums, q1_acceleration_PIN, by = "PIN")
PHQ9_q3_acc_merged <- merge(PHQ9_sums, q3_acceleration_PIN, by = "PIN")

# Wilcoxon tests
PHQ9_group_max_acc <- PHQ9_max_acc_merged$max_acceleration[PHQ9_max_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_max_acc <- PHQ9_max_acc_merged$max_acceleration[PHQ9_max_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_max_acc, PHQ9_subclinical_max_acc)

PHQ9_group_mean_max_acc <- PHQ9_mean_max_acc_merged$mean_max_acceleration[PHQ9_mean_max_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_mean_max_acc <- PHQ9_mean_max_acc_merged$mean_max_acceleration[PHQ9_mean_max_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_mean_max_acc, PHQ9_subclinical_mean_max_acc)

PHQ9_group_min_acc <- PHQ9_min_acc_merged$min_acceleration[PHQ9_min_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_min_acc <- PHQ9_min_acc_merged$min_acceleration[PHQ9_min_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_min_acc, PHQ9_subclinical_min_acc)

PHQ9_group_mean_min_acc <- PHQ9_mean_min_acc_merged$mean_min_acceleration[PHQ9_mean_min_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_mean_min_acc <- PHQ9_mean_min_acc_merged$mean_min_acceleration[PHQ9_mean_min_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_mean_min_acc, PHQ9_subclinical_mean_min_acc)

PHQ9_group_median_acc <- PHQ9_median_acc_merged$median_acceleration[PHQ9_median_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_median_acc <- PHQ9_median_acc_merged$median_acceleration[PHQ9_median_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_median_acc, PHQ9_subclinical_median_acc)

PHQ9_group_q1_acc <- PHQ9_q1_acc_merged$Q1_acceleration[PHQ9_q1_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_q1_acc <- PHQ9_q1_acc_merged$Q1_acceleration[PHQ9_q1_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_q1_acc, PHQ9_subclinical_q1_acc)

PHQ9_group_q3_acc <- PHQ9_q3_acc_merged$Q3_acceleration[PHQ9_q3_acc_merged$Illness_status == "MD"]
PHQ9_subclinical_q3_acc <- PHQ9_q3_acc_merged$Q3_acceleration[PHQ9_q3_acc_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_q3_acc, PHQ9_subclinical_q3_acc)

