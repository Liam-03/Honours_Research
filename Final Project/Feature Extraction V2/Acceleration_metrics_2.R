library(dplyr)
library(tidyr)

# Feature extraction
acceleration_df <- velocity_df %>%
  mutate(acceleration = velocity_diff / (timestamp - lag(timestamp)))

acceleration_metrics_per_click <- acceleration_df %>%
  group_by(PIN, stage, movement_count) %>%
  filter(!is.na(acceleration) & !is.infinite(acceleration)) %>%
  summarise(median_acc_per_click = median(acceleration, na.rm = TRUE),
            max_acc_per_click = max(acceleration, na.rm = TRUE),
            min_acc_per_click = min(acceleration, na.rm = TRUE),
            q1_acc_per_click = quantile(acceleration, 0.25, na.rm = TRUE),
            q3_acc_per_click = quantile(acceleration, 0.75, na.rm = TRUE),
            mean_acc_per_click = mean(acceleration, na.rm = TRUE))

acceleration_metrics <- acceleration_metrics_per_click %>%
  group_by(PIN) %>%
  summarise(median_median_acc = median(median_acc_per_click),
            median_max_acc = median(max_acc_per_click),
            median_mean_acc = median(mean_acc_per_click),
            median_min_acc = median(min_acc_per_click),
            median_q1_acc = median(q1_acc_per_click),
            median_q3_acc = median(q3_acc_per_click))

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

