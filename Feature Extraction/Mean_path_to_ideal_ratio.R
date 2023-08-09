library(dplyr)
library(tidyr)

# Feature extraction
length_per_click_PIN_stage_click <- length_differences_df %>%
  group_by(PIN, stage, click_count) %>%
  summarize(sm_distance = sum(diff_distance)) %>%
  filter(is.finite(sm_distance)) %>%
  filter(sm_distance != 0) %>%
  select(PIN, stage, click_count, sm_distance)

ideal_distances_df_PIN_stage_click <- mouse_data_completed_cumsum %>%
  group_by(PIN, stage) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>% # So that first click for each participant counted
  filter(event_type == "Click") %>%
  mutate(distance = distance(x, y, lag(x), lag(y))) %>%
  mutate(distance = replace_na(distance, 0)) %>%
  filter(distance != 0) %>%
  group_by(PIN, stage, click_count) %>%
  select(PIN, stage, x, y, click_count, distance)

mean_path_ratio_df <- merge(length_per_click_PIN_stage_click, ideal_distances_df_PIN_stage_click, by = "PIN") %>%
  mutate(ratio = sm_distance/distance) %>%
  group_by(PIN) %>%
  mutate(mean_ratio = mean(ratio))

# Inferential statistics preparation
GAD7_mean_path_ratio_PIN_merged <- merge(GAD7_sums, mean_path_ratio_df, by = "PIN")
PHQ9_mean_path_ratio_PIN_merged <- merge(PHQ9_sums, mean_path_ratio_df, by = "PIN")

GAD7_classification <- GAD7_mean_path_ratio_PIN_merged$Illness_status
GAD7_mean_path_ratio_2 <- GAD7_mean_path_ratio_PIN_merged$mean_ratio
data_GAD7_mean_path_ratio_2 <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_path_ratio_2)

PHQ9_classification <- PHQ9_mean_path_ratio_PIN_merged$Illness_status
PHQ9_mean_path_ratio_2 <- PHQ9_mean_path_ratio_PIN_merged$mean_ratio
data_PHQ9_mean_path_ratio_2 <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_path_ratio_2)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_mean_path_ratio_2,
        xlab = "Classification", ylab = "Mean Path Ratio",
        main = "Mean Path Ratio by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_path_ratio_2,
        xlab = "Classification", ylab = "Mean Path Ratio",
        main = "Mean Path Ratio by PHQ9 Classification")

# Wilcoxon tests
GAD7_group_path_ratio_2 <- GAD7_mean_path_ratio_PIN_merged$mean_ratio[GAD7_mean_path_ratio_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_path_ratio_2 <- GAD7_mean_path_ratio_PIN_merged$mean_ratio[GAD7_mean_path_ratio_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_path_ratio_2, GAD7_subclinical_group_path_ratio_2)

PHQ9_group_path_ratio_2 <- PHQ9_mean_path_ratio_PIN_merged$mean_ratio[PHQ9_mean_path_ratio_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_path_ratio_2 <- PHQ9_mean_path_ratio_PIN_merged$mean_ratio[PHQ9_mean_path_ratio_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_path_ratio_2, PHQ9_subclinical_group_path_ratio_2)
