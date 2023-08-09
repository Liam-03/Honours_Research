library(dplyr)
library(tidyr)

# Feature extraction
# 1. By total
length_per_click_PIN <- length_differences_df %>%
  group_by(PIN) %>%
  summarize(
    sm_distance = sum(diff_distance),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_distance / sm_clicks, 2)
  ) %>%
  filter(is.finite(ratio))

ideal_distances_df <- mouse_data_completed %>%
  group_by(PIN) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>% # So that first click for each participant counted
  filter(event_type == "Click") %>%
  mutate(distance = distance(x, y, lag(x), lag(y))) %>%
  mutate(distance = replace_na(distance, 0)) %>%
  filter(!is.na(distance)) %>%
  summarise(
    sm_distance = sum(distance),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_distance / (sm_clicks - 1), 2) # -1 because we added an extra click per participant
  ) %>%
  filter(is.finite(ratio))

path_lengths_ratio_df <- merge(length_per_click_PIN, ideal_distances_df, by = "PIN") %>%
  mutate(distance_ratio = ratio.x/ratio.y)

# 2. By stage
length_per_click_PIN_stage <- length_differences_df %>%
  group_by(PIN, stage) %>%
  summarize(
    sm_distance = sum(diff_distance),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_distance / sm_clicks, 2)
  ) %>%
  filter(is.finite(ratio))

ideal_distances_df_stage <- mouse_data_completed %>%
  group_by(PIN, stage) %>%
  mutate(event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)) %>% # So that first click for each participant counted
  filter(event_type == "Click") %>%
  mutate(distance = distance(x, y, lag(x), lag(y))) %>%
  mutate(distance = replace_na(distance, 0)) %>%
  filter(!is.na(distance)) %>%
  summarise(
    sm_distance = sum(distance),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_distance / (sm_clicks - 1), 2) # -1 because we added an extra click per participant
  ) %>%
  filter(is.finite(ratio))

path_lengths_ratio_df_stage <- merge(length_per_click_PIN_stage, ideal_distances_df_stage, by = "PIN") %>%
  mutate(distance_ratio = ratio.x/ratio.y) %>%
  group_by(PIN, stage)
  mutate(distance_ratio_stage = mean(distance_ratio))

# Inferential statistics preparation
# 1. 
GAD7_length_ratio_PIN_merged <- merge(GAD7_sums, path_lengths_ratio_df, by = "PIN")
PHQ9_length_ratio_PIN_merged <- merge(PHQ9_sums, path_lengths_ratio_df, by = "PIN")

GAD7_classification <- GAD7_length_ratio_PIN_merged$Illness_status
GAD7_mean_path_ratio <- GAD7_length_ratio_PIN_merged$distance_ratio
data_GAD7_mean_path_ratio <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_path_ratio)

PHQ9_classification <- PHQ9_length_ratio_PIN_merged$Illness_status
PHQ9_mean_path_ratio <- PHQ9_length_ratio_PIN_merged$distance_ratio
data_PHQ9_mean_path_ratio <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_path_ratio)

# Box plots
# 1. 
boxplot(Ratio ~ Classification, data = data_GAD7_mean_path_ratio,
        xlab = "Classification", ylab = "Mean Path Ratio",
        main = "Mean Path Ratio by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_path_ratio,
        xlab = "Classification", ylab = "Mean Path Ratio",
        main = "Mean Path Ratio by PHQ9 Classification")

# Wilcoxon tests
# 1. 
GAD7_group_path_ratio <- GAD7_length_ratio_PIN_merged$distance_ratio[GAD7_length_ratio_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_path_ratio <- GAD7_length_ratio_PIN_merged$distance_ratio[GAD7_length_ratio_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_path_ratio, GAD7_subclinical_group_path_ratio)

PHQ9_group_path_ratio <- PHQ9_length_ratio_PIN_merged$distance_ratio[PHQ9_length_ratio_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_path_ratio <- PHQ9_length_ratio_PIN_merged$distance_ratio[PHQ9_length_ratio_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_path_ratio, PHQ9_subclinical_group_path_ratio)
