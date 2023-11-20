library(dplyr)
library(tidyr)

# Feature extraction
ideal_distances_df <- length_differences_df %>%
  group_by(PIN) %>%
  mutate(
    event_type = if_else(PIN != lag(PIN) | is.na(lag(PIN)), "Click", event_type)
  ) %>% 
  filter(event_type == "Click") %>%
  mutate(
    change_scroll_x = scroll_position_x - lag(scroll_position_x),
    change_scroll_y = scroll_position_y - lag(scroll_position_y)
  ) %>%
  mutate(
    distance = distance(x - change_scroll_x, y - change_scroll_y, lag(x), lag(y))
  ) %>%
  filter(!is.na(distance)) 

ideal_distances_df <- ideal_distances_df %>%
  group_by(PIN, stage, movement_count) %>%
  summarise(Ideal_distance = distance)

actual_idealised_merged_df <- merge(length_per_click_PIN, ideal_distances_df) %>%
  mutate(path_ideal_ratio = ifelse(Ideal_distance != 0, Distance_click/Ideal_distance, NA)) %>%
  group_by(PIN) %>%
  summarise(
    mean_path_ideal_ratio = mean(path_ideal_ratio, na.rm = TRUE),
    median_path_ideal_ratio = median(path_ideal_ratio, na.rm = TRUE),
    q1_path_ideal_ratio = quantile(path_ideal_ratio, 0.25, na.rm = TRUE),
    q3_path_ideal_ratio = quantile(path_ideal_ratio, 0.75, na.rm = TRUE)
  )



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
