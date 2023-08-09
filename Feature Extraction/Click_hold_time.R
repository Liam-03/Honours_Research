library(dplyr)
library(tidyr)

# Feature Extraction
complete_clicks <- complete_completed %>%
  filter(event_converted %in% c("left_mouse key pressed", "left_mouse key released"))

click_hold_df_PIN_stage <- complete_clicks %>%
  group_by(PIN, stage) %>%
  mutate(duration = if_else(event_converted == "left_mouse key released", timestamp - lag(timestamp), NA)) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

click_hold_df_PIN <- complete_clicks %>%
  group_by(PIN) %>%
  mutate(duration = if_else(event_converted == "left_mouse key released", timestamp - lag(timestamp), NA)) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Inferential statistics preparation
GAD7_click_hold_PIN_merged <- merge(GAD7_sums, click_hold_df_PIN, by = "PIN")
PHQ9_click_hold_PIN_merged <- merge(PHQ9_sums, click_hold_df_PIN, by = "PIN")

GAD7_classification <- GAD7_click_hold_PIN_merged$Illness_status
GAD7_mean_click_duration <- GAD7_click_hold_PIN_merged$mean_duration
data_GAD7_mean_click_duration <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_click_duration)

PHQ9_classification <- PHQ9_click_hold_PIN_merged$Illness_status
PHQ9_mean_click_duration <- PHQ9_click_hold_PIN_merged$mean_duration
data_PHQ9_mean_click_duration <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_click_duration)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_mean_click_duration,
        xlab = "Classification", ylab = "Mean Click Duration",
        main = "Mean Click Duration by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_click_duration,
        xlab = "Classification", ylab = "Mean Click Duration",
        main = "Mean Click Duration by PHQ9 Classification")

# Wilcoxon tests
GAD7_group_click_duration <- GAD7_click_hold_PIN_merged$mean_duration[GAD7_click_hold_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_click_duration <- GAD7_click_hold_PIN_merged$mean_duration[GAD7_click_hold_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_click_duration, GAD7_subclinical_group_click_duration)

PHQ9_group_click_duration <- PHQ9_click_hold_PIN_merged$mean_duration[PHQ9_click_hold_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_click_duration <- PHQ9_click_hold_PIN_merged$mean_duration[PHQ9_click_hold_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_click_duration, PHQ9_subclinical_group_click_duration)