library(dplyr)
library(tidyr)

# Feature extraction
length_differences_df <- mouse_data_completed %>% # Taken from mean_path_distance
  group_by(PIN, stage) %>%
  mutate(distance = distance(x, y, lag(x), lag(y)),
         new_event = if_else(event_type == "Click" | is.na(lag(event_type)), TRUE, FALSE),
         diff_distance = if_else(new_event, 0, distance)) %>%
  filter(scroll_position_x == lag(scroll_position_x) & scroll_position_y == lag(scroll_position_y))

avg_length_PIN <- length_differences_df %>%
  group_by(PIN) %>%
  summarize(
    sm_distance = sum(diff_distance),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_distance / sm_clicks, 2)
  ) %>%
  filter(is.finite(ratio))

time_distance_combo <- cbind(mean_response_time_PIN, avg_length_PIN$ratio)

time_distance_combo <- time_distance_combo %>%
  mutate(
    avg_velocity_PIN = avg_length_PIN$ratio/Response_Time,
    avg_velocity_PIN_units_per_sec = avg_velocity_PIN)

# Inferential statistics preparation
GAD7_velocity_PIN_merged <- merge(GAD7_sums, time_distance_combo, by = "PIN")
PHQ9_velocity_PIN_merged <- merge(PHQ9_sums, time_distance_combo, by = "PIN")

GAD7_classification <- GAD7_velocity_PIN_merged$Illness_status
GAD7_mean_velocity <- GAD7_velocity_PIN_merged$avg_velocity_PIN_units_per_sec
data_GAD7_mean_vel <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_velocity)

PHQ9_classification <- PHQ9_velocity_PIN_merged$Illness_status
PHQ9_mean_velocity <- PHQ9_velocity_PIN_merged$avg_velocity_PIN_units_per_sec
data_PHQ9_mean_vel <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_velocity)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_mean_vel,
        xlab = "Classification", ylab = "Mean Mouse Velocity (units/s)",
        main = "Mean Mouse Velocity by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_vel,
        xlab = "Classification", ylab = "Mean Mouse Velocity (units/s)",
        main = "Mean Mouse Velocity by PHQ9 Classification")

# Wilcoxon tests
GAD7_group_mean_velocity <- GAD7_velocity_PIN_merged$avg_velocity_PIN_units_per_sec[GAD7_velocity_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_mean_velocity <- GAD7_velocity_PIN_merged$avg_velocity_PIN_units_per_sec[GAD7_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_mean_velocity, GAD7_subclinical_group_mean_velocity)

PHQ9_group_mean_velocity <- PHQ9_velocity_PIN_merged$avg_velocity_PIN_units_per_sec[PHQ9_velocity_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_mean_velocity <- PHQ9_velocity_PIN_merged$avg_velocity_PIN_units_per_sec[PHQ9_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_mean_velocity, PHQ9_subclinical_group_mean_velocity)
