library(dplyr)
library(tidyr)

# Feature Extraction
# a) Function that finds distance between two sets of coordinates
distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2) 
}

# b) Code that adds a column with the difference between each coordinate and the previous coordinate
length_differences_df <- mouse_data_completed %>%
  group_by(PIN, stage) %>%
  mutate(
    change_scroll_x = scroll_position_x - lag(scroll_position_x),
    change_scroll_y = scroll_position_y - lag(scroll_position_y)) %>%
  mutate(distance = distance(x - change_scroll_x, y - change_scroll_y, lag(x), lag(y)),
         new_event = if_else(event_type == "Click" | is.na(lag(event_type)), TRUE, FALSE),
         diff_distance = if_else(new_event, 0, distance)) %>%
  mutate(movement_count = if_else(event_type == "Click", click_count-1, click_count))

# c) Code that finds the avg distance per click per stage per participant
length_per_click_PIN <- length_differences_df %>%
  group_by(PIN, stage, movement_count) %>%
  summarise(Distance_click = sum(diff_distance, na.rm = TRUE))

mean_median_length_df <- length_per_click_PIN %>%
  group_by(PIN) %>%
  summarise(mean_path_length = mean(Distance_click, na.rm = TRUE), median_path_length = median(Distance_click, na.rm = TRUE))


# Inferential statistics preparation
GAD7_length_PIN_merged <- merge(GAD7_sums, length_per_click_PIN, by = "PIN")
PHQ9_length_PIN_merged <- merge(PHQ9_sums, length_per_click_PIN, by = "PIN")

GAD7_classification <- GAD7_length_PIN_merged$Illness_status
GAD7_mean_path_length <- GAD7_length_PIN_merged$ratio
data_GAD7_mean_path_length <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_path_length)

PHQ9_classification <- PHQ9_length_PIN_merged$Illness_status
PHQ9_mean_path_length <- PHQ9_length_PIN_merged$ratio
data_PHQ9_mean_path_length <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_path_length)

# Box plot
boxplot(Ratio ~ Classification, data = data_GAD7_mean_path_length,
        xlab = "Classification", ylab = "Mean Path Length",
        main = "Mean Path Length by GAD7 Classification")
boxplot(Ratio ~ Classification, data = data_PHQ9_mean_path_length,
        xlab = "Classification", ylab = "Mean Path Length",
        main = "Mean Path Length by PHQ9 Classification")

# Wilcoxon test
GAD7_group_path_length <- GAD7_length_PIN_merged$ratio[GAD7_length_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_path_length <- GAD7_length_PIN_merged$ratio[GAD7_length_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_path_length, GAD7_subclinical_group_path_length)

PHQ9_group_path_length <- PHQ9_length_PIN_merged$ratio[PHQ9_length_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_path_length <- PHQ9_length_PIN_merged$ratio[PHQ9_length_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_path_length, PHQ9_subclinical_group_path_length)