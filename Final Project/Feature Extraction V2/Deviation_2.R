library(dplyr)
library(tidyr)

# Feature extraction
perp_dist <- function(point,origin_line,end_line) {
  m <- cbind(origin_line-end_line, point - origin_line)
  d <- abs(det(m))/sqrt(sum((origin_line-end_line)*(origin_line-end_line)))
  d
} 

deviation_df <- mouse_data_completed %>%
  mutate(movement_count = if_else(event_type == "Click", click_count-1, click_count)) %>%
  group_by(PIN, stage) %>% 
  mutate(
    order = cut(
      1:n(), 
      breaks = c(-Inf, which(event_type == "Click"), Inf),
      labels = 0:(length(which(event_type == "Click"))),
      include.lowest = TRUE  # Include the lowest value in the first interval
    )
  ) %>% 
  group_by(PIN, stage, order) %>% 
  mutate(
    start_x = first(x),
    end_x = last(x),
    start_y = first(y),
    end_y = last(y)
  ) %>% 
  rowwise() %>% 
  mutate(dist = perp_dist(c(x, y), c(start_x, start_y), c(end_x, end_y))) %>%
  filter(is.finite(dist))

deviation_per_click <- deviation_df %>%
  group_by(PIN, stage, movement_count) %>%
  summarise(mean_deviation_per_click = mean(dist, na.rm = TRUE),
            max_deviation_per_click = max(dist, na.rm = TRUE),
            median_deviation_per_click = median(dist, na.rm = TRUE))

mean_max_median_deviation <- deviation_per_click %>%
  group_by(PIN) %>%
  summarise(
    median_mean_deviation = median(mean_deviation_per_click, na.rm = TRUE),
    median_median_deviation = median(median_deviation_per_click, na.rm = TRUE),
    median_max_deviation = median(max_deviation_per_click, na.rm = TRUE))

# Inferential statistics preparation
PHQ9_max_dev_PIN_merged <- merge(PHQ9_sums, max_deviation_PIN, by = "PIN")
PHQ9_mean_max_dev_PIN_merged <- merge(PHQ9_sums, mean_max_deviation_PIN, by = "PIN")
PHQ9_mean_dev_PIN_merged <- merge(PHQ9_sums, mean_deviation_PIN, by = "PIN")
PHQ9_avg_mean_dev_PIN_merged <- merge(PHQ9_sums, avg_mean_deviation_PIN, by = "PIN")
PHQ9_median_dev_PIN_merged <- merge(PHQ9_sums, median_deviation_PIN, by = "PIN")

PHQ9_classification <- PHQ9_max_dev_PIN_merged$Illness_status

data_PHQ9_max_dev <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_max_dev_PIN_merged$max_deviation)
data_PHQ9_mean_max_dev <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_max_dev_PIN_merged$mean_max_deviation)
data_PHQ9_mean_dev <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_dev_PIN_merged$mean_deviation)
data_PHQ9_avg_mean_dev <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_avg_mean_dev_PIN_merged$avg_mean_deviation)
data_PHQ9_median_dev <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_median_dev_PIN_merged$median_deviation)

# Wilcoxon tests
PHQ9_group_max_dev <- data_PHQ9_max_dev$Ratio[data_PHQ9_max_dev$Classification == "MD"]
PHQ9_subclinical_group_max_dev <- data_PHQ9_max_dev$Ratio[data_PHQ9_max_dev$Classification == "Subclinical"]
wilcox.test(PHQ9_group_max_dev, PHQ9_subclinical_group_max_dev)

PHQ9_group_mean_max_dev <- data_PHQ9_mean_max_dev$Ratio[data_PHQ9_mean_max_dev$Classification == "MD"]
PHQ9_subclinical_group_mean_max_dev <- data_PHQ9_mean_max_dev$Ratio[data_PHQ9_mean_max_dev$Classification == "Subclinical"]
wilcox.test(PHQ9_group_mean_max_dev, PHQ9_subclinical_group_mean_max_dev)

PHQ9_group_mean_dev <- data_PHQ9_mean_dev$Ratio[data_PHQ9_mean_dev$Classification == "MD"]
PHQ9_subclinical_group_mean_dev <- data_PHQ9_mean_dev$Ratio[data_PHQ9_mean_dev$Classification == "Subclinical"]
wilcox.test(PHQ9_group_mean_dev, PHQ9_subclinical_group_mean_dev)

PHQ9_group_avg_mean_dev <- data_PHQ9_avg_mean_dev$Ratio[data_PHQ9_avg_mean_dev$Classification == "MD"]
PHQ9_subclinical_avg_group_mean_dev <- data_PHQ9_avg_mean_dev$Ratio[data_PHQ9_avg_mean_dev$Classification == "Subclinical"]
wilcox.test(PHQ9_group_avg_mean_dev, PHQ9_subclinical_avg_group_mean_dev)

PHQ9_group_median_dev <- data_PHQ9_median_dev$Ratio[data_PHQ9_median_dev$Classification == "MD"]
PHQ9_subclinical_group_median_dev <- data_PHQ9_median_dev$Ratio[data_PHQ9_median_dev$Classification == "Subclinical"]
wilcox.test(PHQ9_group_median_dev, PHQ9_subclinical_group_median_dev)
