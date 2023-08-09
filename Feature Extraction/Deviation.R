library(dplyr)
library(tidyr)

# Feature extraction
perp_dist <- function(point,origin_line,end_line) {
  m <- cbind(origin_line-end_line, point - origin_line)
  d <- abs(det(m))/sqrt(sum((origin_line-end_line)*(origin_line-end_line)))
  d
} 

deviation_df <- mouse_data_completed %>%
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
  mutate(dist = perp_dist(c(x, y), c(start_x, start_y), c(end_x, end_y)))

max_deviation_PIN <- deviation_df %>%
  group_by(PIN) %>%
  filter(!is.na(dist)) %>%
  summarise(max_deviation = max(dist))

max_deviation_PIN_stage <- deviation_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(dist)) %>%
  summarise(max_deviation = max(dist))

mean_max_deviation_PIN <- deviation_df %>%
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(dist)) %>%
  summarise(max_dev = max(dist)) %>%
  group_by(PIN) %>%
  summarise(mean_max_deviation = mean(max_dev))

mean_max_deviation_PIN_stage <- deviation_df %>%
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(dist)) %>%
  summarise(max_dev = max(dist)) %>%
  group_by(PIN, stage) %>%
  summarise(mean_max_deviation = mean(max_dev))

mean_deviation_PIN <- deviation_df %>%
  group_by(PIN) %>%
  filter(!is.na(dist)) %>%
  summarise(mean_deviation = mean(dist))

mean_deviation_PIN_stage <- deviation_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(dist)) %>%
  summarise(mean_deviation = mean(dist))

avg_mean_deviation_PIN <- deviation_df %>%
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(dist)) %>%
  summarise(mean_dev = mean(dist)) %>%
  group_by(PIN ) %>%
  summarise(avg_mean_deviation = mean(mean_dev))

avg_mean_deviation_PIN_stage <- deviation_df %>%
  group_by(PIN, stage, click_count) %>%
  filter(!is.na(dist)) %>%
  summarise(mean_dev = mean(dist)) %>%
  group_by(PIN, stage) %>%
  summarise(avg_mean_deviation = mean(mean_dev))

median_deviation_PIN <- deviation_df %>%
  group_by(PIN) %>%
  filter(!is.na(dist)) %>%
  summarise(median_deviation = median(dist))

median_deviation_PIN_stage <- deviation_df %>%
  group_by(PIN, stage) %>%
  filter(!is.na(dist)) %>%
  summarise(median_deviation = median(dist))

# Inferential statistics preparation

# Box plots

# Wilcoxon tests

