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

# Box plots

# Wilcoxon tests

