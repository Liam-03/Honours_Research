library(dplyr)
library(tidyr)

# Feature extraction
# 1) Overall velocity for each click
velocity_per_click_df <- length_differences_df %>%
  group_by(PIN, stage) %>%
  mutate(time_change = timestamp - lag(timestamp)) %>%
  mutate(time_change = if_else(is.na(time_change), 0, time_change))

velocity_per_click_values <- velocity_per_click_df %>%
  group_by(PIN, stage, movement_count) %>%
  summarise(Distance_click = sum(diff_distance, na.rm = TRUE),
            Time_click = sum(time_change, na.rm = TRUE),
            Velocity_click = Distance_click/Time_click)

mean_median_velocity <- velocity_per_click_values %>%
  group_by(PIN) %>%
  filter(is.finite(Velocity_click)) %>%
  summarise(mean_velocity = mean(Velocity_click, na.rm = TRUE),
            median_velocity = median(Velocity_click, na.rm = TRUE))

# First timestamp after click doesn't count (velocity after reading prompt, starting next movement)
velocity_per_click_df_2 <- length_differences_df %>% 
  group_by(PIN, stage, movement_count) %>%
  mutate(time_change = timestamp - lag(timestamp)) %>%
  mutate(time_change = if_else(is.na(time_change), 0, time_change))

velocity_per_click_values_2 <- velocity_per_click_df_2 %>%
  group_by(PIN, stage, movement_count) %>%
  summarise(Distance_click = sum(diff_distance, na.rm = TRUE),
            Time_click = sum(time_change, na.rm = TRUE),
            Velocity_click = Distance_click/Time_click)

mean_median_velocity_2 <- velocity_per_click_values %>%
  group_by(PIN) %>%
  filter(is.finite(Velocity_click)) %>%
  summarise(mean_velocity = mean(Velocity_click, na.rm = TRUE),
            median_velocity = median(Velocity_click, na.rm = TRUE))

# 2) Velocity per movement
velocity_df <- length_differences_df %>%
  mutate(time = timestamp - lag(timestamp)) %>%
  mutate(velocity = distance/time) %>%
  mutate(velocity_diff = velocity - lag(velocity)) %>%
  mutate(velocity_change = if_else(sign(velocity_diff) > 0 & sign(lead(velocity_diff)) < 0, 
                                   "Local max", if_else(sign(velocity_diff) < 0 & sign(lead(velocity_diff)) > 0, 
                                                        "Local min", "No change"))) %>%
  mutate(local_max = ifelse(velocity_change == "Local max", 1, 0)) %>%
  mutate(local_min = ifelse(velocity_change == "Local min", 1, 0)) %>%
  mutate(local_max = if_else(is.na(local_max), 0, local_max)) %>%
  mutate(local_min = if_else(is.na(local_min), 0, local_min)) # note global max/min still taken as local max/min

max_locals_velocity_per_click <- velocity_df %>%
  group_by(PIN, stage, movement_count) %>%
  filter(!is.na(velocity) & !is.infinite(velocity)) %>%
  summarise(max_velocity = max(velocity, na.rm = TRUE),
            local_min = sum(local_min, na.rm = TRUE),
            local_max = sum(local_max, na.rm = TRUE),
            median_vel = median(velocity, na.rm = TRUE))

max_locals_velocity <- max_locals_velocity_per_click %>%
  group_by(PIN) %>%
  summarise(global_max_velocity = max(max_velocity),
            median_max_velocity = median(max_velocity),
            mean_max_velocity = mean(max_velocity),
            local_min_per_click = mean(local_min),
            local_max_per_click = mean(local_max),
            median_local_min = median(local_min),
            median_local_max = median(local_max),
            median_median_vel_per_click = median(median_vel))


# Inferential statistics preparation
# 1. Mean velocity
GAD7_mean_velocity_PIN_merged <- merge(GAD7_sums, mean_velocity_PIN, by = "PIN")
PHQ9_mean_velocity_PIN_merged <- merge(PHQ9_sums, mean_velocity_PIN, by = "PIN")

GAD7_classification <- GAD7_mean_velocity_PIN_merged$Illness_status
GAD7_mean_velocity <- GAD7_mean_velocity_PIN_merged$mean_velocity
data_GAD7_mean_velocity <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_velocity)

PHQ9_classification <- PHQ9_mean_velocity_PIN_merged$Illness_status
PHQ9_mean_velocity <- PHQ9_mean_velocity_PIN_merged$mean_velocity
data_PHQ9_mean_velocity <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_velocity)

# 2. Max velocity
GAD7_max_velocity_PIN_merged <- merge(GAD7_sums, max_velocity_PIN, by = "PIN")
PHQ9_max_velocity_PIN_merged <- merge(PHQ9_sums, max_velocity_PIN, by = "PIN")

GAD7_classification <- GAD7_max_velocity_PIN_merged$Illness_status
GAD7_max_velocity <- GAD7_max_velocity_PIN_merged$max_velocity
data_GAD7_max_velocity <- data.frame(Classification = GAD7_classification, Ratio = GAD7_max_velocity)

PHQ9_classification <- PHQ9_max_velocity_PIN_merged$Illness_status
PHQ9_max_velocity <- PHQ9_max_velocity_PIN_merged$max_velocity
data_PHQ9_max_velocity <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_max_velocity)

# 3. Mean max velocity
GAD7_mean_max_velocity_PIN_merged <- merge(GAD7_sums, mean_max_velocity_PIN, by = "PIN")
PHQ9_mean_max_velocity_PIN_merged <- merge(PHQ9_sums, mean_max_velocity_PIN, by = "PIN")

GAD7_classification <- GAD7_mean_max_velocity_PIN_merged$Illness_status
GAD7_mean_max_velocity <- GAD7_mean_max_velocity_PIN_merged$mean_max_velocity
data_GAD7_mean_max_velocity <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_max_velocity)

PHQ9_classification <- PHQ9_mean_max_velocity_PIN_merged$Illness_status
PHQ9_mean_max_velocity <- PHQ9_mean_max_velocity_PIN_merged$mean_max_velocity
data_PHQ9_mean_max_velocity <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_max_velocity)

# 4. Local max velocity
GAD7_local_max_PIN_merged <- merge(GAD7_sums, local_max_PIN, by = "PIN")
PHQ9_local_max_PIN_merged <- merge(PHQ9_sums, local_max_PIN, by = "PIN")

GAD7_classification <- GAD7_local_max_PIN_merged$Illness_status
GAD7_local_max <- GAD7_local_max_PIN_merged$local_max_per_click
data_GAD7_local_max <- data.frame(Classification = GAD7_classification, Ratio = GAD7_local_max)

PHQ9_classification <- PHQ9_local_max_PIN_merged$Illness_status
PHQ9_local_max <- PHQ9_local_max_PIN_merged$local_max_per_click
data_PHQ9_local_max <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_local_max)

# 5. Local min velocity
GAD7_local_min_PIN_merged <- merge(GAD7_sums, local_min_PIN, by = "PIN")
PHQ9_local_min_PIN_merged <- merge(PHQ9_sums, local_min_PIN, by = "PIN")

GAD7_classification <- GAD7_local_min_PIN_merged$Illness_status
GAD7_local_min <- GAD7_local_min_PIN_merged$local_min_per_click
data_GAD7_local_min <- data.frame(Classification = GAD7_classification, Ratio = GAD7_local_min)

PHQ9_classification <- PHQ9_local_min_PIN_merged$Illness_status
PHQ9_local_min <- PHQ9_local_min_PIN_merged$local_min_per_click
data_PHQ9_local_min <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_local_min)

# Box plots
# 1. Mean velocity
boxplot(Ratio ~ Classification, data = data_GAD7_mean_velocity,
        xlab = "Classification", ylab = "Mean Velocity",
        main = "Mean Velocity by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_velocity,
        xlab = "Classification", ylab = "Mean Velocity",
        main = "Mean Velocity by PHQ9 Classification")

# 2. Max velocity
boxplot(Ratio ~ Classification, data = data_GAD7_max_velocity,
        xlab = "Classification", ylab = "Max Velocity",
        main = "Max Velocity by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_max_velocity,
        xlab = "Classification", ylab = "Max Velocity",
        main = "Max Velocity by PHQ9 Classification")

# 3. Mean max velocity
boxplot(Ratio ~ Classification, data = data_GAD7_mean_max_velocity,
        xlab = "Classification", ylab = "Mean Max Velocity",
        main = "Mean Max Velocity by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_max_velocity,
        xlab = "Classification", ylab = "Mean Max Velocity",
        main = "Mean Max Velocity by PHQ9 Classification")

# 4. Local max velocity
boxplot(Ratio ~ Classification, data = data_GAD7_local_max,
        xlab = "Classification", ylab = "Mean Local Max",
        main = "Mean Local Max by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_local_max,
        xlab = "Classification", ylab = "Mean Local Max",
        main = "Mean Local Max by PHQ9 Classification")

# 5. Local min velocity
boxplot(Ratio ~ Classification, data = data_GAD7_local_min,
        xlab = "Classification", ylab = "Mean Local Min",
        main = "Mean Local Min by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_local_min,
        xlab = "Classification", ylab = "Mean Local Min",
        main = "Mean Local Min by PHQ9 Classification")

# Wilcoxon tests
# 1. Mean velocity
GAD7_group_mean_velocity <- GAD7_mean_velocity_PIN_merged$mean_velocity[GAD7_mean_velocity_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_mean_velocity <- GAD7_mean_velocity_PIN_merged$mean_velocity[GAD7_mean_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_mean_velocity, GAD7_subclinical_group_mean_velocity)

PHQ9_group_mean_velocity <- PHQ9_mean_velocity_PIN_merged$mean_velocity[PHQ9_mean_velocity_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_mean_velocity <- PHQ9_mean_velocity_PIN_merged$mean_velocity[PHQ9_mean_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_mean_velocity, PHQ9_subclinical_group_mean_velocity)

# 2. Max velocity
GAD7_group_max_velocity <- GAD7_max_velocity_PIN_merged$max_velocity[GAD7_max_velocity_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_max_velocity <- GAD7_max_velocity_PIN_merged$max_velocity[GAD7_max_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_max_velocity, GAD7_subclinical_group_max_velocity)

PHQ9_group_max_velocity <- PHQ9_max_velocity_PIN_merged$max_velocity[PHQ9_max_velocity_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_max_velocity <- PHQ9_max_velocity_PIN_merged$max_velocity[PHQ9_max_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_max_velocity, PHQ9_subclinical_group_max_velocity)

# 3. Mean max velocity
GAD7_group_mean_max_velocity <- GAD7_mean_max_velocity_PIN_merged$mean_max_velocity[GAD7_mean_max_velocity_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_mean_max_velocity <- GAD7_mean_max_velocity_PIN_merged$mean_max_velocity[GAD7_mean_max_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_mean_max_velocity, GAD7_subclinical_group_max_velocity)

PHQ9_group_mean_max_velocity <- PHQ9_mean_max_velocity_PIN_merged$mean_max_velocity[PHQ9_mean_max_velocity_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_mean_max_velocity <- PHQ9_mean_max_velocity_PIN_merged$mean_max_velocity[PHQ9_mean_max_velocity_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_mean_max_velocity, PHQ9_subclinical_group_mean_max_velocity)

# 4. Local max velocity
GAD7_group_local_max <- GAD7_local_max_PIN_merged$local_max_per_click[GAD7_local_max_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_local_max <- GAD7_local_max_PIN_merged$local_max_per_click[GAD7_local_max_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_local_max, GAD7_subclinical_group_local_max)

PHQ9_group_local_max <- PHQ9_local_max_PIN_merged$local_max_per_click[PHQ9_local_max_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_local_max <- PHQ9_local_max_PIN_merged$local_max_per_click[PHQ9_local_max_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_local_max, PHQ9_subclinical_group_local_max)

# 5. Local min velocity
GAD7_group_local_min <- GAD7_local_min_PIN_merged$local_min_per_click[GAD7_local_min_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_local_min <- GAD7_local_min_PIN_merged$local_min_per_click[GAD7_local_min_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_local_min, GAD7_subclinical_group_local_min)

PHQ9_group_local_min <- PHQ9_local_min_PIN_merged$local_min_per_click[PHQ9_local_min_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_local_min <- PHQ9_local_min_PIN_merged$local_min_per_click[PHQ9_local_min_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_local_min, PHQ9_subclinical_group_local_min)
