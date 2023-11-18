library(dplyr)
library(tidyr)

# Feature extraction
# a) Function that compares x coordinates of adjacent coordinates
x_direction <- function(x1, x2) { 
  ifelse(x2 == x1, 0, x2 - x1)
}

# b) Add a column that has a '1' every time there is a x-flip, otherwise a '0'
x_flips_df_V2 <- mouse_data_completed %>%
  mutate(movement_count = if_else(event_type == "Click", click_count-1, click_count)) %>%
  group_by(PIN, stage, movement_count) %>%
  mutate(x_direction = x_direction(lag(x), x)) %>% 
  filter(x_direction != 0 | event_type == "Click") %>% 
  mutate(x_flip = if_else(x_direction != 0 & (sign(x_direction) != sign(lag(x_direction))), 1, 0)) %>%
  filter(lag(event_type) != "Click")

# c) Count x-flips per click per stage per participant
x_flips_per_click <- x_flips_df_V2 %>%
  group_by(PIN, stage, movement_count) %>%
  summarize(
    x_flips_per_click = sum(x_flip))

x_flips_mean_median <- x_flips_per_click %>%
  group_by(PIN) %>%
  summarise(mean_x_flips = mean(x_flips_per_click, na.rm = TRUE),
            median_x_flips = median(x_flips_per_click, na.rm = TRUE))

x_flips_df_per_click_PIN <- x_flips_df_V2 %>%
  group_by(PIN) %>% # Per participant, not per stage as well
  summarize(
    sm_flips = sum(x_flip),
    sm_clicks = sum(event_type == "Click"),
    ratio = (sm_flips / sm_clicks)
  ) %>%
  filter(is.finite(ratio))

# Inferential statistics preparation
GAD7_xflips_PIN_merged <- merge(GAD7_sums, x_flips_df_per_click_PIN, by = "PIN")
GAD7_classification <- GAD7_xflips_PIN_merged$Illness_status
GAD7_xflips_ratio <- GAD7_xflips_PIN_merged$ratio
data_GAD7_xflips <- data.frame(Classification = GAD7_classification, Ratio = GAD7_xflips_ratio)

PHQ9_xflips_PIN_merged <- merge(PHQ9_sums, x_flips_df_per_click_PIN, by = "PIN")
PHQ9_classification <- GAD7_xflips_PIN_merged$Illness_status
PHQ9_xflips_ratio <- PHQ9_xflips_PIN_merged$ratio
data_PHQ9_xflips <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_xflips_ratio)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_xflips,
        xlab = "Classification", ylab = "X-flips per Click",
        main = "Mean X-Flips by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_xflips,
        xlab = "Classification", ylab = " X-flips per Click",
        main = "Mean X-flips by PHQ9 Classification")

# Wilcoxon tests
GAD7_group_xflips <- GAD7_xflips_PIN_merged$ratio[GAD7_xflips_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_xflips <- GAD7_xflips_PIN_merged$ratio[GAD7_xflips_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_xflips, GAD7_subclinical_group_xflips)

PHQ9_group_xflips <- PHQ9_xflips_PIN_merged$ratio[PHQ9_xflips_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_xflips <- PHQ9_xflips_PIN_merged$ratio[PHQ9_xflips_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_xflips, PHQ9_subclinical_group_xflips)