library(dplyr)
library(tidyr)

# Feature extraction
acute_angles_df <- mouse_data_completed %>%
  select(PIN, event_type, stage, x,y) %>% 
  group_by(PIN, stage) %>%
  mutate(
    lg_x1 = lag(x),
    lg_y1 = lag(y),
    lg_x2 = lag(x, n = 2),
    lg_y2 = lag(y, n = 2)
  ) %>% 
  filter(!is.na(lg_y2) | event_type == "Click") %>% 
  mutate(x.1 = x - lg_x1,
         y.1 = y - lg_y1,
         x.2 = lg_x2 - lg_x1,
         y.2 = lg_y2 - lg_y1,
         d.prod = x.1*x.2 + y.1*y.2,
         f.norm = sqrt(x.1^2 + y.1^2),
         s.norm = sqrt(x.2^2 + y.2^2),
         theta_rad = acos(d.prod / (f.norm*s.norm)),
         theta_degr = round(theta_rad*(180/pi),1)
  ) %>%
  mutate(acute_angle = if_else(theta_degr < 90 & theta_degr > 0, 1, 0)) %>%
  filter(acute_angle == 1 | event_type == "Click") %>%
  mutate(acute_angle = replace_na(acute_angle, 0))

acute_angles_df_per_click_PIN <- acute_angles_df %>%
  group_by(PIN) %>% # Per participant, not per stage as well
  summarize(
    sm_angles = sum(acute_angle),
    sm_clicks = sum(event_type == "Click"),
    ratio = round(sm_angles / sm_clicks, 2)
  ) %>%
  filter(is.finite(ratio))

# Inferential statistics preparation
GAD7_angles_PIN_merged <- merge(GAD7_sums, acute_angles_df_per_click_PIN, by = "PIN")
PHQ9_angles_PIN_merged <- merge(PHQ9_sums, acute_angles_df_per_click_PIN, by = "PIN")

GAD7_classification <- GAD7_angles_PIN_merged$Illness_status
GAD7_angles_ratio <- GAD7_angles_PIN_merged$ratio
data_GAD7_angles <- data.frame(Classification = GAD7_classification, Ratio = GAD7_angles_ratio)

PHQ9_classification <- PHQ9_angles_PIN_merged$Illness_status
PHQ9_angles_ratio <- PHQ9_angles_PIN_merged$ratio
data_PHQ9_angles <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_angles_ratio)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_angles,
        xlab = "Classification", ylab = "Acute angles per Click",
        main = "Mean Acute Angles by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_angles,
        xlab = "Classification", ylab = "Acute angles per Click",
        main = "Mean Acute Angles by PHQ9 Classification")

# Wilcoxon tests
GAD7_group_angles <- GAD7_angles_PIN_merged$ratio[GAD7_angles_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_angles <- GAD7_angles_PIN_merged$ratio[GAD7_angles_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_angles, GAD7_subclinical_group_angles)

PHQ9_group_angles <- PHQ9_angles_PIN_merged$ratio[PHQ9_angles_PIN_merged$Illness_status == "MD"]
PHQ9_subclinical_group_angles <- PHQ9_angles_PIN_merged$ratio[PHQ9_angles_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_angles, PHQ9_subclinical_group_angles)