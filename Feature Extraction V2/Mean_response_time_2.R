library(dplyr)
library(tidyr)

# Feature Extraction
response_times <- mouse_data_completed %>%
  group_by(PIN, stage) %>%
  filter((event_type == "Cursor hover" & row_number() == 1) | event_type == "Click") %>%
  mutate(Response_Time = timestamp - lag(timestamp)) %>%
  filter(event_type == "Click")

mean_response_time_PIN <- aggregate(Response_Time ~ PIN, data = response_times, FUN = mean, na.rm = TRUE)
median_response_time_PIN <- aggregate(Response_Time ~ PIN, data = response_times, FUN = median, na.rm = TRUE)

# Inferential statistics preparation
GAD7_response_time_PIN_merged <- merge(GAD7_sums, mean_response_time_PIN, by = "PIN")
PHQ9_response_time_PIN_merged <- merge(PHQ9_sums, mean_response_time_PIN, by = "PIN")

GAD7_classification <- GAD7_response_time_PIN_merged$Illness_status
GAD7_mean_response_time <- GAD7_response_time_PIN_merged$Response_Time
data_GAD7_mean_response_time <- data.frame(Classification = GAD7_classification, Ratio = GAD7_mean_response_time)

PHQ9_classification <- PHQ9_response_time_PIN_merged$Illness_status
PHQ9_mean_response_time <- PHQ9_response_time_PIN_merged$Response_Time
data_PHQ9_mean_response_time <- data.frame(Classification = PHQ9_classification, Ratio = PHQ9_mean_response_time)

# Box plots
boxplot(Ratio ~ Classification, data = data_GAD7_mean_response_time,
        xlab = "Classification", ylab = "Mean Response Time (ms)",
        main = "Mean Response Time by GAD7 Classification")

boxplot(Ratio ~ Classification, data = data_PHQ9_mean_response_time,
        xlab = "Classification", ylab = "Mean Response Time (ms)",
        main = "Mean Response Time by PHQ9 Classification")

# Wilcoxon test
GAD7_group_response_time <- GAD7_response_time_PIN_merged$Response_Time[GAD7_response_time_PIN_merged$Illness_status == "GAD"]
GAD7_subclinical_group_response_time <- GAD7_response_time_PIN_merged$Response_Time[GAD7_response_time_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(GAD7_group_response_time, subclinical_group_response_time)

PHQ9_group_response_time <- PHQ9_response_time_PIN_merged$Response_Time[PHQ9_response_time_PIN_merged$Illness_status == "MD"]
PHQ_subclinical_group_response_time <- PHQ9_response_time_PIN_merged$Response_Time[PHQ9_response_time_PIN_merged$Illness_status == "Subclinical"]
wilcox.test(PHQ9_group_response_time, PHQ_subclinical_group_response_time)

