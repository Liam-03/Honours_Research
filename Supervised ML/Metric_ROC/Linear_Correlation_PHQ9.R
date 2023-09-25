# 1) Correlation with PHQ9 status
numerical_features <- unsupervised_df_PHQ9 %>%
  select_if(is.numeric)

# Find correlation with outcome (excluding self-correlation)
correlation_PHQ9_status <- cor(numerical_features)[c(1:26), "PHQ9_status"]

# Find min and max correlation
min(correlation_PHQ9_status)
max(correlation_PHQ9_status)

# 2) Correlation with PHQ9 raw sum
PHQ9_raw_sums <- PHQ9_File %>% 
  select(PIN, item, response) %>%
  group_by(PIN) %>%
  summarise(PHQ9_raw_sum = sum(as.numeric(response[item %in% c(1:9)], na.rm = TRUE)))

unsupervised_df_PHQ9_raw_sums <- merge(unsupervised_df, PHQ9_raw_sums, by = "PIN")

unsupervised_df_numerical_PHQ9_raw_sums <- unsupervised_df_PHQ9_raw_sums %>%
  select(-PIN) %>%
  select_if(is.numeric)

# Find correlation with outcome (excluding self-correlation)
correlation_PHQ9_raw_sum <- cor(unsupervised_df_numerical_PHQ9_raw_sums)[c(1:26), "PHQ9_raw_sum"]

# Find min and max correlation
min(correlation_PHQ9_raw_sum)
max(correlation_PHQ9_raw_sum)

