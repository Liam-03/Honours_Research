library(dplyr)

# 1) Correlation with PHQ9 status
numerical_features <- uncorrelated_unsupervised_significant_df_PHQ9_2 %>%
  select_if(is.numeric)

# Find correlation with outcome (excluding self-correlation)
correlation_PHQ9_status <- cor(numerical_features)[c(1:14), "PHQ9_status"]

# Find min and max correlation
min(correlation_PHQ9_status)
max(correlation_PHQ9_status)

# Calculate correlation matrix
correlation_PHQ9 <- cor(numerical_features)
par(cex = 0.5)
corrplot(correlation_PHQ9, addCoef.col = "black")

# 2) remove correlated
uncorrelated_unsupervised_significant_df_PHQ9 <- unsupervised_significant_df_PHQ9 %>%
  select(-c(mean_length_click, mean_deviation, median_deviation))

