# Graph model importances
summary_gbm <- summary(gbm_model)
gbm_importance <- data.frame(feature = summary_gbm$var, importance = summary_gbm$rel.inf)

new_feature_names <- c("Local Velocity Maximums Per Click", "Weight (kg)", "Response Time",
                       "Median Acceleration", "Mean Click Hold Time", " Mean Perpendicular Deviation",
                       "Education Level", "Mean Velocity", "Absolute Maximum Velocity", "Mean Maximum Velocity",
                       "Mean Angle Change", "Minimum Acceleration", "Marital Status", "Gross Annual Household Income ($US)",
                       "Location", "Height (cm)", "Age", "Gender", "Employment Status")

gbm_importance$feature <- new_feature_names


p <- ggplot(data = gbm_importance, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill = importance), width = 0.7) +
  labs(x = NULL, y = "Relative Feature Importance", title = "GBM Feature Importance Plot") +
  coord_flip()

# Display the plot
print(p)