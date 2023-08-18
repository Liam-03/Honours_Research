library(caret)
library(dplyr)
library(ggplot2)
library(randomForest)

scaled_unsupervised_df_PHQ9 <- unsupervised_df_with_outcomes %>%
  # Remove PIN and GAD7 status
  select(-PIN, -GAD7_status) %>%
  # Save categorical features as factors
  mutate_at(c("location", "Gender", "Education_level", "Marital_status", "Employment_status", 
              "Gross_annual_household_income_USD"), as.factor)

# Sepeate data into variables and target outcome
X <- scaled_unsupervised_df_PHQ9 %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- scaled_unsupervised_df_PHQ9$PHQ9_status

# Separate data into training and test sets (not used here)
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

y_train <- y[trainIndex]
y_test  <- y[-trainIndex]

# PART A: OPTIMISING ACCURACY
# 1) RF 
# Define the control using a random forest selection function
control_rf <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10)

# Run RFE
result_rfe1_rf <- rfe(x = X, 
                   y = as.factor(y), 
                   sizes = c(1:34),
                   rfeControl = control_rf)

# Print the results
result_rfe1_rf

# Print the selected features
predictors(result_rfe1_rf)

# Plot RMSE vs feature number
plot(result_rfe1_rf, type = c("g", "o"))

# Visually examining feature importance
varimp_data_rf <- data.frame(feature = row.names(varImp(result_rfe1_rf))[1:8],
                          importance = varImp(result_rfe1_rf)[1:8, 1])

ggplot(data = varimp_data_rf, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# 2) Bagged trees
# Define the control using a random forest selection function
control_bagtree <- rfeControl(functions = treebagFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10)

# Run RFE
result_rfe1_bagtree <- rfe(x = X, 
                   y = as.factor(y), 
                   sizes = c(1:34),
                   rfeControl = control_bagtree)

# Print the results
result_rfe1_bagtree

# Print the selected features
predictors(result_rfe1_bagtree)

# Plot RMSE vs feature number
plot(result_rfe1_bagtree, type = c("g", "o"))

# Visually examining feature importance
varimp_data_bagtree <- data.frame(feature = row.names(varImp(result_rfe1_bagtree))[1:8],
                          importance = varImp(result_rfe1_bagtree)[1:8, 1])

ggplot(data = varimp_data_bagtree, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# PART B: OPTIMISING ROC
# 1) RF 
# Set summary to twoClassSummary as this is binary classification
rfFuncs$summary <- twoClassSummary

# Define the control using a random forest selection function
control_rf <- rfeControl(functions = rfFuncs, # random forest
                         method = "repeatedcv", # repeated cv
                         repeats = 5, # number of repeats
                         number = 10)

# Run RFE
result_rfe1_rf <- rfe(x = X, 
                      y = as.factor(y), 
                      sizes = c(1:34),
                      rfeControl = control_rf)

# Print the results
result_rfe1_rf

# Print the selected features
predictors(result_rfe1_rf)

# Plot RMSE vs feature number
plot(result_rfe1_rf, type = c("g", "o"))

# Visually examining feature importance
varimp_data_rf <- data.frame(feature = row.names(varImp(result_rfe1_rf))[1:8],
                             importance = varImp(result_rfe1_rf)[1:8, 1])

ggplot(data = varimp_data_rf, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# 2) Bagged trees
# Set summary to twoClassSummary as this is binary classification
treebagFuncs$summary <- twoClassSummary

# Define the control using a random forest selection function
control_bagtree <- rfeControl(functions = treebagFuncs, # random forest
                              method = "repeatedcv", # repeated cv
                              repeats = 5, # number of repeats
                              number = 10)

# Run RFE
result_rfe1_bagtree <- rfe(x = X, 
                           y = as.factor(y), 
                           sizes = c(1:34),
                           rfeControl = control_bagtree)

# Print the results
result_rfe1_bagtree

# Print the selected features
predictors(result_rfe1_bagtree)

# Plot RMSE vs feature number
plot(result_rfe1_bagtree, type = c("g", "o"))

# Visually examining feature importance
varimp_data_bagtree <- data.frame(feature = row.names(varImp(result_rfe1_bagtree))[1:8],
                                  importance = varImp(result_rfe1_bagtree)[1:8, 1])

ggplot(data = varimp_data_bagtree, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# PART C: OPTIMISING SENSITIVITY
# Custom summary function to optimize sensitivity

# 1) RF
# Set summary to twoClassSummary as this is binary classification
rfFuncs$summary <- twoClassSummary

# Define the control using a random forest selection function
control_rf <- rfeControl(functions = rfFuncs, # random forest
                         method = "repeatedcv", # repeated cv
                         repeats = 5, # number of repeats
                         number = 10)

# Run RFE
result_rfe1_rf <- rfe(x = X, 
                      y = as.factor(y), 
                      sizes = c(1:34),
                      rfeControl = control_rf,
                      metric = 'Sens')

# Print the results
result_rfe1_rf

# Print the selected features
predictors(result_rfe1_rf)

# Plot RMSE vs feature number
plot(result_rfe1_rf, type = c("g", "o"))

# Visually examining feature importance
varimp_data_rf <- data.frame(feature = row.names(varImp(result_rfe1_rf))[1:8],
                             importance = varImp(result_rfe1_rf)[1:8, 1])

ggplot(data = varimp_data_rf, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# 2) Bagged trees
# Set summary to twoClassSummary as this is binary classification
treebagFuncs$summary <- twoClassSummary

# Define the control using a random forest selection function
control_bagtree <- rfeControl(functions = treebagFuncs, # random forest
                              method = "repeatedcv", # repeated cv
                              repeats = 5, # number of repeats
                              number = 10)

# Run RFE
result_rfe1_bagtree <- rfe(x = X, 
                           y = as.factor(y), 
                           sizes = c(1:34),
                           rfeControl = control_bagtree,
                           metric = 'Sens')

# Print the results
result_rfe1_bagtree

# Print the selected features
predictors(result_rfe1_bagtree)

# Plot RMSE vs feature number
plot(result_rfe1_bagtree, type = c("g", "o"))

# Visually examining feature importance
varimp_data_bagtree <- data.frame(feature = row.names(varImp(result_rfe1_bagtree))[1:8],
                                  importance = varImp(result_rfe1_bagtree)[1:8, 1])

ggplot(data = varimp_data_bagtree, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")
