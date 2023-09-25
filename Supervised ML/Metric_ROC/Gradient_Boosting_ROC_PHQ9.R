library(caret)
library(dplyr)
library(gbm)
library(xgboost)
library(pROC)

# 1) GBM
set.seed(33)  # Set a specific seed value

# Separate data into variables and target
X <- unsupervised_df_PHQ9 %>%
  mutate_at(c("location", "Gender", "Education_level", "Marital_status", "Employment_status", 
              "Gross_annual_household_income_USD"), as.factor) %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(unsupervised_df_PHQ9$PHQ9_status)
levels(y) = c("Subclinical", "MD")
y <- relevel(y, ref = "MD")

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

y_test_num <- y_test
levels(y_test_num) = c(1, 0)

# Cross validation 
control <- trainControl(
  method='cv', 
  number=5, 
  search='grid',
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

# Hyperparameter grid
tunegrid <- expand.grid(
  n.trees = c(100, 200, 400, 600, 800, 1000, 1500),
  interaction.depth = c(1:5),
  shrinkage = c(0.01, 0.1, 0.2, 0.3),
  n.minobsinnode = c(2, 5, 10)
)

# Create model
gbm_model <- train(
  x = X_train,
  y = y_train_fac,
  method = 'gbm',
  tuneGrid = tunegrid,
  metric = 'ROC',
  trControl = control,
)

print(gbm_model)

# Save best model
best_gbm <- gbm_model$finalModel

varImp(best_gbm)   

# Make predictions on test set
pred_test_gbm <- predict(best_gbm, newdata = X_test, type = "response")

# Define the threshold for class assignment
threshold <- 0.5

# Convert probabilities to class predictions
class_predictions <- ifelse(pred_test_gbm >= threshold, "MD", "Subclinical")
class_predictions_num <- ifelse(pred_test_gbm >= threshold, 1, 0)

# Create confusion matrix
confusionMatrix(table(class_predictions, y_test))

# ROC
roc_gbm <- roc(response = y_test_num, predictor = class_predictions_num, levels = c(0, 1))
plot(roc_gbm, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)


# 2) XGB
set.seed(33)  # Set a specific seed value

# Convert data to numerical
X_train_num <- X[trainIndex, ] %>%
  select_if(is.numeric)
X_test_num  <- X[-trainIndex, ] %>%
  select_if(is.numeric)

# Hyperparameter grid
tunegrid_xgb <- expand.grid(
  nrounds = c(50, 100, 200, 500),
  max_depth = c(2:6),
  eta = c(0.01, 0,1, 0.2),
  gamma = c(0, 0.05, 0.2) ,
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.7, 0.8)
)

# Create model
xgb_model <- train(
  x = X_train_num,
  y = y_train_fac,
  method = 'xgbTree',
  tuneGrid = tunegrid_xgb,
  metric = 'ROC',
  trControl = control,
  verbosity = 0
)

print(xgb_model)

# Save best model
best_xgb <- xgb_model$finalModel

# Make predictions on test set
newdata_dmatrix <- xgb.DMatrix(data = as.matrix(X_test_num))
pred_test_xgb <- predict(best_xgb, newdata = newdata_dmatrix)

# Define the threshold for class assignment
threshold <- 0.5

# Convert probabilities to class predictions
class_predictions_xgb <- ifelse(pred_test_xgb >= threshold, "MD", "Subclinical")

# Convert to a factor if needed
class_predictions_xgb <- factor(class_predictions_xgb, levels = levels(y_test))
class_predictions_xgb_num <- ifelse(pred_test_xgb >= threshold, 1, 0)

# Create confusion matrix
confusionMatrix(table(class_predictions_xgb, y_test))

# ROC
roc_xgb <- roc(response = y_test_num, predictor = class_predictions_xgb_num, levels = c(0, 1))
plot(roc_xgb, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)
