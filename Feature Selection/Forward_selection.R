library(caret)
library(dplyr)
library(ggplot2)
library(randomForest)
library(MASS)
library(CAST)
library(xgboost)

# 1) RF using train() function
# Specify the control parameters for the train function
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Perform forward feature selection using the train function with random forest
rf_model <- train(
  x = X,
  y = as.factor(y),
  method = "rf",
  trControl = control,
  tuneLength = 1,  # Use only 1 predictor at a time
  selectionFunction = "forward"
)

# Print the selected variables
rf_final <- rf_model$finalModel

# Get variable importance scores from the final random forest model
variable_importance <- varImp(rf_final)

# Print the importance scores
print(variable_importance)

# 2) RF using ffs() function
ffsmodel_rf <- ffs(X, as.factor(y), method = "rf", tuneLength = 1)
ffsmodel_rf$selectedvars
ffsmodel_rf$selectedvars_perf


