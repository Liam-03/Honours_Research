library(caret)
library(pROC)
library(dplyr)
library(randomForest)
library(ranger)
library(e1071)

# 1) Using randomForest
set.seed(33)  # Set a specific seed value

# Separate data into variables and target
X <- unsupervised_df_PHQ9 %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(unsupervised_df_PHQ9$PHQ9_status)
y <- relevel(y, ref = "1")

# Split into training and test sets (not used here)
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

# Create model
rf_model <- randomForest(x = X_train, y = y_train)
rf_model 

importance(rf_model)   
varImpPlot(rf_model) 

# Make predictions on test set
pred_test <- predict(rf_model, newdata = X_test, type= "class")

confusionMatrix(table(pred_test, y_test))

# ROC
roc <- roc(response = y_test, predictor = as.numeric(pred_test), levels = c(0, 1))
plot(roc, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

# 2) Using Caret 'rf'
set.seed(33)

# Cross validation 
control <- trainControl(
  method='cv', 
  number=3, 
  search='grid'
)

# Hyperparameter grid
tunegrid <- expand.grid(
  .mtry = c(1:10)
)

# Create model
rf_gridsearch <- train(
  x = X_train,
  y = y_train_fac,
  method = 'rf',
  tuneGrid = tunegrid,
  metric = 'Accuracy',
  trControl = control,
)

print(rf_gridsearch)

# Save best model
best_rf_tuned <- rf_gridsearch$finalModel

importance(best_rf_tuned)   
varImpPlot(best_rf_tuned) 

# Make predictions on test set
pred_test_tuned <- predict(best_rf_tuned, newdata = X_test, type = "class")

confusionMatrix(table(pred_test_tuned, y_test))

# ROC
roc_tuned <- roc(response = y_test, predictor = as.numeric(pred_test_tuned), levels = c(0, 1))
plot(roc_tuned, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

# 3) Using Caret 'ranger'
# Cross validation 

# Hyperparameter grid
tunegrid_ranger <- expand.grid(
  .mtry = c(1:10), min.node.size = c(1:12)
)

# Create model
rf_ranger <- train(
  x = X_train,
  y = y_train_fac,
  method = 'ranger',
  tuneGrid = tunegrid_ranger,
  metric = 'Accuracy',
  trControl = control,
)

print(rf_ranger)

# Save best model
best_rf_ranger <- rf_ranger$finalModel

importance(best_rf_ranger)   
varImpPlot(best_rf_ranger) 

# Make predictions on test set
pred_test_ranger <- predict(best_rf_ranger, newdata = X_test, type = "class")

confusionMatrix(table(pred_test_ranger, y_test))

# ROC
roc_ranger <- roc(response = y_test, predictor = as.numeric(pred_test_ranger), levels = c(0, 1))
plot(roc_ranger, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

