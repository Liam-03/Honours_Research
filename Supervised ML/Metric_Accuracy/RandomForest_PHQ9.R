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

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

# Create model
rf_model <- randomForest(x = X_train, y = y_train)
rf_model 

varImp(rf_model)   
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

varImp(best_rf_tuned)   
varImpPlot(best_rf_tuned) 

# Make predictions on test set
pred_test_tuned <- predict(best_rf_tuned, newdata = X_test, type = "class")

confusionMatrix(table(pred_test_tuned, y_test))

# ROC
roc_tuned <- roc(response = y_test, predictor = as.numeric(pred_test_tuned), levels = c(0, 1))
plot(roc_tuned, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

# 2b) Excluding the least important features
features <- varImp(best_rf_tuned)   
important_features <- features %>%
  filter(Overall > 1)

important_data <- unsupervised_df_PHQ9[, c(rownames(important_features), "PHQ9_status")]

set.seed(33)  # Set a specific seed value

# Separate data into variables and target
X_imp <- important_data %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y_imp <- as.factor(important_data$PHQ9_status)
y_imp <- relevel(y_imp, ref = "1")

# Split data into variables and target
trainIndex_imp <- createDataPartition(y_imp, p = .7, list = FALSE, times = 1)[,1]

X_train_imp <- X_imp[trainIndex_imp, ]
X_test_imp  <- X_imp[-trainIndex_imp, ]

y_train_imp <- y_imp[trainIndex_imp]
y_train_fac_imp <- as.factor(y_train_imp)
y_test_imp  <- y_imp[-trainIndex_imp]

rf_gridsearch_imp <- train(
  x = X_train_imp,
  y = y_train_fac_imp,
  method = 'rf',
  tuneGrid = tunegrid,
  metric = 'Accuracy',
  trControl = control,
)

print(rf_gridsearch_imp)

# Save best model
best_rf_imp <- rf_gridsearch_imp$finalModel
varImp(best_rf_imp)   

# Make predictions on test set
pred_test_imp <- predict(best_rf_imp, newdata = X_test_imp, type = "class")

confusionMatrix(table(pred_test_imp, y_test_imp))

# ROC
roc_imp <- roc(response = y_test_imp, predictor = as.numeric(pred_test_imp), levels = c(0, 1))
plot(roc_imp, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)


# 3) Using Caret 'ranger'
set.seed(33)

# Hyperparameter grid
tunegrid_ranger <- expand.grid(
  mtry = c(1:10), 
  min.node.size = c(1:12), 
  splitrule = "gini"
)

# Create model
rf_ranger <- train(
  x = X_train,
  y = y_train_fac,
  method = 'ranger',
  tuneGrid = tunegrid_ranger,
  metric = 'Accuracy',
  trControl = control,
  importance = "impurity"
)

print(rf_ranger)

# Save best model
best_rf_ranger <- rf_ranger$finalModel

importance(best_rf_ranger)   

# Make predictions on test set
pred_test_ranger <- predict(object = best_rf_ranger, data = X_test)

confusionMatrix(table(pred_test_ranger$predictions, reference = y_test))

# ROC
roc_ranger <- roc(response = y_test, predictor = as.numeric(pred_test_ranger$predictions), levels = c(0, 1))
plot(roc_ranger, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

