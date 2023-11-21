library(caret)
library(dplyr)
library(kknn)
library(pROC)

set.seed(33)
# 1) Using 'knn' method
# Create numerical dataframe
numerical_unsupervised_significant_df_PHQ9 <- significant_uncorrelated_dataset_ML_2 %>%
  select_if(is.numeric)

# Separate data into variables and target
X <- numerical_unsupervised_significant_df_PHQ9 %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(significant_uncorrelated_dataset_ML_2$PHQ9_status)
levels(y) = c("Subclinical", "MD")
y <- relevel(y, ref = "MD")

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled <- predict(preProcValues, X_test)

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

y_test_num <- y_test
levels(y_test_num) = c(1, 0)

# Cross validation 
control <- trainControl(
  method='repeatedcv', 
  number=3, 
  repeats = 3,
  search='grid',
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up"
)

# Hyperparameter grid
tunegrid <- expand.grid(
  k = c(2:10) # k = 1, 4 was chosen but overfitting
)

# Create model
knn_gridsearch <- train(
  x = X_train_scaled,
  y = y_train_fac,
  method = 'knn',
  tuneGrid = tunegrid,
  metric = 'ROC',
  trControl = control
)

print(knn_gridsearch)
varImp(knn_gridsearch)
plot(varImp(knn_gridsearch))

best_knn <- knn_gridsearch$finalModel

# Make predictions on test set
pred_test_knn <- predict(best_knn, newdata = X_test_scaled, type = "class")
pred_test_knn_probs <- predict(best_knn, newdata = X_test_scaled, type = "prob")[,1]
pred_test_knn_num <- ifelse(pred_test_knn == "MD", 1, 0)

confusionMatrix(table(pred_test_knn, y_test))

# ROC
roc_knn <- roc(response = y_test_num, predictor = pred_test_knn_probs, levels = c(0, 1))
plot(roc_knn, main = "KNN ROC Curve", auc.polygon = TRUE, grid = TRUE, 
     print.auc = TRUE, las = 1, auc.polygon.col = "aliceblue")


# 2) tuned model with 'kknn'
set.seed(33)
# Hyperparameter grid
tunegrid_2 <- expand.grid(
  kmax = 10,
  distance = c(1:2),
  kernel = c("rectangular", "triangular", "epanechnikov", "optimal")
)

# Create model
knn_gridsearch_2 <- train(
  x = X_train_scaled,
  y = y_train_fac,
  method = 'kknn',
  tuneGrid = tunegrid_2,
  metric = 'ROC',
  trControl = control,
)

print(knn_gridsearch_2)

best_knn_2 <- knn_gridsearch_2$finalModel

# Make predictions on test set
pred_test_knn_2 <- predict(best_knn_2, newdata = X_test_scaled)
pred_test_knn_2_probs <- predict(best_knn_2, newdata = X_test_scaled, type = "prob")[,1]
pred_test_knn_2_num <- ifelse(pred_test_knn_2 == "MD", 1, 0)

confusionMatrix(table(pred_test_knn_2, y_test))

# ROC
roc_knn_2 <- roc(response = y_test_num, predictor = pred_test_knn_2_probs, levels = c(0, 1))
plot(roc_knn_2, main = "KNN ROC Curve", auc.polygon = TRUE, grid = TRUE, 
     print.auc = TRUE, las = 1, auc.polygon.col = "aliceblue")

