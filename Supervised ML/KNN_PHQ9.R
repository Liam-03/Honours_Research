library(caret)
library(dplyr)
library(kknn)
library(pROC)

set.seed(33)
# 1) Using 'knn' method
# Create numerical dataframe
numerical_unsupervised_df_PHQ9 <- unsupervised_df_PHQ9 %>%
  select_if(is.numeric)

# Separate data into variables and target
X <- numerical_unsupervised_df_PHQ9 %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(numerical_unsupervised_df_PHQ9$PHQ9_status)
y <- relevel(y, ref = "1")

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

# Cross validation 
control <- trainControl(
  method='cv', 
  number=3, 
  search='grid'
)

# Hyperparameter grid
tunegrid <- expand.grid(
  k = c(5:10) # k = 1, 4 was chosen but overfitting
)

# Create model
knn_gridsearch <- train(
  x = X_train_scaled,
  y = y_train_fac,
  method = 'knn',
  tuneGrid = tunegrid,
  metric = 'Accuracy',
  trControl = control,
)

print(knn_gridsearch)
varImp(knn_gridsearch)

best_knn <- knn_gridsearch$finalModel

# Make predictions on test set
pred_test_knn <- predict(best_knn, newdata = X_test_scaled, type = "class")

confusionMatrix(table(pred_test_knn, y_test))

# ROC
roc_knn <- roc(response = y_test, predictor = as.numeric(pred_test_knn), levels = c(0, 1))
plot(roc_knn, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

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
  metric = 'Accuracy',
  trControl = control,
)

print(knn_gridsearch_2)

best_knn_2 <- knn_gridsearch_2$finalModel

# Make predictions on test set
pred_test_knn_2 <- predict(best_knn_2, newdata = X_test_scaled)

confusionMatrix(table(pred_test_knn_2, y_test))

# ROC
roc_knn_2 <- roc(response = y_test, predictor = as.numeric(pred_test_knn_2), levels = c(0, 1))
plot(roc_knn_2, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)


