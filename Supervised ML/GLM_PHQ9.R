library(dplyr)
library(pROC)
library(caret)
library(glmnet)
library(Matrix)

set.seed(33)

# 1) Basic GLM
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

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

# Cross validation 
control <- trainControl(
  method='cv', 
  number=3,
)

# Create model
glm_model = train(
  x = X_train,
  y = y_train_fac,
  method = 'glm',
  preProcess = c('center', 'scale'),
  trControl = control,
  metric = 'Accuracy'
)

print(glm_model)
varImp(glm_model)

# Make predictions on test set
pred_test_glm <- predict(glm_model, newdata = X_test)

confusionMatrix(table(pred_test_glm, y_test))

# ROC
roc_glm <- roc(response = y_test, predictor = as.numeric(pred_test_glm), levels = c(0, 1))
plot(roc_glm, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

# 2) Regularised GLM
tunegrid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),  # Explore alpha values from 0 to 1 in increments of 0.1
  lambda = 10^seq(-4, 4, by = 0.2)  # Explore a range of lambda values on a logarithmic scale
)

# Create model
glmnet_model = train(
  x = X_train,
  y = y_train_fac,
  method = 'glmnet',
  preProcess = c('center', 'scale'),
  tuneGrid = tunegrid,
  trControl = control,
  metric = 'Accuracy'
)

print(glmnet_model)
varImp(glmnet_model)

# Make predictions on test set
pred_test_glmnet <- predict(glmnet_model, newdata = X_test)

confusionMatrix(table(pred_test_glmnet, y_test))

# ROC
roc_glmnet <- roc(response = y_test, predictor = as.numeric(pred_test_glmnet), levels = c(0, 1))
plot(roc_glmnet, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

