library(dplyr)
library(pROC)
library(caret)
library(glmnet)
library(Matrix)

# 1) Basic GLM
# Create numerical dataframe
set.seed(33)
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

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

y_test_num <- y_test
levels(y_test_num) = c(1, 0)

# Cross validation 
control <- trainControl(
  method='repeatedcv', 
  number =3,
  repeats = 3,
  search='grid',
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up"
)

# Create model
glm_model = train(
  x = X_train,
  y = y_train_fac,
  method = 'glm',
  preProcess = c('center', 'scale'),
  trControl = control,
  metric = 'ROC'
)

print(glm_model)
varImp(glm_model)

# Make predictions on test set
pred_test_glm_probs <- predict(glm_model, newdata = X_test, type = "prob")[,1]
pred_test_glm <- predict(glm_model, newdata = X_test)
pred_test_glm_num <- ifelse(pred_test_glm == "MD", 1, 0)

confusionMatrix(table(pred_test_glm, y_test))

# ROC
roc_glm <- roc(response = y_test_num, predictor = pred_test_glm_probs, levels = c(0, 1))
plot(roc_glm, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

# 2) Regularised GLM
set.seed(33)

tunegrid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),  # Explore alpha values from 0 to 1 in increments of 0.1
  lambda = 10^seq(-3, 3, by = 0.2)  # Explore a range of lambda values on a logarithmic scale
)

# Create model
glmnet_model = train(
  x = X_train,
  y = y_train_fac,
  method = 'glmnet',
  preProcess = c('center', 'scale'),
  tuneGrid = tunegrid,
  trControl = control,
  metric = 'ROC'
)

print(glmnet_model)
varImp(glmnet_model)
plot(varImp(glmnet_model))

# Make predictions on test set
pred_test_glmnet <- predict(glmnet_model, newdata = X_test)
pred_test_glmnet_probs <- predict(glmnet_model, newdata = X_test, type = "prob")[,1]
pred_test_glmnet_num <- ifelse(pred_test_glmnet == "MD", 1, 0)

confusionMatrix(table(pred_test_glmnet, y_test))

# ROC
roc_glmnet <- roc(response = y_test_num, predictor = pred_test_glmnet_probs, levels = c(0, 1))
plot(roc_glmnet, main = "GLM ROC Curve", auc.polygon = TRUE, grid = TRUE, 
     print.auc = TRUE, las = 1, auc.polygon.col = "aliceblue")
