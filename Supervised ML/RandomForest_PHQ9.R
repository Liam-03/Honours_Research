library(caret)
library(dplyr)
library(randomForest)

# 1) Model 1
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

# 2) Model 2
control <- trainControl(
  method='cv', 
  number=3, 
  search='grid',
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

tunegrid <- expand.grid(
  .mtry = c(1:10)
)

rf_gridsearch <- train(
  x = X_train,
  y = y_train_fac,
  method = 'rf',
  tuneGrid = tunegrid,
  metric = 'Accuracy',
  trControl = control,
  class.weights = c(1, 7)
)

print(rf_gridsearch)

# Make predictions on test set
pred_test_tuned <- predict(rf_gridsearch, newdata = X_test)

confusionMatrix(table(pred_test_tuned, y_test))

