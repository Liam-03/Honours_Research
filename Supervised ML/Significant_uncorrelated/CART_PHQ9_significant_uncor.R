library(caret)
library(pROC)
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(33)  # Set a specific seed value

# Separate data into variables and target
# Remove loc/ES as there were unseen categories in the test data 
unsupervised_significant_df_PHQ9_no_loc_emp <- uncorrelated_unsupervised_significant_df_PHQ9 %>%
  select(-location, -Employment_status) 

X_no_loc_emp <- unsupervised_significant_df_PHQ9_no_loc_emp %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(uncorrelated_unsupervised_significant_df_PHQ9$PHQ9_status)
levels(y) = c("Subclinical", "MD")
y <- relevel(y, ref = "MD")

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X_no_loc_emp[trainIndex, ]
X_test  <- X_no_loc_emp[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

y_test_num <- y_test
levels(y_test_num) = c(1, 0)

# Create model
control <- trainControl(
  method='repeatedcv', 
  number =3,
  search='grid',
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up"
)

tunegrid <- expand.grid(
  cp = c(0.001, 0.01, 0.05, 0.1)) 

CART = train(
  x = X_train,
  y = y_train_fac,
  method = 'rpart',
  tuneGrid = tunegrid,
  trControl = control,
  metric = 'ROC'
)

CART_final = CART$finalModel

printcp(CART_final) # display the results

# plot tree and obtain importance
rpart.plot(CART_final)
CART_final$variable.importance

# Make predictions
pred_test_CART <- predict(CART_final, X_test, type = "class")
pred_test_CART_probs <- predict(CART_final, X_test, type = "prob")[,1]
pred_test_CART <- relevel(pred_test_CART, ref = "MD")

pred_test_CART_num = ifelse(pred_test_CART == "MD", 1, 0)
             
confusionMatrix(table(pred_test_CART, y_test))

# ROC
roc_CART <- roc(response = y_test_num, predictor = pred_test_CART_probs, levels = c(0, 1))
plot(roc_CART, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

