library(caret)
library(pROC)
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(33)  # Set a specific seed value

# Separate data into variables and target
unsupervised_df_PHQ9_no_loc_emp <- unsupervised_df_PHQ9 %>%
  select(-location, -Employment_status)

X_no_loc_emp <- unsupervised_df_PHQ9_noloc %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(unsupervised_df_PHQ9$PHQ9_status)
y <- relevel(y, ref = "1")

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X_no_loc_emp[trainIndex, ]
X_test <- X_no_loc_emp[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

# Create model
CART <- rpart(PHQ9_status ~ ., data = unsupervised_df_PHQ9_noloc[trainIndex, ], method = "class")

printcp(CART) # display the results
plotcp(CART) # visualize cross-validation results

# plot tree and obtain importance
rpart.plot()
CART$variable.importance

# Make predictions
pred_test_CART <- predict(CART, X_test, type = "class")
pred_test_CART <- relevel(pred_test_CART, ref = "1")
             
confusionMatrix(table(pred_test_CART, y_test))

# ROC
roc_CART <- roc(response = y_test, predictor = as.numeric(pred_test_CART), levels = c(0, 1))
plot(roc_CART, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)

