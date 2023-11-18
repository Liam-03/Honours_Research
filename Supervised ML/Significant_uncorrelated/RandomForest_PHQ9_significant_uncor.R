library(caret)
library(pROC)
library(dplyr)
library(randomForest)
library(ranger)
library(e1071)

# 1) Using randomForest
set.seed(33)  # Set a specific seed value

# Separate data into variables and target
X <- uncorrelated_unsupervised_significant_df_PHQ9 %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y <- as.factor(uncorrelated_unsupervised_significant_df_PHQ9$PHQ9_status)
levels(y) = c("Subclinical", "MD")
y <- relevel(y, ref = "MD")

# Split into training and test sets
trainIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)[,1]

X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]

y_train <- y[trainIndex]
y_train_fac <- as.factor(y_train)
y_test  <- y[-trainIndex]

y_train_num <- y_train
levels(y_train_num) = c(1, 0)

y_test_num <- y_test
levels(y_test_num) = c(1, 0)

# Create model
rf_model <- randomForest(x = X_train, y = y_train)
rf_model 

varImp(rf_model)   
varImpPlot(rf_model) 

# Make predictions on test set
pred_test <- predict(rf_model, newdata = X_test, type= "class")
pred_test_probs <- predict(rf_model, newdata = X_test, type= "prob")[,1]
pred_test_num <- ifelse(pred_test == "MD", 1, 0)

confusionMatrix(table(pred_test, y_test))

# ROC
roc <- roc(response = y_test_num, predictor = pred_test_probs, levels = c(0, 1))
plot(roc, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, print.auc = TRUE)


# 2) Using Caret 'rf'
set.seed(33)

# Cross validation 
control <- trainControl(
  method='repeatedcv', 
  number=3,
  repeats = 3,
  search='grid',
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up",
  savePredictions = TRUE
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
  metric = 'ROC',
  trControl = control
)

print(rf_gridsearch)

# Save best model
best_rf_tuned <- rf_gridsearch$finalModel

varImp(best_rf_tuned)   
varImpPlot(best_rf_tuned, n.var = 5) 

# Predictions on training set
pred_train_tuned_class <- predict(best_rf_tuned, newdata = X_train, type = "class")
confusionMatrix(table(pred_train_tuned_class, y_train))

# Make predictions on test set
pred_test_tuned_class <- predict(best_rf_tuned, newdata = X_test, type = "class")
pred_test_tuned_probs <- predict(best_rf_tuned, newdata = X_test, type = "prob")[,1]
pred_test_tuned_num <- ifelse(pred_test_tuned == "MD", 1, 0)

confusionMatrix(table(pred_test_tuned_class, y_test))

# ROC
roc_tuned <- roc(response = y_test_num, predictor = pred_test_tuned_probs, levels = c(0, 1))
par(cex = 1)
plot(roc_tuned, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, 
     print.auc = TRUE, las = 1, auc.polygon.col = "aliceblue")


# 2b) Excluding the least important features
features <- varImp(best_rf_tuned)   
important_features <- features %>%
  filter(Overall > 1.5)

important_data <- uncorrelated_unsupervised_significant_df_PHQ9[, c(rownames(important_features), "PHQ9_status")]

set.seed(33)  # Set a specific seed value

# Separate data into variables and target
X_imp <- important_data %>%
  select(-PHQ9_status) %>%
  as.data.frame()

y_imp <- as.factor(important_data$PHQ9_status)
levels(y_imp) = c("Subclinical", "MD")
y_imp <- relevel(y, ref = "MD")

# Split data into variables and target
trainIndex_imp <- createDataPartition(y_imp, p = .7, list = FALSE, times = 1)[,1]

X_train_imp <- X_imp[trainIndex_imp, ]
X_test_imp  <- X_imp[-trainIndex_imp, ]

y_train_imp <- y_imp[trainIndex_imp]
y_train_fac_imp <- as.factor(y_train_imp)
y_test_imp  <- y_imp[-trainIndex_imp]

y_test_imp_num <- y_test_imp
levels(y_test_imp_num) = c(1, 0)

# Create model
rf_gridsearch_imp <- train(
  x = X_train_imp,
  y = y_train_fac_imp,
  method = 'rf',
  tuneGrid = tunegrid,
  metric = 'ROC',
  trControl = control,
)

print(rf_gridsearch_imp)

# Save best model
best_rf_imp <- rf_gridsearch_imp$finalModel
varImp(best_rf_imp)   

# Make predictions on test set
pred_test_imp <- predict(best_rf_imp, newdata = X_test_imp)
pred_test_imp_probs <- predict(best_rf_imp, newdata = X_test_imp, type = "prob")[,1]
pred_test_imp_num <- ifelse(pred_test_imp == "MD", 1, 0)

confusionMatrix(table(pred_test_imp, y_test_imp))

# ROC
roc_imp <- roc(response = y_test_imp_num, predictor = pred_test_imp_probs, levels = c(0, 1))
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
  metric = 'ROC',
  trControl = control,
  importance = "impurity"
)

print(rf_ranger)

# Save best model
best_rf_ranger <- rf_ranger$finalModel
varImp(rf_ranger)
a <- importance(best_rf_ranger)  

# Graph model importances
a <- c("Mean Click Duration" = 5.470383, "Mean Response Time" = 6.502750, "Mean Velocity" = 4.941365, "Mean Angle Change" = 4.896404, 
       "Mean Deviation from Ideal" = 5.462847, "Median Acceleration" = 6.151039)

df <- data.frame(Variable = names(a), Importance = a)
df <- df[order(-df$Importance),]

ggplot(df, aes(x = Importance, y = reorder(Variable, Importance), fill = df$Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Decrease Gini (Importance)",
       y = "") +
  geom_text(aes(label = round(Importance, 2), hjust = -0.2), vjust = 0.5, size = 4) +
  guides(fill = "none") + 
  coord_cartesian(xlim = c(0, max(df$Importance) * 1.05)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  ggtitle("Top 6 Feature Importances for Random Forest") +  # Set the title
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 12))

b <- data.frame(
  Variable = c("Location", "Gender", "Age", "Education Level", "Marital Status", 
               "Employment_status", "Gross Annual Household Income (US$)", 
               "Height (cm)", "Weight (kg)", "Mean Click Duration", "Response Time", 
               "Mean Velocity", "Mean Angle Change", "Mean Deviation from Ideal", 
               "Minimum Acceleration", "Median Acceleration"),
  Value = c(1.740927, 1.330472, 3.864723, 3.436593, 3.120144, 1.836787, 
            1.988896, 4.111899, 4.818427, 5.470383, 6.502750, 4.941365, 
            4.896404, 5.462847, 3.995373, 6.151039)
)

b <- b[order(-b$Value), ]
ggplot(b, aes(x = Value, y = reorder(Variable, Value), fill = b$Value)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Decrease Gini (Importance)",
       y = "") +
  geom_text(aes(label = round(Value, 2), hjust = -0.2), vjust = 0.5, size = 4) +
  guides(fill = "none") + 
  coord_cartesian(xlim = c(0, max(b$Value) * 1.05)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  ggtitle("Top 6 Feature Importances for Random Forest") +  # Set the title
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 12))

# Make predictions on test set
pred_test_ranger <- predict(object = best_rf_ranger, data = X_test)
pred_test_ranger_classes <- ifelse(pred_test_ranger$predictions[, "MD"] > 0.5, 1, 0)
pred_test_ranger_classes <- factor(pred_test_ranger_classes, levels = levels(y_test_num))

confusionMatrix(as.factor(pred_test_ranger_classes), as.factor(y_test_num))

# ROC
roc_ranger <- roc(response = y_test_num, predictor = pred_test_ranger$predictions[,1], levels = c(0, 1))
plot(roc_ranger, main = "ROC Curve", auc.polygon = TRUE, grid = TRUE, 
     print.auc = TRUE, las = 1, auc.polygon.col = "aliceblue", legacy.axes = T)
