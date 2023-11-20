library(kernelshap)
library(shapviz)

# RF
s <- kernelshap(rf_ranger, X = X_train, bg_X = X_train, type = "prob")
sv <- shapviz(s)
sv_importance(sv, kind = "beeswarm", max_display = 7)

imp <- (varImp(rf_ranger))

# GBM
s <- kernelshap(best_gbm, X = X_train, bg_X = X_train, type = "response")
sv <- shapviz(s)
sv_importance(sv, kind = "beeswarm", max_display = 7)

imp <- as.data.frame(varImp(best_rf_tuned))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

# GLM
s <- kernelshap(glm_model, X = X_train, bg_X = X_train, type = "prob")
sv <- shapviz(s)
sv_importance(sv, kind = "beeswarm", max_display = 7)

imp <- as.data.frame(varImp(best_rf_tuned))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

# KNN
s <- kernelshap(knn_gridsearch, X = X_train, bg_X = X_train, type = "prob")
sv <- shapviz(s)
sv_importance(sv, kind = "beeswarm", max_display = 10)

imp <- as.data.frame(varImp(best_rf_tuned))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]


