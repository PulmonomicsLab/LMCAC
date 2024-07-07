#Required Packages
library(caret)

set.seed(78495)

#Input Data
data_80 <- read.table("split_80.csv", sep = ",", header = TRUE)
data_80 <- data_80[,-1]
data_a <- data_80[,-1]
data_b <- data_a[,-1]
data_b$Group <- as.factor(data_b$Group)
Blind_input <- read.csv("split_20.csv", header = TRUE)
Blind <- Blind_input [,  -1:-2]
Blind$Group <- as.factor(Blind$Group)

#Random Forest model parameter tuning
set.seed(78495)
seeds_rf <- vector(mode = "list", length = 6) 
for(i in 1:6) seeds_rf[[i]] <- rep(78495, 20) 
seeds_rf <- lapply(seeds_rf, as.vector)
grid_RF <- expand.grid(mtry = sample(1:ncol(data_b), 20))
set.seed(78495)
fitControl_RF <- trainControl(method = "CV", savePredictions = "all", p = 0.8, verboseIter = TRUE,  classProbs = TRUE, number = 5, summaryFunction = multiClassSummary, seeds = seeds_rf)
RF_model <- train(Group ~ ., data = data_b, method = "rf", trControl = fitControl_RF, verbose = TRUE, tuneGrid = grid_RF, importance = TRUE)

#for selecting best mtry
View(RF_model$results)

#Tuning model with best mtry and genertion of final model
set.seed(78495)
seeds_rf_1 <- vector(mode = "list", length = 6) 
for(i in 1:6) seeds_rf_1[[i]] <- rep(78495, 1)
seeds_rf_1 <- lapply(seeds_rf_1, as.vector)
grid_RF_1 <- expand.grid(mtry = 25)
fitControl_RF_1 <- trainControl(method = "CV", savePredictions = "all", p = 0.8, verboseIter = TRUE,  classProbs = TRUE, number = 5, summaryFunction = multiClassSummary, seeds = seeds_rf_1)
RF_model_mtry_25 <- train(Group ~ ., data = data_b, method = "rf", trControl = fitControl_RF_1, verbose = TRUE, tuneGrid = grid_RF_1, importance = TRUE)
RF_results <- RF_model_mtry_25$results
RF_pred <- RF_model_mtry_25$pred
RF_cf <- confusionMatrix(data = RF_pred$pred, reference = RF_pred$obs, mode = "everything")
RF_MC_summary <- multiClassSummary(RF_pred, lev = levels(RF_pred$pred))
RF_MC_summary
RF_cf

#Blind Dataset Validation
prediction_RF_model_mtry_25 <- predict.train(RF_model_mtry_25, Blind, probability = TRUE)
pred_RF_model_mtry_25_prob <- predict.train(RF_model_mtry_25, Blind, type = "prob")
pred_RF_model_mtry_25_prob$pred <- prediction_RF_model_mtry_25
pred_RF_model_mtry_25_prob$obs <- Blind$Group
RF_model_mtry_25_test_MC <- multiClassSummary(pred_RF_model_mtry_25_prob, lev = levels(pred_RF_model_mtry_25_prob$pred))
RF_model_mtry_25_test_cf <- confusionMatrix(data = prediction_RF_model_mtry_25, reference = Blind$Group, mode = "everything")
RF_model_mtry_25_test_MC
RF_model_mtry_25_test_cf