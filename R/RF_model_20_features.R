
#Feature Selection
var <- varImp(RF_model_mtry_25)

#Top 20 features
View(var$importance)
features <- rownames(var$importance[var$importance$Asthma >52.87,])


# Minimalistic model with top 20 features
data_20_F <- data_b[, c("Group", features)]
data_20_F_Blind<- Blind[, c("Group", features)]

#Minimalistic RF model parameter tuning
set.seed(78495)
seeds_RF_MM3 <- vector(mode = "list", length = 6) 
for(i in 1:6) seeds_RF_MM3[[i]] <- rep(78495, 20 ) 
seeds_RF_MM3<- lapply(seeds_RF_MM3, as.vector)
grid_RF_MM3 <- expand.grid(mtry = sample(1:ncol(data_20_F), 20))
set.seed(78495)
fitControl_RF_MM3 <- trainControl(method = "CV", savePredictions = "all", p = 0.8, verboseIter = TRUE,  classProbs = TRUE, number = 5, summaryFunction = multiClassSummary, seeds = seeds_RF_MM3)
RF_MM3 <- train(Group ~ ., data_20_F, method = "rf", trControl = fitControl_RF_MM3 , verbose = TRUE, tuneGrid = grid_RF_MM3)

#for selecting best mtry
View(RF_MM3$results)

#Tuning model with best mtry and genertion of final minimalistic RF model
set.seed(78495)
seeds_RF_MM3_1 <- vector(mode = "list", length = 6) 
for(i in 1:6) seeds_RF_MM3_1[[i]] <- rep(78495, 1)
seeds_RF_MM3_1 <- lapply(seeds_RF_MM3_1 , as.vector)
grid_RF_MM3_1 <- expand.grid(mtry =6)
set.seed(78495)
fitControl_RF_MM3_1 <- trainControl(method = "CV", savePredictions = "all", p = 0.8, verboseIter = TRUE,  classProbs = TRUE, number = 5, summaryFunction = multiClassSummary, seeds =seeds_RF_MM3_1)
RF_MM3_1_mtry_6  <- train(Group ~ ., data = data_20_F, method = "rf", trControl = fitControl_RF_MM3_1, verbose = TRUE, tuneGrid = grid_RF_MM3_1)
RF_MM3_1_mtry_6_results <- RF_MM3_1_mtry_6$results
RF_MM3_1_mtry_6_pred <- RF_MM3_1_mtry_6$pred
RF_MM3_1_mtry_6_cf <- confusionMatrix(data = RF_MM3_1_mtry_6_pred$pred, reference = RF_MM3_1_mtry_6_pred$obs, mode = "everything")
RF_MM3_1_mtry_6_MC_summary <- multiClassSummary(RF_MM3_1_mtry_6_pred, lev = levels(RF_MM3_1_mtry_6_pred$pred))
RF_MM3_1_mtry_6_MC_summary
RF_MM3_1_mtry_6_cf

#Blind Dataset Validation
prediction_RF_MM3_1_mtry_6 <- predict.train(RF_MM3_1_mtry_6, data_20_F_Blind, probability = TRUE)
pred_RF_MM3_1_mtry_6_prob <- predict.train(RF_MM3_1_mtry_6, data_20_F_Blind, type = "prob")
pred_RF_MM3_1_mtry_6_prob$pred <- prediction_RF_MM3_1_mtry_6
pred_RF_MM3_1_mtry_6_prob$obs <- data_20_F_Blind$Group
RF_MM3_1_mtry_6_test_MC <- multiClassSummary(pred_RF_MM3_1_mtry_6_prob, lev = levels(pred_RF_MM3_1_mtry_6_prob$pred))
RF_MM3_1_mtry_6_test_cf <- confusionMatrix(data = prediction_RF_MM3_1_mtry_6, reference = data_20_F_Blind$Group, mode = "everything")
RF_MM3_1_mtry_6_test_MC
RF_MM3_1_mtry_6_test_cf