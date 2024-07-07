#Required packages
library(DALEX)

#SHAP explainer generation with minimalistic RF model (20 features)
explainer_rf_20 <- explain(RF_MM3_1_mtry_6,
                     data = as.data.frame(data_20_F), 
                     y = as.numeric(data_20_F$Group),
                     label = "RF_TOP20_FEATURES")