# Creation of Train/Test and Blind set data from the OTU table

#Required packages
library(splitstackshape)
library(caret)

set.seed(78495)

#80% data for trainig/testing
data <- read.table("otu_table_ML.csv", sep = ",", header = TRUE)
split_80 <- stratified(data, c("BioProject" ,"Group"), size= 0.8, select = NULL, replace = FALSE)
write.csv(split_80, file = "split_80.csv")

#20% data for blind 
split_20 <- read.csv("Split_20_runs_only.csv", header = TRUE)
split <- read.csv("otu_table_ML.csv", header = TRUE)
split_20_samples <- merge(split, split_20, by.x = "Run")
write.csv(split_20_samples, file = "split_20.csv")
