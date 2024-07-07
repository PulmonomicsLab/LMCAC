
#Generation of OTU table as an input file 

#Required Packages
library(phyloseq)
library(microeco)
library(file2meco)

#Read the biom file
A = import_biom("table.biom", parseFunction=parse_taxonomy_default)
colnames(A@tax_table) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
A@tax_table@.Data = substring(A@tax_table@.Data, 4)

#Add metadata
A_meta <- read.csv("metadata.csv", header = TRUE, row.names = 1)
sample_data(A) <- A_meta
A_1 <- subset_taxa(A, Phylum != "Chordata")

#Phyloseq object to microeco object
data <- phyloseq2meco(A_1)
data$cal_abund()
t2 <- trans_classifier$new(dataset = data, y.response = "Group", x.predictors = "Genus")

#Saving the output file
write.csv(t2$data_feature, file = "otu_table_ML.csv")
