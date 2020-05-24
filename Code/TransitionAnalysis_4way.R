# !diagnostics off
# April, 2017

source("Code/MetaAnalysis_4way_k.r")

k_RVEModel <- RVEModel

#### Read in data from EXCEL file ####

sheet <- "Processed - Stability"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
classes <- c("B", "A", "C", "D")
identifier <- ".4"
fullRobu <- T

data <- getTransitionsData(filename, sheet, identifier, classes)

#### Build transitions tables ####
user_beta <- buildTransitionsTables(data, classes, fullRobu)
