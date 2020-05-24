# !diagnostics off
# April, 2017

source("Code/MetaAnalysis_4way_k.r")

k_RVEModel <- RVEModel

#### Read in data from EXCEL file ####

sheet <- "Processed - Stability"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
classes <- c("C", "NotC")
identifier <- ".4"
fullRobu <- T

data <- getTransitionsData(filename, sheet, identifier, classes)

# Condense to 2-way table
dataNum <- data[, grep(identifier, names(data))]
data$NotC.NotC.4 <- rowSums(dataNum[, grep("C", names(dataNum), invert = T)])
data$C.NotC.4 <- rowSums(dataNum[, c("C.A.4", "C.B.4", "C.D.4")])
data$NotC.C.4 <- rowSums(dataNum[, c("A.C.4", "B.C.4", "D.C.4")])
data[, c("C.A.4", "C.B.4", "C.D.4")] <- NULL

#### Build transitions tables ####
user_beta <- buildTransitionsTables(data, classes, fullRobu)
