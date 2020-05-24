# !diagnostics off
# April, 2017

source("Code/MetaAnalysis_4way_k.r")

k_RVEModel <- RVEModel

#### Read in data from EXCEL file ####

sheet <- "Processed - Stability"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
classes <- c("D", "NotD")
identifier <- ".4"
fullRobu <- T

data <- getTransitionsData(filename, sheet, identifier, classes)

# Condense to 2-way table
dataNum <- data[, grep(identifier, names(data))]
data$NotD.NotD.4 <- rowSums(dataNum[, grep("D", names(dataNum), invert = T)])
data$D.NotD.4 <- rowSums(dataNum[, c("D.A.4", "D.B.4", "D.C.4")])
data$NotD.D.4 <- rowSums(dataNum[, c("A.D.4", "B.D.4", "C.D.4")])
data[, c("D.A.4", "D.B.4", "D.C.4")] <- NULL

#### Build transitions tables ####
user_beta <- buildTransitionsTables(data, classes, fullRobu)
