# !diagnostics off
# April, 2017

source("Code/MetaAnalysis_4way_k.r")

k_RVEModel <- RVEModel

#### Read in data from EXCEL file ####

sheet <- "Processed - Stability"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
classes <- c("A", "NotA")
identifier <- ".4"
fullRobu <- T

data <- getTransitionsData(filename, sheet, identifier, classes)

# Condense to 2-way table
dataNum <- data[, grep(identifier, names(data))]
data$NotA.NotA.4 <- rowSums(dataNum[, grep("A", names(dataNum), invert = T)])
data$A.NotA.4 <- rowSums(dataNum[, c("A.B.4", "A.B.4", "A.D.4")])
data$NotA.A.4 <- rowSums(dataNum[, c("B.A.4", "C.A.4", "D.A.4")])
data[, c("A.B.4", "A.C.4", "A.D.4")] <- NULL

#### Build transitions tables ####
user_beta <- buildTransitionsTables(data, classes, fullRobu)
