# !diagnostics off
# April, 2017

source("Code/MetaAnalysis_4way_k.r")

k_RVEModel <- RVEModel

#### Read in data from EXCEL file ####

sheet <- "Processed - Stability"
filename <- "Data/20180121_Data_Extraction_Table.xlsx"
classes <- c("B", "NotB")
identifier <- ".4"
fullRobu <- T

data <- getTransitionsData(filename, sheet, identifier, classes)

# Condense to 2-way table
dataNum <- data[, grep(identifier, names(data))]
data$NotB.NotB.4 <- rowSums(dataNum[, grep("B", names(dataNum), invert = T)])
data$B.NotB.4 <- rowSums(dataNum[, c("B.A.4", "B.C.4", "B.D.4")])
data$NotB.B.4 <- rowSums(dataNum[, c("A.B.4", "C.B.4", "D.B.4")])
data[, c("B.A.4", "B.C.4", "B.D.4")] <- NULL

#### Build transitions tables ####
user_beta <- buildTransitionsTables(data, classes, fullRobu)
