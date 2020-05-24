# April, 2017

source("Code/Utilities/init.RVE.r")

#### Read in data from EXCEL file ####

sheet <- "4-way"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
dataNames <- c(
  "StudyID",
  "Authors",
  "Sample",
  "Subsample",
  "Interval",
  "n",
  "Corr",
  "PA",
  "Year",
  "Country",
  "CodingMethod",
  "PriorInclusion",
  "SocialRisk",
  "MedicalRisk",
  "Published",
  "Interrater Reliability",
  "PriorInclusion_DO",
  "Gender"
)
numericCols <- c(1, 3, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17)

data <- getData(filename, sheet, dataNames, numericCols, F, F)

# Compare classes
sink(file = "Output/ComparisonOfPatterns.txt", split = T)
classes <- c('BB', 'AA', 'CC', 'DD')
for (class1 in classes)
{
  for (class2 in classes[match(class1, classes):length(classes)])
  {
    if (class1 != class2)
    {
      print(paste(class1, 'vs', class2))
      reg <- compareClasses_IPD(data, c(class1, class2))
      print(reg)
    }
  }
}

class1 <- 'BB'
class2 <- 'ISIS'
print(paste(class1, 'vs', class2))
reg <- compareClasses_IPD(data, c(class1, class2))
print(reg)

class1 <- 'DD'
class2 <- 'OO'
print(paste(class1, 'vs', class2))
reg <- compareClasses_IPD(data, c(class1, class2))
print(reg)

print("All classes")
reg <- compareClasses_IPD(data, classes)
print(reg)

sink()

