# April, 2017

source("Code/Utilities/init.RVE.r")

#### Read in data from EXCEL file ####

sheet <- "IsD r"
filename <- "Data/20200213_Data_Extraction_Table.xlsx"
zTrans <- T
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
  "PriorInclusion_SIS",
  "SocialRisk",
  "MedicalRisk",
  "Published",
  "Interrater Reliability",
  "PriorInclusion",
  "Gender",
  "k"
)
numericCols <- c(1, 3, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17, 19)

dataRaw <- getData(filename, sheet, dataNames, numericCols)

#### Build random-effects models ####

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "r",
  measureCol = "Corr",
  n = "n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  zTrans = zTrans
)

Summary.RVE(
  RVEModel,
  fileName = "Output/StatisticalResults_IsD_20200213.txt"
)

# Get k for plotting along side r values
RVEModel$data$k <- format(round(RVEModel$data$k, 2), trim = F)
RVEModel$data$k[grepl("NA",RVEModel$data$k)] <- ""

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Pearson's r",
  fileName = "Figures/IsD_Forest_20200213.tiff",
  extraCol = "k",
  extraColName = "k"
) 

funnel.RVE(
  RVEModel,
  fileName = "Figures/IsD_Funnel_20200213.tiff",
  trimnfill = T,
  plotReg = T,
  effSizeName = "Pearson's r"
)

