# April, 2017

source("Code/Utilities/init.RVE.r")

#### Read in data from EXCEL file ####

sheet <- "2-way k"
filename <- "Data/20180121_Data_Extraction_Table.xlsx"
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
  "PriorInclusion",
  "SocialRisk",
  "MedicalRisk",
  "Published",
  "Interrater Reliability",
  "PriorInclusion_DO",
  "Gender"
)
numericCols <- c(1, 3, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17)

dataRaw <- getData(filename, sheet, dataNames, numericCols)

#### Build random-effects models ####

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "k",
  measureCol = "Corr",
  n = "n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  zTrans = zTrans
)

Summary.RVE(
  RVEModel,
  fileName = "Output/StatisticalResults_2x2_k_20190330.txt"
)

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Cohens's k",
  fileName = "Figures/2x2_Forest_k_20190330.tiff"
)

