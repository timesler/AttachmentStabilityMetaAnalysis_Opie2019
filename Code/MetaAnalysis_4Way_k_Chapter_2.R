# April, 2017

source("Code/Utilities/init.RVE.r")

#### Read in data from EXCEL file ####

sheet <- "4-way Chapter 2"
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

dataRaw <- getData(filename, sheet, dataNames, numericCols, F)

#### Build random-effects models ####

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "k",
  measureCol = "Corr",
  n = "n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans
)

Summary.RVE(
  RVEModel,
  fileName = "Output/StatisticalResults_4way_k_Ch2_20190330.txt"
)

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Cohen's k",
  fileName = "Figures/4way_k_Forest_Ch2_20190330.tiff"
) 

funnel.RVE(
  RVEModel,
  fileName = "Figures/4way_k_Funnel_Ch2_20190330.tiff",
  trimnfill = T,
  plotReg = T
)

if (zTrans) {
  transf <- tanh
} else {
  transf <- function(x) {x}
}
correlations <- lapply(RVEModel$REModels, function(x) transf(x$reg_table$b.r))
save(correlations, file = "Output/STAB/4-way/Correlations_Ch2_20190330.RData")

