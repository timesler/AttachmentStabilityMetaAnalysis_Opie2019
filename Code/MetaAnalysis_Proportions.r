# Feb, 2018

source("Code/TransitionAnalysis_4way.r")
user_beta_4w <- user_beta

source("Code/TransitionAnalysis_BB.r")
user_beta_BB <- user_beta

source("Code/TransitionAnalysis_DD.r")
user_beta_DD <- user_beta

#### Read in data from EXCEL file ####

sheet <- "4-way"
filename <- "Data/20180121_Data_Extraction_Table.xlsx"
zTrans <- F
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
  "PriorInclusionSIS",
  "SocialRisk",
  "MedicalRisk",
  "Published",
  "Interrater Reliability",
  "PriorInclusionODO",
  "Gender"
)
numericCols <- c(1, 3, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17)

dataRaw <- getData(filename, sheet, dataNames, numericCols, F, F)

#### Build random-effects models ####

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "AA Proportion",
  n = "AA n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_4w, function(x) x$A),
  measureSub = "AA expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/AA_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "BB Proportion",
  n = "BB n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_4w, function(x) x$B),
  measureSub = "BB expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/BB_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "CC Proportion",
  n = "CC n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_4w, function(x) x$C),
  measureSub = "CC expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/CC_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "DD Proportion",
  n = "DD n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_4w, function(x) x$D),
  measureSub = "DD expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/DD_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "ISIS Proportion",
  n = "ISIS n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_BB, function(x) x$NotB),
  measureSub = "ISIS expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/ISIS_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "p",
  measureCol = "OO Proportion",
  n = "OO n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = zTrans,
  user_beta = lapply(user_beta_DD, function(x) x$NotD),
  measureSub = "OO expected proportion"
)
RVEModel$data <- dataRaw[, c("StudyID", "Authors", "Sample", "Subsample", "Interval")] %>% left_join(RVEModel$data)
RVEModel$data$grp1 <- RVEModel$data$Interval
RVEModel$data$grp2 <- RVEModel$data$StudyID

forest.RVE(
  RVEModel,
  studyNames = "Authors",
  sampleNames = "Subsample",
  effSizeName = "Percentage Residual",
  fileName = "Figures/OO_p_Forest_20190330.tiff"
)

RVEModel <- MetaAnalysis.RVE(
  data = dataRaw,
  measureType = "k",
  measureCol = "Corr",
  n = "n",
  grping1 = "Interval",
  grping2 = "StudyID",
  published = "Published",
  PA = "PA",
  zTrans = T
)

funnel.RVE(
  RVEModel,
  fileName = "Figures/Four_way_k_Funnel_20190330.tiff",
  trimnfill = F,
  plotReg = T,
  effSizeName = expression(paste("Cohen's", ~italic(kappa)))
)
