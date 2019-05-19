#load libraries
load.libraries <-
  c('ggplot2' ,
    'dplyr',
    'tidyr',
    'chron',
    'lubridate',
    'mosaic',
    'readr',
    'zoo',
    'corrplot',
    'plyr',
    'gridExtra',
    'arulesCBA',
    'caTools','car','MASS', 'carData','cellranger','car', 'caret','lattice', 'reshape')
install.lib <-
  load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib)
  install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


#Set working directory
setwd("C:/Study/Data Science/Capstone/Final Project - Priyanka")



#read data
ReadmissionData<-read.csv('Readmissions and Deaths - Hospital.csv', stringsAsFactors = F, na.strings = c("Not Available"))

#summary and structure of data
summary(ReadmissionData)

str(ReadmissionData)
# Total 18 variables with 67452 obs

#No. of NA data 
sum(is.na(ReadmissionData)) #117518

# No. of NA in score
sum(is.na(ReadmissionData$Score)) #25742, Means all NA in only score field.

unique(ReadmissionData$Measure.ID) # total 14 measures, 
#"MORT_30_AMI"        "MORT_30_CABG"       "MORT_30_COPD"       "MORT_30_HF"
#"MORT_30_PN"         "MORT_30_STK"        "READM_30_AMI"       "READM_30_CABG"
#"READM_30_COPD"      "READM_30_HF"        "READM_30_HIP_KNEE"  "READM_30_HOSP_WIDE"
#"READM_30_PN"        "READM_30_STK" 

#There are total 14 measures , 6 for Mortality and 8 for Readmission.

#take only Provider ID, measureID and score, rest are the coloumn are irrelevent 
tempReadmissionData = ReadmissionData[,c(1,10,13)]

#reshape the data in wide format
reshapeReadmissionData = reshape(tempReadmissionData, idvar = "Provider.ID", timevar = "Measure.ID", direction = "wide") 

#find number of NA in each measure ID
sapply(reshapeReadmissionData, function(x) (length(which(is.na(x)))))
# Provider.ID        Score.MORT_30_AMI       Score.MORT_30_CABG       Score.MORT_30_COPD 
# 0                     2430                     3780                     1227 
# Score.MORT_30_HF         Score.MORT_30_PN        Score.MORT_30_STK       Score.READM_30_AMI 
# 1200                      730                     2142                     2655 
# Score.READM_30_CABG      Score.READM_30_COPD        Score.READM_30_HF  Score.READM_30_HIP_KNEE 
# 3791                     1170                     1168                     2087 
# Score.READM_30_HOSP_WIDE        Score.READM_30_PN       Score.READM_30_STK 
# 423                      729                     2210 

#check if there are any duplicate row, nothing found
sum(duplicated(reshapeReadmissionData$Provider.ID)) #0, no duplicate data

#Lets drop the column for Mortality and only go ahead with Readmission measures.
reshapeReadmissionData <- reshapeReadmissionData[,c(1,8:15)]

#Scale the data
reshapeReadmissionData$Score.READM_30_AMI <-scale(reshapeReadmissionData$Score.READM_30_AMI)
reshapeReadmissionData$Score.READM_30_CABG <-scale(reshapeReadmissionData$Score.READM_30_CABG)
reshapeReadmissionData$Score.READM_30_COPD <-scale(reshapeReadmissionData$Score.READM_30_COPD)
reshapeReadmissionData$Score.READM_30_HF <-scale(reshapeReadmissionData$Score.READM_30_HF)
reshapeReadmissionData$Score.READM_30_HIP_KNEE <-scale(reshapeReadmissionData$Score.READM_30_HIP_KNEE)
reshapeReadmissionData$Score.READM_30_HOSP_WIDE <-scale(reshapeReadmissionData$Score.READM_30_HOSP_WIDE)
reshapeReadmissionData$Score.READM_30_PN <-scale(reshapeReadmissionData$Score.READM_30_PN)
reshapeReadmissionData$Score.READM_30_STK <-scale(reshapeReadmissionData$Score.READM_30_STK)


str(reshapeReadmissionData)



#######EDA - readmission start###################


contin_var <-
  c(
    "Score.READM_30_AMI",
    "Score.READM_30_CABG",
    "Score.READM_30_COPD",
    "Score.READM_30_HF",
    "Score.READM_30_HIP_KNEE",
    "Score.READM_30_HOSP_WIDE",
    "Score.READM_30_PN",
    "Score.READM_30_STK"
    
  )

#Lets create correlation plot for different features
continousData <-
  reshapeReadmissionData[, (colnames(reshapeReadmissionData) %in% contin_var)]
continousData <- as.matrix(continousData)
corr <- cor(continousData, use="pairwise.complete.obs")
#plot_readmission <- corrplot(corr, method = "circle")
plot_readmission <- corrplot(corr, method = "number")

#Highly corelated 
#Score.READM_30_HF and  Score.READM_30_HOSP_WIDE
#Score.READM_30_COPD and Score.READM_30_HOSP_WIDE
#Score.READM_30_PN and Score.READM_30_HOSP_WIDE

#Moderately corelated
#Score.READM_30_AMI and Score.READM_30_HOSP_WIDE
#Score.READM_30_AMI and Score.READM_30_HOSP_WIDE
#Score.READM_30_STK and  Score.READM_30_PN


#######EDA - readmission End###################

