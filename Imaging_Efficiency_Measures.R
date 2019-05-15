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
ImagingData<-read.csv('Outpatient Imaging Efficiency - Hospital.csv', stringsAsFactors = F, na.strings = c("Not Available"))

#summary and structure of data
summary(ImagingData)

str(ImagingData)
# Total 14 variables with 28908 obs

#No. of NA data 
sum(is.na(ImagingData)) #12595

# No. of NA in score
sum(is.na(ImagingData$Score)) #12595, Means all NA in only score field.

unique(ImagingData$Measure.ID) # total 6 measures, OP_10 OP_11 OP_13 OP_14 OP_8  OP_9

#take only Provider ID, measureID and score, rest are the coloumn are irrelevent 
tempImageData = ImagingData[,c(1,9,11)]

#reshape the data in wide format
reshapeImageData = reshape(tempImageData, idvar = "Provider.ID", timevar = "Measure.ID", direction = "wide") 

#find number of NA in each measure ID
sapply(reshapeImageData, function(x) (length(which(is.na(x)))))
#Op_10 = 1189, op_11= 1469, op_13=2585, op_14=2514, op_8=3294, op_9= 1544

#check if there are any duplicate row, nothing found
sum(duplicated(reshapeImageData$Provider.ID)) #0, no duplicate data

#Scale the data
reshapeImageData$Score.OP_10 <-scale(reshapeImageData$Score.OP_10)
reshapeImageData$Score.OP_11 <-scale(reshapeImageData$Score.OP_11)
reshapeImageData$Score.OP_13 <-scale(reshapeImageData$Score.OP_13)
reshapeImageData$Score.OP_14 <-scale(reshapeImageData$Score.OP_14)
reshapeImageData$Score.OP_8 <-scale(reshapeImageData$Score.OP_8)
reshapeImageData$Score.OP_9 <-scale(reshapeImageData$Score.OP_9)

str(reshapeImageData)

 

#######EDA - Imaging start###################


contin_var <-
  c(
    "Score.OP_10",
    "Score.OP_11",
    "Score.OP_13",
    "Score.OP_14",
    "Score.OP_8",
    "Score.OP_9"
    
  )

#Lets create correlation plot for different features
continousData <-
  reshapeImageData[, (colnames(reshapeImageData) %in% contin_var)]
continousData <- as.matrix(continousData)
corr <- cor(continousData, use="pairwise.complete.obs")
plot_medical_imaging <- corrplot(corr, method = "circle")

#OP_11 and OP_10 only shows some significant correlation.



#######EDA - Imaging End###################

