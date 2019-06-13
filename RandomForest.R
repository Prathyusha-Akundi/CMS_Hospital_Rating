library(randomForest)
install.packages("e1071")
library(e1071)

# reading the data 
final_data_random <- read.csv("final_data.csv")

# CMS ratings data from general information
rating_random <- read.csv("Hospital General Information.csv")
rating_random <- rating_random[, c(1, 13)]

#Merge the data
master_data_random <- merge(final_data_random, rating_random, by="Provider.ID")

# check for Missing value
sapply(master_data_random, function(x) sum(is.na(x)))

#Lets remove column where na are > 1500
# MORT_30_AMI_score   MORT_30_CABG_score   MORT_30_STK_score PSI_4_SURG_COMP_score                                 
# Score.READM_30_AMI Score.READM_30_CABG Score.READM_30_HIP_KNEE Score.READM_30_STK
# HAI_1_SIR  HAI_1a_SIR    HAI_2_SIR                   
# HAI_2a_SIR    HAI_3_SIR   HAI_4_SIR
# HAI_5_SIR   HAI_6_SIR    COMP_HIP_KNEE PSI_90_SAFETY    Score.OP_13 Score.OP_14   
# Score.OP_8  Score.OP_9 OP_21_Score OP_3b_Score OP_5_Score   CAC_3_Score                                                        
# OP_22_Score   OP_23_Score OP_29_Score   OP_30_Score     OP_4_Score
# PC_01_Score   STK_4_Score   STK_5_Score STK_6_Score   STK_8_Score
# VTE_2_Score    VTE_3_Score  VTE_5_Score VTE_6_Score
badDataCol <- c( 'MORT_30_AMI_score'  , 'MORT_30_CABG_score' ,  'MORT_30_STK_score', 'PSI_4_SURG_COMP_score',                                 
                 'Score.READM_30_AMI', 'Score.READM_30_CABG', 'Score.READM_30_HIP_KNEE', 'Score.READM_30_STK',
                 'HAI_1_SIR',  'HAI_1a_SIR',    'HAI_2_SIR',                   
                 'HAI_2a_SIR' ,'HAI_3_SIR' ,  'HAI_4_SIR',
                'HAI_5_SIR' ,  'HAI_6_SIR',    'COMP_HIP_KNEE', 'PSI_90_SAFETY',    'Score.OP_13', 'Score.OP_14' ,  
                 'Score.OP_8' , 'Score.OP_9' , 'OP_21_Score' , 'OP_3b_Score' , 'OP_5_Score' ,  'CAC_3_Score' ,                                                        
                 'OP_22_Score' ,  'OP_23_Score' , 'OP_29_Score' ,  'OP_30_Score' ,    'OP_4_Score',
                 'PC_01_Score'  , 'STK_4_Score'  , 'STK_5_Score' , 'STK_6_Score' ,  'STK_8_Score',
                 'VTE_2_Score'  ,  'VTE_3_Score' , 'VTE_5_Score',  'VTE_6_Score')

master_data_random <- master_data_random[, -which(names(master_data_random) %in% badDataCol)]
str(master_data_random)

# check for Missing value
sapply(master_data_random, function(x) sum(is.na(x)))

#Replace with mean
for(i in 1:ncol(master_data_random)){
  master_data_random[is.na(master_data_random[,i]), i] <- mean(master_data_random[,i], na.rm = TRUE)
}

#Let see Not available data
sapply(master_data_random, function(x) length(which(x == "Not Available")))

#Hospital.overall.rating has such values,lets convert to NA
str(master_data_random)
master_data_random[which(master_data_random$Hospital.overall.rating == "Not Available"), 30] <- NA

#Lets remove NA from overall ratng
master_data_random <- master_data_random[-which(is.na(master_data_random$Hospital.overall.rating)), ]

#check for Duplicate rows
sum(duplicated(master_data_random))
#no duplicate rows .

#remove provider id and row numbers
master_data_random <- master_data_random[ , c(-1,-2)]
str(master_data_random)

#convert into factor
master_data_random$Hospital.overall.rating <- factor(master_data_random$Hospital.overall.rating)

#Building the Model

# Shuffle the data
shuffledata <- master_data_random[sample(nrow(master_data_random)), ]

# Split the data into train and test
ntrain <- as.integer(nrow(shuffledata)*0.8)
traindata <- shuffledata[1:ntrain, ]
testdata <- shuffledata[(ntrain+1):nrow(shuffledata), ]

# Build the random forest
set.seed(71)
data.rf <- randomForest(Hospital.overall.rating ~., data=traindata, 
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
data.rf
testPred <- predict(data.rf, newdata=testdata)
table(testPred, testdata$Hospital.overall.rating)
confusionMatrix(testPred, testdata$Hospital.overall.rating)
#Accuracy : 0.7164 
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.11111  0.48148   0.8725   0.6915  0.39130
# Specificity           0.99860  0.94286   0.6525   0.9225  1.00000

#Lets try with ntree=1500, mtry=15
data.rf <- randomForest(Hospital.overall.rating ~., data=traindata, 
                        ntree=1500, mtry=15, do.trace=TRUE, na.action=na.omit)
data.rf
testPred <- predict(data.rf, newdata=testdata)
table(testPred, testdata$Hospital.overall.rating)
confusionMatrix(testPred, testdata$Hospital.overall.rating)
#Accuracy : 0.7315
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity          0.222222   0.5407   0.8555   0.7164  0.47826
# Specificity          0.997191   0.9429   0.6923   0.9187  0.99859
#Lets try with ntree=1500, mtry=15

#Lets try with ntree=1500, mtry=20
data.rf <- randomForest(Hospital.overall.rating ~., data=traindata, 
                        ntree=1500, mtry=17, do.trace=TRUE, na.action=na.omit)
data.rf
testPred <- predict(data.rf, newdata=testdata)
table(testPred, testdata$Hospital.overall.rating)
confusionMatrix(testPred, testdata$Hospital.overall.rating)

