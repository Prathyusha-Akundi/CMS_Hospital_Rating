library(dplyr)
library(tidyr)


#####Timely and Effective Care - Hospital.csv  -------Timeliness-------------#####



timeliness = read.csv('Timely and Effective Care - Hospital.csv')


str(timeliness)
View(timeliness)

#Check for duplicate rows

sum(duplicated(timeliness))
# 0


#check for na's

sum(is.na(timeliness))   #0



#Checking total no of "not avialable" values in dataframe.

length(which(timeliness=='Not Available'))  #256511


length(which(timeliness$Score=='Not Available'))   #129024


length(which(timeliness$Sample=='Not Available'))  #127487


#Replace "Not Avilable" with "na"
timeliness[timeliness == "Not Available" ] <- NA


#Consider only Provider.Id,Measure.Id,Score and Sample Columns

timeliness <- timeliness[,c(1,10,12,13)]

temp<-timeliness[,c(1,2)]


#Convert score and sample columns to wider format

score_df <- timeliness[,c(1,2,3)]
score_wide <- spread(score_df, Measure.ID, Score)


sample_df <- timeliness[,c(1,2,4)]
sample_wide <- spread(sample_df, Measure.ID, Sample)

#Merge temp,score_df and sample_df by Provider.ID

timeliness_final <- merge(score_wide, sample_wide, by="Provider.ID", suffixes=c("_sample","_score"))
timeliness_final <- merge(temp, timeliness_final, by="Provider.ID")



sapply(timeliness_final, function(x) (length(which(is.na(x)))))


timeliness <- timeliness_final[,c(1,2,6,7,8,10,11,13,15,52,53,54,56,57,59)]



