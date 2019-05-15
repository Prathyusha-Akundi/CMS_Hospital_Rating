library(dplyr)
library(tidyr)

################## MORTALITY #######################

readmission_deaths_raw = read.csv('Readmissions and Deaths - Hospital.csv')
readmission_deaths <- readmission_deaths_raw[,-c(3:9,11,14:18)]
str(readmission_deaths)

# 'data.frame':	67452 obs. of  4 variables:
# $ Provider.ID: int  10001 10001 10001 10001 10001 10001 10001 10001 10001 10001 ...
# $ Measure.ID : Factor w/ 14 levels "MORT_30_AMI",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Denominator: Factor w/ 2772 levels "100","1000","1001",..: 2408 1247 2167 2507 2192 2012 2484 1228 2366 2751 ...
# $ Score      : Factor w/ 259 levels "1.4","1.5","1.6",..: 32 201 252 31 62 61 72 58 128 131 ...

## Measure.ID, Denominator and Score are factors. We need to convert them to numeric type. 
View(readmission_deaths)
## We can see that there are lot of rows with "Not Available" as their entry
length(which(readmission_deaths=="Not Available")) 
# Total Not Available: 51484

#Checking for each of score, denominator, provider ID and Measure.ID
length(which(readmission_deaths$Score=="Not Available")) #25742
length(which(readmission_deaths$Measure.ID=="Not Available")) #0
length(which(readmission_deaths$Denominator=="Not Available")) #25742
length(which(readmission_deaths$Provider.ID=="Not Available")) #0

#Replace "Not Available" to NA so that we can convert factor type to numeric
readmission_deaths[readmission_deaths=="Not Available"] <- NA

readmission_deaths$Score = as.numeric(readmission_deaths$Score)
readmission_deaths$Denominator = as.numeric(readmission_deaths$Denominator)

#Check if it was correctly replaced
length(which(readmission_deaths$Score=="Not Available")) #0
length(which(readmission_deaths$Denominator=="Not Available")) #0

sum(is.na(readmission_deaths$Score)) #25742
sum(is.na(readmission_deaths$Denominator)) #25742

hospital_info <- readmission_deaths[,1:2]
#check if there are any duplicate rows:
sum(duplicated(hospital_info)) # 62634
hospital_info <- hospital_info[ which(!duplicated(hospital_info)),]

## Store score and denominator in separate files to convert them into wider format
score_df <- readmission_deaths[,c(1,3,5)]
denominator_df <- readmission_deaths[,c(1,3,4)]

score_wide <- spread(score_df, Measure.ID, Score)
denominator_wide <- spread(denominator_df, Measure.ID, Denominator)

#Merge all hospital info, score and denominator by Provide.ID

merged_data <- merge(score_wide, denominator_wide, by="Provider.ID", suffixes=c("_denominator","_score"))
merged_data <- merge(hospital_info, merged_data, by="Provider.ID")

## The mortality group contains all measures that start with 'MORT_'
mortality_data <- merged_data[,c(1,3:8,17:22)]

## We need to consider data residing in another file: Complications-Hospital.csv for another measure

complications_df <- read.csv("Complications - Hospital.csv")
complications <- complications_df[, c(1,10,12,13)]

str(complications)
length(which(complications$Score=="Not Available")) # 21874
length(which(complications$Measure.ID=="Not Available")) #0
length(which(complications$Denominator=="Not Available")) #25098
length(which(complications$Provider.ID=="Not Available")) #0

#Replace "Not Available" to NA so that we can convert factor type to numeric
complications[complications=="Not Available"] <- NA

#Check if it was correctly replaced
length(which(complications$Score=="Not Available")) #0
length(which(complications$Denominator=="Not Available")) #0

sum(is.na(complications$Score)) #21874
sum(is.na(complications$Denominator)) #25098

complications$Denominator <- as.numeric(complications$Denominator)
complications$Score <- as.numeric(complications$Score)

str(complications)

complications_mortality <- complications[which(complications$Measure.ID == "PSI_4_SURG_COMP"), ]
names(complications_mortality)=c("Provider.ID","Measure.ID","PSI_4_SURG_COMP_denominator","PSI_4_SURG_COMP_score")
complications_mortality <- complications_mortality[,-2]


mortality_data <- merge(mortality_data,complications_mortality, by="Provider.ID")

write.csv(mortality_data, "grouped_data\\mortality.csv")
write.csv(mortality_data[,c(1,8:13,15)], "grouped_data\\mortality_score.csv")



######################## Safety of Care ###########################
hc_infections_data <- read.csv("Healthcare Associated Infections - Hospital.csv")
hc_infections <- hc_infections_data[,c(1:2,10,12)]

str(hc_infections)

hc_infections[hc_infections=="Not Available"] <- NA

hc_infections$Score = as.numeric(hc_infections$Score)

str(hc_infections)

hospital_info_infections <- hc_infections[,1:2]
#check if there are any duplicate rows:
sum(duplicated(hospital_info_infections)) # 226446
hospital_info_infections <- hospital_info_infections[ which(!duplicated(hospital_info_infections)),]

#Store score in a separate dataframe and convert it into wider format
score_df <- hc_infections[,c(1,3,4)]
score_wide <- spread(score_df, Measure.ID, Score)


#Merge all hospital info, score  by Provide.ID
merged_data_infections <- merge(hospital_info_infections, score_wide, by="Provider.ID")

complications_safety <- complications[which(complications$Measure.ID == "COMP_HIP_KNEE" | complications$Measure.ID == "PSI_90_SAFETY"), ]

names(complications_safety)=c("Provider.ID","Measure.ID","complications_denominator","complications_score")
complications_safety <- complications_safety[,-3]
complications_score_wide <- spread(complications_safety, Measure.ID, complications_score)

merged_data_safety <- merge(merged_data_infections,complications_score_wide, by="Provider.ID")

safety_score <- merged_data_safety[,c(1,8,14,20,26,32,38,44,50:52)]
write.csv(safety_score, "grouped_data\\safety.csv")
