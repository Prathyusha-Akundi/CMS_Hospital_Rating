library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(dplyr)
library(Information)
library(gridExtra)
############# DATA UNDERSTANDING #################################

#We are considering at the hospital level to make it simple to understand

#as per the cms the hospital compare results are classified into the below 7 groups
#Mortality
#Survey of patients' experiences
#Timely & effective care
#Complications
#Readmissions & deaths
#Use of medical imaging
#Payment & value of care

#lets classify to 7 groups and  bring the data at the provider Id level

#------------------------- HOSPITAL GENERAL INFORMATION-----------------------------------#

Genral_info_H <- read.csv("Hospital General Information.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
dim(Genral_info_H)
## 4818 obs. of 28 variables

summary(Genral_info_H)

H_Quaterly_MSPB<- read.csv("HOSPITAL_QUARTERLY_MSPB_6_DECIMALS.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
dim(H_Quaterly_MSPB)
## 3232 obs. of 6 variables

summary(H_Quaterly_MSPB)

H_Quaterly_Quality<- read.csv("HOSPITAL_QUARTERLY_QUALITYMEASURE_PCH_HOSPITAL.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
dim(H_Quaterly_Quality)
## 33 obs. of 15 variables

summary(H_Quaterly_Quality)




#-----------------------------------------------DATA CLEANING-------------------------------------

#check for duplicate provider ID

sum(duplicated(Genral_info_H$Provider.ID))
#0 duplicates 

#check for NA

sapply(Genral_info_H, function(x) sum(is.na(x)))
#there are NA values lets check them in percentage for better understanding

sapply(Genral_info_H, function(x) (sum(is.na(x))/nrow(Genral_info_H)*100))

# We can see tha footnotes are having na ranging from 57.9 to 76.8 this is high side we can remove them 

colnames(Genral_info_H)

Genral_info_H<-Genral_info_H[,-which(names(Genral_info_H) %in% c("Hospital.overall.rating.footnote","Mortality.national.comparison.footnote",
                                                                             "Safety.of.care.national.comparison.footnote","Readmission.national.comparison.footnote",
                                                                             "Patient.experience.national.comparison.footnote","Effectiveness.of.care.national.comparison.footnote",
                                                                             "Timeliness.of.care.national.comparison.footnote","Efficient.use.of.medical.imaging.national.comparison.footnote"))]

dim(Genral_info_H)
summary(Genral_info_H)

#Now few columns are value as 'Not Available', which is similar to NA. So convert them to NA
Genral_info_H[ Genral_info_H == "Not Available" ] <- NA

#Again lets check the  NA value count

sapply(Genral_info_H, function(x) (sum(is.na(x))/nrow(Genral_info_H)*100))

#lets impute the data 

#1. County Name, with 15 NA values

Genral_info_H[which(is.na(Genral_info_H$County.Name)),]

##getting the city names for county with NA values
City_Names_County<-Genral_info_H[which(is.na(Genral_info_H$County.Name)),]$City

## checking if county names are available for those cities
Genral_info_H[which(Genral_info_H$City %in% City_Names_County),][,c(4,7)]

## 3 county names can be imputed with existing city names
Genral_info_H[which(Genral_info_H$City=="HOMER"),]$County.Name<- "CLAIBORNE"
Genral_info_H[which(Genral_info_H$City=="SEWARD"),]$County.Name<- "SEWARD"
Genral_info_H[which(Genral_info_H$City=="PETERSBURG"),]$County.Name<- "PETERSBURG CITY"

sapply(Genral_info_H, function(x) (sum(is.na(x))/nrow(Genral_info_H)*100))

##Now, lets convert columns to factors whichever required


colsToFactor<- c("City","State","ZIP.Code","County.Name","Hospital.Type","Hospital.Ownership","Emergency.Services",
                         "Meets.criteria.for.meaningful.use.of.EHRs","Hospital.overall.rating","Mortality.national.comparison",
                         "Safety.of.care.national.comparison","Readmission.national.comparison","Patient.experience.national.comparison",
                         "Effectiveness.of.care.national.comparison","Timeliness.of.care.national.comparison",
                         "Efficient.use.of.medical.imaging.national.comparison")

Genral_info_H[colsToFactor] <- lapply(Genral_info_H[colsToFactor], factor)

summary(Genral_info_H)

###############################################Hospital_Quaterly_Quality###########################################################

#Few columns are value as 'Not Available', which is similar to NA. So convert them to NA
H_Quaterly_Quality[ H_Quaterly_Quality == "Not Available" ] <- NA


#Checking for columns with NA count

sapply(H_Quaterly_Quality, function(x) sum(is.na(x)))

#Footnote column is irrelevent
H_Quaterly_Quality<-H_Quaterly_Quality[,-13]

## converting date columns to POSIXlt

H_Quaterly_Quality$RPTG_PRD_START_DT<-as.POSIXlt(H_Quaterly_Quality$RPTG_PRD_START_DT, format = "%m/%d/%Y")
H_Quaterly_Quality$RPTG_PRD_END_DT<-as.POSIXlt(H_Quaterly_Quality$RPTG_PRD_END_DT, format = "%m/%d/%Y")

#Calculating Reporting duration in months

H_Quaterly_Quality$RPTG_Duration_Months<- interval(H_Quaterly_Quality$RPTG_PRD_START_DT, H_Quaterly_Quality$RPTG_PRD_END_DT) %/% months(1)
# Reporting duration for all are 11 months so these columns are irrelevent 

H_Quaterly_Quality<-H_Quaterly_Quality[,-c(13:15)]

colnames(H_Quaterly_Quality)




###############################################Hospital_Quaterly_MSPB###########################################################

summary(H_Quaterly_MSPB)

# Start Date and End Date is irrelevent for analysis as duration is 1 year for all
# Footnotes are again irrelevent 


H_Quaterly_MSPB <- H_Quaterly_MSPB[,1:3]

#Converting Measure_Id to factor 

H_Quaterly_MSPB$Measure_ID<- as.factor(H_Quaterly_MSPB$Measure_ID)

summary(H_Quaterly_MSPB$Measure_ID)

# As we have ony one value throughout the dataset, we can omit variable "Measure_ID" out of the context

H_Quaterly_MSPB<- H_Quaterly_MSPB[,c(1,3)]

# Checking the existence of NA values

sapply(H_Quaterly_MSPB, function(x) sum(is.na(x)))



############################################################################################

#UNIVARIATE ANALYSIS-IDENTIFYING AND COMPARING GROUP IMPORTANCE

star_ratings <- Genral_info_H
str(star_ratings)
summary(star_ratings)
View(star_ratings)

# Hospital Type - Most are acute care type, which are the ones included in star ratings
summary(star_ratings$Hospital.Type) #3382 acute care hospitals
table(star_ratings$Hospital.Type, star_ratings$Hospital.overall.rating)
plot1 <- ggplot(star_ratings, aes(x= star_ratings$Hospital.overall.rating)) + geom_bar() + labs(title="Overall Ratings") 
# Most acute care hospitals are given ratings, none of the children care hospitals 
# are given ratings, and most critical care hospitals are not given ratings

# Overall rating
summary(star_ratings$Hospital.overall.rating)
star_ratings$Hospital.overall.rating <- as.numeric(star_ratings$Hospital.overall.rating)

# Segmented univariate analysis - segmenting across different 'group' categorical variables 
# and comparing the average overall ratings

avg_by_ownership <- star_ratings %>% group_by(Hospital.Ownership) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())
# low ratings for tribal, local government; high for physician, voluntary non-profit etc.

# mortality
avg_by_mortality <- star_ratings %>% group_by(Mortality.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n()) %>%
  arrange(desc(avg_rating))

avg_by_mortality

# looking at the difference between the avg ratings of 'above' and 'below' the national
# average
avg_by_mortality$avg_rating[1] - avg_by_mortality$avg_rating[nrow(avg_by_mortality)]

# Thus, the average star rating varies by about 1.06 in different levels of mortality
# We can similarly compare the avg rating across other groups and observe which ones affect the rating more


# a function to do the same procedure for all group categorical variables
plot_rating <- function(cat_var, var_name){
  a <- aggregate(Hospital.overall.rating~cat_var,star_ratings, mean)
  b <- aggregate(Hospital.overall.rating~cat_var, star_ratings, length)
  
  colnames(a) <- var_name
  colnames(b)[1] <- var_name
  agg_response <- merge(a, b, by = var_name)
  
  colnames(agg_response) <- c(var_name, "avg_rating","count")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  agg_response <- arrange(agg_response, desc(avg_rating))
  agg_response[, 2] <- as.numeric(agg_response[, 2])
  
  p.plot <- ggplot(agg_response, aes(agg_response[, 1], avg_rating, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
  return(list(agg_response[1, 2] - agg_response[nrow(agg_response), 2], p.plot, agg_response))
  
}

diff_mortality = plot_rating(star_ratings$Mortality.national.comparison, "mortality")
diff_mortality[[1]]
diff_mortality[[2]]
diff_mortality[[3]]

# safety of care
diff_safety = plot_rating(star_ratings$Safety.of.care.national.comparison, "safety")
diff_safety[[1]]
diff_safety[[2]]
diff_safety[[3]]

##
avg_by_safety <- star_ratings %>% group_by(Safety.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_safety
ggplot(star_ratings, aes(x=factor(Safety.of.care.national.comparison), 
                    fill=factor(Hospital.overall.rating))) + geom_bar(position = "dodge")

# readmission

diff_readmission = plot_rating(star_ratings$Readmission.national.comparison, "readmission")
diff_readmission[[1]]
diff_readmission[[2]]

#
avg_by_readmission <- star_ratings %>% group_by(Readmission.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_readmission

# patient experience
diff_experience = plot_rating(star_ratings$Patient.experience.national.comparison, "experience")
diff_experience[[1]]
diff_experience[[2]]

#
avg_by_experience <-star_ratings %>% group_by(Patient.experience.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_experience

# effectiveness
diff_effectiveness = plot_rating(star_ratings$Effectiveness.of.care.national.comparison, "effectiveness")
diff_effectiveness[[1]]
diff_effectiveness[[2]]


avg_by_effectiveness <- star_ratings %>% group_by(Effectiveness.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_effectiveness

# medical
diff_medical = plot_rating(star_ratings$Efficient.use.of.medical.imaging.national.comparison, "medical")
diff_medical[[1]]
diff_medical[[2]]


avg_by_medical <- star_ratings %>% group_by(Efficient.use.of.medical.imaging.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_medical

# there's not much difference between the average ratings of above and same as 
# national average hospitals by medical group variables

# timeliness
diff_timeliness = plot_rating(star_ratings$Timeliness.of.care.national.comparison, "timeliness")
diff_timeliness[[1]]
diff_timeliness[[2]]

avg_by_timeliness <- star_ratings %>% group_by(Timeliness.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_timeliness
# timeliness seems to affect avg rating significantly (from 2.62 to 3.24 )


# summary of groups on avg star rating
group_effect <- c(mortality = diff_mortality[[1]], timeliness = diff_timeliness[[1]], 
                  medical = diff_medical[[1]], effectiveness = diff_effectiveness[[1]], 
                  experience = diff_experience[[1]], safety = diff_safety[[1]], 
                  readmission = diff_readmission[[1]])

group_effect[order(group_effect, decreasing=T)]

# This is also expected - the top 4 groups have been assigned an importance of 22% each, 
# while the bottom three have 4% weightage

# let's store the imporance of groups in a named vector group_importance
group_importance <- group_effect[order(group_effect, decreasing=T)]


################################################################################################







