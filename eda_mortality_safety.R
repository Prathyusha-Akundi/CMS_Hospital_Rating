library(tidyr)
library(ggplot2)
library(dplyr)
library(corrplot)

ratings <- read.csv("Hospital General Information.csv")
ratings[ratings=='Not Available'] <- NA

mortality <- read.csv("mortality_score_clean.csv")

mortality_cor <- cor(mortality[, -c(1, 2)], use="pairwise.complete.obs")

mortality_cor_plot <- corrplot(mortality_cor,method='number')


mortality <- mortality[,-1]



mortality_hgi <- merge(mortality, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

mort_summary <- mortality_hgi[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

# Remove last row as it corresponds to NA rating
mort_summary2 <- mort_summary[-6,]

# MORT_30_AMI_score
ggplot(data=mort_summary2, aes(x=MORT_30_AMI_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Shows decreasing trend in general

# MORT_30_CABG_score
ggplot(data=mort_summary2, aes(x=MORT_30_CABG_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# No consistent trend


# MORT_30_COPD_score
ggplot(data=mort_summary2, aes(x=MORT_30_COPD_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Decreasing trend for all ratings except for 5

# MORT_30_HF_score
ggplot(data=mort_summary2, aes(x=MORT_30_HF_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Has a decreasing ternd in general

# MORT_30_PN_score
ggplot(data=mort_summary2, aes(x=MORT_30_PN_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Decreasing trend for all ratings except for 5

# MORT_30_STK_score
ggplot(data=mort_summary2, aes(x=MORT_30_STK_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# No consistent trend

# PSI_4_SURG_COMP_score
ggplot(data=mort_summary2, aes(x=PSI_4_SURG_COMP_score, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Has a clear increasing trend.

############### SAFETY.CSV ################

safety <- read.csv("safety_clean.csv")
safety <- safety[,-1]

safety_cor <- cor(safety[, -c(1)], use='pairwise.complete.obs')
safety_cor_plot <- corrplot(safety_cor, method='number')

safety_hgi <- merge(safety,  ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")
safety_summary <- safety_hgi[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

# Remvoe last row
safety_summary2 <- safety_summary[-6,]
safety_summary$HAI_1_SIR
#HAI_1_SIR
ggplot(data=safety_summary2, aes(x=HAI_1_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Clear increasing trend. i.e. as measure value increases, the rating increases

#HAI_1a_SIR
ggplot(data=safety_summary2, aes(x=HAI_1a_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Increasing trend expect for 4 rating

#HAI_2_SIR
ggplot(data=safety_summary2, aes(x=HAI_2_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# INcreasing trend in general

ggplot(data=safety_summary2, aes(x=HAI_2a_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Inconsistent trend

ggplot(data=safety_summary2, aes(x=HAI_3_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
#Inconsistent trend

ggplot(data=safety_summary2, aes(x=HAI_4_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Inconstent trends

ggplot(data=safety_summary2, aes(x=HAI_5_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Clear increasing trends


ggplot(data=safety_summary2, aes(x=HAI_6_SIR, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
#Inconsistent trend

ggplot(data=safety_summary2, aes(x=COMP_HIP_KNEE, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Clear increasing trend

ggplot(data=safety_summary2, aes(x=PSI_90_SAFETY, y=Hospital.overall.rating,group=1))+geom_line(arrow=arrow())+ geom_point()
# Clear increasing trend

# All the graphs with clear indication of trends i.e. either increasing or decreasing the measure will determine the rating are important.
# Measures with inconsistent trends may be correlated to other measures to determine the rating or may not be as important
