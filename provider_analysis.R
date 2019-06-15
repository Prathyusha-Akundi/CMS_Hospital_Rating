library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)

### Analysis for Provider ID = 140010: EVANSTON HOSPITAL ###

group_scores <- read.csv("group_scores.csv")
scores_evanston <- group_scores[which(group_scores$Provider_ID==140010),]
final_scores <- read.csv("final_scores.csv")
#final_scores_evanston <- final_scores[which(final_scores$Provider_ID==140010),]
general_info <- read.csv("Hospital General Information.csv")
info_evanstorn <- general_info[which(general_info$Provider.ID==140010),]
rating_evanston <- as.numeric(info_evanstorn$Hospital.overall.rating)
rating_evanston  # Evanston has a 3 rating.
colnames(group_scores)[2] = "Provider.ID"
group_scores <- merge(group_scores, genearl_info[,c(1,13)], by="Provider.ID")

# Get the average score for each group based on their ratings
g <- group_scores %>% group_by(Hospital.overall.rating) %>%
  summarise(mean(med_score, na.rm=T), mean(mortality_score, na.rm = T), 
            mean(safety_score, na.rm = T), mean(eff_score, na.rm = T), 
            mean(exp_score, na.rm=T),mean(Readmission_score, na.rm = T),
            mean(timeliness_score, na.rm = T), mean(final_score, na.rm = T))


mean_scr_4 <- g[which(g$Hospital.overall.rating==4),]
							
# Hospital.overall.rating                         4
# mean(final_score, na.rm = T)                    0.03795815
# mean(mortality_score, na.rm = T)                0.02112715   
# mean(safety_score, na.rm = T)                   0.01650972
# mean(eff_score, na.rm = T)                      0.002011186
# mean(Readmission_score, na.rm = T)              0.0567257
# mean(timeliness_score, na.rm = T)               0.04434579
# mean(med_score, na.rm = T)                      0.02398387
# mean(exp_score, na.rm = T)	                    0.05253921

## Score info of evanston

# Provider_ID                 140010
# med_scores                  0.049
# mortality_scores          	0.28
# safety_score               -0.034
# eff_scores                  0.026                                
# exp_scores                  0.013
# Readmission_score           0.049
# timeliness_scores           0.054
# final_score                 0.06764

med_score_comp <- cbind(mean_scr_4$`mean(med_score, na.rm = T)`, scores_evanston$med_score)
colnames(med_score_comp) <- c("Average med_score for rating 4","Evanston med_score")
barplot(med_score_comp)
#Evanston medical score is higher than average for rating 4

mortality_score_comp <- cbind(mean_scr_4$`mean(mortality_score, na.rm = T)`, scores_evanston$mortality_score)
colnames(mortality_score_comp) <- c("Average mortality_score for rating 4","Evanston mortality_score")
barplot(mortality_score_comp)
#Evanston mortality score is higher than average for rating 4

safety_score_comp <- cbind(mean_scr_4$`mean(safety_score, na.rm = T)`, scores_evanston$safety_score)
colnames(safety_score_comp) <- c("Average safety_score for rating 4","Evanston safety_score")
barplot(safety_score_comp)
#Evanston safety score is very low than average for rating 4

eff_score_comp <- cbind(mean_scr_4$`mean(eff_score, na.rm = T)`, scores_evanston$eff_score)
colnames(eff_score_comp) <- c("Average eff_scores for rating 4","Evanston eff_scores")
barplot(eff_score_comp)
#Evanston effectiveness score is higher than average for rating 4


exp_score_comp <- cbind(mean_scr_4$`mean(exp_score, na.rm = T)`, scores_evanston$exp_score)
colnames(exp_score_comp) <- c("Average exp_score for rating 4","Evanston exp_score")
barplot(exp_score_comp)
#Evanston experience score is lower than average for rating 4


readmission_score_comp <- cbind(mean_scr_4$`mean(Readmission_score, na.rm = T)`, scores_evanston$Readmission_score)
colnames(readmission_score_comp) <- c("Average Readmission_score for rating 4","Evanston Readmission_score")
barplot(readmission_score_comp)
#Evanston readmisson score is higher than average for rating 4


timeliness_score_comp <- cbind(mean_scr_4$`mean(timeliness_score, na.rm = T)`, scores_evanston$timeliness_score)
colnames(timeliness_score_comp) <- c("Average timeliness for rating 4","Evanston timeliness")
barplot(timeliness_score_comp)
#Evanston timeliness score is higher than average for rating 4


### So overall Evanston has higher scores in all groups except in SAFETY and EXPERIENCE ###

##Check the safety measures that it has to improve:
safety_scores <- read.csv("safety_clean.csv")
safety_scores <- merge(safety_scores, genearl_info[,c(1,13)], by="Provider.ID")
avg_safety_r4 <- round(sapply(safety_scores[which(safety_scores$Hospital.overall.rating==4), 3:10], function(x) mean(x, na.rm = T)), 3)
evanston_safety <-round(safety_scores[which(safety_scores$Provider.ID == 140010),][3:10], 3)
safety_comp <- rbind(evanston_safety,avg_safety_r4)

# After comparing the scores of each saftety measure, it is observed that HAI_1_SIR, HAI_2_SIR
# HAI_3_SIR, HAI_6_SIR, COMP_HIP_KNEE, PSI_90_SAFETY_SCORE has low scores for Evanston than that of
# Average scores of the above measures for the rating 4. It is to be noted that PSI_90_SAFETY_SCORE
# score is much lower than average and hence must be contributing more to the overall rating loss



##Check the Experience measures that it has to improve:
experience_scores <- read.csv("experience_clean.csv")
experience_scores <- merge(experience_scores, genearl_info[,c(1,13)], by="Provider.ID")
avg_exp_r4 <- round(sapply(experience_scores[which(experience_scores$Hospital.overall.rating==4), 3:13], function(x) mean(x, na.rm = T)), 3)
evanston_exp <-round(experience_scores[which(experience_scores$Provider.ID == 140010),][3:13], 3)
exp_comp <- rbind(evanston_exp,avg_exp_r4)

# All the measures of experience are less than that of average exp scores. Of them, H_CLEAN_LINEAR_SCORE_MEAN, H_COMP_1_LINEAR_SCORE_MEAN, H_COMP_2_LINEAR_SCORE_MEAN,
# H_COMP_3_LINEAR_SCORE_MEAN, H_COMP_4_LINEAR_SCORE_MEAN, H_COMP_5_LINEAR_SCORE_MEAN,
# H_COMP_6_LINEAR_SCORE_MEAN, H_COMP_7_LINEAR_SCORE_MEAN, H_QUIET_LINEAR_SCORE_MEAN are worse than average