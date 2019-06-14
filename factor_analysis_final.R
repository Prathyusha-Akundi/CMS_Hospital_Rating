library(psych)
library(Information)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(gridExtra)
library(cowplot)

f_group_scores <- function(main = measures_file){
  
  
  #Remove row numbers and the ID
  measures <- main[, -c(1, 2)]
  str(measures)
  sum(is.na(measures))
  
  
  #Do Factor analysis and extract loadings
  measures.fa <- fa(measures, fm="ml", scores = "tenBerge")
  measures_loadings <- measures.fa$loadings
  measures_loadings
  
  
  measures_loads <- vector(mode="numeric", length=length(measures_loadings))
  
  for (i in 1:(length(measures_loadings))){
    
    measures_loads[i] <- measures_loadings[i]
  }
  
  round(measures_loads, 2) 
  
  #Get the weights from the factor analysis instance
  measures_weights <- measures.fa$weights
  measures_weights <- measures_weights/sum(measures_weights)
  
  measures$invalid <- FALSE
  ##################### Handling the case with the constraint that atleast 3 measures should be present for a hospital ######################
  for (row in 1:nrow(measures)){
    if (sum(!is.na(measures[row, ])) <= 3) {measures[row, c("invalid")] = TRUE}
    else {measures[row, c("invalid")] = FALSE}
  }

  valid_indices <- which(!measures$invalid)
  

  measures <- measures[which(!measures$invalid), ]
  measures <- measures[, -ncol(measures)]
  
 #Dealing with Missing values by replacing with median
  f_na <- function(measure){
    measure[which(is.na(measure))] <- median(measure, na.rm=T)
    return(measure)
  }
  
  measures <- data.frame(sapply(measures, f_na))
  
  # calculating group scores
  #measures_weights
  n <- ncol(measures) 
  measures$score <- 0
  
  for (row in 1:nrow(measures)){
    measures[row, c("score")] <- round(sum(measures[row, 1:n]*measures_weights)/length(measures_weights), 3)
  }
  
  main <- main[valid_indices, ]
  main$score <- measures$score
  main_scores <- main[, c("Provider.ID", "score")]
  colnames(main_scores) <- c("Provider.ID", "group_score")
  group_scores <- main_scores
  
  return(group_scores)
  
}

# # Experience
exp <- read.csv("experience_clean.csv")
#exp <- exp[, colMeans(is.na(exp)) <= 0.5]
exp_scores <- f_group_scores(exp)
colnames(exp_scores) <- c("Provider_ID","exp_score")


#Medical
med <- read.csv("medical_clean.csv")
#med <- med[, colMeans(is.na(med)) <= 0.5]
med_scores <- f_group_scores(med)
colnames(med_scores) <- c("Provider_ID","med_score")


#Mortality
mortality <- read.csv("mortality_clean.csv")
#mortality <- mortality[, colMeans(is.na(mortality)) <= 0.5]
mortality_scores <- f_group_scores(mortality)
colnames(mortality_scores) <- c("Provider_ID","mortality_score")


#Readmission
Readmission <- read.csv("readmission_clean.csv")
#Readmission <- Readmission[, colMeans(is.na(Readmission)) <= 0.5]
Readmission_scores <- f_group_scores(Readmission)
colnames(Readmission_scores) <- c("Provider_ID","Readmission_score")


#safety
safety <- read.csv("safety_clean.csv")
#safety <- safety[, colMeans(is.na(safety)) <= 0.5]
safety_scores <- f_group_scores(safety)
colnames(safety_scores) <- c("Provider_ID","safety_score")


#timeliness
timeliness <- read.csv("timeliness_clean.csv")
#timeliness <- timeliness[, colMeans(is.na(timeliness)) <= 0.5]
timeliness_scores <- f_group_scores(timeliness)
colnames(timeliness_scores) <- c("Provider_ID","timeliness_score")


# Effectiveness
eff <- read.csv("effectiveness_clean.csv")
#eff <- eff[, colMeans(is.na(eff)) <= 0.5]
eff_scores <- f_group_scores(eff)
colnames(eff_scores) <- c("Provider_ID","eff_score")


t1 <- merge(med_scores, mortality_scores, by="Provider_ID", all=T)
t2 <- merge(t1, safety_scores, by="Provider_ID", all=T)
t3 <- merge(t2, eff_scores, by="Provider_ID", all=T)
t4 <- merge(t3, exp_scores, by="Provider_ID", all=T)
t5 <- merge(t4, Readmission_scores, by="Provider_ID", all=T)
group_scores <- merge(t5, timeliness_scores, by="Provider_ID", all=T)

weights <- c(medical = 0.04, mortality = 0.22, safety = 0.22, effectiveness = 0.04,
             experience = 0.22, readmission = 0.22, timeliness = 0.04)

group_scores$final_score <- 100

for (row in 1:nrow(group_scores)){
  if ( sum(is.na(group_scores[row, ])) > 0){
    invalid_indices = which(is.na(group_scores[row, ])) - 1
    s = sum(weights[-invalid_indices])
    reproportioned_weights <- weights[-invalid_indices]/s
    
    
    group_scores$final_score[row] <- sum(group_scores[row, 2:8][-invalid_indices]*reproportioned_weights)
  }
  
  else {
    group_scores$final_score[row] <- sum(group_scores[row, 2:8]*weights)
  }
  
  
}

final_scores <- group_scores[, c(1, ncol(group_scores))]
summary(final_scores$final_score)

# clustering
score_cluster <- kmeans(final_scores$final_score, 5)
summary(score_cluster)
summary(factor(score_cluster$cluster))
final_scores$cluster_id <- score_cluster$cluster

f = final_scores %>% group_by(cluster_id) %>%
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))
f
for (row in 1:nrow(final_scores)){
  # swap x with 5 - which(f$cluster_id == x) + 1
  id = final_scores$cluster_id[row]
  final_scores$newcluster_id[row] = 5 - which(f$cluster_id == id) + 1
  
}

final_scores$newcluster_id <- as.factor(final_scores$newcluster_id)
summary(final_scores$newcluster_id)
colnames(final_scores) <- c("Provider.ID","final_score","cluster_id","newcluster_id")
######### Test ##########
rating_file <- read.csv("Hospital General Information.csv")
ratings <- rating_file[, c(1, 13)]
#ratings <- ratings[-which(ratings$Hospital.overall.rating=="Not Available"),]
ratings$Hospital.overall.rating <- factor(ratings$Hospital.overall.rating)

final_scores <- merge(final_scores, ratings, by="Provider.ID", all=T)
final_scores$newcluster_id <- as.character(final_scores$newcluster_id)
final_scores$newcluster_id[which(is.na(final_scores$newcluster_id))] <- "Not Available"
final_scores$newcluster_id <- factor(final_scores$newcluster_id)
summary(final_scores$newcluster_id)

t = table(final_scores$newcluster_id, final_scores$Hospital.overall.rating)
t
str(t)

for (i in 1:5){
  print(t[i,i] / sum(t[i, ]))
}

confusionMatrix(t)

# Accuracy: 0.5874
# Statistics by Class:
#   
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 
# Sensitivity           0.69231  0.70029   0.5869   0.6546  0.66667               
# Specificity           0.97426  0.88099   0.8185   0.8503  0.94795               
