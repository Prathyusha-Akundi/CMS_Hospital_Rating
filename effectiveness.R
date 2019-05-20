library(corrplot)
library(dplyr)
library(tidyr)

general_standardization <- function(vec){
  return( (vec - mean(vec, na.rm = T))/(sd(vec, na.rm = T)) )
}

negative_standardization <- function(vec){
  return( (mean(vec, na.rm = T) -  vec)/(sd(vec, na.rm = T)) )
}

winsorization <- function (x, min_frac=.125, max_frac = .99875){
  lim <- quantile(x, probs=c(min_frac, max_frac), na.rm = T)
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  return(x)
}

effective_care <- read.csv("Timely and Effective Care - Hospital.csv")

effective_care <- effective_care[,-c(14,15,16)]

#Replace "Not Available" to NA so that we can convert factor type to numeric
effective_care[effective_care=="Not Available"] <- NA

table <- effective_care[,1:8]
score <- effective_care[,c(1,10,12)]
denominator <- effective_care[,c(1,10,13)]

score_wide <- spread(score,Measure.ID,Score)
score_wide <- score_wide[,c(1,4,9,10,16,17,18,19,21,23,35:41,43,44)]
denominator_wide <- spread(denominator,Measure.ID,Sample)
denominator_wide <- denominator_wide[,c(1,4,9,10,16,17,18,19,21,23,35:41,43,44)]

data <- merge(denominator_wide,score_wide,by="Provider.ID",suffixes=c("_denominator","_Score"))
data <- merge(table,data,by="Provider.ID")

effectiveness_data <- data[which(!duplicated(data)),]

effectiveness_score<-effectiveness_data[,c(1,27:44)]
#write.csv(effectiveness_score, "group_data\\effectiveness.csv")

effectiveness <- effectiveness_score

for (col in 1:(ncol(effectiveness))) {
  effectiveness[, col] <- as.numeric(effectiveness[, col])
}


positive_measures <- c(2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18)
negative_measures <- c(5, 10, 19)

effectiveness[, positive_measures] <- sapply(effectiveness[, positive_measures], 
                                             general_standardization)

effectiveness[, negative_measures] <- sapply(effectiveness[, negative_measures], 
                                             negative_standardization)

effectiveness[,2:ncol(effectiveness)] <- sapply(effectiveness[,-1], winsorization)


effectiveness_cor <- cor(effectiveness[, -c(1)], use="pairwise.complete.obs")

effectiveness_cor_plot <- corrplot(effectiveness_cor)

