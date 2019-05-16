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


################### CLEAN MORTALITY.CSV ######################

mortality_raw <- read.csv('mortality_score.csv',header = TRUE)
mortality_raw <- mortality_raw[,-1]

# The measures starting with MORT_ gives the mortality rate, hence the lower the better. 
# Also the measure PSI_4_SURG_COMP is Deaths among patients with serious treatable complications after surgery.
# Hence all the measures indicate lower is better. So convert such that it's direction is reversed i.e. higher the better
# Also standardize the data so that mean=0 and var=1

mortality_raw[,2:ncol(mortality_raw)] <- sapply(mortality_raw[,-1], negative_standardization)

# According to documentation, CMS further winsorizes the standardized measure score at the 0.125th percentile (Z= -3) 
# and the 99.875 percentile (Z=3) of a Standard Normal distribution; 
# thus, all standardized scores above 3 were set to be 3, and all standardized scores bellow -3 are set to be -3.
# This is done to avoid outlier performance

mortality_raw[,2:ncol(mortality_raw)] <- sapply(mortality_raw[,-1], winsorization)

write.csv(mortality_raw, "mortality_score_clean.csv")


################### CLEAN SAFETY.CSV #######################

safety <- read.csv('safety.csv')
safety <- safety[,-1]

# The HAI measures show how often patients in a particular hospital contract certain infections 
# during the course of their medical treatment, when compared to like hospitals. Hence the lower these measures are, the better for the hospitals.
# So we reverse the direction such that higher will be better

safety[,2:ncol(safety)] <- sapply(safety[,-1], negative_standardization)

# According to documentation, CMS further winsorizes the standardized measure score at the 0.125th percentile (Z= -3) 
# and the 99.875 percentile (Z=3) of a Standard Normal distribution; 
# thus, all standardized scores above 3 were set to be 3, and all standardized scores bellow -3 are set to be -3.
# This is done to avoid outlier performance

safety[,2:ncol(safety)] <- sapply(safety[,-1], winsorization)

write.csv(safety, "safety_clean.csv")

