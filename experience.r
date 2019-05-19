#HCAHPS - Hospital.csv  -------Experience-------------

experience = read.csv('HCAHPS - Hospital.csv')

str(experience)
View(experience)


experience <- experience[,c(1,2,9,10,11,19)]

#check duplicate rows

sum(duplicated(experience))
# 0

#check for na's

sum(is.na(experience))

#0


summary(experience)

#Checking total no of "not avialable" values in dataframe.
 
length(which(experience=='Not Available'))
#31845

length(which(experience$Survey.Response.Rate.Percent=='Not Available'))  # 31845 


#Replace "Not Available" with "na"
experience[ experience == "Not Available" ] <- NA








