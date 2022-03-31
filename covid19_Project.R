data<-read.csv(file.choose())
head(data)
tail(data)
install.packages("Hmisc")
library(Hmisc) #Import
describe(data) #Hmisc Command for analyzing the data set

#Cleaning the data set
# 1) The variable death has 0 or 1 as a value. Additionally, it also has dates as input values

data$death_dummy<-as.integer(data$death !=0)

#Calculating Death Rate (How many people died)

death_rate<-sum(data$death_dummy)/nrow(data)

#Age
#Testing_Claim: The average person who dies from Covid 19 is older then the person who survives.

dead = subset(data,death_dummy==1)
alive = subset(data, death_dummy==0)

#Therefore there are 63 people who are dead and 1022 people who survived.

#Calculating the mean age of the people who are dead and the people who survived.
#To do this we will also have to account for missing values (N/A).

Mean_Age_Victims_Covid_19<- mean(dead$age, na.rm=T) #68.59 Years
Mean_Age_Surviours<- mean(alive$age,na.rm=T) #48.07 Years
#Difference in age

Difference<-Mean_Age_Victims_Covid_19 - Mean_Age_Surviours

#But is this Statistically Significant?

t.test(alive$age,dead$age,alternative = "two.sided", conf.level = 0.95)

#We observer that there is a 95% chance that the difference between the age of the person who is alive and who is dead is between 16 years to 24years old. 

#On average the person who is alive is much younger

#The p value < 0.05. Therefore, reject the null hypothesis that there is no difference in ages between the people who have dies and the people who survived.
#Our result is statistically significant.

#People who die from coraona virus are much older than the people who survive. 

#Gender
#Gender has no effect on Covid19 deaths

men=subset(data, gender=="male")
women=subset(data, gender=="female")
mean(men$death_dummy, na.rm=T) #Men death rate=8.5%
mean(women$death_dummy, na.rm=T) #female death rate=3.7%

#Is this statistically significant?
t.test(men$death_dummy,women$death_dummy,alternative = "two.sided", conf.level = 0.95)

#p value is <0.05. Therefore, we reject the null hypothesis that there is no difference between genders and our result is statistically significant. Men have higher death rates then women in the sample and that is representative of the population. In 95% of the cases men have a 2% to 8% higher chance of dying than woman due to the Covid 19 virus.
