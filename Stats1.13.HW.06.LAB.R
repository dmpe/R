setwd("c:/Users/Dima/Documents/R/coursera")

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

novy <-  read.table("~/R/coursera/Stats1.13.novy.06.txt", header=T)



# In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model?
model1 <- lm(novy$salary ~ novy$years)
summary(model1)

#5638

# In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model? 

confint(model1)

# In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model?
model2 <- lm(novy$salary ~ novy$years + novy$courses)
summary(model2)

#4807


#In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model?
confint(model2)

#What is the predicted difference in salary between Doctors and Lawyers assuming an equal and average number of years and courses?


dept.code <- C(novy$profession, treatment)

model3 <- lm(novy$salary ~ novy$years + novy$courses + (dept.code))
summary(model3)
#9204
#In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model?

confint(model3)


#What is the predicted difference in salary between Doctors and Teachers assuming an equal and average number of years and courses?


dept.code <- C(novy$profession, treatment)

model5 <- lm(novy$salary ~ novy$years + novy$courses + (dept.code))
summary(model5)
#15903

#What is the actual difference in mean salary between Doctors and Teachers?

prof.code = C(novy$profession, treatment) 
model4 = lm(novy$salary ~ novy$years + novy$courses + (prof.code)) 
summary(model4) 
tapply(novy$salary, novy$profession, mean)
#24611

anova(model1, model3)
#ye, co, prof
anova(model2, model3)