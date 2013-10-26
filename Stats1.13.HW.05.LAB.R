library(psych)
library(gclus)
library(rgl)
library(sm)
library(plyr)

setwd("C:/Users/Dima/Documents/R/coursera")
# open data
info <- read.table("Stats1.13.HW.04.txt", header=T)

infod <- sframe(info)



# Run a regression model with salary as the outcome variable and years of experience as the
# predictor variable. What is the 95% confidence interval for the regression coefficient? Type # your answer exactly as it appears in R but include only two decimal places (for example, if # the 95% confidence interval is -1 to +1 then type -1.00 1.00)

mosdel1 = lm(info$salary ~ info$years)
confint(mosdel1)


#Run a regression model with salary as the outcome variable and courses as the predictor # variable. What is the 95% confidence interval for the regression coefficient?

mosdel2 = lm(info$salary ~ info$courses)
confint(mosdel2)

# Run a multiple regression model with both predictors and compare it with both the model from Question 1 and the model from Question 2. Is the model with both predictors significantly better than:

model <- lm(info$years ~ info$courses)
summary(model)

# Run a standardized multiple regression model with both predictors. Do the confidence interval values differ from the corresponding unstandardized model?
model2 <- lm(scale(info$years) ~ scale(info$courses))
summary(model2)

confint(model)
confint(model2)

# What function could you use to take a random subset of the data?

sample()
# Run the following command in R: set.seed(1). Now take a random subset of the original data so that N=15. Is the correlation coefficient between salary and years of experience in this sample higher or lower than in the whole data set?
             
set.seed(1)             
sap <- info[sample(nrow(info),15),]     
sap

cor.test(sap$salary, sap$years) # lower


# Take a subset of the original data from row 51 to 70. What is the percentage of variance explained by a multiple regression model with both predictors (Provide your result with no decimal place)


data.subset = info[51:70] 
model3.subset = lm(so$salary ~ so$years + so$courses) 
summary(model3.subset)

infod
so <- subset(infod, ID >51 & ID < 70)
so

ms <- lm(so$salary~ so$years + so$courses)
summary(ms)



# Using model comparison, which model provides the best fit for the subsetted data from Question 7?

model1.subset = lm(so$salary ~ so$years) 
model2.subset = lm(so$salary ~ so$courses)
model3.subset = lm(so$salary ~ so$years + so$courses) 

anova(model1.subset, model2.subset)
anova(model2.subset, model3.subset) # this
anova(model3.subset, model1.subset)



model1.subset = lm(so$salary ~ so$years)
model2.subset = lm(so$salary ~ so$courses) 
model3.subset = lm(so$salary ~ so$years + so$courses) 
summary(model1.subset) 
summary(model2.subset) 
summary(model3.subset) 
anova(model1.subset, model3.subset) 
anova(model2.subset, model3.subset)


#What is the correlation between the salary values predicted by the multiple regression model and the actual salary scores in the subsetted data? (Provide your result rounded to 2 decimal places)

cor.test(info$salary, infod$salary)

round(cor(so[2:4]), 2) # Round to 2 decimal places 


so$predicted = fitted(model3.subset)
cor(so$predicted, so$salary)


#


so$error = resid(model3.subset) 
cor(so$predicted,so$error)

