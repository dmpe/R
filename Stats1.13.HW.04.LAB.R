library(psych)
library(gclus)
library(rgl)
library(sm)
library(plyr)

getwd()
setwd("C:/Users/Dima/Documents/R/coursera")
# open data
info <- read.table("Stats1.13.HW.04.txt", header=T)

# 1. What is the correlation between salary and years of professional experience?
round(cor(info[2:3]),2)

# 2. What is the correlation between salary and courses completed?
round(cor(info$salary, info$courses),2)

# 3. What is the percentage of variance explained in a regression model with salary as the
# outcome variable and professional experience as the predictor variable?

model1 <- lm(info$salary ~ info$years)
summary(model1)

# 4. Compared to the model from Question 3, would a regression model predicting salary from the
# number of courses be considered a better fit to the data?


model2 <- lm(info$salary ~ info$courses)
summary(model2)


# 5. Now let's include both predictors (years of professional experience and courses completed)
# in a regression model with salary as the outcome. Now what is the percentage of variance
# explained?

model3 <- lm(info$salary ~ info$years + info$courses)
summary(model3)

# 6. What is the standardized regression coefficient for years of professional experience, 
# predicting salary?

model1.z <- lm(scale(info$salary) ~ scale(info$years))
summary(model1.z)

# 7. What is the standardized regression coefficient for courses completed, predicting salary?

model2.z <- lm(scale(info$salary) ~ scale(info$courses))
summary(model2.z)


# 8. What is the mean of the salary distribution predicted by the model including both years of
# professional experience and courses completed as predictors? (with 0 decimal places)

# model4 <- fitted(model3)
# mean(model4)
mean(info$salary)

data.predicted = fitted(model3)
mean(data.predicted)


# 9. What is the mean of the residual distribution for the model predicting salary from both
# years of professional experience and courses completed? (with 0 decimal places)
info$e <- resid(model3)
mean(info$e)

# 10. Are the residuals from the regression model with both predictors normally distributed?
hist(info$e)








