# Statistics One, 2013, Lab 5

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses, including NHSTs
#   Conduct regression analyses, emphasis on standard error, confidence intervals, and model comparison 

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Initial analyses assume a sample size of N = 200 
#     Analyses are then repeated with a sample size of N = 20

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")

# Load packages
library(psych)
library(ggplot2)
setwd("C:/Users/Dima/Documents/R/coursera")
# Read data into a dataframe called PE (physical endurance)
PE <- read.table("stats1_datafiles_Stats1.13.Lab.05.txt", header = T)

# If you want to view the data
View(PE)
# edit(PE)

# Summary statistics
describe(PE) 

# Illustration of standard error calculation
# Standard error = Standard deviation divided by the square root of sample size
# se = sd / sqrt(N)
table1 <- describe(PE)
table1
age.sd <- table1[2,4] # 2 row, 4 collumn
age.sd
age.n <- table1[2,2]
age.n # sample size
age.se <- table1[2,4] / sqrt(table1[2,2])
age.se
age.se == table1[2,13]

# Correlation analysis 
cor(PE[2:4]) 

# NHST for each correlation coefficient
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Save the correlations in a table to illustrate calculation of regression coefficients
table2 <- cor(PE[2:4])


# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)

# Illustration of calculation of regression coefficient (age and endurence)
# Regression coefficient = correlation coefficient * (standard deviation of Y / standard deviation of X)
# B = r * (sdy / sdx)
table2
table1[4,4]
table1[2,4]
table2[3,1]
model1.B <- table2[3,1] * (table1[4,4] / table1[2,4])
model1.B

# Illustration of calculation of standard error of the regression coefficient
# Standard error = Square root [ (Sums of Squares.Residual / (N - 2) ) / (Sums of Squares.X) ]
# se.B = sqrt[ (SS.resid / (N-2) ) / SS.X )]
table3 <- anova(model1)
table3
SS.resid <- table3[2,2]
SS.resid
df <- table3[2,1]
df
SS.X <- table3[1,2] + table3[2,2]
SS.X
se.B <- sqrt( (SS.resid / df) / SS.X) 
se.B

# Print 95% confidence interval for the regression coefficient
confint(model1) 

# Illustration of calculation of confidence interval
# Upper value = B + (tcrit * se.B) and Lower value = B - (tcrit * se.B)
tcrit <- qt(c(.025, .975), df = 198)
tcrit
interval <- -0.08772 + (tcrit*se.B)
interval

# Scatterplot with confidence interval around the regression line
ggplot(PE, aes(x = age, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 
  
  
model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)
confint(model2) #Prints 95% confidence intervals for the regression coefficients

ggplot(PE, aes(x = activeyears, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 


model3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model3)
confint(model3) # Prints 95% confidence intervals for the regression coefficients

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)

ggplot(PE, aes(x = predicted, y = endurance)) + geom_smooth(method = "lm") + 
  geom_point() 

  
# Conduct a model comparison NHST to compare the fit of model2 to the fit of model3
anova(model2, model3)


# Regression analyses, standardized
model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)
confint(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)
confint(model2.z)

model3.z <- lm(scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)
confint(model3.z)

# Conduct a model comparison NHST to compare the fit of model2.z to the fit of model3.z
anova(model2.z, model3.z)
# Note that the F value and the p value are the same as from the unstandardized model comparison
anova(model2, model3)


# Now take a random subset of the data such that N = 20 
PE.20 <- PE[sample(nrow(PE), 20), ]

# Summary statistics
describe(PE.20) 

# Correlation analysis 
round(cor(PE.20[2:4]), 2) # Round to 2 decimal places 

cor.test(PE.20$age, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$age)

# Regression analyses, unstandardized
model1.20 <- lm(PE.20$endurance ~ PE.20$age)
summary(model1.20)
confint(model1.20)
  
model2.20 <- lm(PE.20$endurance ~ PE.20$activeyears)
summary(model2.20)
confint(model2.20)

model3.20 <- lm(PE.20$endurance ~ PE.20$age + PE.20$activeyears)
summary(model3.20)
confint(model3.20)

# Conduct a model comparison NHST to compare the fit of model2.20 and model3.20
anova(model2.20, model3.20)

