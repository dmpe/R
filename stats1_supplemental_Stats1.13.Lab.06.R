# Statistics One, 2013, Lab 6

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, emphasis on models that include a categorical predictor variable

# Example
#   A correlational study investigating predictors of salary among University faculty members
#     Outcome variable (Y) is annual salary in US Dollars ($)
#     Predictors (X) are age, number of years as faculty member, number of publications, and academic department (History, Psychology, Sociology)
#     Sample size is N = 100 

# Check your working directory
# getwd()
# If necessary, set your working directory
setwd("c:/Users/Dima/Documents/R/coursera")

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called FS (Faculty Salary)
FS <- read.table("stats1_datafiles_Stats1.13.Lab.06.txt", header = T)

# If you want to view the data
# View(FS)
edit(FS)

# Summary statistics
describe(FS) 
summary(FS)

# Correlation analysis 
cor(FS[1:4]) 

# Regression analyses, unstandardized
# Model0 demonstrates that age and years are, for the most part redundnant, so we will only use years in the next set of models
model0 <- lm(FS$salary ~ FS$years + FS$age)
summary(model0)

model1 <- lm(FS$salary ~ FS$years)
summary(model1)
confint(model1)

model2 <- lm(FS$salary ~ FS$pubs)
summary(model2)
confint(model2) 

model3 <- lm(FS$salary ~ FS$years + FS$pubs)
summary(model3)
confint(model3) 

# Compare Model3 to both Model1 and Model2 to determine if including both predictors is best
anova(model1, model3)
anova(model2, model3)

# Now let's conduct regression analyses that include a categorical predictor
# We need to use dummy codes to represent the nominal variable (dept) as numeric 
# In R there are several ways to do this, the following is just one example, using the function C (for contrasts)
dept.code <- C(FS$dept, treatment)

model4 <- lm(FS$salary ~ FS$years + FS$pubs + (dept.code))
summary(model4)
confint(model4)

# Compre Model4 to Model3 to determine if including dept improves the predictions of the model
anova(model3, model4)

# Let's examine the salary difference between History and Sociology
# To quickly view the means, use the tapply function
tapply(FS$salary, FS$dept, mean)

# The actual means are not that different, so why are the means predicted by the model so different?
# There must be differences across departments in years and/or pubs
# Let's look at years
tapply(FS$years, FS$dept, mean)

# Let's look at pubs
tapply(FS$pubs, FS$dept, mean)

# The actual salary for Sociology is not that different from the other departments BUT they have more years on the job and more publications, on average, than the other departments, so their PREDICTED salary, based on an AVERAGE number of years and publications is lower, which is a more accuracte refelction of the discrepancies across departments.
