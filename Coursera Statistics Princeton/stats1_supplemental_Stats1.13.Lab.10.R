# Statistics One, 2013, Lab 10

# Lab goals
#   Conduct a binary logisitc regression 

# Example
# The data are based on a mock jury study conducted by Shari Diamond and Jonathan Casper 
# Participants (N = 100) watched a videotaped sentencing phase trial in which the defendant had 
#   already been found guilty  
# The issue for the jurors to decide was whether the defendant deserved the death penalty  
# These data were collected “pre-deliberation” (i.e., each juror was asked to provide 
#   his/her vote on the death penalty verdict, then the jurors met as a group to decide the 
#   overall jury verdict)
# The initial individual verdicts are given in this data set

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
#install.packages("aod")
# install.packages("QuantPsyc")

# Load packages
library(psych)
library(aod)
library(QuantPsyc)
setwd("c:/Users/Dima/Documents/R/coursera")
# Read the data into a dataframe called BL
BL <- read.table("stats1_datafiles_Stats1.13.Lab.10.txt", header = T)

# If you want to view the data
View(BL)
#edit(BL)

# Summary statistics
describe(BL) 

# Binary logistic regression
lrfit <- glm(BL$verdict ~ BL$danger + BL$rehab + BL$punish + BL$gendet + BL$specdet + BL$incap, family = binomial)
summary(lrfit)

confint(lrfit) # Conf.Intervals using profiled log-likelihood (default for logistic models)
confint.default(lrfit) # CIs using standard errors

# Model fit
with(lrfit, null.deviance - deviance) #difference in deviance for the two models
with(lrfit, df.null - df.residual) #df for the difference between the two models
with(lrfit, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

# Wald tests
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 2) #danger
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 3) #rehab
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 4) #punish
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 5) #gendet
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 6) #specdet
wald.test(b = coef(lrfit), Sigma = vcov(lrfit), Terms = 7) #incap

# Odds ratios
exp(coef(lrfit)) #exponentiated coefficients

# Classification table
ClassLog(lrfit, BL$verdict)

# Significant predictors (danger, rehab, gendet)
par(mfrow=c(1,3))
plot(BL$danger, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$danger), col="blue", lwd=5)
plot(BL$rehab, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$rehab), col="blue", lwd=5)
plot(BL$gendet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$gendet), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Significant predictors")

# Non-significant predictors (punish, specdet, incap)
par(mfrow=c(1,3))
plot(BL$punish, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$punish), col="blue", lwd=5)
plot(BL$specdet, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$specdet), col="blue", lwd=5)
plot(BL$incap, predict(lrfit), bty="n")
abline(lm(predict(lrfit) ~ BL$incap), col="blue", lwd=5)
par(mfrow=c(1,1))
title("Non-significant predictors")
