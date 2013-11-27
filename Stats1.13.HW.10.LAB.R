library(psych)
library(aod)
library(QuantPsyc)


setwd("c:/Users/Dima/Documents/R/coursera")
# Read the data into a dataframe called BL
h10 <- read.table("Stats1.13.HW.10.txt", header = T)

# view
View(h10)

#q1
#q1 <- subset(h10, h10$change==1)
#round(median(q1$age),2)

describeBy(h10$age, h10$change==1)
round(25.9, 2)
round(38.95,2)

#q2
lr <- glm(h10$change ~ h10$age +h10$educ+h10$gdp+h10$co2 , family = binomial)
summary(lr)
#q4
confint(lr)
round(c(-31.171217249, -3.03349629), 2)

#q5
confint.default(lr)
round(c(0.092877885, 0.651260815),2)

#q6s
with(lr, null.deviance - deviance)
round(16.30328,2)

#q7
with(lr, df.null - df.residual)

#q8
with(lr, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = FALSE)) #p-value

#q9
wald.test(b = coef(lr), Sigma = vcov(lr), Terms = 2) #edu

#q10
ClassLog(lr, h10$change)
round(0.8148148,2)












































