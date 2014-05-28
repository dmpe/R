# Statistics One, 2013, Lab 7

# Lab goals
#   Conduct moderation and mediation analyses

# Segment 1
#   Moderation analysis
#     Example
#     An experimental research investigation of the effects of stereotype threat on intelligence testing 
#       Dependent variable (Y) is score on an intelligence test (IQ)
#       Independent variable (X) is the treatment condition (3 levels: control, explicit threat, implicit threat)
#       Moderator variable is score on a working memory task
#       Sample size of N = 150 (n = 50)

# Segment 2
#   Mediation analysis
#     Example
#       An experimental research investigation of the effects of stereotype threat on intelligence testing 
#         Dependent variable (Y) is score on an intelligence test (IQ)
#         Independent variable (X) is the treatment condition (2 levels: control, threat)
#         Mediator variable (M) is score on a working memory task
#         Sample size of N = 100 (n = 50)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")
setwd("c:/Users/Dima/Documents/R/coursera")
# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

# Segment 1

# Read data into a dataframe called MOD
MOD <- read.table("stats1_datafiles_Stats1.13.Lab.07.txt", header = T)

# If you want to view the data
# View(MOD)
edit(MOD)

# Summary statistics
describeBy(MOD, MOD$condition) 

# First, is there an effect of stereotype threat?
model0 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2)
summary(model0)
confint(model0)

# We could also use the aov function (for analysis of variance) followed by the TukeyHSD function (Tukey's test of pairwise comparisons, which adjusts the p value to prevent infaltion of Type I error rate)
model0a <- aov(MOD$IQ ~ MOD$condition)
summary(model0a)
TukeyHSD(model0a)

# Moderation analysis (uncentered): model1 tests for "first-order effects"; model2 tests for moderation
model1 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)
summary(model1)

ggplot(MOD, aes(x = WM, y = IQ)) + geom_smooth(method = "lm") + 
  geom_point() 

# Create new predictor variables
MOD$WM.D1 <- (MOD$WM * MOD$D1)
MOD$WM.D2 <- (MOD$WM * MOD$D2)

model2 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2)
summary(model2)

anova(model1, model2)

# Scatter plot by group and all in one
WM.control <- MOD$WM[1:50]
IQ.control <- MOD$IQ[1:50]
WM.threat1 <- MOD$WM[51:100]
IQ.threat1 <- MOD$IQ[51:100]
WM.threat2 <- MOD$WM[101:150]
IQ.threat2 <- MOD$IQ[101:150]

#ggplot(MOD, aes(x = WM.control, y = IQ.control)) + geom_smooth(method = "lm") + 
#  geom_point()
ggplot(MOD, aes(x = WM.threat1, y = IQ.threat1)) + geom_smooth(method = "lm") + 
 geom_point()
#ggplot(MOD, aes(x = WM.threat2, y = IQ.threat2)) + geom_smooth(method = "lm") + 
#  geom_point()

color <- c("red","green","blue")
ggplot(MOD, aes(x = WM, y = IQ)) + stat_smooth(method="lm", se=F) +
  geom_point(aes(color=condition))
ggplot(MOD, aes(x = WM, y = IQ)) + 
  geom_smooth(aes(group=condition), method="lm", se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))
  
# Segment 2

# Read data into a dataframe called MED
MED <- read.table("stats1_datafiles_Stats1.13.Lab.07b.txt", header = T)

# If you want to view the data
# View(MED)
edit(MED)

# Summary statistics
describeBy(MED, MED$condition) 

# The function sobel in the multilevel package executes the entire mediation analysis in one step but first we will do it with 3 lm models
model.YX <- lm(MED$IQ ~ MED$condition)
model.YXM <- lm(MED$IQ ~ MED$condition + MED$WM)
model.MX <- lm(MED$WM ~ MED$condition)

summary(model.YX)
summary(model.YXM)
summary(model.MX)

# Compare the results to the output of the sobel function
model.ALL <- sobel(MED$condition, MED$WM, MED$IQ) 
model.ALL
