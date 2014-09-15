# Statistics One, 2013, Lab 9

# Lab Goals
#    Conduct a between groups factorial ANOVA
#    Example
#    A randomized controlled experiment designed to investigate teh effects of talking on a cell phone while driving
#      DV = Number of driving errors
#      Two IVs
#         (A) Conversation difficulty (3 levels): Control, Easy, Difficult
#         (B) Driving difficulty (2 levels): Easy, Difficult

# If necessary, install packages
# install.packages("psych")
# install.packages("car")
# install.packages("lsr")

library(psych)
library(car)
library(lsr)

# Segment 1
setwd("c:/Users/Dima/Documents/R/coursera")
# Read data into a dataframe called AB
AB <- read.table("stats1_datafiles_Stats1.13.Lab.09.txt", header = T)

# Let's look at the data 
edit(AB)

# Test the homogeneity of variance assumption
leveneTest(AB$errors ~ AB$driving * AB$conversation)

# Conduct the factorial ANOVA
AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)

# Conduct simple effects analysis (of Conversation at each level of Driving)
AB1 <- subset(AB, AB$driving == "Easy")
AB2 <- subset(AB, AB$driving == "Difficult")

aov.AB1 <- aov(AB1$errors ~ AB1$conversation)
summary(aov.AB1)

aov.AB2 <- aov(AB2$errors ~ AB2$conversation)
summary(aov.AB2)

#Both simple effects are significant, so why is there an interaction? Let's look at effect sizes:
etaSquared(aov.AB1, anova = T)
etaSquared(aov.AB2, anova = T)

# Finally, let's look at pairwise comparisons for the simple effects
TukeyHSD(aov.AB1)
TukeyHSD(aov.AB2)








