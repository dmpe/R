# Statistics One, 2013, Lab 9

# Lab goals
# Conduct a between groups factorial ANOVA
# Example
#  An experiment designed to investigate the effects of talking on a cell phone while driving
#  DV = Driving errors
#  There are two IVs
#    Driving difficulty (2 levels): Easy, Difficult
#    Conversation difficulty (3 levels): Control, Easy, Difficult

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages(“psych”)
# install.packages(“car”)

# Load packages
library(psych)
library(car)

# Read data into a data frame called AB
AB <- read.table("Stats1.13.Lab.09.txt", header = T)

# Haste and Prime were interpreted by R as integers, so let's create factors
AB$Haste = factor(AB$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))
AB$Prime = factor(AB$Prime, levels = c(1,2), labels = c("Parable", "Control"))
class(AB$Haste)
class(AB$Prime)

# Omnibus analysis is a 3x2 factorial ANOVA with Haste and Prime as the IVs and Helping as the DV
aov.AB = aov(AB$Helping ~ AB$Haste * AB$Prime)
summary(aov.AB)
aov.table = summary(aov.AB)

# Post-hoc comparisons
TukeyHSD(aov.AB)

# Create a vector of SS to calculate effect size estimates
SS.vector = aov.table[[1]]$"Sum Sq"
SS.vector

# Effect sizes (partial eta-squared)
eta.squared.A = SS.vector[1] / (SS.vector[1] + SS.vector[4])
eta.squared.A
eta.squared.B = SS.vector[2] / (SS.vector[2] + SS.vector[4])
eta.squared.B
eta.squared.AB = SS.vector[3] / (SS.vector[3] + SS.vector[4])
eta.squared.AB

# Effect sizes (complete eta-squared)
SS.total = SS.vector[1] + SS.vector[2] + SS.vector[3] + SS.vector[4]
eta.squared.A = SS.vector[1] / SS.total
eta.squared.A
eta.squared.B = SS.vector[2] / SS.total
eta.squared.B
eta.squared.AB = SS.vector[3] / SS.total
eta.squared.AB

#Simple effects analysis (of Prime at each level of Haste)
A1.B = subset(AB, AB$Haste == "1")
A2.B = subset(AB, AB$Haste == "2")
A3.B = subset(AB, AB$Haste == "3")

aov.A1.B = aov(A1.B$Helping ~ A1.B$Prime)
summary(aov.A1.B)
aov.A2.B = aov(A2.B$Helping ~ A2.B$Prime)
summary(aov.A2.B)
aov.A3.B = aov(A3.B$Helping ~ A3.B$Prime)
summary(aov.A3.B)

# Effect sizes for simple effects
aov.table.A1.B = summary(aov.A1.B)
aov.table.A2.B = summary(aov.A2.B)
aov.table.A3.B = summary(aov.A3.B)

SS.A1.B = aov.table.A1.B[[1]]$"Sum Sq"
SS.A2.B = aov.table.A2.B[[1]]$"Sum Sq"
SS.A3.B = aov.table.A3.B[[1]]$"Sum Sq"

eta.squared.A1.B = SS.A1.B[1] / (SS.A1.B[1] + SS.A1.B[2])
eta.squared.A1.B
eta.squared.A2.B = SS.A2.B[1] / (SS.A2.B[1] + SS.A2.B[2])
eta.squared.A2.B
eta.squared.A3.B = SS.A3.B[1] / (SS.A3.B[1] + SS.A3.B[2])
eta.squared.A3.B

#Simple effects analysis (of Haste at each level of Prime)
A.B1 = subset(A.B, A.B$Prime == "1")
A.B2 = subset(A.B, A.B$Prime == "2")

Haste = factor(A.B1$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))

aov.A.B1 = aov(A.B1$Helping ~ Haste)
summary(aov.A.B1)
aov.A.B2 = aov(A.B2$Helping ~ Haste)
summary(aov.A.B2)

# Effect sizes for simple effects
aov.table.A.B1 = summary(aov.A.B1)
aov.table.A.B2 = summary(aov.A.B2)

SS.A.B1 = aov.table.A.B1[[1]]$"Sum Sq"
SS.A.B2 = aov.table.A.B2[[1]]$"Sum Sq"

eta.squared.A.B1 = SS.A.B1[1] / (SS.A.B1[1] + SS.A.B1[2])
eta.squared.A.B1
eta.squared.A.B2 = SS.A.B2[1] / (SS.A.B2[1] + SS.A.B2[2])
eta.squared.A.B2

