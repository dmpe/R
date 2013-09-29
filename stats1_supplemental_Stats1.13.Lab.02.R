# Statistics One Lab 2 

# Lab goals
#   Read a datafile into R
#   Learn more about object types
#   Print summary statistics
#   Examine distributions using histograms

# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal
#     Memory composite visual
#     Visual motor speed composite
#     Reaction time composite
#     Impulse control composite
#     Total symptom score


# Check your working directory
getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("sm")

# Load packages
library(psych)
library(sm)

# Read data into a dataframe called impact
impact <- read.table("stats1-datafiles-Stats1.13.Lab.02.txt", header = T) 

# Get the dimensions of the dataframe
dim(impact)
nrow(impact)
ncol(impact)

edit(impact)

# Object types
class(impact) 
names(impact) 

class(impact$verbal_memory_baseline)
class(impact$reaction_time_baseline)
class(impact$subject)

impact$subject <- factor(impact$subject) 
class(impact$subject)

# Summary statistics
mean(impact$verbal_memory_baseline) 
sd(impact$verbal_memory_baseline)

describe(impact) 

describeBy(impact, impact$condition)

# Subsetting
edit(impact)

control <- subset(impact, impact[, 2]=="control")
control

concussed <- subset(impact, impact[, 2]=="concussed")
concussed

# Histograms of control group at baseline
par(mfrow = c(2,3)) # To view 6 histograms on one page 
hist(control[, 3], xlab = "Verbal memory", main = "") 
hist(control[, 4], xlab = "Visual memory")
hist(control[, 5], xlab = "Visual motor speed")
hist(control[, 6], xlab = "Reaction time")
hist(control[, 7], xlab = "Impulse control")
hist(control[, 8], xlab = "Total symptom score")

# To demonstrate that there is more than one way to access a variable
par(mfrow = c(1,2)) # To view 2 histograms on one page 
hist(control[, 3], xlab = "Verbal memory", main = "") 
hist(control$verbal_memory_baseline, xlab = "Verbal memory", main = "") 

# Histograms of concussed group at baseline
par(mfrow = c(2,3))
hist(concussed[, 3], xlab = "Verbal memory", main = "")
hist(concussed[, 4], xlab = "Visual memory", main = "")
hist(concussed[, 5], xlab = "Visual motor speed", main = "")
hist(concussed[, 6], xlab = "Reaction time", main = "")
hist(concussed[, 7], xlab = "Impulse control", main = "")
hist(concussed[, 8], xlab = "Total symptom score", main = "")

# Histograms of control group at retest
par(mfrow = c(2,3))
hist(control[, 9], xlab = "Verbal memory", main = "") 
hist(control[, 10], xlab = "Visual memory", main = "")
hist(control[, 11], xlab = "Visual motor speed", main = "")
hist(control[, 12], xlab = "Reaction time", main = "")
hist(control[, 13], xlab = "Impulse control", main = "")
hist(control[, 14], xlab = "Total symptom score", main = "")

# Histograms of concussed group at retest
par(mfrow = c(2,3))
hist(concussed[, 9], xlab = "Verbal memory", main = "")
hist(concussed[, 10], xlab = "Visual memory", main = "")
hist(concussed[, 11], xlab = "Visual motor speed", main = "")
hist(concussed[, 12], xlab = "Reaction time", main = "")
hist(concussed[, 13], xlab = "Impulse control", main = "")
hist(concussed[, 14], xlab = "Total symptom score", main = "")

# Density plots
par(mfrow = c(1,2))
hist(concussed[, 14], xlab = "Total symptom score", main = "")
plot(density(concussed[, 14]), xlab = "Total sympton score", main = "")

# Compare density plots
par(mfrow = c(1,1))
sm.density.compare(impact$total_symptom_retest, 
                   impact$condition, xlab = "Total symptom score")               



