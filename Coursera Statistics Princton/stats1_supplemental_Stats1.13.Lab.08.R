# Statistics One, 2013, Lab 8

# Lab goals
#   Conduct group comparisons
#     Dependent t-tests
#     Independent t-tests
#     Analysis of Variance (ANOVA)

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("Users/aconway/Dropbox/STATS1-V2.0/Labs")

# If necessary, install packages
# install.packages("psych")
# install.packages("car")
setwd("c:/Users/Dima/Documents/R/coursera")
# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

# Read data into a dataframe called wm
wm = read.table("stats1_datafiles_Stats1.13.Lab.08.txt", header = T)

# If you want to view the data
View(wm)
#edit(wm)

# Summary statistics by all groups (control, 8 sessions, 12 sessions, 17 sessions, 19 sessions)
describeBy(wm, wm$cond)

# Create two subsets of data: One for the control group and another for the training groups
wm.c = subset(wm, wm$train == "0")
wm.t = subset(wm, wm$train == "1")

# Save summary statistics in tables to illustrate calculation of effect size
wm.c.out = describe(wm.c)
wm.c.out
wm.t.out = describe(wm.t)
wm.t.out

# Dependent t-tests

# First, compare pre and post scores in the control group
t.test(wm.c$post, wm.c$pre, paired = T)

# Next, compare pre and post scores in the training groups
t.test(wm.t$post, wm.t$pre, paired = T)

# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores

d.c = (wm.c.out[4,3]) / (wm.c.out[4,4])
d.c

#or
cohensD(wm.c$post, wm.c$pre, method="paired")

d.t = (wm.t.out[4,3]) / (wm.t.out[4,4])
d.t
#or
cohensD(wm.t$post, wm.t$pre, method="paired")
 
# Boxplot
long.wm <- melt(wm, id=c("cond", "train", "gain"))

ggplot(long.wm, aes(x=cond, y=value, color=variable)) + 
  geom_boxplot() +
  guides(fill=FALSE) 
  
# Independent t-test
# Compare the gain scores in the control and training groups 
t.test(wm$gain ~ wm$train, var.equal = T)

# Cohen's d for independent t-tests
# d = (M1 - M2) / Pooled Standard Deviation

pooled.sd = (79/118 * wm.t.out[4,4]) + (39/118 * wm.c.out[4,4])

d.ct = (wm.t.out[4,3] - wm.c.out[4,3]) / pooled.sd
d.ct
# or
cohensD(wm$gain ~ wm$train, method="pooled")

# Boxplot
ggplot(wm, aes(x=cond, y=gain, fill=cond)) + 
  geom_boxplot() +
  guides(fill=FALSE)
  
# To compare the gain scores across all groups, use ANOVA
# First, check the homogeneity of variance assumption
leveneTest(wm.t$gain, wm.t$cond, center="mean")
leveneTest(wm.t$gain, wm.t$cond)

aov.model = aov(wm.t$gain ~ wm.t$cond)
summary(aov.model)

# Save results in a table to illustrate calculation of effect size
aov.table = summary(aov.model)

# Effect size for ANOVA
ss = aov.table[[1]]$"Sum Sq"
eta.sq = ss[1] / (ss[1] + ss[2])
eta.sq
#or
etaSquared(aov.model, anova=T)

# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)
