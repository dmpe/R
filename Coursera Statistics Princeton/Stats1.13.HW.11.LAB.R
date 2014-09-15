library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape2)

setwd("C:/Users/Dima/Documents/R/coursera/")
file <- read.table("Stats1.13.HW.11.txt", header=T)

View(file)
#q1
# Using a t-test, compare verbal scores before and after training in the fixed 
# condition. Is the difference pre-test to post-test significant?

fileSF <- subset(file, file$cond == "fixed")
fileSF

t.test(fileSF$verbal.pre,fileSF$verbal.post, paired=T )
#q2
t.test(fileSF$spatial.pre, fileSF$spatial.post, paired=T)
#t.test(file$spatial.pre, file$spatial.post, paired=T)

#q3
# Run a Wilcoxon test for the same comparison (pre-test to post-test on spatial 
# scores, fixed condition). Which of the two tests gives the highest p-value for 
# the comparison?
t.test(fileSF$spatial.pre, fileSF$spatial.post, paired=T)
wilcox.test(fileSF$spatial.pre, fileSF$spatial.post, paired=F)


#q4
# What is the effect size (Cohen's d) for the difference between pre-test and post
#-test spatial scores for the malleable condition? (round to two decimal places)

fileSM <- subset(file, file$cond == "malleable")
fileSM

round(cohensD(fileSF$spatial.pre, fileSF$spatial.post, method="paired"),2)

# q5
fileSF.gainV <- fileSF$verbal.post- fileSF$verbal.pre
fileSF["fileSF.gainV"] <- fileSF.gainV

fileSF.gainS <- fileSF$spatial.post- fileSF$spatial.pre
fileSF["fileSF.gainS"] <- fileSF.gainS

fileSF.gainI <- fileSF$intel.post- fileSF$intel.pre
fileSF["fileSF.gainI"] <- fileSF.gainI

describe(fileSF)

#improvments
cohensD(fileSF$verbal.pre, fileSF$verbal.post, method="paired") 
cohensD(fileSF$spatial.pre, fileSF$spatial.post, method="paired")
cohensD(fileSF$intel.pre, fileSF$intel.post, method="paired")


#q6

fileSM.gainV <- fileSM$verbal.post- fileSM$verbal.pre
fileSM["fileSM.gainV"] <- fileSM.gainV

fileSM.gainS <- fileSM$spatial.post- fileSM$spatial.pre
fileSM["fileSM.gainS"] <- fileSM.gainS

fileSM.gainI <- fileSM$intel.post- fileSM$intel.pre
fileSM["fileSM.gainI"] <- fileSM.gainI

describe(fileSM)

#improvments
cohensD(fileSM$verbal.pre, fileSM$verbal.post, method="paired") 
cohensD(fileSM$spatial.pre, fileSM$spatial.post, method="paired")
cohensD(fileSM$intel.pre, fileSM$intel.post, method="paired")
#q7
#intel to neni, all !!!!!!!!!
wilcox.test(file$spatial.pre,file$verbal.pre, paired = F)
wilcox.test(file$spatial.pre,file$intel, paired = F)
wilcox.test(file$verbal.pre,file$intel.pre, paired = F)

#q8
pre.m = fileSM$verbal.pre + fileSM$spatial.pre + fileSM$intel.pre
post.m = fileSM$verbal.post + fileSM$spatial.post + fileSM$intel.post 
cohensD(pre.m, post.m, method="paired")
pre.f = fileSF$verbal.pre + fileSF$spatial.pre + fileSF$intel.pre
post.f = fileSF$verbal.post + fileSF$spatial.post + fileSF$intel.post
cohensD(pre.f, post.f, method="paired")
#9
cohensD(fileSM$verbal.pre, fileSM$verbal.post, method="paired") 
cohensD(fileSM$spatial.pre, fileSM$spatial.post, method="paired")
cohensD(fileSM$intel.pre, fileSM$intel.post, method="paired") 
cohensD(fileSF$verbal.pre, fileSF$verbal.post, method="paired")
cohensD(fileSF$spatial.pre, fileSF$spatial.post, method="paired") 
cohensD(fileSF$intel.pre, fileSF$intel.post, method="paired")

TukeyHSD(aov.model)