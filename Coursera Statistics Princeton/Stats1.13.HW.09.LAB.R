
library(psych)
library(car)
library(lsr)

# Segment 1
setwd("c:/Users/Dima/Documents/R/coursera")

Tes <- read.table("Stats1.13.HW.09.txt", header = T)

# AB$Haste = factor(Tes$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late"))
# AB$Prime = factor(Tes$Prime, levels = c(1,2), labels = c("Parable", "Control"))

class(Tes$Haste)
class(Tes$Prime)

Haste = factor(Tes$Haste, levels = c(1,2,3), labels = c("Early", "On Time", "Late")) 
Prime = factor(Tes$Prime, levels = c(1,2), labels = c("Parable", "Control")) 
aov.Tes = aov(Tes$Helping ~ Haste * Prime) 
summary(aov.Tes)
TukeyHSD(aov.Tes) # no ?!!!
etaSquared(aov.Tes,anova=T)

aov.table = summary(TES.modell) 


aov.Tes1 <- aov(Tes$Helping ~ Tes$Prime)
summary(aov.Tes1)

aov.Tes2 <- aov(Tes$Helping ~ Tes$Haste)
summary(aov.Tes2)

etaSquared(aov.Tes1, anova = T)
etaSquared(aov.Tes2, anova = T)


A1.B = subset(Tes, Tes$Haste == "1")
A2.B = subset(Tes, Tes$Haste == "2") 
A3.B = subset(Tes, Tes$Haste == "3") 
aov.A1.B = aov(A1.B$Helping ~ A1.B$Prime) 
summary(aov.A1.B) 
aov.A2.B = aov(A2.B$Helping ~ A2.B$Prime) 
summary(aov.A2.B) 
aov.A3.B = aov(A3.B$Helping ~ A3.B$Prime) 
summary(aov.A3.B)

etaSquared(aov.A1.B,anova=T)

#all of above