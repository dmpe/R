library(psych)
library(ggplot2)
library(multilevel)

# Segment 1
setwd("c:/Users/Dima/Documents/R/coursera")
# Read data into a dataframe called MOD
ma <- read.table("Stats1.13.HW.07.txt", header = T)



dop <- cor.test(ma$extra, ma$happy)
da<-cor.test(ma$extra, ma$diverse)
cor.test(ma$diverse, ma$happy)

dop2 = lm(ma$happy~ma$extra)
confint(dop2)
summary(dop2)

model3 = lm(ma$happy ~ ma$extra + ma$diverse) 
summary(model3)
confint(model3)


cor.test(ma$extra, ma$happy)
cor.test(ma$extra, ma$happy+ ma$diverse)


indirect = sobel(ma$extra, ma$diverse, ma$happy) 
indirect