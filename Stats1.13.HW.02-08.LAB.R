
# Segment 1
setwd("c:/Users/Dima/Documents/R/coursera")
# Read data into a dataframe called MOD
ma <- read.table("Stats1.13.HW.02-08.txt", header = T)
ma

describeBy(ma, ma$condition)

# Using a dependent t-test, is the difference between pre and post-test scores significant?

ma.1 <- subset(ma, ma$time=="pre")
ma.2 <- subset(ma, ma$time=="post")

t.test(ma.1$SR, ma.2$SR, paired=T)


# Create subsets for each training condition. Which group shows no difference between pre and post-test scores? 

# POST + PRE
test1 <- subset(ma, ma$condition=="WM")

test1.pre <- subset(test1, test1$time == "pre" )
test1.post <- subset(test1, test1$time == "post" )

test2 <- subset(ma, ma$condition=="PE")

test2.pre <- subset(test2, test2$time == "pre" )
test2.post <- subset(test2, test2$time == "post" )

test3 <- subset(ma, ma$condition=="DS")

test3.pre <- subset(test3, test3$time == "pre" )
test3.post <- subset(test3, test3$time == "post" )

t.test(test1.pre$SR, test1.post$SR, paired = T)

t.test(test2.pre$SR, test2.post$SR, paired = TRUE)

t.test(test3.pre$SR, test3.post$SR, paired = TRUE)


# Reshape the data into a wide format, and create a new variable for gain score. Now subset the new dataframe based on the training conditions. Which comparison between training conditions does not show a significant difference?
data <- dcast(ma, subject + condition ~ time)

data.wide$gain = data.wide$post - data.wide$pre 
wm.wide = subset(data.wide, data.wide$condition=="WM") 
pe.wide = subset(data.wide, data.wide$condition=="PE") 
ds.wide = subset(data.wide, data.wide$condition=="DS")
t.test(wm.wide$gain, pe.wide$gain, var.equal=T) 
t.test(wm.wide$gain, ds.wide$gain, var.equal=T)
t.test(ds.wide$gain, pe.wide$gain, var.equal=T)



datap <- data$post - data$pre 
datap

#To compare the gain scores across all groups, we now turn to ANOVA. Is the homogeneity of variance assumption violated?

leveneTest(datap, data$condition, center="mean")
leveneTest(datap, data$condition)

#Run an ANOVA model on the gain scores as a function of training condition. Is the effect of condition significant?

aov.model = aov(datap ~ data$condition)
summary(aov.model)

aov.table = summary(aov.model)
aov.table

etaSquared(aov.model, anova=T)


TukeyHSD(aov.model)
