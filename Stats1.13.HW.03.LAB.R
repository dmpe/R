library(psych)
library(gclus)
library(rgl)
library(sm)
library(plyr)

getwd()
setwd("C:/Users/Dima/Documents/R/coursera")
# open data
info <- read.table("Stats1.13.HW.03.txt", header=T)

# add columns (pre x post and 1x2) together in one column
info["S.pre"] <- NA
info["V.pre"] <- NA
info["S.post"] <- NA
info["V.post"] <- NA

info$S.pre <- info$S1.pre + info$S2.pre
info$V.pre <- info$V1.pre + info$V2.pre
info$S.post <- info$S1.post + info$S2.post
info$V.post <- info$V1.post + info$V2.post

# question 1
# What is the correlation between S1 and S2 pre-training?

round(cor(info[3:4]),2)

# question 2
# What is the correlation between V1 and V2 pre-training?

cor(info[7:8])

# question 3
# With respect to the measurement of two distinct constructs, spatial 
# reasoning and verbal reasoning, the pattern of correlations pre-training reveals:

round(cor(info[11:12]),2)

# Answer:  The question is simple -- how "high" do correlations need to 
# be to provide evidence for convergence and how "low" do they need to be 
# to provide evidence for discrimination? And the answer is -- we don't
# know! In general we want convergent correlations to be as high as possible
# and discriminant ones to be as low as possible, but there is no hard and 
# fast rule. 
# url: http://www.socialresearchmethods.net/kb/convdisc.php

# question 4
# Correlations from the control group could be used to estimate test/retest 
# reliability. If so, which test is most reliable?


control <- subset(info, info[, 2]=="aer")
control

cor(control$S1.pre, control$S1.post) 
cor(control$S2.pre, control$S2.post) 
cor(control$V1.pre, control$V1.post) 
cor(control$V2.pre, control$V2.post) # this one


# question 5 
# Does there appear to be a correlation between spatial reasoning before 
# training and the amount of improvement in spatial reasoning?

rp <- info$S.pre

info["Srozdil"] <- NA
info["Srozdil"] <- info$S.post - info$S.pre
dr <- info$Srozdil
cor(rp, dr)
plot(info$S.pre, info$Srozdil)

# his

info$S.pre = (info$S1.pre + info$S2.pre) / 2 
info$V.pre = (info$V1.pre + info$V2.pre) / 2
info$S.post = (info$S1.post + info$S2.post) / 2 
info$V.post = (info$V1.post + info$V2.post) / 2 
info$Sgain = info$S.post - info$S.pre 
info$Vgain = info$V.post - info$V.pre 
cor(info$S.pre, info$Sgain)




# question 6
# Does there appear to be a correlation between verbal reasoning before 
# training and the amount of improvement in verbal reasoning?
rp2 <- info$V.pre

info["Vrozdil"] <- NA
info["Vrozdil"] <- info$V.post - info$V.pre
dr2 <- info$Vrozdil
cor(rp2, dr2)
plot(info$V.pre, info$Vrozdil)

# his

cor(info$V.pre, info$Vgain)



# question 7
# Which group exhibited more improvement in spatial reasoning?

aer <- subset(info, info[,2]=="aer")
hist(aer$Srozdil)

des <- subset(info, info[,2]=="des")
hist(des$Srozdil)

describeBy(info$Sgain, info$cond)

# question 8
# Create a color scatterplot matrix for all 4 measures at 
# pre-test. Do the scatterplots suggest two reliable and valid constructs?

base <- info[3:4]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

base <- info[7:8]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")


# his
prd = abs(cor(cbind(info[3], info[4], info[7], info[8]))) 
cpairs(prd, order.single(prd), panel.colors = dmat.color(prd), gap=.5)

# question 9
# Create a color scatterplot matrix for all 4 measures at post-test. 
# Do the scatterplots suggest two reliable and valid constructs?


base <- info[5:6]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")

base <- info[9:10]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r) 
cpairs(base, base.order, panel.colors = base.color, gap = .5,
       main = "Variables Ordered and Colored by Correlation")
# his
postd = abs(cor(cbind(info[5], info[6], info[9], info[10]))) 
cpairs(postd, order.single(postd), panel.colors = dmat.color(postd), gap=.5)


# question 10
# What is the major change from pre-test to post-test visible on the color matrix?

var(info$S1.pre)
var(info$S2.pre)
var(info$S1.post)
var(info$S2.post)

var(info$V1.pre)
var(info$V2.pre)
var(info$V1.post)
var(info$V2.post)


