
library(psych)

setwd("C:/Users/Dima/Documents/R/coursera/")
file <- read.table("Stats1.13.HW.02.txt", header=T)
names(file)
dim(file) # number of rows

mean(file$SR)
var(file$SR)


subfile1 <- subset(file, file$time=="pre")
mean(subfile1$SR)

subfile2 <- subset(file, file$time=="post")
sd(subfile2$SR)
median(subfile2$SR)

subfile1
subfile2

# Either you do the subsetting or just 
# describeBy(subfile2, subfile2$condition)

# I did the subsetting

# POST
test1 <- subset(subfile2, subfile2$condition=="WM")
mean(test1$SR)

test2 <- subset(subfile2, subfile2$condition=="PE")
mean(test2$SR)

test3 <- subset(subfile2, subfile2$condition=="DS")
mean(test3$SR)

# PRE

test4 <- subset(subfile1, subfile1$condition=="WM")
mean(test4$SR)

test5 <- subset(subfile1, subfile1$condition=="PE")
mean(test5$SR)

test6 <- subset(subfile1, subfile1$condition=="DS")
mean(test6$SR)

par(mfrow= c(2,3))
file1 = subset(subfile1, subfile1$condition=="WM") 
file2 = subset(subfile2, subfile2$condition=="WM") 
file3 = subset(subfile1, subfile1$condition=="PE") 
file4 = subset(subfile2, subfile2$condition=="PE") 
file5 = subset(subfile1, subfile1$condition=="DS") 
file6 = subset(subfile2, subfile2$condition=="DS") 
# http://stackoverflow.com/a/10759521/1607152
hist(file1$SR) 
hist(file2$SR) 
hist(file3$SR)
hist(file4$SR) 
hist(file5$SR) 
hist(file6$SR)

# Beware: if you compare density then the more "normal distributed" is Pre PM
# But this is wrong. The right one is Post WM
par(mfrow = c(2,3))
plot(density(test1[, 4]), xlab = "Total sympton score", main = "")
plot(density(test2[, 4]), xlab = "Total sympton score", main = "")
plot(density(test3[, 4]), xlab = "Total sympton score", main = "")
plot(density(test4[, 4]), xlab = "Total sympton score", main = "")
plot(density(test5[, 4]), xlab = "Total sympton score", main = "")
plot(density(test6[, 4]), xlab = "Total sympton score", main = "")

subfile1.wm = subset(subfile1, subfile1$condition=="WM")
subfile2.wm = subset(subfile2, subfile2$condition=="WM")
subfile1.pe = subset(subfile1, subfile1$condition=="PE")
subfile2.pe = subset(subfile2, subfile2$condition=="PE")
subfile1.ds = subset(subfile1, subfile1$condition=="DS") 
subfile2.ds = subset(subfile2, subfile2$condition=="DS") 
mean(subfile2.wm$SR)-mean(subfile1.wm$SR) 
mean(subfile2.pe$SR)-mean(subfile1.pe$SR)
mean(subfile2.ds$SR)-mean(subfile1.ds$SR)

