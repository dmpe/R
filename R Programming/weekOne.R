# Q3
# search atomic


# Q4
x<-4
class(x)

# Q5
y <- c(4, TRUE)
y
class(y)

# Q6
a <- c(3, 2, 10)
b <- c(1,3, 5) 
cbind(a,b)

#Q7
dim(a)

# Q8
c <- list(2, "a", "b", TRUE)
c[[1]]

# Q9
d <- 1:4
e <- 2:3
class(d+e)


# Q 10
f <- c(17, 14, 4, 5, 13, 12, 10) 
f[f > 10] <- 4

# Q11, 12, 13, 14
names(hw1_data)
nrow(hw1_data)
hw1_data[1:2,]
tail(hw1_data)

## Q 15-20

# What is the value of Ozone in the 47th row?
hw1_data[47,]

# How many missing values are in the Ozone column of this data frame
sum(is.na(hw1_data$Ozone))
# What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(na.omit(hw1_data$Ozone))

# Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. 
# What is the mean of Solar.R in this subset?
hw1_data.sub1 <- subset(hw1_data, Ozone > 31 & Temp > 90)
mean(hw1_data.sub1$Solar.R)

# What is the mean of "Temp" when "Month" is equal to 6? 
mean(hw1_data.sub2$Temp)

# What was the maximum ozone value in the month of May (i.e. Month = 5)?
hw1_data.sub3 <- subset(na.omit(hw1_data), Month == 5)
max(hw1_data.sub3$Ozone)


























