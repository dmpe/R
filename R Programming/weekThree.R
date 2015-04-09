

library(datasets)
data(iris)
?iris
# Q1
irisSub <- subset(iris, iris$Species == 'virginica')
mean(irisSub$Sepal.Length)
# OR
tapply(iris$Sepal.Length, iris$Species == 'virginica', mean)

# Q2
apply(iris[, 1:4], 1, mean)
# apply(iris, 1, mean)
# apply(iris, 2, mean)
apply(iris[, 1:4], 2, mean)

# Q3
library(datasets)
data(mtcars)
?mtcars
tapply(mtcars$cyl, mtcars$mpg, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

sapply(split(mtcars$hp, mtcars$cyl), mean)
# 82.63636 122.28571 209.21429 





