##############
# Questions
##############

library(nlme)
library(lattice)
library(ggplot2)
library(datasets)

xyplot(weight ~ Time | Diet, BodyWeight)


data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
p

qplot(votes, rating, data = movies)
qplot(votes, rating, data = movies) + stats_smooth("loess")
qplot(votes, rating, data = movies) + geom_smooth()


g <- ggplot(movies, aes(votes, rating))
print(g)

qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
