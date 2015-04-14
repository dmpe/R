
# Q1
set.seed(1)
rpois(5, 2)


# Q5

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
y

# Q8
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)












