# Q1
cube <- function(x, n) {
  x^3
}
cube(3)

# Q2
y <- 1:10
length(y)
if(y > 5) {
  y <- 0
}

# Q3
f <- function(x) {
  g <- function(a) {
    a + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

# Q4
b <- 5
c <- if(b < 3) {
  NA
} else {
  10
}

# Q5
h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

h(3, 4)







