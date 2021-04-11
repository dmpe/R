'Data Types in R'

    'Vector'

'It is a sequence of data elements of the same basic type. For example:'

o <- c(1,2,5.3,6,-2,4)                             	 # Numeric vector
p <- c("one","two","three","four","five","six")    	 # Character vector
q <- c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE)              # Logical vector
o;p;q                                       # Compiling and executing the three vectors.

# exit-num.1 : [1]  1.0  2.0  5.3  6.0 -2.0  4.0
# exit-num.2 : [1] "one"   "two"   "three" "four"  "five"  "six"
# exit-num.3 : [1]  TRUE  TRUE FALSE  TRUE FALSE

    'Matrix'

'It is a two-dimensional rectangular data set. The components in a matrix
also must be of the same basic type like vector. For example:'

m <- matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
m # we will have a matrix with 2 rows and 3 columns 
    # with elements divided equally

# exit:
#        [,1][,2][,3]
#   [1,] "a" "a" "b" 
#   [2,] "c" "b" "a"

    'Data Frame'

'It is more general than a matrix, in that different columns can have 
different basic data types. For example:'

d <- c(1,2,3,4)                             # Numeric vector
e <- c("red", "white", "red", NA)           # String vector
f <- c(TRUE,TRUE,TRUE,FALSE)                # Boolean vector
mydata <- data.frame(d,e,f)                 # Data frame created with 3 vectors
names(mydata) <- c("ID","Color","Passed")   # We added a row in 'mydata'
mydata                                      # The result :

 'Lists'

'It is an R-object which can contain many different types of elements 
inside it like vectors, functions and even another list inside it. For example:'

list1 <- list(c(2,5,3),21.3,sin)
list1
# exit-num1 : [[1]]
# exit-num2 : [1] 2 5 3

# exit-num3 : [[2]]
# exit-num4 : [1] 21.3

# exit-num5 : [[3]]
# exit-num6 : function (x)  .Primitive("sin")