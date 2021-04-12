'Functions in R'

'A function allows you to define a reusable block of code that can be 
executed many times within your program.'

'Functions can be named and called repeatedly or can be run anonymously 
in place (similar to lambda functions in python).'

'Developing full understanding of R functions requires understanding 
of environments. Environments are simply a way to manage objects. 
An example of environments in action is that you can use a redundant 
variable name within a function, that won’t be affected if the larger 
runtime already has the same variable. Additionally, if a function calls 
a variable not defined within the function it will check the higher level 
environment for that variable.'

  'Syntax'

'In R, a function definition has the following features:'

  '1. The keyword function
   2. a function name
   3. input parameters (optional)
   4. some block of code to execute
   5. a return statement (optional)'

# a function with no parameters or returned values
sayHello = function(){
  "Hello!"
}

sayHello()  # calls the function, 'Hello!' is printed to the console
# exit : [1] "Hello!"

# a function with a parameter
helloWithName = function(name){
  paste0("Hello, ", name, "!")
}

helloWithName("Carlos")  # calls the function, 'Hello, Carlos!' is printed to the console
# exit : [1] "Hello, Carlos!"

# a function with multiple parameters with a return statement
multiply = function(val1, val2){
  val1 * val2
}
  
multiply(3, 5)  # prints 15 to the console
# exit : [1] 15

'Functions are blocks of code that can be reused simply by calling the function. 
This enables simple, elegant code reuse without explicitly re-writing sections 
of code. This makes code both more readable, makes for easier debugging, and 
limits typing errors.'

'Functions in R are created using the function keyword, along with a function 
name and function parameters inside parentheses.'

'The return() function can be used by the function to return a value, and is 
typically used to force early termination of a function with a returned value. 
Alternatively, the function will return the final printed value.'

# return a value explicitly or simply by printing
sum <- function(a, b){
  c = a + b
  return(c)
}

sum <- function(a, b){
  a + b
}

result = sum(1, 2)
result
# exit : [1] 3

'You can also define default values for the parameters, which R will use 
when a variable is not specified during function call.'

sum = function(a, b = 3){
  a + b
}

result = sum(a = 1)
result
# exit : [1] 4

'You can also pass the parameters in the order you want, using the name of the parameter.'

result = sum(b=2, a=2)
result
# exit : [1] 4

'R can also accept additional, optional parameters with ’…’ '

sum = function(a, b, ...){
  a + b + ...
}

result = sum(1, 2, 3) 
result
# exit : [1] 6

'Functions can also be run anonymously. These are very useful in combination 
with the ‘apply’ family of functions.'

# loop through 1, 2, 3 - add 1 to each
sapply(1:3,
  function(i){
    i + 1}
    )
# exit : [1] 2 3 4