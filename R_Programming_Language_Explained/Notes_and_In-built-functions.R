'Notes'

'If a function definition includes arguments without default values specified, 
values for those values must be included.'

sum = function(a, b = 3){
a + b
}

result = sum(b = 2)
# Error in sum(b = 2) : argument "a" is missing, with no default
result 
# Error: object 'result' not found

'Variables defined within a function only exist within the scope of that function, 
but will check larger environment if variable not specified'

double = function(a){
a * 2
}

result = double(x)  
result
# Error in double(x) : object 'x' not found


double = function(){
    a * 2
}

a = 3
double()
# exit : [1] 6

'In-built functions in R'

'R comes with many functions that you can use to do sophisticated tasks like 
random sampling.
For example, you can round a number with the round(), or calculate its factorial
with the factorial().'

round(4.147)
# exit : [1] 4
factorial(3)
# exit : [1] 6
round(mean(1:6))
# exit : [1] 4

'The data that you pass into the function is called the function’s argument.
You can simulate a roll of the die with R’s sample() function. The sample() 
function takes two arguments:a vector named x and a number named size. For example:'

sample(x = c(1:4), size = 2)
# exit : [1] 4 2
sample(x = die, size = 1)
# exit : [1] 3
dice = sample(die, size = 2, replace = TRUE)
dice
# exit : [1] 2 4
sum(dice)
# exit : [1] 6

'If you’re not sure which names to use with a function, you can look up the 
function’s arguments with args.'

args(round)
# exit : function (x, digits = 0) 
#        NULL