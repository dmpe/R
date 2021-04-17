'Objects in R'

'R allows to save the data by storing it inside an R object.'

'What’s an object?
It is just a name that you can use to call up stored data. For example, 
you can save data into an object like a or b.'

a <- 5
a
# exit : [1] 5

    'How to create an Object in R?'

'To create an R object, choose a name and then use the less-than symbol, <, 
followed by a minus sign, -, to save data into it. This combination looks 
like an arrow, <-. R will make an object, give it your name, and store in 
it whatever follows the arrow.'

'When you ask R what’s in a, it tells you on the next line. For example:'

die <- 1:6
die
# exit : [1] 1 2 3 4 5 6

'You can name an object in R almost anything you want, but there are a few rules. 
First, a name cannot start with a number. Second, a name cannot use some special 
symbols, like ^, !, $, @, +, -, /, or *:'

'R also understands capitalization (or is case-sensitive), so name and Name will 
refer to different objects. You can see which object names you have already 
used with the function ls().'
