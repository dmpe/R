__author__ = 'jm'

import matplotlib as mp

print "Testing my hello Wordl"

#x = 15
#x = x +52
#print x

#name = raw_input("Enter your name")
#print "Hello " + name

## This first line is provided for you

#hrs = raw_input("Enter Hours:")
#rph = raw_input("Enter rate:")

#fhrs = float(hrs)
#frph = float(rph)

#pay = fhrs * frph
#print pay


# 3.2
#hrs = raw_input("Enter Hours:")
#h = float(hrs)

#rph = raw_input("Enter rph:")
#r = float(rph)

#if h <= 40:
    #pay = h * r
    #print pay    
#elif h > 40:
    #hos = h - 40
    #newR = (r * 1.5) * hos
    #print hos, newR
    #pay = (h-hos) * r + newR
    #print pay,     
    
# 3.3
#number = raw_input("Enter number:")
#number1 = float(number)

#if number1 <= 0 and number1 >= 1.0:
    #print "error"
    #exit
#elif number1 >= 0.9 and number1 <= 1.0:
    #print "A"
#elif number1 >= 0.8 and number1 < 0.9:
    #print "B"
#elif number1 >= 0.7 and number1 < 0.8:
    #print "C"
#elif number1 >= 0.6 and number1 < 0.7:
    #print "D"
#else:
    #print "F"


#4.6
""" 
Write a program to prompt the user for hours and rate per hour 
using raw_input to compute gross pay. Award time-and-a-half for the 
hourly rate for all hours worked above 40 hours. 
Put the logic to do 
the computation of time-and-a-half in a function called computepay() 
and use the function to do the computation. The function should return 
a value. 
Use 45 hours and a rate of 10.50 per hour to test the program 
(the pay should be 498.75). You should use raw_input to read a string
and float() to convert the string to a number. Do not worry about error
checking the user input unless you want to - you can assume the user types numbers properly.
"""
#def computepay(h,r):
    #if h <= 40:
        #pay = h * r
        #print pay    
    #elif h > 40:    
        #hos = h - 40
        #newR = (r * 1.5) * hos
        ##print hos, newR
        #pay = (h-hos) * r + newR    
        #return pay

#hrs = float(raw_input("Enter Hours:"))
#rph =  float(raw_input("Enter rph:"))

#p = computepay(hrs,rph)
#print "Pay",p

# 5.2
"""
Write a program that repeatedly prompts a user for integer numbers until the user enters 'done'. 
Once 'done' is entered, print out the largest and smallest of the numbers. If the user
enters anything other than a valid number catch it with a try/except and put out an 
appropriate message and ignore the number. Enter the numbers from the book for problem 5.1 and 
Match the desired output as shown.
"""

mylist = []
largest = None
smallest = None
while True:
    input1 = raw_input("Enter a number: ")
    
    if input1 == "done" :
        largest = max(mylist[0:])
        smallest = min(mylist[0:])
        print "Maximum is", largest, "\n", "Minimum is", smallest
        break    
    else:
        try:
            input1 = int(input1)
            mylist.append(input1)
        except: 
            print "Invalid input"
            continue