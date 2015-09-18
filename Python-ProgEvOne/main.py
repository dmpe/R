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



hrs = raw_input("Enter Hours:")
h = float(hrs)

rph = raw_input("Enter rph:")
r = float(rph)


if h <= 40:
    pay = h * r
    print pay    
elif h > 40:
    hos = h - 40
    newR = (r * 1.5) * hos
    print hos, newR
    pay = (h-hos) * r + newR
    print pay,     
    
