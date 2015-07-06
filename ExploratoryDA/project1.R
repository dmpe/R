# The following descriptions of the 9 variables in the dataset are taken from the UCI web site:
#   
#   Date: Date in format dd/mm/yyyy
# Time: time in format hh:mm:ss
# Global_active_power: household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage: minute-averaged voltage (in volt)
# Global_intensity: household global minute-averaged current intensity (in ampere)
# Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
# Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.


# download dataset
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# download.file(url='https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip',
#               destfile='localfile.zip', method='curl')

library(readr)
library(lubridate)

consumption <- read_csv2("ExploratoryDA/household_power_consumption.txt", na = "?")

sapply(consumption, class)

consumption$Date <- dmy(consumption$Date)
# consumption$Time <- strptime(consumption$Time, "%H:%M:%S")
# strptime("17:24:00", "%H:%M:%S")

# Subset data between 2007-02-01 and 2007-02-02
consumption.sub <- consumption[consumption$Date >= ymd(20070201) & consumption$Date <= ymd(20070202), ]

 
# Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# Name each of the plot files as plot1.png, plot2.png, etc.
# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file.
# Add the PNG file and R code file to the top-level folder of your git repository (no need for separate sub-folders)


########################
# Plot 1 
########################

png(filename="plot1.png")

hist(consumption.sub$Global_active_power, freq = T, xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power", col = "#ff2500")

dev.off()

########################
# Plot 2 
########################

png(filename="plot2.png")



dev.off()


########################
# Plot 3
########################
png(filename="plot3.png")



dev.off()

########################
# Plot 4
########################
png(filename="plot4.png")

par(mfrow=c(2,2))

dev.off()









