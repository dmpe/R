library(readr)
library(lubridate)

# read dataset
consumption <- read_csv2("ExploratoryDA/household_power_consumption.txt", na = "?")

consumption$Date <- dmy(consumption$Date)

# Subset data between 2007-02-01 and 2007-02-02
consumption.sub <- consumption[consumption$Date >= ymd(20070201) & consumption$Date <= ymd(20070202), ]

# paste date and time together
consumption.sub$together <- strptime(paste(consumption.sub$Date, consumption.sub$Time), "%Y-%m-%d %H:%M:%S")


########################
# Plot 2 
# Inspired by http://www.harding.edu/fmccown/r/
########################

png(filename="plot2.png")

plot(x = consumption.sub$together, y = consumption.sub$Global_active_power,
     ylab = "Global Active Power (kilowatts)",  type="l", xlab = "")
axis(1, at=1:3, lab=c("Thu","Fri", "Sat"))

# dev.off()
