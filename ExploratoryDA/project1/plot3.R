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
# Plot 3
########################
png(filename="plot3.png")

plot(x = consumption.sub$together, y = consumption.sub$Sub_metering_1, ylab = "Energy sub metering",  
     type="l", xlab = "", col = "#3F3F3F") # black

axis(1, at=1:3, lab=c("Thu","Fri", "Sat"))

legend("topright", col = c("#3F3F3F", "#ff2500", "#4D6FFF"), legend = c("Sub_metering_1", 
                                                                        "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=3)

lines(consumption.sub$together, consumption.sub$Sub_metering_2, col = "#ff2500") # red
lines(consumption.sub$together, consumption.sub$Sub_metering_3, col = "#4D6FFF")

# dev.off()
