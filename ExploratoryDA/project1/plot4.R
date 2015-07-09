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
# Plot 4
########################
png(filename="plot4.png")

par(mfrow=c(2,2))

plot(x = consumption.sub$together, y = consumption.sub$Global_active_power,
     ylab = "Global Active Power",  type="l", xlab = "")
axis(1, at=1:3, lab=c("Thu","Fri", "Sat"))

plot(x = consumption.sub$together, y = consumption.sub$Voltage,
     ylab = "Voltage",  type="l", xlab = "datetime", axes=T)
# axis(1, at = seq(1,3, by =1),  labels = c("Thu", "Fri", "Sat"))
axis(2, at = seq(234, 246, by=4), labels = seq(234, 246, by=4))

plot(x = consumption.sub$together, y = consumption.sub$Sub_metering_1,
     ylab = "Energy sub metering",  type="l", xlab = "", col = "#3F3F3F", lwd=2) # black
axis(1, at=1:3, lab=c("Thu","Fri", "Sat"))
legend("topright", col = c("#3F3F3F", "#ff2500", "#4D6FFF"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=3, bty = "n")
lines(consumption.sub$together, consumption.sub$Sub_metering_2, col = "#ff2500", lwd=2) # red
lines(consumption.sub$together, consumption.sub$Sub_metering_3, col = "#4D6FFF", lwd=2)

plot(x = consumption.sub$together, y = consumption.sub$Global_reactive_power, 
     ylab = "Global_reactive_power", type="l", xlab = "datetime")

dev.off()
