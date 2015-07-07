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
# Plot 1 
########################

png(filename="plot1.png")

hist(consumption.sub$Global_active_power, freq = T, xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power", col = "#ff2500")

# dev.off()
