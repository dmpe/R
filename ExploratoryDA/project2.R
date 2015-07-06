library(lubridate)

# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile="ile.zip", method='curl')

NEI <- readRDS("ExploratoryDA/summarySCC_PM25.rds")
SCC <- readRDS("ExploratoryDA/Source_Classification_Code.rds")





