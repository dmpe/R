library(lubridate)

# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile="ile.zip", method='curl')

NEI <- readRDS("ExploratoryDA/summarySCC_PM25.rds")
SCC <- readRDS("ExploratoryDA/Source_Classification_Code.rds")


# The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine 
# particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

# Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings in the Emissions 
# table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and 
# you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as 
# “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.


##### Q1 ###
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.
##### 

plot(x = NEI$year, y = NEI$Emissions, type = "l")




##### Q2 ###
#
#
#
#####


