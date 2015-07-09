library(lubridate)
library(dplyr)

# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile="ile.zip", method='curl')

NEI <- readRDS("ExploratoryDA/summarySCC_PM25.rds")
NEI <- as.tbl(NEI)

SCC <- readRDS("ExploratoryDA/Source_Classification_Code.rds")
SCC <- as.tbl(SCC)

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

NEI.allUS <- NEI %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))


plot(y = NEI.allUS$Emissions, x = NEI.allUS$year, xlab = "Year", type = "l",
        ylab = "Emissions in tons", main = "Emissions in the USA")


##### Q2 ###
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 
# 2008? Use the base plotting system to make a plot answering this question.
#####

NEI.bal <- NEI %>% 
            filter(fips == "24510") %>%
            group_by(year) %>% 
            summarise(Emissions = sum(Emissions))

barplot(NEI.bal$Emissions, xlab = "Year", ylim = c(0, 3500),
        ylab = "Emissions in tons", main = "Emissions in Baltimore City, Maryland", names.arg=c("1999","2002" ,"2005", "2008"))

##### Q3 ###
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of 
# these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen 
# increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
#####

##### Q4 ###
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#####

##### Q5 ###
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#####

NEI.bal <- NEI %>% 
  filter(fips == "24510") %>%
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions))
NEI.bal


barplot(NEI.bal$Emissions, xlab = "Year", ylim = c(0, 3500),
        ylab = "Emissions in tons", main = "Emissions in Baltimore City, Maryland", names.arg=c("1999","2002" ,"2005", "2008"))


##### Q6 ###
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
# sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
# over time in motor vehicle emissions?
#####
