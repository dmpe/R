library(XML)
source("xmlFaster.R")
shipments <- xmlParse("Shipments.xml")

# convert data to table format
# xmlToDataFrame is slow with large datasets
system.time(data <- xmlToDataFrame(shipments))
# custom function is not much better with my datasets
# system.time(data2 <- xmlToDF(shipments,xpath = "/TABLE/BOXTURE_SHIPMENTS" ))
# Further used only data, not data2

# head(data)

data$Price <- format(data$Price, digits = 2)
data$Price <- as.numeric(data$Price)
data$Price <- format(data$Price, digits = 2)

data$MarginEUR <- format(data$MarginEUR, digits = 2, nsmall = 2)
data$MarginEUR <- as.numeric(data$MarginEUR)
data$MarginEUR <- format(data$MarginEUR, digits = 2, nsmall = 2)

# need to convert time into more readeble format

data$OriginScheduled <- strptime(data$OriginScheduled, "%Y-%m-%dT%H:%M:%S")
data$OriginActual <- strptime(data$OriginActual, "%Y-%m-%dT%H:%M:%S")
data$DestinationScheduled <- strptime(data$DestinationScheduled, "%Y-%m-%dT%H:%M:%S")
data$DestinationActual <- strptime(data$DestinationActual, "%Y-%m-%dT%H:%M:%S")
data$PurchasedAt <- strptime(data$PurchasedAt, "%Y-%m-%dT%H:%M:%S")

View(data)
