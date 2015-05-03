
library(XML)
shipments <- xmlParse("Shipments.xml")

# convert data to table format
# xmlToDataFrame is slow with large datasets
system.time(data <- xmlToDataFrame(nodes = getNodeSet(shipments, "//TABLE/BOXTURE_SHIPMENTS")))

# custom function is not much better with my datasets
# system.time(data2 <- xmlToDF(shipments,xpath = "/TABLE/BOXTURE_SHIPMENTS" ))
# Further used only data, not data2

# head(data)

data$Price <- as.numeric(format(data$Price, digits = 2))
data$Price <- format(data$Price, digits = 2)

data$MarginEUR <- as.numeric(format(data$MarginEUR, digits = 2, nsmall = 2))
data$MarginEUR <- format(data$MarginEUR, digits = 2, nsmall = 2)

# need to convert time into more readeble format

data$OriginScheduled <- strptime(data$OriginScheduled, "%Y-%m-%dT%H:%M")
data$OriginActual <- strptime(data$OriginActual, "%Y-%m-%dT%H:%M")
data$DestinationScheduled <- strptime(data$DestinationScheduled, "%Y-%m-%dT%H:%M")
data$DestinationActual <- strptime(data$DestinationActual, "%Y-%m-%dT%H:%M")
data$PurchasedAt <- strptime(data$PurchasedAt, "%Y-%m-%dT%H:%M")


#data$State[data$State == ""] <- NA
#data$UserFirstName[data$UserFirstName == ""] <- NA
#data$UserLastName[data$UserLastNam==""] <-NA

newd = data
newd$PurchasedAt <- NULL
newd[18:19] = list(NULL)
newd[25:26] = list(NULL)
names(newd)
names(data)

newd[newd==""] <-NA

for (i in 1:ncol(data)){
  #data[data==""] <- NA
  if(colnames(data)[i] == "OriginScheduled") &
  if(colnames(data)[i] == "DestinationScheduled") &
  if(colnames(data)[i] == "OriginActual") &
  if(colnames(data)[i] == "DestinationActual")&
  if(colnames(data)[i] == "PurchasedAt") {
    print("Test")
  }
    
  
  #print(colnames(data)[i])
}


View(data)





