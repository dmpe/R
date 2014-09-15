library(XML)
source("xmlFaster.R")
nps <- xmlParse("NPS_Results.xml")

system.time(data3 <- xmlToDF(nps,xpath = "/TABLE/NPS_RESULTS" ))

data3$Created <- strptime(data3$Created, "%Y-%m-%dT%H:%M:%S")
View(data3)