
# http://r.789695.n4.nabble.com/Import-Multiple-csv-files-and-merge-into-one-Master-file-td2967823.html
# http://novicemetrics.blogspot.cz/2011/04/merging-multiple-data-files-into-one.html
# https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fpollutantmean-demo.html
# testing , pollutant, id = 1:332
# setwd("~/R-github")
# directory <- "specdata"
# pollutant <- "nitrate"
# id <- 70:72

# source("rprog_scripts_submitscript1.R")

# pollutantmean("specdata", "sulfate", 1:10)
# pollutantmean("specdata", "nitrate", 70:72)
# pollutantmean("specdata", "nitrate", 23)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # sWD <- paste(getwd(), "R Programming", sep = "/")
  # paste(getwd(), "R Programming", sep = "/")
  library(plyr)
  sWD2 <- paste(getwd(), directory, sep = "/")
  setwd(sWD2)
  getwd()
  filenames <- list.files(path = getwd())
  
  dataFunc <- ldply(filenames, read.csv)
  dataFunc <- subset(dataFunc, ID %in% id)
  
  setwd("~/R-github/R Programming/ass1")
  
  # complete.cases, anyway a bit cheating here with na.rm.
  if(pollutant == "sulfate") {
    return(mean(dataFunc$sulfate, na.rm = TRUE))
  } else {
    return(mean(dataFunc$nitrate, na.rm = TRUE))
  }
}

