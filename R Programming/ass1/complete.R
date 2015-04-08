# https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcomplete-demo.html
# source("rprog_scripts_submitscript1.R")

# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10, 12))
# complete("specdata", 30:25)
# complete("specdata", 3)
# 
# directory <- "specdata"
# id <- c(2, 4, 8, 10, 12)
library(plyr)

complete <- function(directory, id = 1:332) {
  sWD2 <- paste(getwd(), directory, sep = "/")
  setwd(sWD2)
  getwd()
  filenames <- list.files(path = getwd())
  # sum(complete.cases(read.csv("001.csv")))
  
  dataFunc <- NULL
  nobs <- NULL
  
  for(i in 1:length(filenames)){
    dataFunc[[i]] <- ldply(filenames[i], read.csv)  
    nobs[[i]] <- sum(complete.cases(dataFunc[[i]]))
  }
  setwd("~/R-github/R Programming/ass1")
  
  df <- data.frame(id = 1:length(filenames), nobs = nobs)
  df[id, ]
}