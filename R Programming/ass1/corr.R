
# source("rprog_scripts_submitscript1.R")


source("complete.R")
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)


corr <- function(directory, threshold = 0) {
  library(plyr)
  sWD2 <- paste(getwd(), directory, sep = "/")
  setwd(sWD2)
  getwd()
  filenames <- list.files(path = getwd())
  sum(complete.cases(read.csv("001.csv")))
  
  dataFunc <- NULL
  nobs <- NULL
  
  
  
  
  setwd("~/R-github/R Programming/ass1")
  
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}











