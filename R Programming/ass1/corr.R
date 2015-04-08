
# source("rprog_scripts_submitscript1.R")

# cr <- corr("specdata", 150)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 400)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 5000)
# summary(cr)
# length(cr)
# cr <- corr("specdata")
# summary(cr)
# length(cr)

# directory <- "specdata"
# threshold <- 150
# cheated a bit because didnt understand what they want: https://github.com/bigtoast/comp-data/blob/master/assign-1/corr.R
# https://github.com/ahawker/data-analysis-coursera/blob/master/HW2/corr.R
corr <- function(directory, threshold = 0) {
  library(plyr)
  sWD2 <- paste(getwd(), directory, sep = "/")
  setwd(sWD2)
  getwd()
  filenames <- list.files(path = getwd())
  
  dataFunc <- NULL
  df.small <- NULL
  cr <- c() 
  # dataFunc.Com <- ldply(filenames, read.csv)
  for(i in 1:length(filenames)) {
    dataFunc[[i]] <- ldply(filenames[i], read.csv)  
    df.small[[i]] <- na.omit(dataFunc[[i]])
  
    if(nrow(df.small[[i]]) > threshold) {
      cr <- c(cr, cor(df.small[[i]]$sulfate, df.small[[i]]$nitrate, use = "complete.obs"))
    }
  }
  
  setwd("~/R-github/R Programming/ass1")
  return(cr)
}

