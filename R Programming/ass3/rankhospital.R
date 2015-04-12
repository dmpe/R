# source("submitscript3.R")
# KTK6X8BuqQ
# https://class.coursera.org/rprog-013/assignment

# state <- "TX"
# outcome <- "heart failure"

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital <- function(state, outcome, num = "best") {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  df <- df[, !(colnames(df) %in% c("Provider.Number", "Address.1", "Address.2", "Address.3", 
                                   "City", "ZIP.Code", "County.Name", "Phone.Number"))]
  
  df <- df[, c("State", "Hospital.Name", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  df[df == "Not Available"] <- NA
  df <- na.omit(df)
  # sapply(df, class)
  
  for (i in 3:5) {
    df[,i] <- as.numeric(df[,i])
  }  
  
#'   The  num  argument can take values \best", \worst",  or an integer indicating the ranking
#'   (smaller numbers are better).  If the number given by
#'   num is larger than the number of hospitals in that
#'   state, then the function should return
#'   NA. Hospitals that do not have data on a particular outcome should
#'   be excluded from the set of hospitals when deciding the rankings.
  
  if(outcome == "heart attack") {
    
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    df.ha <- df.ha[df.ha$State == state, ]
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ][ ,2])
    
  } else if(outcome == "heart failure") {
    
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    df.ha <- df.ha[df.ha$State == state, ]
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ][ ,2])
    
  } else { # if(outcome == "pneumonia")
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    df.ha <- df.ha[df.ha$State == state, ]
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ][ ,2])
    
  }
}




















