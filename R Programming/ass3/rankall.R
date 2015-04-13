
# https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2FProgAssignment3.pdf
# source("submitscript3.R")
# https://class.coursera.org/rprog-013/assignment

# outcome <- "heart failure"
# # num <- "best"
# num <- 4

# tail(rankall("heart failure"), 10)
# tail(rankall("pneumonia", "worst"), 3)
# head(rankall("heart attack", 20), 10)
# rankall("heart attack", "best")

rankall <- function(outcome, num = "best") {
  library(plyr)
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  df <- df[, !(colnames(df) %in% c("Provider.Number", "Address.1", "Address.2", "Address.3", 
                                   "City", "ZIP.Code", "County.Name", "Phone.Number"))]
  
  df <- df[, c("State", "Hospital.Name", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  df[df == "Not Available"] <- NA
  # df <- na.omit(df)
  
  for (i in 3:5) {
    df[,i] <- as.numeric(df[,i])
  }
  
  if( !outcome %in% c("pneumonia", "heart failure", "heart attack") ) {
    stop("invalid outcome")
  }
  
  if( !state %in% unique(df$State) ) {
    stop("invalid state")
  }
  
  
  if(outcome == "heart attack") {
    
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    df.ha <- plyr::arrange(df.ha, df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df.ha$Hospital.Name)
    
    #     df.ret <- data.frame(hospital = , state =)
    
    for(i in length(unique(df$State))) {
      
    }
    
    if (num == "worst") {
      df.ha <- df.ha[which.max(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ][ ,2]
    } else if (num == "best") {
      df.ha <- df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ][ ,2]
    } else {
      df.ha <- df.ha[num, 2]
    }
    
    return(df.ha)
    
  } else if(outcome == "heart failure") {
    
    df.ha <- df[, c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    df.ha <- plyr::arrange(df.ha, df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df.ha$Hospital.Name)
    
    if(num == "worst"){
      df.ha <- df.ha[which.max(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ][ ,2]
    } else if (num == "best") {
      df.ha <- df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ][ ,2]
    } else {
      df.ha <- df.ha[num, 2]
    }
    
    return(df.ha)
    
  } else { # if(outcome == "pneumonia")
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    df.ha <- plyr::arrange(df.ha,df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df.ha$Hospital.Name)
    
    if(num == "worst") {
      df.ha <- df.ha[which.max(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ][ ,2]
    } else if (num == "best") {
      df.ha <- df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ][ ,2]
    } else {
      df.ha <- df.ha[num, 2]
    }
    return(df.ha)
    
  }
  
}