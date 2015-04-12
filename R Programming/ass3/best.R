# head(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])
# http://stackoverflow.com/questions/10445851/converting-a-character-to-a-numeric-value-in-r
# http://stackoverflow.com/questions/9742512/selecting-the-min-of-a-group-with-ddply
# http://mazamascience.com/WorkingWithData/?p=929
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "pneumonia")
# best("MD", "heart attack")
# best("BB", "heart attack")
# best("NY", "hert attack")


# source("submitscript3.R")
# state <- "BB"
# outcome <- "hert attack"

best <- function(state, outcome) {
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
  
  
  
  if( !outcome %in% c("pneumonia", "heart failure", "heart attack") ) {
    stop("invalid outcome")
  }
  
  if( !state %in% unique(df$State) ) {
    stop("invalid state")
  }
  
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

