# head(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])
# http://stackoverflow.com/questions/10445851/converting-a-character-to-a-numeric-value-in-r
# http://stackoverflow.com/questions/9742512/selecting-the-min-of-a-group-with-ddply
# http://mazamascience.com/WorkingWithData/?p=929
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "pneumonia")
best("MD", "heart attack")
best("BB", "heart attack")
best("NY", "hert attack")

# state <- "TX"
# outcome <- "heart attack"

best <- function(state, outcome) {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  df <- df[, !(colnames(df) %in% c("Provider.Number", "Address.1", "Address.2", "Address.3", 
                                   "City", "ZIP.Code", "County.Name", "Phone.Number"))]
  
  df <- df[, c("State", "Hospital.Name", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  df[df == "Not Available"] <- NA
  sapply(df, class)
  
  for (i in 3:5) {
    df[,i] <- as.numeric(df[,i])
  }  
  
  df <- na.omit(df)
  
  if(outcome == "heart attack") {
    
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    df.ha <- df.ha[df.ha$State == state, ]
    
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),][,2])
    
  } else if(outcome == "heart failure") {
    
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    df.ha <- df.ha[df.ha$State == state, ]
    
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),][,2])
    
  } else if(outcome == "pneumonia") { 
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    df.ha <- df.ha[df.ha$State == state, ]
    
    return(df.ha[which.min(df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),][,2])
  } else {
    stop(outcome)
#     tryCatch(stop(outcome), finally = print("invalid outcome"))
#     tryCatch(stop(state), finally = print("invalid state"))
#     
#     msg <- geterrmessage()
#     print(msg)
  }
}









