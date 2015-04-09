


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
  sapply(df, class)
  
  for (i in 3:5) {
    df[,i] <- as.numeric(df[,i])
  }  
  
  df <- na.omit(df)
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}




















