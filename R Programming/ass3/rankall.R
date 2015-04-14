# source("submitscript3.R")
# https://class.coursera.org/rprog-013/assignment
# Cheated: Idea borrowed from https://github.com/danielfrg/coursera-comp-for-data-analysis/blob/master/Project%202/r-code/rankall.R 
# outcome <- "heart failure"
# # num <- "best"
# num <- 4
# i = 4

# tail(rankall("heart failure"), 10)
# tail(rankall("pneumonia", "worst"), 3)
# head(rankall("heart attack", 20), 10)
# rankall("heart attack", "best")
# rankall("heart attack", 20)
# rankall("pneumonia", "worst")

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
  
  
  if(outcome == "heart attack") {
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    df.ha <- plyr::arrange(df.ha, df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df.ha$Hospital.Name)
    names(df.ha)[3] = "Deaths"
  } else if(outcome == "heart failure") {
    df.ha <- df[, c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    df.ha <- plyr::arrange(df.ha, df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df.ha$Hospital.Name)
    names(df.ha)[3] = "Deaths"
  } else { # if(outcome == "pneumonia")
    df.ha <- df[, c("State", "Hospital.Name", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    df.ha <- plyr::arrange(df.ha,df.ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df.ha$Hospital.Name)
    names(df.ha)[3] = "Deaths"
  }
  
  xasdasd <- split(df.ha, df.ha$State) 
  # xasdasd <- dlply(df.ha, .(State), function(x)return(x)) ## OR with plyr
  
  # Thanks
  ans = lapply(xasdasd, function(df.ha, num) {
    if(class(num) == "character") {
      
      if(num == "best") {
        return (df.ha$Hospital.Name[1])
      } else if(num == "worst") {
        return (df.ha$Hospital.Name[nrow(df.ha)])
      }
      
    }
    else {
      return (df.ha$Hospital.Name[num])
    }
  }, num)
  
  return (data.frame(hospital=unlist(ans), state=names(ans)))
  
}