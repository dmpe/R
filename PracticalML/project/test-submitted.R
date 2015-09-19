# answers = rep("A", 20)

source("~/Documents/R/PracticalML/project/project.R")

ppModel1.22


pml_write_files = function(x){
  setwd("~/Documents/R/PracticalML/project/submitted")
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
  setwd("~/Documents/R")
}

pml_write_files(ppModel1.22)