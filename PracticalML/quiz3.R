
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)






library(pgmm)
data(olive)
olive = olive[,-1]



library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}






library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 






