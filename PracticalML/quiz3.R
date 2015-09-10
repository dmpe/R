#################
#### Q1  ########
#################

library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(rattle)
data(segmentationOriginal)
set.seed(125)

trainingSub <- subset(segmentationOriginal, segmentationOriginal$Case == "Train")
testingSub <- subset(segmentationOriginal, segmentationOriginal$Case == "Test")

trCl <- train(Class ~ ., method="rpart", data=trainingSub)

print(trCl$finalModel)

plot(trCl$finalModel, uniform=TRUE, main="Classification Tree")
text(trCl$finalModel,  use.n=TRUE, all=TRUE, cex=0.8)

fancyRpartPlot(trCl$finalModel)
predictedVal <- predict(trCl,newdata=testingSub)

fancyRpartPlot(predictedVal)

#################
#### Q3  ########
#################

library(pgmm)
data(olive)
olive = olive[,-1]


trCl <- train(Area ~ ., method="rpart", data=trainingSub)

print(trCl$finalModel)

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






