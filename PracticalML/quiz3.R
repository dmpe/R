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
library(tree)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

model <- train(Area ~ ., data=olive, method="rpart")
predict(model, newdata)


trCl <- tree(olive$Area ~ ., data=olive)
predict(trCl, newdata)



#################
#### Q4  ########
#################


library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

trainModel <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method="glm", family="binomial", data = trainSA)
ppModel <- predict(trainModel, trainSA)

print(trainModel)
print(trainModel$finalModel)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, ppModel)
missClass(testSA$chd, predict(trainModel, testSA))


#################
#### Q5  ########
#################
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

trainMod <- train(y ~ . , data = vowel.train, method = "rf")
varImp(trainMod)

