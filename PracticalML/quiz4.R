#################
#### Q1  ########
#################
library(ElemStatLearn)
library(caret)
library(gbm)
library(AppliedPredictiveModeling)

data(vowel.train)
data(vowel.test) 
set.seed(33833)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

trainMod <- train(y ~ . , data = vowel.train, method = "rf")
trainMod2 <- train(y ~ . , data = vowel.train, method = "gbm")

ppModel1.1 <- predict(trainMod, vowel.test)
ppModel1.2 <- predict(trainMod2, vowel.test)

confusionMatrix(vowel.test$y, ppModel1.1)
confusionMatrix(vowel.test$y, ppModel1.2)

idx_agreed <- (ppModel1.1 == ppModel1.2)

confusionMatrix(vowel.test$y[idx_agreed], ppModel1.1[idx_agreed])$overall['Accuracy']
confusionMatrix(vowel.test$y[idx_agreed], ppModel1.2[idx_agreed])$overall['Accuracy']

#################
#### Q2  ########
#################
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

t1 <- train(diagnosis ~ ., data=training, method = "rf")
t2 <- train(diagnosis ~ ., data=training, method = "gbm")
t3 <- train(diagnosis ~ ., data=training, method = "lda")

pred1 <- predict(t1,testing)
pred2 <- predict(t2,testing)
pred3 <- predict(t3,testing)

predDF <- data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)
combPred <- predict(combModFit, testing)

confusionMatrix(testing$diagnosis, combPred)
confusionMatrix(testing$diagnosis, pred1)
confusionMatrix(testing$diagnosis, pred2)
confusionMatrix(testing$diagnosis, pred3)


#################
#### Q3  ########
#################
set.seed(3523)
library(AppliedPredictiveModeling)
library(glmnet)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

lm1 <- train(CompressiveStrength ~ ., data = training, method = "lasso")
lassoPred <- predict(lm1,testing)
plot.enet(lm1$finalModel, xvar="penalty", use.color=T)


#################
#### Q4  ########
#################
library(lubridate)
library(forecast)
dat <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

bt <- bats(tstrain)
ft <- forecast(bt, level = 0.95, h = nrow(testing))
plot(ft)
accuracy(ft, testing$visitsTumblr)

count <- 0
for (i in 1:nrow(testing)) {
  if(testing$visitsTumblr[i] > ft$lower[i] & testing$visitsTumblr[i] < ft$upper[i]) {
    count <- count + 1
  }
}

1-count/nrow(testing)

#################
#### Q5  ########
#################
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)

sv <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(sv, testing)

sqrt( mean( (pred-testing$CompressiveStrength)^2 , na.rm = TRUE ) )
