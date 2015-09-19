library(e1071)
library(rpart)
library(gbm)
library(randomForest)
library(caret)
library(foreach)

set.seed(5152)

# load both of them
pml.testing <- read.csv("PracticalML/project/pml-testing.csv",  na.strings = c("NA", ""))
pml.training <- read.csv("PracticalML/project/pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
# sapply(pml.training, class)
# sapply(pml.testing, class)

# delete factors + logicals + NZV
# https://topepo.github.io/caret/preprocess.html#nzv
pml.training <- pml.training[,!names(pml.training) %in% c("user_name", "new_window", "cvtd_timestamp",
                                                          "skewness_yaw_dumbbell", "kurtosis_yaw_dumbbell",
                                                          "skewness_yaw_belt", "kurtosis_yaw_belt",
                                                          "kurtosis_yaw_forearm", "skewness_yaw_forearm",
                                                          "amplitude_yaw_belt", "amplitude_yaw_dumbbell",
                                                          "amplitude_yaw_forearm", "X")]

pml.testing <- pml.testing[,!names(pml.testing) %in% c("user_name", "new_window", "cvtd_timestamp",
                                                       "skewness_yaw_dumbbell", "kurtosis_yaw_dumbbell",
                                                       "skewness_yaw_belt", "kurtosis_yaw_belt",
                                                       "kurtosis_yaw_forearm", "skewness_yaw_forearm",
                                                       "amplitude_yaw_belt", "amplitude_yaw_dumbbell",
                                                       "amplitude_yaw_forearm", "X")]

# pml.testing <- Filter(function(x)all(is.na(x)), pml.testing)
# pml.training <- Filter(function(x)!all(is.na(x)), pml.training)

# select only those columns which have 0 NA values in them (columnwise, not rowwise)
pml.training <- pml.training[, colSums(is.na(pml.training)) == 0]
pml.testing <- pml.testing[, colSums(is.na(pml.testing)) == 0]

# This will not result in to the same number of variables. Can be used in part.
# nzv <- nearZeroVar(pml.training, saveMetrics = T)
# nzv2 <- nearZeroVar(pml.testing, saveMetrics = T)
# pml.training <- pml.training[, -nearZeroVar(pml.training)]
# pml.testing <- pml.testing[, -nearZeroVar(pml.testing)]


# inBuild <- createDataPartition(y=pml.training$classe,p=0.6, list=FALSE)
# validation <- Wage[-inBuild,];
# buildData <- Wage[inBuild,]
# inTrain <- createDataPartition(y=pml.training$classe, p=0.6, list=FALSE)
# training <- buildData[inTrain,];
# testing <- buildData[-inTrain,]

# Here variable number must be the same

inTrain <- createDataPartition(y = pml.training$classe, p = 0.6, list = FALSE)
trainingSUB <- pml.training[inTrain,]
testingSUB <- pml.training[-inTrain,]


### all non nulls
# takes very long on 2 Core PC + 4GB ram

modelfitKNN <- train(classe ~ ., data = trainingSUB, method = "knn")
modelfitRF <- train(classe ~ ., data = trainingSUB, method = "rf")
modelfitRPART <- train(classe ~ ., data = trainingSUB, method = "rpart")
modelfitGBM <- train(classe ~ ., data = trainingSUB, method = "gbm")
# modelfitSVM <- svm(train$classe ~ ., data = trainingSUB)
# modelfitGLM <- train(train$classe ~ ., data = trainingSUB, method="glm", family="binomial")


ppModel1.1 <- predict(modelfitKNN, testingSUB)
ppModel1.2 <- predict(modelfitRF, testingSUB)
ppModel1.3 <- predict(modelfitRPART, testingSUB)
ppModel1.4 <- predict(modelfitGBM, testingSUB)
# ppModel1.5 <- predict(modelfitSVM, testingSUB)

confusionMatrix(ppModel1.1, testingSUB$classe)
confusionMatrix(testingSUB$classe, ppModel1.2) # this
confusionMatrix(testingSUB$classe, ppModel1.3)
confusionMatrix(testingSUB$classe, ppModel1.4) # or almost this
# confusionMatrix(testingSUB$classe, ppModel1.5)

# Unnecessary.
# predDF <- data.frame(ppModel1.2,ppModel1.4,classe=testingSUB$classe)
# combModFit <- train(classe ~ ., method="rf", data=predDF)
# combPred <- predict(combModFit, testingSUB)
# confusionMatrix(testingSUB$classe, combPred)

ppModel1.22 <- predict(modelfitRF, pml.testing)
