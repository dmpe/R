library(e1071)
library(rpart)
library(randomForest)
library(caret)
library(foreach)
# names(getModelInfo())
rfParam <- expand.grid(mtry =100, importance=TRUE)

set.seed(5152)

pml.testing <- read.csv("PracticalML/project/pml-testing.csv",  na.strings = c("NA", ""))
pml.training <- read.csv("PracticalML/project/pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))

# delete factors + logicals
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

pml.training <- pml.training[, colSums(is.na(pml.training)) == 0]
pml.testing <- pml.testing[, colSums(is.na(pml.testing)) == 0]

# nzv <- nearZeroVar(pml.training)
# nzv2 <- nearZeroVar(pml.testing)
# pml.training <- pml.training[, -nzv]
# pml.testing <- pml.testing[, -nzv2]

# sapply(pml.training, class)
# sapply(pml.testing, class)
#
# inBuild <- createDataPartition(y=pml.training$classe,p=0.6, list=FALSE)
# validation <- Wage[-inBuild,];
# buildData <- Wage[inBuild,]
#
# inTrain <- createDataPartition(y=buildData$wage, p=0.6, list=FALSE)
# training <- buildData[inTrain,];
# testing <- buildData[-inTrain,]

# Here variable number must be the same

inTrain <- createDataPartition(y = pml.training$classe, p = 0.6, list = FALSE)
trainingSUB <- pml.training[inTrain,]
testingSUB <- pml.training[-inTrain,]

### all
# preProALL <- preProcess(training[,!names(training) %in% "classe"], method = "pca", thresh = 0.85, na.remove = TRUE)
# trainPCALL <- predict(preProALL, training)
# modelfitALL <- train(training$classe ~ ., data = trainPCALL, method = "knn", prePro="pca")
#
# testPC <- predict(prePro, testing[, grep("IL|diagnosis", names(testing))][2:13])
# confusionMatrix(testing$diagnosis,predict(modelfit,testPC))
#

### all non nulls
# nzv <- nearZeroVar(trainingSUB, saveMetrics= TRUE)


modelfitKNN <- train(classe ~ ., data = trainingSUB, method = "knn")
modelfitRF <- train(classe ~ ., data = trainingSUB, method = "parRF", tuneGrid=rfParam)
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
confusionMatrix(testingSUB$classe, ppModel1.4) # this
# confusionMatrix(testingSUB$classe, ppModel1.5)

predDF <- data.frame(ppModel1.2,ppModel1.4,classe=testingSUB$classe)
combModFit <- train(classe ~ ., method="rf", data=predDF)
combPred <- predict(combModFit, testingSUB)
confusionMatrix(testingSUB$classe, combPred)


