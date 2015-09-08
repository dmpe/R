#################
#### Q1  ########
#################

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p = 0.5, list = FALSE)
training = adData[trainIndex, ]
testing = adData[trainIndex, ]

adData = data.frame(diagnosis, predictors)
testIndex = createDataPartition(diagnosis, p = 0.50, list = FALSE)
training = adData[-testIndex, ]
testing = adData[testIndex, ]

# this
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis, p = 0.5, list = FALSE)
training = adData[trainIndex, ]
testing = adData[-trainIndex, ]

adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p = 0.5, list = FALSE)
training = adData[trainIndex, ]
testing = adData[trainIndex, ]



#################
#### Q2  ########
#################
library(AppliedPredictiveModeling)
library(ggplot2)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3 / 4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

featurePlot(x = training[c("Cement", "Water", "Age")],
            y = training$Superplasticizer,
            plot = "pairs")

ggplot(testing, aes(x = Superplasticizer)) + geom_histogram() + scale_x_log10()

hist(log(testing$Superplasticizer))
hist(testing$Superplasticizer)


#################
#### Q3  ########
#################
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]


training <- training[, grep("IL", names(training))[1:12]]
preOBj <- preProcess(training, method = "pca", thresh = 0.9)

#################
#### Q4  ########
#################
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training1 = adData[inTrain, ]
testing = adData[-inTrain, ]

training <- training1[, grep("IL|diagnosis", names(training1))][1:13]

preOBj <- preProcess(training[2:13], method = "pca", thresh = 0.8)
trainPC <- predict(preOBj, training[2:13])
modelfit <- train(training$diagnosis~ ., data = trainPC, method = "glm", preProcess = "pca")

testPC <- predict(preOBj, testing[, grep("IL|diagnosis", names(testing))][2:13])

preOBj1 <- preProcess(training[2:13])
trainPC1 <- predict(preOBj1, training[2:13])
modelfit1 <- train(training$diagnosis~ ., data = trainPC1, method = "glm")



