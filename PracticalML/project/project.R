library(caret)
set.seed(5152)

# pml.testing <- read.csv("PracticalML/project/pml-testing.csv",  na.strings = "NA")
# sapply(pml.testing, class)
pml.training <- read.csv("PracticalML/project/pml-training.csv", na.strings = c("NA", "#DIV/0!"))
# sapply(pml.training, class)


inTrain <- createDataPartition(y = pml.training$classe,
                               p = 0.7,
                               list = FALSE)
training <- pml.training[inTrain,]
testing <- pml.training[-inTrain,]

te <- training[complete.cases(training),]
te2 <- training[complete.cases(training),]
te <- te[sapply(te,is.numeric)]

prePro <- preProcess(te, method = "pca", thresh = 0.9, na.remove = TRUE)
trainPC <- predict(prePro, te)
modelfit <- train(te2$classe ~ ., data = trainPC, method = "knn", prePro="pca")

testPC <- predict(prePro, testing[, grep("IL|diagnosis", names(testing))][2:13])
confusionMatrix(testing$diagnosis,predict(modelfit,testPC))



