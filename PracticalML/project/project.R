library(caret)
set.seed(5152)

pml.testing <- read.csv("PracticalML/project/pml-testing.csv")
# sapply(pml.testing, class)
pml.training <- read.csv("PracticalML/project/pml-training.csv")
# sapply(pml.training, class)


inTrain <- createDataPartition(y = pml.training$user_name,
                               p = 0.65,
                               list = FALSE)
training <- pml.training[inTrain,]
testing <- pml.training[-inTrain,]





