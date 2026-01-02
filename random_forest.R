rm(list=ls())
library(randomForest)

data(iris)
head(iris)

set.seed(42)

trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))

trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

rf_model <- randomForest(Species ~ ., data = trainData)
print(rf_model)

library(caret)

predictions <- predict(rf_model, testData)
confusionMatrix(predictions, testData$Species)


rf_tuned <- randomForest(Species ~ ., data = trainData, ntree = 500, mtry = 2)
print(rf_tuned)

importance(rf_model)
varImpPlot(rf_model)