rm(list = ls())
library(caret)
library(e1071) 
library(caTools)
library(ggplot2)

data <- read.csv("~/pacotes_r/03/social.csv")

set.seed(123)

data$Gender <- as.numeric(factor(data$Gender, levels = c("Male", "Female"), labels = c(0, 1)))

data[, c("Age", "EstimatedSalary")] <- scale(data[, c("Age", "EstimatedSalary")])

split <- sample.split(data$Purchased, SplitRatio = 0.75)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


classifier <- svm(Purchased ~ Age + EstimatedSalary + Gender, 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'radial', 
                  gamma = 0.1)

y_pred <- predict(classifier, newdata = test_set)
table(test_set$Purchased, y_pred)

accuracy <- sum(diag(table(test_set$Purchased, y_pred))) / sum(table(test_set$Purchased, y_pred))
cat("Accuracy: ", accuracy)

confusionMatrix(table(test_set$Purchased, y_pred))

#===============================================================================
X1 = seq(min(training_set$Age) - 1, max(training_set$Age) + 1, by = 0.01)
X2 = seq(min(training_set$EstimatedSalary) - 1, max(training_set$EstimatedSalary) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
grid_set$Gender = median(training_set$Gender)  # Default Gender value for grid

y_grid = predict(classifier, newdata = grid_set)

ggplot() +
  geom_tile(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = training_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine')) +
  scale_color_manual(values = c('green4', 'red3')) +
  labs(title = 'SVM Decision Boundary (Training set)', x = 'Age', y = 'Estimated Salary') +
  theme_minimal() +
  theme(legend.position = "none")