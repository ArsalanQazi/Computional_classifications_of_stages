# Data 

library(dplyr)
library(caret)


Main <- read.csv("~/boruta_phase_3_main63.csv")
str(Main)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(Main), replace = T, prob = c(0.7, 0.3))
train <- Main[ind == 1,]
test <- Main[ind == 2,]

# Support Vector Machine
 
library(e1071)

SVM <- svm(Class~., data = train, kernel = "linear", cost = 1)
summary(SVM)


# Hyperparameter optimization (tuning)

set.seed(357)
tuned_model <- tune(svm, Class~., data = train, ranges = list(epsilon = seq(0, 3, 0.1), cost = (0.1:7)))

plot(tuned_model)
summary(tuned_model)

#best model

SVM <- tuned_model$best.model
summary(SVM)


# Confusion Matrix and Misclassification Error

pred <- predict(SVM, test)
confusionMatrix(pred, test$Class)
tab <- table(pred, test$Class)

1-sum(diag(tab))/sum(tab)

