# Libraries
library(caret)
library(pROC)
library(mlbench)

# (Classification)
data <- read.csv("~/boruta_phase_3_main63.csv")
str(data)

table(data$Class)


# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]

# KNN Model
trControl <- trainControl(method = "repeatedcv",
                          number = 12,
                          repeats = 5)


set.seed(222)
fit <- train( Class~.,
              data = train,
              method = 'knn',
              trControl = trControl,
              preProc = c('center', 'scale'),
              metric = 'Accuracy',
              tuneGrid = expand.grid(k = 1:20))



# Model Performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$Class)



