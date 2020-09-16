#read data

library(dplyr)

Main <- read.csv("~/boruta_phase_3_main63.csv")
str(Main)

table(Main$Class)

#data partition

set.seed(123)
ind <- sample(2, nrow(Main), replace = TRUE, prob = c(0.7, 0.3))
train <- Main[ind==1,]
test <- Main[ind==2,]

# Random Forest 
library(randomForest)

set.seed(450)
rf <- randomForest(Class~., data = train, ntree = 650,
                   mtry = 3,
                   importance = TRUE,
                   proximity = TRUE)
rf

#Prediction and confusion matrix - train data

library(caret)
p_train <- predict(rf, train)
confusionMatrix(p_train, train$Class)

#Prediction and confusion matrix - test data
p_test <- predict(rf, test)
confusionMatrix(p_test, test$Class)


#Error rate of Random Forest
plot(rf, main = "Error rate of Random Forest")


# Tune mtry

tune <- tuneRF(train[,-1], train[,1],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 700,
       trace = TRUE,
       improve = 0.05)

# # No. of Nodes for the Trees
# 
# hist(treesize(rf),
#      main = "No. of Nodes for the Trees",
#      col = "yellow")
# 
# 
# # Variable importance 
# varImpPlot(rf,
#            sort = T,
#            n.var = 63,
#            main = "Top ranked CpG sites",
#            col = "purple")
# 
# importance(rf)
# 
# varUsed(rf)
# 
# 
# #partial dependence plot
# 
# partialPlot(rf, train, cg19746719, "Tstage1")
# 
# # extract single tree
# 
# tree_1 <- getTree(rf, 599, labelVar = TRUE)
# tree_1 <- as.data.frame(tree_1)
# tree_1 <- select(tree_1, prediction)
# table(tree_1)
# 
# 
