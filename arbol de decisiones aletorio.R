install.packages("randomForest")
install.packages("datasets")
install.packages("caret")

library(randomForest)
library(datasets)
library(caret)

# Getting data
data<-iris
str(data)

# See more
data$Species <- as.factor(data$Species)
table(data$Species)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Model training
rf <- randomForest(Species~., data=train, proximity=TRUE)
print(rf)

# Prediction - Train Data
p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)

# Prediction - Test Data
p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)

# See graph
plot(rf)

# Tuning model
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

