# Load necessary libraries
library(caret)
library(randomForest)
library(rpart)
library(class)
library(pROC)
library(ggplot2)
library(dplyr)

# Load the dataset
data("USArrests")

# Convert the target variable into a categorical variable
USArrests$UrbanPopCat <- ifelse(USArrests$UrbanPop > median(USArrests$UrbanPop), "High", "Low")
USArrests$UrbanPopCat <- factor(USArrests$UrbanPopCat, levels = c("Low", "High"))

# Remove the original UrbanPop column
USArrests <- USArrests[, -3]

# Check the structure of the dataset
str(USArrests)

# Split the data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(USArrests$UrbanPopCat, p = 0.7, list = FALSE)
trainData <- USArrests[trainIndex, ]
testData <- USArrests[-trainIndex, ]

# Train KNN model
knn_pred <- knn(trainData[, -4], testData[, -4], trainData$UrbanPopCat, k = 5)

# Train random forest model
rf_model <- randomForest(UrbanPopCat ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
rf_prob <- predict(rf_model, testData, type = "prob")

# Train decision tree model
dt_model <- rpart(UrbanPopCat ~ ., data = trainData, method = "class")
dt_pred <- predict(dt_model, testData, type = "class")
dt_prob <- predict(dt_model, testData, type = "prob")

# Train logistic regression model
logit_model <- train(UrbanPopCat ~ ., data = trainData, method = "glm", family = binomial())
logit_pred <- predict(logit_model, testData)
logit_prob <- predict(logit_model, testData, type = "prob")

# Evaluate the models
# Confusion matrix and accuracy for KNN
knn_cm <- confusionMatrix(knn_pred, testData$UrbanPopCat)
knn_acc <- knn_cm$overall['Accuracy']

# Confusion matrix and accuracy for random forest
rf_cm <- confusionMatrix(rf_pred, testData$UrbanPopCat)
rf_acc <- rf_cm$overall['Accuracy']

# Confusion matrix and accuracy for decision tree
dt_cm <- confusionMatrix(dt_pred, testData$UrbanPopCat)
dt_acc <- dt_cm$overall['Accuracy']

# Confusion matrix and accuracy for logistic regression
logit_cm <- confusionMatrix(logit_pred, testData$UrbanPopCat)
logit_acc <- logit_cm$overall['Accuracy']

# ROC and AUC for random forest
rf_roc <- roc(testData$UrbanPopCat, rf_prob[,"High"])
rf_auc <- auc(rf_roc)

# ROC and AUC for decision tree
dt_roc <- roc(testData$UrbanPopCat, dt_prob[,"High"])
dt_auc <- auc(dt_roc)

# ROC and AUC for logistic regression
logit_roc <- roc(testData$UrbanPopCat, logit_prob[,"High"])
logit_auc <- auc(logit_roc)

# KNN does not provide probability scores directly for ROC/AUC analysis
#1. Determine, what is the best model to predict the main variable (choose one and
                                                                   explain, why this selection).

# Combine accuracy results
accuracy_results <- data.frame(
  Model = c("KNN", "Random Forest", "Decision Tree", "Logistic Regression"),
  Accuracy = c(knn_acc, rf_acc, dt_acc, logit_acc)
)

# Combine AUC results for random forest, decision tree, and logistic regression
auc_results <- data.frame(
  Model = c("Random Forest", "Decision Tree", "Logistic Regression"),
  AUC = c(rf_auc, dt_auc, logit_auc)
)

# Print the results
accuracy_results
auc_results
#Use the different indicators to justify the answer?
# Visualize the results using ggplot2
# Accuracy plot
accuracy_plot <- ggplot(accuracy_results, aes(x = Model, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0, 1) +
  labs(title = "Model Accuracy Comparison", y = "Accuracy", x = "Model")

# AUC plot
auc_plot <- ggplot(auc_results, aes(x = Model, y = AUC)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  ylim(0, 1) +
  labs(title = "Model AUC Comparison", y = "AUC", x = "Model")

# Display the plots
accuracy_plot
auc_plot



# Determine the best model
best_model_accuracy <- accuracy_results[which.max(accuracy_results$Accuracy), "Model"]
best_model_accuracy

best_model_auc <- auc_results[which.max(auc_results$AUC), "Model"]
best_model_auc
