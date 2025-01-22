install.packages("DAAG")
install.packages("rpart.plot")
install.packages("rpart")
install.packages("mlbench")
install.packages("caret")
install.packages("pROC")
install.packages("tree")

library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)

# General information about object spam7
str(spam7)

# Save
mydata <- spam7

# Data partition
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
train <- mydata[ind == 1,]
test <- mydata[ind == 2,]

# Classification
tree <- rpart(yesno ~., data = train)
rpart.plot(tree)

# Results
printcp(tree)
plotcp(tree)

# Another example
tree <- rpart(yesno ~., data = train,cp=0.07444)

#Note: You can change the cp value according to your data set.  
# Please note lower cp value means bigger the tree.
# If you are using too lower cp that leads to overfitting also.

# Confusion matrix
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$yesno, positive='y')

# Curva ROC
p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$yesno, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')


