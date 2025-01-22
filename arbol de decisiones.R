install.packages('datasets')
install.packages('caTools')
install.packages('party')
install.packages('dplyr')
install.packages('magrittr')

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

data("readingSkills")
head(readingSkills)

# Splitting dataset into 4:1 ratio for train and test data
sample_data = sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

# Create the decision tree model using ctree and plot the model
model<- ctree(nativeSpeaker ~ ., train_data)
plot(model)

# Making a predict
# testing the people who are native speakers
# and those who are not
predict_model<-predict(model, test_data)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_data$nativeSpeaker, predict_model)
m_at

# Determining the accuracy of the model developed
ac_Test <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test))


