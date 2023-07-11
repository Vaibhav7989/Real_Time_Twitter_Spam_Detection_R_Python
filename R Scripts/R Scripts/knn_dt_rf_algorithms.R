
############################### Decision Tree ######################
#install.packages("party")
#library(party) # has a function ctree() which is used to create and analyze decision tree
library(rpart)
library(rpart.plot)

#install.packages("readxl")
library("readxl")  # To read any type of excel file
inp_dat <- read_excel("C:\\Users\\akhil\\Desktop\\FDA Jcomp\\Datasets\\acc-fda data set.xlsx")
head(inp_dat)
#Dividing the dataset into training and test dataset
set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(inp_dat), replace=TRUE, prob=c(0.7,0.3))


# Building the decision tree
myFormula <- label ~ accout_age + no_follower + no_following + no_userfavorites + no_lists + no_tweets + no_retweets + no_tweetfavourites + no_hashtag + no_usermention + no_urls + no_char + no_digits
tree <- rpart(myFormula, data = trainData, methi)

## To predict train data itself
#train_predict <- predict(iris_ctree)
train_predict <- predict(tree,trainData)
test_predict <- predict(tree,testData)
head(test_predict)
spam_ham <- function(hm, sp) {
  if (sp>hm) {
    return("spammer")
  }
  else {
    return("ham")
  }
}
testData$Pred = testData$label
## Prediction for test data
for (i in 1:nrow(test_predict)) {
  msg = spam_ham(test_predict[i,1], test_predict[i,2])
  #test_predict[i] = msg
  testData$Pred[i]=msg
}
testData$Pred
## Creating confusion matrix and misclassification errors
xtab <- table(testData$label,testData$Pred)
## 
rpart.plot(tree)
## Accuracy
#install.packages("caret")
#library("ggplot2")
#library("lattice")
library("caret")
caret::confusionMatrix(xtab)







############################# Random Forest ###############
## Importing required libraries
library(readxl)
library(dplyr)
library(caret)
library(ranger)
library("Hmisc")
library(class) # Functions are provided for classification, including k-nearest
#neighbour, Learning Vector Quantization and Self-Organizing Maps
set.seed(321)

## Loading dataset and randomly splitting the dataset
# Loading dataset
twitterspam<-read_excel("C:\\Users\\akhil\\Desktop\\FDA Jcomp\\Datasets\\acc-fda data set.xlsx")
# Splitting the dataset into 2 (training - 90%, Testing - 10%)
index <- createDataPartition(twitterspam$label,p=0.9,times=1, list=FALSE)
training<-twitterspam[index,]
test<-twitterspam[-index,]
## Using training dataset to train the model with random forest algorithm
model<-train(as.factor(label) ~.,data = training, method = 'rf',trControl = trainControl(method = 'cv',number = 5)) # Evaluate the model with a grid search of 5 fold
# By default random  search will be taken which will be evaluated over randomly chosen combination
# you pass in the function, using cross validation 
model # Accuracy around 87%
## Using testing dataset to test and evaluate the model trained
predict<-predict(model,test) # Predicting possible outcomes for test data
# Using confusion matrix to evaluate the model
caret::confusionMatrix(predict,as.factor(test$label))





############################# K-nearest neighbours ####################
library(caret)
training<-training[,-nearZeroVar(training)]
set.seed(42)
trainctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
knn_model <- train(label~., data=training, method = "knn",
                   tuneLength = 10,
                   preProcess = c("center", "scale"),
                   trControl = trainctrl,
                   metric="Kappa")
knn_model
## Using testing data to test and evaluate model
predict<-predict(knn_model, test)
## Creating  confusion matrix and finding the accuracy, kappa, Prevalence,etc
confusionMatrix(predict,as.factor(test$label))




