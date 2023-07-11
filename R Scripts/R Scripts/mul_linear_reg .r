install.packages("readxl")
library(readxl)
data <- read_excel("data_set.xlsx")
data
no_followers <- data$no_follower
no_following <- data$no_following
no_tweets <- data$no_tweets
no_retweets <- data$no_retweets
input <- data[,c("no_follower","no_following","no_tweets","no_retweets","label")]
input
model <- lm(label~no_followers+no_following+no_tweets+no_retweets,data=input)
print(model)

model1 <- lm(label~no_followers+no_following,data=input)
print(model1)

model2 <- lm(label~no_tweets+no_retweets,data=input)
print(model2)


test_data <- read_excel("TestDataset.xlsx")
test_data
test_data$perdicted <- 5.424e-01 + 3.653e-07*test_data$no_follower + -2.269e-06*test_data$no_tweets + -1.408e-05*test_data$no_retweets
test_data$perdicted

spam_ham <- function(sp_or_hm){
  if(sp_or_hm>0.5){
    return ("Spam")
  }
  else{
    return ("Ham")
  }
}
nrow(test_data)
for(i in 1:nrow(test_data)){
  output = spam_ham(test_data$perdicted[i])
  test_data$perdicted[i] = output
}


for(i in 1:nrow(test_data)){
  output = spam_ham(test_data$label[i])
  test_data$label[i] = output
}
test_data$perdicted
test_data$label
test_data

pos = 0
total = 0
for(i in 1:nrow(test_data)){
  if(test_data$perdicted[i]==test_data$label[i]){
    pos = pos + 1
    total = total + 1
  }
  else{
    total = total + 1
  }
}
accuracy = pos/nrow(test_data)
accuracy


table(test_data$label,test_data$perdicted)
install.packages("caret")
caret::confusionMatrix(table(test_data$label,test_data$perdicted))

library(ggplot2)

ggplot <- ggplot(Orange,aes(x=test_data$no_follower,y=test_data$no_following))+geom_point()+theme_classic()
ggplot

# Plotting multiple Regression Lines
ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=Tree))


#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train <- data[sample, ]
test <- data[!sample, ]  
model <- glm(label~no_followers+no_following+no_tweets+no_retweets, family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)
pscl::pR2(model)["McFadden"]
caret::varImp(model)
data::vif(model)
new <- data.frame(no_followers=10,no_following=20,no_tweets=15,no_retweets=50,label=c("Yes","No"))

#predict probability of defaulting
predict(model, new, type="response")
predicted <- predict(model, test, type="response")
install.packages("InformationValue")
library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test$default <- ifelse(test$default=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$default, predicted)[1]
optimal
confusionMatrix(test$default, predicted)

