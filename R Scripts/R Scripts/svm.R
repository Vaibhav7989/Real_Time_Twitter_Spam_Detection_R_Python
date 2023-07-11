# Load Libraries
library(tm)         # text mining package
library(plyr)
library(class)
library(caret)
library(e1071)
library(knitr)      # used to make kable tables
library("readxl")   # Reading xlsx or xls files
library(ggplot2)    # used for plots
library(dplyr)      # Manipulate data frames 

# Read data
rawdata <- read_excel("C:\\Users\\akhil\\Desktop\\FDA Jcomp\\Datasets\\stoprem_svm.xlsx")
names(rawdata) <- c("Class","Message")
rawdata$Class <- as.factor(rawdata$Class)

#Sample data
kable(rawdata[1:8,])

# Find total number of characters in each tweet
NumberOfChar <- as.numeric(lapply(rawdata$Message,FUN=nchar))

# Find number of numeric digits in each tweet

number.digits <- function(vect) {
  length(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(vect)), ""))))
}

NumberOfDigits <- as.numeric(lapply(rawdata$Message,FUN=number.digits))

# Function to clean text in the tweet

clean.text = function(x)
{ 
  # tolower
  x = tolower(x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # remove common words
  x = removeWords(x,stopwords("en"))
  return(x)
}
#print(stopwords("en"))
cleanText <- clean.text(rawdata$Message)
#cleanText

#getSources() # seeing what sources are available for tm package

# Build Corpus
# VectorSource a vector of characters (treats each component as a document)
corpus <- Corpus(VectorSource(cleanText))

# Build Term Document Matrix
tdm <- DocumentTermMatrix(corpus)

# Convert TDM to Dataframe
tdm.df <- as.data.frame(data.matrix(tdm),stringsAsFactors=FALSE)

# Remove features with total frequency less than 3
tdm.new <- tdm.df[,colSums(tdm.df) > 2]

# Split data
# Prepare final data with TDM, NumberofChar, NumberOfDigits as features

cleandata <- cbind("Class" = rawdata$Class, NumberOfChar, NumberOfDigits, tdm.new)

# Split Data into training (80%) and testing(20%) datasets

set.seed(1234)
inTrain <- createDataPartition(cleandata$Class,p=0.7,list=FALSE)
train <- cleandata[inTrain,]
test <- cleandata[-inTrain,]

#options(stringsAsFactors = TRUE) 

#Build SVM Model
## Linear Kernel
default.stringsAsFactors()
#train$Class <- as.factor(train$Class)
class(train$Class)
svm.linear <- svm(Class~., data=train, scale=FALSE, na.action = na.omit ,type='C-classification', kernel='linear')
test <- test[complete.cases(test),]
pred.linear <- predict(svm.linear, test[,-1])
#test$Class <- as.factor(test$Class)
class(test$Class)
linear <- confusionMatrix(pred.linear,test$Class)
linear

