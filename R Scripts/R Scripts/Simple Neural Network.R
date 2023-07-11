install.packages("readxl")
library(readxl)
data <- read_excel("data_set.xlsx")
data
str(data)

data$no_following <- (data$no_following - min(data$no_following)) / (max(data$no_following) - min(data$no_following))
hist(data$no_following)

data$no_follower <- (data$no_follower - min(data$no_follower)) / (max(data$no_follower) - min(data$no_follower))
hist(data$no_follower)

set.seed(222)
inp <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
training_data <- data[inp==1, ]
test_data <- data[inp==2, ]


install.packages("neuralnet")
library(neuralnet)
set.seed(333)
n <- neuralnet(label~no_follower + no_following + no_tweets + no_retweets,
               data = training_data,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 2,
               algorithm = "rprop+",
               stepmax = 100000)

plot(n, rep = 1)

n$result.matrix

output <- compute(n, rep = 1, training_data[, -1])
head(output$net.result)


head(training_data[1, ])

output <- compute(n, rep = 1, training_data[, -1])
p1 <- output$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, training_data$label)
tab1

caret::confusionMatrix(tab1)

1 - sum(diag(tab1)) / sum(tab1)
