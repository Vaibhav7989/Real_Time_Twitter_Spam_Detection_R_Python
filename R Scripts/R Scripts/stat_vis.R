#1. Load TwitterSpam dataset into R studio, 
TwitterSpam <- read.csv("normal.csv")
View(TwitterSpam)

#Density plot for no of followers spam vs ham
library('ggplot2')
ggplot(TwitterSpam, aes(x = no_follower , fill = factor(label))) +
  geom_density(alpha = .5) + xlim(c(0, 6000))

ggplot(TwitterSpam, aes(x = no_follower , fill = factor(label))) +
  geom_density() + facet_grid(label ~ .) + xlim(c(0, 6000))

#2. Load TwitterSpam dataset into R studio, 
TwitterSpam <- read.csv("normal.csv")
View(TwitterSpam)

#Density plot for no of following spam vs ham
library('ggplot2')
ggplot(TwitterSpam, aes(x = no_following , fill = factor(label))) +
  geom_density(alpha = .5) + xlim(c(0, 6000))

ggplot(TwitterSpam, aes(x = no_following , fill = factor(label))) +
  geom_density() + facet_grid(label ~ .) + xlim(c(0, 6000))

# the relation of posted tweets number and the number of follower
ggplot(TwitterSpam, aes(x = no_tweets, y= no_follower )) + geom_point()


#account age vs no of followers on spam accounts
ggplot(data = TwitterSpam, mapping = aes(x = accout_age, y = no_follower)) +
  geom_line() + theme_bw()
