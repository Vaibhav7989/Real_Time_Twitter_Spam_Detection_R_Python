data <- read_excel("data_set.xlsx")
data

summary(data)

plot(label ~ no_follower, data = data)
plot(label ~ no_following, data = data)

cor(data$label, data$no_follower)
cor(data$label, data$no_following)

label_no_follower <- lm(label ~ no_follower, data = data)
summary(label_no_follower)

label_no_following <- lm(label ~ no_following, data = data)
summary(label_no_following)


value <-ggplot(data, aes(x=no_following, y=no_follower))+ geom_point()
value

value <-ggplot(data, aes(x=no_follower, y=no_following))+ geom_point()
value

plot <- plot + geom_line(data=data, aes(x=no_follower, y=no_following.y, color=smoking), size=1.25)

plot
