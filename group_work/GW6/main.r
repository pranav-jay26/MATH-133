#!/usr/bin/env Rscript
library(ggplot2)
library(dplyr)
library(class)

set.seed(12345)

df <- read.csv("../../data/womenShots.csv")
df$xj <- scale(df$xj)
df$yj <- scale(df$yj)
df$Goal <- as.factor(df$Goal)

n <- nrow(df)
s=sample(1:n, 0.7*n)
train = df[s, ]
test = df[-s, ]

goal_knn = knn(train[,c("xj","yj")], test[,c("xj","yj")], cl=train$Goal, k=10)

x <- seq(0,1,by=0.01)
y <- seq(0,1,by=0.01)
ggplot(df, aes(x=xj, y=yj, color=Goal)) + geom_point()
new_df <- expand.grid(xj=x, yj=y)
new_df$predict <- knn(df[,c("xj","yj")], new_df[,c("xj","yj")], cl=df$Goal, k=10)

err_rate <- sum(goal_knn!=test$Goal)/length(test$Goal)
sprintf("Error rate for k = 10: %s", err_rate)


ggplot(new_df, aes(x=xj, y=yj, col=predict)) +
  geom_point(size=3) +
  scale_color_manual(values=c("#0072B2","#E69F00"))

fits <- data.frame(K=1:100, er=NA)
for (i in 1:100) {
  pred=knn(train[,c("xj", "yj")], test[,c("xj", "yj")], cl=train$Goal, k=fits$K[i])
  fits$er[i]=sum(pred!=test$Goal)/length(test$Goal)
}

fits %>% ggplot(aes(x=K, y=er))+geom_line()
optimal <- fits$K[which.min(fits$er)]

sprintf("Optimal K: %s", optimal)

goal_knn <- knn(train[,c("xj","yj")], test[,c("xj","yj")], cl=train$Goal, k=optimal)
err_rate <- sum(goal_knn!=test$Goal)/length(test$Goal)
sprintf("Error rate for k = %s: %s", optimal, err_rate)

new_df$predict <- knn(df[,c("xj","yj")], new_df[,c("xj","yj")], cl=df$Goal, k=optimal)

ggplot(new_df, aes(x=xj, y=yj, col=predict)) +
  geom_point(size=3) +
  scale_color_manual(values=c("#0072B2","#E69F00"))



