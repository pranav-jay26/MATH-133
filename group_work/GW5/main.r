#!/usr/bin/env Rscript
library(ISLR2)

data <- Carseats

colnames(data)

sales_lm <- lm(Sales~., data = data)

sales_lmUpdated <- update(sales_lm,.~.-Population-Education-Urban-US)

summary(sales_lm)
summary(sales_lmUpdated)
anova(sales_lmUpdated, sales_lm)

