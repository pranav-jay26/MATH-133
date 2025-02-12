#!/usr/bin/env Rscript

data <- read.csv("~/Downloads/Credit.csv")
colnames(data)

income_model = lm(Balance~Rating*Limit*Income*Cards, data=data)
summary(income_model)
