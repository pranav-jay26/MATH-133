#!/usr/bin/env Rscript

# SLM
model <- lm(mpg ~ hp, data = mtcars)
summary(model)
plot(model, which = 1)  # Residuals vs. Fitted

# Log-Log Model
loglog_model <- lm(log(mpg) ~ log(hp), data = mtcars)
summary(loglog_model)

# Outlier Check
plot(model, which = 4)  # Cook's Distance
rstandard(model)[abs(rstandard(model)) > 2]

