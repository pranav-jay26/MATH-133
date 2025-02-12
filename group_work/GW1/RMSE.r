#!/usr/bin/env Rscript
library(ggplot2)

set.seed(123)

fit_linear_model <- function(y, x, raw_data) {

  # train test split
  n <- nrow(raw_data)
  trainIndex <- sample(n, round(0.8 * n, 0))
  train <- raw_data[trainIndex, ]
  test <- raw_data[-trainIndex, ]
  
  # construct formula
  formula <- as.formula(paste(y, "~", x))
  
  # fit model
  model <- lm(formula, data = train)

  # Store model summary
  model_summary <- summary(model)
  print(model_summary)  # This will print the summary when the function runs
  
  # predict on testing data
  y_test <- test[[y]]
  y_hat <- predict(model, newdata = test)
  
  # analyze accuracy
  SSE <- sum((y_test - y_hat)^2)
  MSE <- SSE / nrow(test)  
  RMSE <- sqrt(MSE)
  SST <- sum((y_test - mean(y_test))^2)
  R2 <- 1 - SSE / SST
  
  return(list(
    summary = model_summary,  # Include summary in return value
    SSE = SSE, 
    MSE = MSE, 
    RMSE = RMSE, 
    SST = SST, 
    R2 = R2
  ))
}

main <- function() {
  Beerwings <- read.csv("../../data/Beerwings.csv")
  result <- fit_linear_model("Hotwings", "Beer", Beerwings)
  print(result)
  
  ggplot(Beerwings, aes(x = Beer, y = Hotwings, color = Gender)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("Hotwings vs. Beer") +
    xlab("Beer") +
    ylab("Hotwings") +
    theme_minimal()
}

if (interactive() || identical(Sys.getenv("R_SCRIPT"), "")) {
  main()
}
