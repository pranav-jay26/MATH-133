#!/usr/bin/env Rscript

library(ggplot2)

set.seed(123)

fit_linear_model <- function(y, x, raw_data) {
  # Train-test split
  n <- nrow(raw_data)
  trainIndex <- sample(n, round(0.7 * n, 0))
  train <- raw_data[trainIndex, ]
  test <- raw_data[-trainIndex, ]
  
  # Construct formula
  formula <- as.formula(paste(y, "~", x))
  
  # Fit model
  model <- lm(formula, data = train)

  overview = summary(model)
  print(overview)
  
  # Predict on testing data
  y_test <- test[[y]]
  y_hat <- predict(model, newdata = test)
  
  # Analyze accuracy
  SSE <- sum((y_test - y_hat)^2)
  MSE <- SSE / nrow(test)  
  RMSE <- sqrt(MSE)
  SST <- sum((y_test - mean(y_test))^2)
  R2 <- 1 - SSE / SST
  
  return(list(SSE = SSE, MSE = MSE, RMSE = RMSE, SST = SST, R2 = R2))
}

main <- function() {
  # Load data
  data <- read.csv("../../data/survey_fall2023.csv")
  
  # Define predictors
  predictors <- c("maximum_alcohol_consumed", "gpa", "screen_time")
  results <- list()
  
  # Open PDF device for saving all plots
  pdf("Rplots.pdf")  # This will save all plots sequentially to Rplots.pdf
  
  # Iterate over predictors
  for (predictor in predictors) {
    if (!predictor %in% colnames(data)) {
      cat("\nWarning: Predictor", predictor, "not found in dataset. Skipping...\n")
      next
    }
    
    cat("\nLinear model for stress score ~", predictor, "\n")
    result <- fit_linear_model("stress_score", predictor, data)
    
    # Store results for later comparison
    results[[predictor]] <- result
    
    # Format and print results
    formatted_results <- lapply(result[1:5], function(x) format(round(x, 4), nsmall = 4))
    print(formatted_results)
    
    # Create scatterplot for the predictor
    ggplot(data, aes_string(x = predictor, y = "stress_score")) +
      geom_point(color = "blue", size = 2) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      ggtitle(paste("stress_score vs", predictor)) +
      xlab(predictor) +
      ylab("Stress Score") +
      theme_minimal() -> plot  # Assign ggplot to a variable

    print(plot)  # Ensure the plot is sent to the PDF file
  }

  hist(data$screen_time, breaks = "Freedman-Diaconis") -> plot1
  print(plot1)
  
  # Close PDF device
  dev.off()
  
  cat("\nAll plots saved to Rplots.pdf.\n")
}

# Run the script if executed directly
if (interactive() || identical(Sys.getenv("R_SCRIPT"), "")) {
  main()
}

