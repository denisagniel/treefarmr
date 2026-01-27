# Helper functions for probability validation tests

#' Validate probability outputs from TreeFARMS models
#' 
#' @param probabilities Matrix of probabilities [P(class=0), P(class=1)]
#' @param loss_function Loss function used ("misclassification" or "log_loss")
#' @param tolerance Numerical tolerance for sum-to-1 check
#' @param info Additional information for test output
expect_valid_probabilities <- function(probabilities, loss_function = NULL, 
                                      tolerance = 1e-10, info = NULL) {
  # Check that probabilities is a matrix
  expect_true(is.matrix(probabilities), 
              info = paste(info, "- probabilities should be a matrix"))
  
  # Check dimensions
  expect_true(ncol(probabilities) == 2, 
              info = paste(info, "- probabilities should have 2 columns"))
  expect_true(nrow(probabilities) > 0, 
              info = paste(info, "- probabilities should have at least 1 row"))
  
  # Check bounds [0, 1]
  expect_true(all(probabilities >= 0), 
              info = paste(info, "- all probabilities should be >= 0"))
  expect_true(all(probabilities <= 1), 
              info = paste(info, "- all probabilities should be <= 1"))
  
  # Check row sums sum to 1.0
  row_sums <- rowSums(probabilities)
  expect_true(all(abs(row_sums - 1) < tolerance), 
              info = paste(info, "- probability rows should sum to 1.0"))
  
  # For log-loss, check bounds away from 0/1
  if (!is.null(loss_function) && loss_function == "log_loss") {
    # Check bounded away from 0 (e.g., > 0.01)
    expect_true(all(probabilities > 0.01), 
                info = paste(info, "- log-loss probabilities should be > 0.01"))
    # Check bounded away from 1 (e.g., < 0.99)
    expect_true(all(probabilities < 0.99), 
                info = paste(info, "- log-loss probabilities should be < 0.99"))
  }
  
  # Check for NaN or Inf
  expect_true(all(is.finite(probabilities)), 
              info = paste(info, "- probabilities should be finite (no NaN/Inf)"))
}

#' Validate that predictions are consistent with probabilities
#' 
#' @param predictions Vector of binary predictions (0/1)
#' @param probabilities Matrix of probabilities [P(class=0), P(class=1)]
#' @param info Additional information for test output
expect_predictions_consistent <- function(predictions, probabilities, info = NULL) {
  # Check dimensions match
  expect_equal(length(predictions), nrow(probabilities),
               info = paste(info, "- predictions and probabilities should have same length"))
  
  # Check predictions are binary
  expect_true(all(predictions %in% c(0, 1)),
              info = paste(info, "- predictions should be binary (0 or 1)"))
  
  # Check consistency: predictions = argmax(probabilities)
  # For class 1: predict 1 if P(class=1) >= 0.5, else 0
  expected_predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
  # Handle ties (P(class=1) == 0.5) - either 0 or 1 is acceptable
  # But in practice, we expect predictions to match
  expect_equal(predictions, expected_predictions,
               info = paste(info, "- predictions should match argmax(probabilities)"))
}

#' Validate probability calibration
#' 
#' @param probabilities Matrix of probabilities [P(class=0), P(class=1)]
#' @param y_true True class labels
#' @param tolerance Tolerance for calibration check
#' @param info Additional information for test output
expect_probability_calibration <- function(probabilities, y_true, 
                                           tolerance = 0.1, info = NULL) {
  # Check that probabilities reflect class distribution
  prob_class_1 <- probabilities[, 2]
  mean_prob <- mean(prob_class_1)
  class_freq <- mean(y_true)
  
  # Mean probability should be close to class frequency
  # Allow some tolerance (model may not be perfectly calibrated)
  expect_true(abs(mean_prob - class_freq) < tolerance,
              info = paste(info, "- mean probability should be close to class frequency"))
}

#' Compute cross-entropy loss manually for verification
#' 
#' @param probabilities Matrix of probabilities [P(class=0), P(class=1)]
#' @param y_true True class labels
#' @return Cross-entropy loss value
compute_cross_entropy_loss <- function(probabilities, y_true) {
  # Cross-entropy: -sum(y * log(p) + (1-y) * log(1-p))
  # For binary classification: -sum(y * log(p1) + (1-y) * log(p0))
  # Where p1 = P(class=1), p0 = P(class=0)
  
  p1 <- probabilities[, 2]
  p0 <- probabilities[, 1]
  
  # Avoid log(0) by using small epsilon
  epsilon <- 1e-15
  p1 <- pmax(p1, epsilon)
  p0 <- pmax(p0, epsilon)
  
  # Compute loss
  loss <- -sum(y_true * log(p1) + (1 - y_true) * log(p0))
  loss <- loss / length(y_true)  # Average loss
  
  return(loss)
}

#' Validate that log-loss probabilities are bounded
#' 
#' @param probabilities Matrix of probabilities [P(class=0), P(class=1)]
#' @param lower_bound Lower bound (default 0.01)
#' @param upper_bound Upper bound (default 0.99)
#' @param info Additional information for test output
expect_logloss_bounds <- function(probabilities, lower_bound = 0.01, 
                                  upper_bound = 0.99, info = NULL) {
  expect_true(all(probabilities > lower_bound),
              info = paste(info, "- log-loss probabilities should be >", lower_bound))
  expect_true(all(probabilities < upper_bound),
              info = paste(info, "- log-loss probabilities should be <", upper_bound))
}

#' Validate probability outputs from get_probabilities() function
#' 
#' @param model treefarms_model object
#' @param loss_function Expected loss function
#' @param info Additional information for test output
expect_valid_get_probabilities <- function(model, loss_function = NULL, info = NULL) {
  # Get probabilities using get_probabilities()
  probs <- get_probabilities(model)
  
  # Validate probabilities
  expect_valid_probabilities(probs, loss_function = loss_function, info = info)
  
  # Check that probabilities match model$probabilities (if computed)
  if (!is.null(model$probabilities)) {
    expect_equal(probs, model$probabilities, tolerance = 1e-10,
                info = paste(info, "- get_probabilities() should match model$probabilities"))
  }
}

#' Validate lazy probability computation
#' 
#' @param model treefarms_model object (with compute_probabilities=FALSE)
#' @param loss_function Expected loss function
#' @param info Additional information for test output
expect_lazy_probabilities <- function(model, loss_function = NULL, info = NULL) {
  # Initially, probabilities may be NULL
  # Access via get_probabilities() should compute them
  probs <- get_probabilities(model)
  
  # Validate computed probabilities
  expect_valid_probabilities(probs, loss_function = loss_function, info = info)
  
  # After first access, probabilities should be cached
  probs2 <- get_probabilities(model)
  expect_equal(probs, probs2, tolerance = 1e-10,
              info = paste(info, "- probabilities should be cached after first access"))
}

#' Create test dataset with known probability distribution
#' 
#' @param n_samples Number of samples
#' @param class_freq Class 1 frequency (0 to 1)
#' @param seed Random seed
#' @return List with X (data.frame) and y (vector)
create_probability_test_data <- function(n_samples = 100, class_freq = 0.5, seed = 42) {
  set.seed(seed)
  
  # Create features
  n_features <- 5
  X <- as.data.frame(matrix(sample(0:1, n_samples * n_features, replace = TRUE), 
                           ncol = n_features))
  colnames(X) <- paste0("feature_", 1:n_features)
  
  # Create labels with specified frequency
  n_class_1 <- round(n_samples * class_freq)
  n_class_0 <- n_samples - n_class_1
  y <- c(rep(0, n_class_0), rep(1, n_class_1))
  y <- sample(y)  # Shuffle
  
  return(list(X = X, y = y))
}

#' Create test dataset with perfect separation
#' 
#' @param n_samples Number of samples (must be even)
#' @param seed Random seed
#' @return List with X (data.frame) and y (vector)
create_perfect_separation_data <- function(n_samples = 100, seed = 42) {
  set.seed(seed)
  
  if (n_samples %% 2 != 0) {
    n_samples <- n_samples + 1
  }
  
  # Create features that perfectly separate classes
  # Class 0: feature_1 == 0
  # Class 1: feature_1 == 1
  X <- data.frame(
    feature_1 = c(rep(0, n_samples/2), rep(1, n_samples/2)),
    feature_2 = sample(0:1, n_samples, replace = TRUE),
    feature_3 = sample(0:1, n_samples, replace = TRUE)
  )
  
  y <- c(rep(0, n_samples/2), rep(1, n_samples/2))
  
  return(list(X = X, y = y))
}



