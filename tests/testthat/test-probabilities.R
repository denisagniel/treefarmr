# Test suite for probability validation
# Tests probability outputs: sum to 1, bounds [0,1], consistency with predictions,
# calibration, and bounded away from 0/1 for log-loss

library(testthat)
library(treefarmr)

# Setup test environment
setup_test_environment()

test_that("probabilities sum to 1.0 for misclassification loss", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "misclassification")
  
  # Check probability sums
  row_sums <- rowSums(model$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10), 
              info = "Probability rows should sum to 1.0")
})

test_that("probabilities sum to 1.0 for log-loss", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check probability sums
  row_sums <- rowSums(model$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10), 
              info = "Log-loss probability rows should sum to 1.0")
})

test_that("probabilities are in [0, 1] bounds for misclassification", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "misclassification")
  
  # Check bounds
  expect_true(all(model$probabilities >= 0), 
              info = "All probabilities should be >= 0")
  expect_true(all(model$probabilities <= 1), 
              info = "All probabilities should be <= 1")
})

test_that("probabilities are in [0, 1] bounds for log-loss", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check bounds
  expect_true(all(model$probabilities >= 0), 
              info = "All log-loss probabilities should be >= 0")
  expect_true(all(model$probabilities <= 1), 
              info = "All log-loss probabilities should be <= 1")
})

test_that("log-loss probabilities are bounded away from 0/1", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check bounded away from 0
  expect_true(all(model$probabilities > 0.01), 
              info = "Log-loss probabilities should be > 0.01")
  # Check bounded away from 1
  expect_true(all(model$probabilities < 0.99), 
              info = "Log-loss probabilities should be < 0.99")
})

test_that("predictions are consistent with probabilities", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "misclassification")
  
  # Check consistency: predictions = argmax(probabilities)
  expected_predictions <- ifelse(model$probabilities[, 2] >= 0.5, 1, 0)
  expect_equal(model$predictions, expected_predictions,
               info = "Predictions should match argmax(probabilities)")
})

test_that("predictions are consistent with probabilities for log-loss", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check consistency: predictions = argmax(probabilities)
  expected_predictions <- ifelse(model$probabilities[, 2] >= 0.5, 1, 0)
  expect_equal(model$predictions, expected_predictions,
               info = "Log-loss predictions should match argmax(probabilities)")
})

test_that("probability calibration for balanced data", {
  # Create balanced dataset (50/50)
  test_data <- create_probability_test_data(n_samples = 200, class_freq = 0.5, seed = 42)
  
  model <- safe_treefarms(test_data$X, test_data$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check calibration
  prob_class_1 <- model$probabilities[, 2]
  mean_prob <- mean(prob_class_1)
  class_freq <- mean(test_data$y)
  
  # Mean probability should be close to class frequency (within 0.1)
  expect_true(abs(mean_prob - class_freq) < 0.1,
              info = "Mean probability should be close to class frequency")
})

test_that("probability calibration for imbalanced data", {
  # Create imbalanced dataset (90/10)
  test_data <- create_probability_test_data(n_samples = 200, class_freq = 0.1, seed = 42)
  
  model <- safe_treefarms(test_data$X, test_data$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check calibration
  prob_class_1 <- model$probabilities[, 2]
  mean_prob <- mean(prob_class_1)
  class_freq <- mean(test_data$y)
  
  # Mean probability should be close to class frequency (within 0.1)
  expect_true(abs(mean_prob - class_freq) < 0.1,
              info = "Mean probability should reflect class imbalance")
})

test_that("probabilities are finite (no NaN or Inf)", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check for NaN or Inf
  expect_true(all(is.finite(model$probabilities)),
              info = "All probabilities should be finite (no NaN/Inf)")
})

test_that("get_probabilities() returns valid probabilities", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         compute_probabilities = FALSE,  # Lazy evaluation
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get probabilities using get_probabilities()
  probs <- get_probabilities(model)
  
  # Validate probabilities
  expect_valid_probabilities(probs, loss_function = "log_loss", 
                            info = "get_probabilities() output")
  
  # Should match model$probabilities after computation
  expect_equal(probs, model$probabilities, tolerance = 1e-10,
              info = "get_probabilities() should match model$probabilities")
})

test_that("lazy probability computation works", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         compute_probabilities = FALSE,  # Lazy evaluation
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Initially, probabilities may be NULL (lazy)
  # Access via get_probabilities() should compute them
  probs1 <- get_probabilities(model)
  expect_valid_probabilities(probs1, loss_function = "log_loss",
                            info = "First access to probabilities")
  
  # After first access, probabilities should be cached
  probs2 <- get_probabilities(model)
  expect_equal(probs1, probs2, tolerance = 1e-10,
              info = "Probabilities should be cached after first access")
})

test_that("probabilities for perfect separation are bounded (log-loss)", {
  # Create perfect separation dataset
  test_data <- create_perfect_separation_data(n_samples = 100, seed = 42)
  
  model <- safe_treefarms(test_data$X, test_data$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Even with perfect separation, log-loss probabilities should be bounded
  expect_true(all(model$probabilities > 0.01),
              info = "Log-loss probabilities should be bounded away from 0 even with perfect separation")
  expect_true(all(model$probabilities < 0.99),
              info = "Log-loss probabilities should be bounded away from 1 even with perfect separation")
})

test_that("probabilities have correct dimensions", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Check dimensions
  expect_equal(nrow(model$probabilities), nrow(simple_dataset$X),
              info = "Probability rows should match training data rows")
  expect_equal(ncol(model$probabilities), 2,
              info = "Probabilities should have 2 columns")
})

test_that("probabilities work with different regularization values", {
  regularization_values <- c(0.01, 0.1, 0.5, 1.0)
  
  for (reg in regularization_values) {
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = reg,
                           verbose = FALSE)
    
    expect_valid_treefarms_model(model, "log_loss")
    
    # All models should produce valid probabilities
    expect_valid_probabilities(model$probabilities, loss_function = "log_loss",
                               info = paste("Regularization =", reg))
  }
})

test_that("probabilities work with different dataset sizes", {
  dataset_sizes <- c(50, 100, 500, 1000)
  
  for (n in dataset_sizes) {
    test_data <- create_probability_test_data(n_samples = n, class_freq = 0.5, seed = 42)
    
    model <- safe_treefarms(test_data$X, test_data$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
    
    expect_valid_treefarms_model(model, "log_loss")
    
    # All models should produce valid probabilities
    expect_valid_probabilities(model$probabilities, loss_function = "log_loss",
                               info = paste("Dataset size =", n))
  }
})

# Cleanup
teardown_test_environment()



