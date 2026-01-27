# Test suite for treefarmr package
# Tests the main treefarms functionality with Rcpp implementation

library(testthat)
library(treefarmr)

# Create test data
set.seed(42)
test_data <- list(
  X = data.frame(
    feature_1 = sample(0:1, 100, replace = TRUE),
    feature_2 = sample(0:1, 100, replace = TRUE),
    feature_3 = sample(0:1, 100, replace = TRUE)
  ),
  y = sample(0:1, 100, replace = TRUE)
)

# Create data with clear pattern for testing
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

test_that("treefarms basic functionality works", {
  # Test with misclassification loss
  expect_no_error({
    model <- treefarms(test_data$X, test_data$y, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  # Check model structure
  expect_true(inherits(model, "treefarms_model"))
  expect_true(is.numeric(model$n_trees))
  expect_true(is.numeric(model$accuracy))
  expect_true(is.character(model$loss_function))
  expect_equal(model$loss_function, "misclassification")
  
  # Test with log_loss
  expect_no_error({
    model_log <- treefarms(test_data$X, test_data$y, 
                          loss_function = "log_loss", 
                          regularization = 0.1,
                          verbose = FALSE)
  })
  
  expect_equal(model_log$loss_function, "log_loss")
})

test_that("treefarms input validation works", {
  # Test invalid X
  expect_error(treefarms("invalid", test_data$y), "X must be a data.frame or matrix")
  
  # Test invalid y
  expect_error(treefarms(test_data$X, "invalid"), "y must be numeric or logical")
  
  # Test length mismatch
  expect_error(treefarms(test_data$X, test_data$y[1:50]), "Length of y must match number of rows in X")
  
  # Test non-binary y
  expect_error(treefarms(test_data$X, c(test_data$y, 2)), "y must contain only binary values")
  
  # Test non-binary features
  X_invalid <- test_data$X
  X_invalid$feature_1[1] <- 2
  expect_error(treefarms(X_invalid, test_data$y), "Feature feature_1 must contain only binary values")
  
  # Test invalid loss function
  expect_error(treefarms(test_data$X, test_data$y, loss_function = "invalid"), 
               "loss_function must be either 'misclassification' or 'log_loss'")
  
  # Test negative regularization
  expect_error(treefarms(test_data$X, test_data$y, regularization = -0.1), 
               "regularization must be non-negative")
  
  # Test negative rashomon_bound_multiplier
  expect_error(treefarms(test_data$X, test_data$y, rashomon_bound_multiplier = -0.1), 
               "rashomon_bound_multiplier must be non-negative")
})

test_that("treefarms with different parameters", {
  # Test with different regularization values
  expect_no_error({
    model1 <- treefarms(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.05,
                       verbose = FALSE)
  })
  
  expect_no_error({
    model2 <- treefarms(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.2,
                       verbose = FALSE)
  })
  
  # Models should have different properties
  expect_true(model1$regularization != model2$regularization)
  
  # Test with different rashomon_bound_multiplier
  expect_no_error({
    model3 <- treefarms(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.1,
                       rashomon_bound_multiplier = 0.1,
                       verbose = FALSE)
  })
})

test_that("treefarms returns expected structure", {
  model <- treefarms(test_data$X, test_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Check required fields
  required_fields <- c("model", "predictions", "probabilities", "accuracy", 
                      "loss_function", "regularization", "n_trees", 
                      "X_train", "y_train", "training_time", "training_iterations")
  
  for (field in required_fields) {
    expect_true(field %in% names(model), 
                info = paste("Missing field:", field))
  }
  
  # Check data types
  expect_true(is.numeric(model$n_trees))
  expect_true(is.numeric(model$accuracy))
  expect_true(is.character(model$loss_function))
  expect_true(is.numeric(model$regularization))
  expect_true(is.data.frame(model$X_train))
  expect_true(is.numeric(model$y_train))
  expect_true(is.numeric(model$training_time))
  expect_true(is.numeric(model$training_iterations))
  
  # Check predictions and probabilities
  expect_true(is.numeric(model$predictions))
  expect_true(is.matrix(model$probabilities))
  expect_equal(length(model$predictions), nrow(test_data$X))
  expect_equal(nrow(model$probabilities), nrow(test_data$X))
  expect_equal(ncol(model$probabilities), 2)
})

test_that("treefarms handles edge cases", {
  # Test with minimal data
  X_minimal <- data.frame(f1 = c(0, 1))
  y_minimal <- c(0, 1)
  
  expect_no_error({
    model <- treefarms(X_minimal, y_minimal, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  # Test with all same class
  y_same <- rep(0, nrow(test_data$X))
  expect_no_error({
    model <- treefarms(test_data$X, y_same, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  # Test with matrix input
  X_matrix <- as.matrix(test_data$X)
  expect_no_error({
    model <- treefarms(X_matrix, test_data$y, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
})

test_that("treefarms auto-tuning works", {
  # Test auto-tuning with NULL regularization
  expect_no_error({
    model_auto <- treefarms(test_data$X, test_data$y, 
                           loss_function = "misclassification", 
                           regularization = NULL,
                           verbose = FALSE)
  })
  
  expect_true(is.numeric(model_auto$regularization))
  expect_true(model_auto$regularization > 0)
  
  # Test auto-tuning with NULL rashomon_bound_multiplier
  expect_no_error({
    model_auto2 <- treefarms(test_data$X, test_data$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            rashomon_bound_multiplier = NULL,
                            verbose = FALSE)
  })
  
  expect_true(is.numeric(model_auto2$rashomon_bound_multiplier))
  expect_true(model_auto2$rashomon_bound_multiplier > 0)
})

test_that("treefarms with pattern data generates trees", {
  # Use data with clear pattern
  model <- treefarms(pattern_data$X, pattern_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.01,  # Low regularization to allow trees
                    verbose = FALSE)
  
  # Should generate at least one tree for clear pattern
  expect_true(model$n_trees >= 1)
  expect_true(model$accuracy > 0.5)  # Should be better than random
})

test_that("treefarms verbose output works", {
  # Test that verbose doesn't cause errors
  expect_no_error({
    model <- treefarms(test_data$X, test_data$y, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = TRUE)
  })
})

test_that("treefarms handles different target_trees and max_trees", {
  expect_no_error({
    model <- treefarms(test_data$X, test_data$y, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      target_trees = 2,
                      max_trees = 5,
                      verbose = FALSE)
  })
  
  # Test invalid target_trees
  expect_error(treefarms(test_data$X, test_data$y, target_trees = 0), 
               "target_trees must be at least 1")
  
  # Test invalid max_trees
  expect_error(treefarms(test_data$X, test_data$y, max_trees = 0), 
               "max_trees must be at least 1")
  
  # Test max_trees < target_trees
  expect_error(treefarms(test_data$X, test_data$y, target_trees = 3, max_trees = 2), 
               "max_trees must be at least target_trees")
})

test_that("treefarms model object structure", {
  model <- treefarms(test_data$X, test_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Check model object structure
  expect_true(is.list(model$model))
  expect_true("result_data" %in% names(model$model))
  expect_true("config" %in% names(model$model))
  expect_true("time" %in% names(model$model))
  expect_true("iterations" %in% names(model$model))
  expect_true("size" %in% names(model$model))
  expect_true("status" %in% names(model$model))
})

test_that("treefarms with logical y works", {
  y_logical <- as.logical(test_data$y)
  
  expect_no_error({
    model <- treefarms(test_data$X, y_logical, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  expect_true(is.numeric(model$y_train))  # Should be converted to numeric
})

test_that("treefarms probability bounds are reasonable", {
  model <- treefarms(test_data$X, test_data$y, 
                    loss_function = "log_loss", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Probabilities should be between 0 and 1
  expect_true(all(model$probabilities >= 0))
  expect_true(all(model$probabilities <= 1))
  
  # Probabilities should sum to 1 for each row
  row_sums <- rowSums(model$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("treefarms predictions are binary", {
  model <- treefarms(test_data$X, test_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Predictions should be 0 or 1
  expect_true(all(model$predictions %in% c(0, 1)))
  
  # Predictions should match length of training data
  expect_equal(length(model$predictions), length(test_data$y))
})