# Test suite for optimaltrees package
# Tests the main treefarms functionality with Rcpp implementation

library(testthat)
library(optimaltrees)

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

test_that("optimaltrees basic functionality works", {
  # Test with misclassification loss
  expect_no_error({
    model <- optimaltrees(test_data$X, test_data$y,
                      loss_function = "misclassification",
                      regularization = 0.1,
                      verbose = FALSE)
  })

  # Check model structure
  expect_true(S7::S7_inherits(model, OptimalTreesModel))
  expect_true(is.numeric(model@n_trees))
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_equal(model@loss_function, "misclassification")

  # Test with log_loss
  expect_no_error({
    model_log <- optimaltrees(test_data$X, test_data$y,
                          loss_function = "log_loss",
                          regularization = 0.1,
                          verbose = FALSE)
  })

  expect_equal(model_log$loss_function, "log_loss")
})

test_that("optimaltrees input validation works", {
  # Test invalid X
  expect_error(optimaltrees("invalid", test_data$y), "X must be a data.frame or matrix")
  
  # Test invalid y
  expect_error(optimaltrees(test_data$X, "invalid"), "y must be numeric or logical")
  
  # Test length mismatch
  expect_error(optimaltrees(test_data$X, test_data$y[1:50]), "Length of y must match number of rows in X")
  
  # Test non-binary y (for classification)
  # Note: continuous features are automatically discretized, so we can't test non-binary features
  y_invalid <- test_data$y
  y_invalid[1] <- 2  # Keep same length but invalid value
  expect_error(optimaltrees(test_data$X, y_invalid), "y must contain only binary values")

  # Test invalid loss function
  expect_error(optimaltrees(test_data$X, test_data$y, loss_function = "invalid"),
               "loss_function must be one of")
})

test_that("optimaltrees with different parameters", {
  # Test with different regularization values
  expect_no_error({
    model1 <- optimaltrees(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.05,
                       verbose = FALSE)
  })
  
  expect_no_error({
    model2 <- optimaltrees(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.2,
                       verbose = FALSE)
  })
  
  # Models should have different properties
  expect_true(model1$regularization != model2$regularization)
  
  # Test with different rashomon_bound_multiplier
  expect_no_error({
    model3 <- optimaltrees(test_data$X, test_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.1,
                       rashomon_bound_multiplier = 0.1,
                       verbose = FALSE)
  })
})

test_that("optimaltrees returns expected structure", {
  # Use store_training_data=TRUE to test y_train
  model <- optimaltrees(test_data$X, test_data$y,
                    loss_function = "misclassification",
                    regularization = 0.1,
                    store_training_data = TRUE,
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
  expect_true(is.numeric(model@n_trees))
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_true(is.numeric(model@regularization))
  expect_true(is.data.frame(model@X_train))
  expect_true(is.numeric(model@y_train))  # Only numeric when store_training_data=TRUE
  expect_true(is.numeric(model@training_time))
  expect_true(is.numeric(model@training_iterations))

  # Check predictions and probabilities
  expect_true(is.numeric(model@predictions))
  expect_true(is.matrix(model@probabilities))
  expect_equal(length(model@predictions), nrow(test_data$X))
  expect_equal(nrow(model@probabilities), nrow(test_data$X))
  expect_equal(ncol(model@probabilities), 2)
})

test_that("optimaltrees handles edge cases", {
  # Test with minimal data
  X_minimal <- data.frame(f1 = c(0, 1))
  y_minimal <- c(0, 1)
  
  expect_no_error({
    model <- optimaltrees(X_minimal, y_minimal, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  # Test with all same class
  y_same <- rep(0, nrow(test_data$X))
  expect_no_error({
    model <- optimaltrees(test_data$X, y_same, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
  
  # Test with matrix input
  X_matrix <- as.matrix(test_data$X)
  expect_no_error({
    model <- optimaltrees(X_matrix, test_data$y, 
                      loss_function = "misclassification", 
                      regularization = 0.1,
                      verbose = FALSE)
  })
})

test_that("optimaltrees auto-tuning works", {
  # Test auto-tuning with NULL regularization
  # This triggers internal auto_tune_optimaltrees call
  expect_no_error({
    model_auto <- optimaltrees(test_data$X, test_data$y,
                           loss_function = "misclassification",
                           regularization = NULL,
                           verbose = FALSE)
  })

  # Check that a regularization value was selected
  expect_true(is.numeric(model_auto$regularization))
  expect_true(model_auto$regularization > 0)

  # Test auto-tuning with NULL rashomon_bound_multiplier
  expect_no_error({
    result_auto2 <- optimaltrees(test_data$X, test_data$y,
                            loss_function = "misclassification",
                            regularization = 0.1,
                            rashomon_bound_multiplier = NULL,
                            verbose = FALSE)
  })

  # Auto-tuning returns a list with model, rashomon_bound_multiplier, etc.
  expect_true(is.list(result_auto2))
  expect_true("model" %in% names(result_auto2))
  expect_true(inherits(result_auto2$model, "optimaltrees_model"))
})

test_that("optimaltrees with pattern data generates trees", {
  # Use data with clear pattern
  model <- optimaltrees(pattern_data$X, pattern_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.01,  # Low regularization to allow trees
                    verbose = FALSE)
  
  # Should generate at least one tree for clear pattern
  expect_true(model@n_trees >= 1)
  expect_true(model@accuracy > 0.5)  # Should be better than random
})

test_that("optimaltrees verbose output works", {
  # Test that verbose doesn't cause errors
  expect_no_error({
    model <- optimaltrees(test_data$X, test_data$y,
                      loss_function = "misclassification",
                      regularization = 0.1,
                      verbose = TRUE)
  })
})

test_that("optimaltrees handles different target_trees and max_trees", {
  # target_trees and max_trees are used in auto-tuning
  # Just verify they don't cause errors when passed
  expect_no_error({
    model <- optimaltrees(test_data$X, test_data$y,
                      loss_function = "misclassification",
                      regularization = 0.1,
                      target_trees = 2,
                      max_trees = 5,
                      verbose = FALSE)
  })

  # Note: Validation of target_trees and max_trees happens in auto_tune_optimaltrees,
  # not in the main optimaltrees function. So we don't test validation here.
})

test_that("optimaltrees model object structure", {
  model <- optimaltrees(test_data$X, test_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Check model object structure
  expect_true(is.list(model@model))
  expect_true("result_data" %in% names(model@model))
  expect_true("config" %in% names(model@model))
  expect_true("time" %in% names(model@model))
  expect_true("iterations" %in% names(model@model))
  expect_true("size" %in% names(model@model))
  expect_true("status" %in% names(model@model))
})

test_that("optimaltrees with logical y works", {
  y_logical <- as.logical(test_data$y)

  expect_no_error({
    model <- optimaltrees(test_data$X, y_logical,
                      loss_function = "misclassification",
                      regularization = 0.1,
                      store_training_data = TRUE,  # Need this to check y_train
                      verbose = FALSE)
  })

  expect_true(is.numeric(model@y_train))  # Should be converted to numeric
})

test_that("optimaltrees probability bounds are reasonable", {
  model <- optimaltrees(test_data$X, test_data$y, 
                    loss_function = "log_loss", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Probabilities should be between 0 and 1
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
  
  # Probabilities should sum to 1 for each row
  row_sums <- rowSums(model@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("optimaltrees predictions are binary", {
  model <- optimaltrees(test_data$X, test_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Predictions should be 0 or 1
  expect_true(all(model@predictions %in% c(0, 1)))
  
  # Predictions should match length of training data
  expect_equal(length(model@predictions), length(test_data$y))
})