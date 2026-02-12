# Test suite for R-level interface and basic workflows
# Tests the main treefarms() function and its parameters

library(testthat)
library(treefarmr)

# Setup test environment
setup_test_environment()

test_that("treefarms basic functionality works", {
  # Test with misclassification loss
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "misclassification")
  
  # Test with log_loss
  expect_no_error({
    model_log <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                               loss_function = "log_loss", 
                               regularization = 0.1,
                               verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_log, "log_loss")
})

test_that("treefarms input validation works", {
  # Test invalid X
  expect_error(treefarms("invalid", simple_dataset$y), 
               "X must be a data.frame or matrix")
  
  # Test invalid y
  expect_error(treefarms(simple_dataset$X, "invalid"), 
               "y must be numeric or logical")
  
  # Test length mismatch
  expect_error(treefarms(simple_dataset$X, simple_dataset$y[1:50]), 
               "Length of y must match number of rows in X")
  
  # Test non-binary y
  expect_error(treefarms(simple_dataset$X, c(simple_dataset$y, 2)), 
               "y must contain only binary values")
  
  # Test non-binary features
  X_invalid <- simple_dataset$X
  X_invalid$feature_1[1] <- 2
  expect_error(treefarms(X_invalid, simple_dataset$y), 
               "Feature feature_1 must contain only binary values")
  
  # Test invalid loss function
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, loss_function = "invalid"), 
               "loss_function must be either 'misclassification' or 'log_loss'")
  
  # Test negative regularization
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, regularization = -0.1), 
               "regularization must be non-negative")
  
  # Test negative rashomon_bound_multiplier
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, rashomon_bound_multiplier = -0.1), 
               "rashomon_bound_multiplier must be non-negative")
})

test_that("treefarms with different parameters", {
  # Test with different regularization values
  expect_no_error({
    model1 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.05,
                            verbose = FALSE)
  })
  
  expect_no_error({
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.2,
                            verbose = FALSE)
  })
  
  # Models should have different properties
  expect_true(model1$regularization != model2$regularization)
  
  # Test with different rashomon_bound_multiplier
  expect_no_error({
    model3 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            rashomon_bound_multiplier = 0.1,
                            verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model3)
})

test_that("treefarms returns expected structure", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model)
  
  # Check model object structure
  expect_true(is.list(model$model))
  expect_true("result_data" %in% names(model$model))
  expect_true("config" %in% names(model$model))
  expect_true("time" %in% names(model$model))
  expect_true("iterations" %in% names(model$model))
  expect_true("size" %in% names(model$model))
  expect_true("status" %in% names(model$model))
})

test_that("treefarms handles edge cases", {
  # Test with minimal data
  expect_no_error({
    model <- safe_treefarms(minimal_dataset$X, minimal_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with all same class
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, single_class_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with matrix input
  X_matrix <- as.matrix(simple_dataset$X)
  expect_no_error({
    model <- safe_treefarms(X_matrix, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("treefarms auto-tuning works", {
  # Test auto-tuning with NULL regularization
  expect_no_error({
    model_auto <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = NULL,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_auto)
  expect_true(is.numeric(model_auto$regularization))
  expect_true(model_auto$regularization > 0)
  
  # Test auto-tuning with NULL rashomon_bound_multiplier
  expect_no_error({
    model_auto2 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 rashomon_bound_multiplier = NULL,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_auto2)
  expect_true(is.numeric(model_auto2$rashomon_bound_multiplier))
  expect_true(model_auto2$rashomon_bound_multiplier > 0)
})

test_that("treefarms with pattern data generates trees", {
  # Use data with clear pattern
  model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.01,  # Low regularization to allow trees
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model)
  
  # Should generate at least one tree for clear pattern
  expect_true(model$n_trees >= 1)
  expect_true(model$accuracy > 0.5)  # Should be better than random
})

test_that("treefarms verbose output works", {
  # Test that verbose doesn't cause errors
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = TRUE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("treefarms handles different target_trees and max_trees", {
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           target_trees = 2,
                           max_trees = 5,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test invalid target_trees
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, target_trees = 0), 
               "target_trees must be at least 1")
  
  # Test invalid max_trees
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, max_trees = 0), 
               "max_trees must be at least 1")
  
  # Test max_trees < target_trees
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, target_trees = 3, max_trees = 2), 
               "max_trees must be at least target_trees")
})

test_that("treefarms with logical y works", {
  y_logical <- as.logical(simple_dataset$y)
  
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, y_logical, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  # y_train is numeric when stored, or NULL when store_training_data=FALSE
  expect_true(is.numeric(model$y_train) || is.null(model$y_train))
})

test_that("treefarms probability bounds are reasonable", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Probabilities should be between 0 and 1
  expect_true(all(model$probabilities >= 0))
  expect_true(all(model$probabilities <= 1))
  
  # Probabilities should sum to 1 for each row
  row_sums <- rowSums(model$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("treefarms predictions are binary", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model)
  
  # Predictions should be 0 or 1
  expect_true(all(model$predictions %in% c(0, 1)))
  
  # Predictions should match length of training data
  expect_equal(length(model$predictions), length(simple_dataset$y))
})

test_that("treefarms handles different data sizes", {
  # Test with small dataset
  expect_no_error({
    model_small <- safe_treefarms(minimal_dataset$X, minimal_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_small)
  
  # Test with larger dataset
  expect_no_error({
    model_large <- safe_treefarms(many_features_dataset$X, many_features_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_large)
})

test_that("treefarms handles imbalanced data", {
  expect_no_error({
    model <- safe_treefarms(imbalanced_dataset$X, imbalanced_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Should handle imbalanced data without crashing
  expect_true(is.finite(model$accuracy))
})

test_that("treefarms parameter ranges are validated", {
  # Test zero regularization
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.0,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test very high regularization
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 10.0,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test zero rashomon_bound_multiplier
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           rashomon_bound_multiplier = 0.0,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

# Cleanup
teardown_test_environment()
