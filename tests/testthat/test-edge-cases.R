# Test suite for edge cases and error handling
# Tests invalid inputs, extreme parameters, and boundary conditions

library(testthat)
library(treefarmr)

# Setup test environment
setup_test_environment()

test_that("empty dataset handling", {
  # Test with empty dataset (0 rows)
  expect_error(treefarms(empty_dataset$X, empty_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Length of y must match number of rows in X")
})

test_that("single row dataset handling", {
  # Test with single row dataset
  expect_no_error({
    model <- safe_treefarms(single_row_dataset$X, single_row_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("all same class labels handling", {
  # Test with all same class labels
  expect_no_error({
    model <- safe_treefarms(single_class_dataset$X, single_class_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Should handle single class gracefully
  expect_true(is.finite(model$accuracy))
})

test_that("minimal features handling", {
  # Test with single feature
  expect_no_error({
    model <- safe_treefarms(one_feature_dataset$X, one_feature_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("many features handling", {
  # Test with many features
  expect_no_error({
    model <- safe_treefarms(many_features_dataset$X, many_features_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Should handle many features without issues
  expect_true(is.finite(model$accuracy))
})

test_that("extreme regularization values", {
  # Test with very high regularization
  expect_no_error({
    model_high <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 10.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_high)
  
  # Test with zero regularization
  expect_no_error({
    model_zero <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_zero)
  
  # Test with very low regularization
  expect_no_error({
    model_low <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                               loss_function = "misclassification", 
                               regularization = 0.001,
                               verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_low)
})

test_that("extreme rashomon_bound_multiplier values", {
  # Test with zero rashomon_bound_multiplier
  expect_no_error({
    model_zero <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.1,
                                rashomon_bound_multiplier = 0.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_zero)
  
  # Test with very high rashomon_bound_multiplier
  expect_no_error({
    model_high <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.1,
                                rashomon_bound_multiplier = 1.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_high)
})

test_that("invalid inputs raise informative errors", {
  # Test non-binary features
  X_invalid <- simple_dataset$X
  X_invalid$feature_1[1] <- 2
  expect_error(treefarms(X_invalid, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
  
  # Test non-binary labels
  y_invalid <- c(simple_dataset$y, 3)
  expect_error(treefarms(simple_dataset$X, y_invalid, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must contain only binary values")
  
  # Test mismatched dimensions
  expect_error(treefarms(simple_dataset$X, simple_dataset$y[1:50], 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Length of y must match number of rows in X")
  
  # Test NULL features
  expect_error(treefarms(NULL, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "X must be a data.frame or matrix")
  
  # Test NULL labels
  expect_error(treefarms(simple_dataset$X, NULL, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must be numeric or logical")
})

test_that("missing values handling", {
  # Test with NA in features
  X_na <- simple_dataset$X
  X_na$feature_1[1] <- NA
  expect_error(treefarms(X_na, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
  
  # Test with NA in labels
  y_na <- simple_dataset$y
  y_na[1] <- NA
  expect_error(treefarms(simple_dataset$X, y_na, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must contain only binary values")
})

test_that("empty column names handling", {
  # Test with empty column names
  X_empty_names <- simple_dataset$X
  names(X_empty_names) <- c("", "feature_2", "feature_3")
  
  expect_no_error({
    model <- safe_treefarms(X_empty_names, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("numeric stability tests", {
  # Test with all zeros
  expect_no_error({
    model_zeros <- safe_treefarms(all_zeros_dataset$X, all_zeros_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_zeros)
  
  # Test with all ones
  expect_no_error({
    model_ones <- safe_treefarms(all_ones_dataset$X, all_ones_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.1,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_ones)
})

test_that("extreme target_trees and max_trees values", {
  # Test with very high target_trees
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           target_trees = 100,
                           max_trees = 200,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with target_trees = max_trees
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           target_trees = 5,
                           max_trees = 5,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("extreme worker_limit values", {
  # Test with very high worker_limit
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           worker_limit = 100L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with log-loss", {
  # Test log-loss with extreme regularization
  expect_no_error({
    model_high <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "log_loss", 
                                regularization = 10.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_high, "log_loss")
  
  # Test log-loss with zero regularization
  expect_no_error({
    model_zero <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                loss_function = "log_loss", 
                                regularization = 0.0,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_zero, "log_loss")
  
  # Test log-loss with single class data
  expect_no_error({
    model <- safe_treefarms(single_class_dataset$X, single_class_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
})

test_that("NaN values in inputs raise errors", {
  # Test NaN in features
  X_nan <- simple_dataset$X
  X_nan$feature_1[1] <- NaN
  expect_error(treefarms(X_nan, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
  
  # Test NaN in labels
  y_nan <- simple_dataset$y
  y_nan[1] <- NaN
  expect_error(treefarms(simple_dataset$X, y_nan, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must contain only binary values")
})

test_that("Inf values in inputs raise errors", {
  # Test Inf in features
  X_inf <- simple_dataset$X
  X_inf$feature_1[1] <- Inf
  expect_error(treefarms(X_inf, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
  
  # Test Inf in labels
  y_inf <- simple_dataset$y
  y_inf[1] <- Inf
  expect_error(treefarms(simple_dataset$X, y_inf, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must contain only binary values")
})

test_that("negative values in inputs raise errors", {
  # Test negative in features
  X_neg <- simple_dataset$X
  X_neg$feature_1[1] <- -1
  expect_error(treefarms(X_neg, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
  
  # Test negative in labels
  y_neg <- simple_dataset$y
  y_neg[1] <- -1
  expect_error(treefarms(simple_dataset$X, y_neg, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "y must contain only binary values")
})

test_that("edge cases with auto-tuning", {
  # Test auto-tuning with extreme target values
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = NULL,
                           target_trees = 1,
                           max_trees = 100,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test auto-tuning with single class data
  expect_no_error({
    model <- safe_treefarms(single_class_dataset$X, single_class_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = NULL,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with imbalanced data", {
  # Test with highly imbalanced data
  expect_no_error({
    model <- safe_treefarms(imbalanced_dataset$X, imbalanced_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with imbalanced data and log-loss
  expect_no_error({
    model <- safe_treefarms(imbalanced_dataset$X, imbalanced_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
})

test_that("edge cases with matrix input", {
  # Test with matrix input
  X_matrix <- as.matrix(simple_dataset$X)
  expect_no_error({
    model <- safe_treefarms(X_matrix, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with single-column matrix
  X_single_col <- as.matrix(one_feature_dataset$X)
  expect_no_error({
    model <- safe_treefarms(X_single_col, one_feature_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with logical y", {
  # Test with logical y
  y_logical <- as.logical(simple_dataset$y)
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, y_logical, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with all TRUE logical y
  y_all_true <- rep(TRUE, nrow(simple_dataset$X))
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, y_all_true, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with all FALSE logical y
  y_all_false <- rep(FALSE, nrow(simple_dataset$X))
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, y_all_false, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with verbose output", {
  # Test verbose output with edge cases
  expect_no_error({
    model <- safe_treefarms(single_row_dataset$X, single_row_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = TRUE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test verbose output with extreme regularization
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 10.0,
                           verbose = TRUE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with different data types", {
  # Test with integer data
  X_int <- simple_dataset$X
  X_int$feature_1 <- as.integer(X_int$feature_1)
  X_int$feature_2 <- as.integer(X_int$feature_2)
  X_int$feature_3 <- as.integer(X_int$feature_3)
  
  expect_no_error({
    model <- safe_treefarms(X_int, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with factor data (should fail)
  X_factor <- simple_dataset$X
  X_factor$feature_1 <- as.factor(X_factor$feature_1)
  
  expect_error(treefarms(X_factor, simple_dataset$y, 
                        loss_function = "misclassification", 
                        regularization = 0.1,
                        verbose = FALSE),
               "Feature feature_1 must contain only binary values")
})

test_that("edge cases with very small datasets", {
  # Test with 2x2 dataset
  X_tiny <- data.frame(f1 = c(0, 1), f2 = c(1, 0))
  y_tiny <- c(0, 1)
  
  expect_no_error({
    model <- safe_treefarms(X_tiny, y_tiny, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with 1x1 dataset (single sample, single feature)
  X_single <- data.frame(f1 = 1)
  y_single <- 1
  
  expect_no_error({
    model <- safe_treefarms(X_single, y_single, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("edge cases with extreme parameter combinations", {
  # Test with very low regularization and very low rashomon_bound_multiplier
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.001,
                           rashomon_bound_multiplier = 0.001,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with very high regularization and very high rashomon_bound_multiplier
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 10.0,
                           rashomon_bound_multiplier = 1.0,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

# Cleanup
teardown_test_environment()
