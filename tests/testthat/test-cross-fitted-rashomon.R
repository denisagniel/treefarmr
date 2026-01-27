# Test suite for cross_fitted_rashomon functionality
# Tests the primary use case functionality

library(testthat)
library(treefarmr)

# Create test data with clear pattern for cross-fitted Rashomon
set.seed(42)
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

# Create larger test data for cross-validation
large_data <- list(
  X = data.frame(
    feature_1 = sample(0:1, 200, replace = TRUE),
    feature_2 = sample(0:1, 200, replace = TRUE),
    feature_3 = sample(0:1, 200, replace = TRUE)
  ),
  y = sample(0:1, 200, replace = TRUE)
)

test_that("cross_fitted_rashomon basic functionality works", {
  expect_no_error({
    result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  })
  
  # Check result structure
  expect_true(inherits(result, "cf_rashomon"))
  expect_true(is.numeric(result$n_intersecting))
  expect_true(is.numeric(result$K))
  expect_true(is.character(result$loss_function))
  expect_equal(result$loss_function, "misclassification")
})

test_that("cross_fitted_rashomon input validation works", {
  # Test invalid K
  expect_error(cross_fitted_rashomon(pattern_data$X, pattern_data$y, K = 1), 
               "K must be at least 2")
  
  expect_error(cross_fitted_rashomon(pattern_data$X, pattern_data$y, K = 200), 
               "K cannot be larger than the number of observations")
  
  # Test invalid data
  expect_error(cross_fitted_rashomon("invalid", pattern_data$y, K = 3), 
               "X must be a data.frame or matrix")
  
  expect_error(cross_fitted_rashomon(pattern_data$X, "invalid", K = 3), 
               "y must be numeric or logical")
  
  # Test length mismatch
  expect_error(cross_fitted_rashomon(pattern_data$X, pattern_data$y[1:50], K = 3), 
               "Length of y must match number of rows in X")
  
  # Test invalid loss function
  expect_error(cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   loss_function = "invalid", K = 3), 
               "loss_function must be either 'misclassification' or 'log_loss'")
})

test_that("cross_fitted_rashomon returns expected structure", {
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 verbose = FALSE)
  
  # Check required fields
  required_fields <- c("intersecting_trees", "n_intersecting", "K", 
                      "loss_function", "regularization", "rashomon_sizes",
                      "X_train", "y_train", "fold_assignments", "fold_models")
  
  for (field in required_fields) {
    expect_true(field %in% names(result), 
                info = paste("Missing field:", field))
  }
  
  # Check data types
  expect_true(is.numeric(result$n_intersecting))
  expect_true(is.numeric(result$K))
  expect_true(is.character(result$loss_function))
  expect_true(is.numeric(result$regularization))
  expect_true(is.data.frame(result$X_train))
  expect_true(is.numeric(result$y_train))
  expect_true(is.list(result$fold_models))
  expect_true(is.numeric(result$fold_assignments))
})

test_that("cross_fitted_rashomon with different K values", {
  # Test with K = 2
  expect_no_error({
    result2 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                    K = 2,
                                    loss_function = "misclassification", 
                                    regularization = 0.01,
                                    verbose = FALSE)
  })
  
  # Test with K = 4
  expect_no_error({
    result4 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                    K = 4,
                                    loss_function = "misclassification", 
                                    regularization = 0.01,
                                    verbose = FALSE)
  })
  
  expect_equal(result2$K, 2)
  expect_equal(result4$K, 4)
  
  # Should have correct number of fold models
  expect_equal(length(result2$fold_models), 2)
  expect_equal(length(result4$fold_models), 4)
})

test_that("cross_fitted_rashomon prediction works", {
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 verbose = FALSE)
  
  # Create new data for prediction
  X_new <- data.frame(
    x1 = c(0, 1, 0),
    x2 = c(0, 0, 1)
  )
  
  # Test class predictions
  expect_no_error({
    pred_class <- predict(result, X_new, type = "class")
  })
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict(result, X_new, type = "prob")
  })
  
  # Check prediction structure
  expect_true(is.numeric(pred_class))
  expect_true(is.matrix(pred_prob))
  expect_equal(length(pred_class), nrow(X_new))
  expect_equal(nrow(pred_prob), nrow(X_new))
  expect_equal(ncol(pred_prob), 2)
  
  # Predictions should be binary
  expect_true(all(pred_class %in% c(0, 1)))
  
  # Probabilities should be valid
  expect_true(all(pred_prob >= 0))
  expect_true(all(pred_prob <= 1))
  
  # Probabilities should sum to 1
  row_sums <- rowSums(pred_prob)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("cross_fitted_rashomon handles edge cases", {
  # Test with minimal data
  X_minimal <- data.frame(f1 = c(0, 1, 0, 1, 0, 1))
  y_minimal <- c(0, 1, 0, 1, 0, 1)
  
  expect_no_error({
    result <- cross_fitted_rashomon(X_minimal, y_minimal, 
                                   K = 2,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  })
  
  # Test with all same class
  y_same <- rep(0, nrow(pattern_data$X))
  expect_no_error({
    result <- cross_fitted_rashomon(pattern_data$X, y_same, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  })
})

test_that("cross_fitted_rashomon reproducibility works", {
  # Test with same seed
  set.seed(123)
  result1 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                  K = 3,
                                  loss_function = "misclassification", 
                                  regularization = 0.01,
                                  seed = 42,
                                  verbose = FALSE)
  
  set.seed(123)
  result2 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                  K = 3,
                                  loss_function = "misclassification", 
                                  regularization = 0.01,
                                  seed = 42,
                                  verbose = FALSE)
  
  # Results should be identical with same seed
  expect_equal(result1$n_intersecting, result2$n_intersecting)
  expect_equal(result1$rashomon_sizes, result2$rashomon_sizes)
  expect_equal(result1$fold_assignments, result2$fold_assignments)
})

test_that("cross_fitted_rashomon with different loss functions", {
  # Test with misclassification
  result_misclass <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                         K = 3,
                                         loss_function = "misclassification", 
                                         regularization = 0.01,
                                         verbose = FALSE)
  
  # Test with log_loss
  result_logloss <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                        K = 3,
                                        loss_function = "log_loss", 
                                        regularization = 0.01,
                                        verbose = FALSE)
  
  expect_equal(result_misclass$loss_function, "misclassification")
  expect_equal(result_logloss$loss_function, "log_loss")
})

test_that("cross_fitted_rashomon with auto-tuning", {
  # Test with auto-tuning
  expect_no_error({
    result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = NULL,
                                   verbose = FALSE)
  })
  
  expect_true(is.numeric(result$regularization))
  expect_true(result$regularization > 0)
})

test_that("cross_fitted_rashomon fold assignments are valid", {
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 verbose = FALSE)
  
  # Check fold assignments
  fold_assignments <- result$fold_assignments
  expect_equal(length(fold_assignments), nrow(pattern_data$X))
  expect_true(all(fold_assignments >= 1))
  expect_true(all(fold_assignments <= 3))
  
  # Each fold should have roughly equal size
  fold_sizes <- table(fold_assignments)
  expect_true(all(fold_sizes > 0))
  expect_true(max(fold_sizes) - min(fold_sizes) <= 1)
})

test_that("cross_fitted_rashomon with different regularization values", {
  # Test with different regularization values
  result1 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                  K = 3,
                                  loss_function = "misclassification", 
                                  regularization = 0.001,
                                  verbose = FALSE)
  
  result2 <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                  K = 3,
                                  loss_function = "misclassification", 
                                  regularization = 0.1,
                                  verbose = FALSE)
  
  expect_equal(result1$regularization, 0.001)
  expect_equal(result2$regularization, 0.1)
  
  # Different regularization should potentially give different results
  # (though not guaranteed)
  expect_true(is.numeric(result1$n_intersecting))
  expect_true(is.numeric(result2$n_intersecting))
})

test_that("cross_fitted_rashomon verbose output", {
  # Test that verbose doesn't cause errors
  expect_no_error({
    result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = TRUE)
  })
})

test_that("cross_fitted_rashomon with matrix input", {
  # Test with matrix input
  X_matrix <- as.matrix(pattern_data$X)
  
  expect_no_error({
    result <- cross_fitted_rashomon(X_matrix, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  })
  
  expect_true(is.data.frame(result$X_train))  # Should be converted to data.frame
})

test_that("cross_fitted_rashomon with logical y", {
  # Test with logical y
  y_logical <- as.logical(pattern_data$y)
  
  expect_no_error({
    result <- cross_fitted_rashomon(pattern_data$X, y_logical, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  })
  
  expect_true(is.numeric(result$y_train))  # Should be converted to numeric
})

test_that("cross_fitted_rashomon intersecting trees structure", {
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 verbose = FALSE)
  
  # Check intersecting trees structure
  expect_true(is.list(result$intersecting_trees))
  expect_true(is.numeric(result$n_intersecting))
  expect_true(result$n_intersecting >= 0)
  expect_true(result$n_intersecting <= length(result$intersecting_trees))
  
  # Check rashomon sizes
  expect_true(is.numeric(result$rashomon_sizes))
  expect_equal(length(result$rashomon_sizes), result$K)
  expect_true(all(result$rashomon_sizes >= 0))
})

test_that("cross_fitted_rashomon fold models structure", {
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 verbose = FALSE)
  
  # Check fold models structure
  expect_true(is.list(result$fold_models))
  expect_equal(length(result$fold_models), result$K)
  
  # Each fold model should be a treefarms_model
  for (i in seq_along(result$fold_models)) {
    expect_true(inherits(result$fold_models[[i]], "treefarms_model"))
  }
})

test_that("cross_fitted_rashomon with single_tree=TRUE works", {
  # Test with single_tree = TRUE (exactly one tree per fold)
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 single_tree = TRUE,
                                 verbose = FALSE)
  
  # Check that all folds have exactly 1 tree
  for (k in 1:result$K) {
    fold_model <- result$fold_models[[k]]
    expect_equal(fold_model$n_trees, 1, 
                 info = paste("Fold", k, "should have exactly 1 tree"))
    
    trees <- get_rashomon_trees(fold_model)
    expect_equal(length(trees), 1,
                 info = paste("Fold", k, "should have exactly 1 tree in rashomon set"))
  }
  
  # Rashomon sizes should all be 1
  expect_true(all(result$rashomon_sizes == 1))
})

test_that("cross_fitted_rashomon with single_tree=FALSE works", {
  # Test with single_tree = FALSE (rashomon sets per fold)
  result <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                 K = 3,
                                 loss_function = "misclassification", 
                                 regularization = 0.01,
                                 rashomon_bound_multiplier = 0.05,
                                 single_tree = FALSE,
                                 verbose = FALSE)
  
  # Check that all folds have at least 1 tree
  for (k in 1:result$K) {
    fold_model <- result$fold_models[[k]]
    expect_true(fold_model$n_trees >= 1,
                info = paste("Fold", k, "should have at least 1 tree"))
    
    trees <- get_rashomon_trees(fold_model)
    expect_true(length(trees) >= 1,
                info = paste("Fold", k, "should have at least 1 tree in rashomon set"))
    
    # Number of trees should match
    expect_equal(fold_model$n_trees, length(trees),
                 info = paste("Fold", k, "n_trees should match rashomon set size"))
  }
  
  # Rashomon sizes should all be >= 1
  expect_true(all(result$rashomon_sizes >= 1))
})

test_that("cross_fitted_rashomon single_tree parameter defaults correctly", {
  # Test default behavior (single_tree = FALSE)
  result_default <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                         K = 3,
                                         loss_function = "misclassification", 
                                         regularization = 0.01,
                                         rashomon_bound_multiplier = 0.05,
                                         verbose = FALSE)
  
  # Should allow rashomon sets (may have multiple trees)
  expect_true(all(result_default$rashomon_sizes >= 1))
  
  # Compare with explicit single_tree = FALSE
  result_explicit <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                           K = 3,
                                           loss_function = "misclassification", 
                                           regularization = 0.01,
                                           rashomon_bound_multiplier = 0.05,
                                           single_tree = FALSE,
                                           verbose = FALSE)
  
  # Both should have same structure
  expect_equal(result_default$K, result_explicit$K)
  expect_equal(length(result_default$fold_models), length(result_explicit$fold_models))
})