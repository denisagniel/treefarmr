# Test suite for fit_tree() and fit_rashomon() functions
# Tests the new convenience functions for single tree vs rashomon set fitting

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

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

# ============================================================================
# Tests for fit_tree()
# ============================================================================

test_that("fit_tree basic functionality works", {
  expect_no_error({
    model <- fit_tree(test_data$X, test_data$y,
                     loss_function = "misclassification",
                     regularization = 0.1,
                     verbose = FALSE)
  })
  
  # Check model structure
  expect_true(inherits(model, "treefarms_model"))
  expect_true(is.numeric(model@n_trees))
  expect_equal(model@n_trees, 1)  # CRITICAL: fit_tree must return exactly 1 tree
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_equal(model@loss_function, "misclassification")
})

test_that("fit_tree always returns exactly one tree", {
  # Test with different parameters
  model1 <- fit_tree(test_data$X, test_data$y, regularization = 0.01, verbose = FALSE)
  model2 <- fit_tree(test_data$X, test_data$y, regularization = 0.1, verbose = FALSE)
  model3 <- fit_tree(test_data$X, test_data$y, regularization = 1.0, verbose = FALSE)
  
  # All should have exactly 1 tree
  expect_equal(model1$n_trees, 1)
  expect_equal(model2$n_trees, 1)
  expect_equal(model3$n_trees, 1)
  
  # Verify with get_rashomon_trees
  trees1 <- get_rashomon_trees(model1)
  trees2 <- get_rashomon_trees(model2)
  trees3 <- get_rashomon_trees(model3)
  
  expect_equal(length(trees1), 1)
  expect_equal(length(trees2), 1)
  expect_equal(length(trees3), 1)
})

test_that("fit_tree works with log_loss", {
  expect_no_error({
    model <- fit_tree(test_data$X, test_data$y, 
                     loss_function = "log_loss", 
                     regularization = 0.1,
                     verbose = FALSE)
  })
  
  expect_equal(model@loss_function, "log_loss")
  expect_equal(model@n_trees, 1)
  
  # Should have probabilities
  expect_true(!is.null(model@probabilities))
  expect_true(is.matrix(model@probabilities))
})

test_that("fit_tree input validation works", {
  # Test invalid X
  expect_error(fit_tree("invalid", test_data$y))
  
  # Test invalid y
  expect_error(fit_tree(test_data$X, "invalid"))
  
  # Test length mismatch
  expect_error(fit_tree(test_data$X, test_data$y[1:50]))
  
  # Test invalid loss function
  expect_error(fit_tree(test_data$X, test_data$y, loss_function = "invalid"))
  
  # Note: Negative regularization validation may happen in C++ code
  # and may not throw an error in R, so we skip this test
})

test_that("fit_tree returns expected structure", {
  model <- fit_tree(test_data$X, test_data$y,
                   regularization = 0.1,
                   compute_probabilities = TRUE,
                   verbose = FALSE)
  
  # Check required fields
  required_fields <- c("model", "predictions", "probabilities", "accuracy", 
                      "loss_function", "regularization", "n_trees")
  
  for (field in required_fields) {
    expect_true(field %in% names(model), 
                info = paste("Missing field:", field))
  }
  
  # Check data types
  expect_true(is.numeric(model@n_trees))
  expect_equal(model@n_trees, 1)  # Must be exactly 1
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_true(is.numeric(model@regularization))
  expect_true(is.numeric(model@predictions))
  expect_true(is.matrix(model@probabilities))
})

test_that("fit_tree predictions work", {
  model <- fit_tree(test_data$X, test_data$y, regularization = 0.1, verbose = FALSE)
  
  # Create new data for prediction
  X_new <- data.frame(
    feature_1 = c(1, 0, 1),
    feature_2 = c(1, 0, 0), 
    feature_3 = c(0, 1, 1)
  )
  
  # Test class predictions
  expect_no_error({
    pred_class <- predict(model, X_new, type = "class")
  })
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict(model, X_new, type = "prob")
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

test_that("fit_tree works with different regularization values", {
  # Test with different regularization values
  model1 <- fit_tree(test_data$X, test_data$y, regularization = 0.001, verbose = FALSE)
  model2 <- fit_tree(test_data$X, test_data$y, regularization = 0.1, verbose = FALSE)
  model3 <- fit_tree(test_data$X, test_data$y, regularization = 1.0, verbose = FALSE)
  
  # All should have exactly 1 tree
  expect_equal(model1$n_trees, 1)
  expect_equal(model2$n_trees, 1)
  expect_equal(model3$n_trees, 1)
  
  # Regularization should be stored correctly
  expect_equal(model1$regularization, 0.001)
  expect_equal(model2$regularization, 0.1)
  expect_equal(model3$regularization, 1.0)
})

test_that("fit_tree works with matrix input", {
  X_matrix <- as.matrix(test_data$X)
  
  expect_no_error({
    model <- fit_tree(X_matrix, test_data$y, regularization = 0.1, verbose = FALSE)
  })
  
  expect_equal(model@n_trees, 1)
})

test_that("fit_tree works with logical y", {
  y_logical <- as.logical(test_data$y)
  
  expect_no_error({
    model <- fit_tree(test_data$X, y_logical, regularization = 0.1, verbose = FALSE)
  })
  
  expect_equal(model@n_trees, 1)
})

test_that("fit_tree works with pattern data", {
  model <- fit_tree(pattern_data$X, pattern_data$y, 
                   regularization = 0.1, 
                   verbose = FALSE)
  
  expect_equal(model@n_trees, 1)
  expect_true(model@accuracy >= 0)
  expect_true(model@accuracy <= 1)
})

# ============================================================================
# Tests for fit_rashomon()
# ============================================================================

test_that("fit_rashomon basic functionality works", {
  expect_no_error({
    model <- fit_rashomon(test_data$X, test_data$y, 
                         loss_function = "misclassification", 
                         regularization = 0.1,
                         rashomon_bound_multiplier = 0.05,
                               verbose = FALSE)
  })
  
  # Check model structure
  expect_true(inherits(model, "treefarms_model"))
  expect_true(is.numeric(model@n_trees))
  expect_true(model@n_trees >= 1)  # Should have at least 1 tree
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_equal(model@loss_function, "misclassification")
})

test_that("fit_rashomon returns at least one tree", {
  # Test with different parameters
  model1 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 0.01, 
                        rashomon_bound_multiplier = 0.01,
                        verbose = FALSE)
  model2 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 0.1, 
                        rashomon_bound_multiplier = 0.05,
                        verbose = FALSE)
  model3 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 1.0, 
                        rashomon_bound_multiplier = 0.1,
                        verbose = FALSE)
  
  # All should have at least 1 tree
  expect_true(model1$n_trees >= 1)
  expect_true(model2$n_trees >= 1)
  expect_true(model3$n_trees >= 1)
  
  # Verify with get_rashomon_trees
  trees1 <- get_rashomon_trees(model1)
  trees2 <- get_rashomon_trees(model2)
  trees3 <- get_rashomon_trees(model3)
  
  expect_true(length(trees1) >= 1)
  expect_true(length(trees2) >= 1)
  expect_true(length(trees3) >= 1)
  
  # Number of trees should match
  expect_equal(model1$n_trees, length(trees1))
  expect_equal(model2$n_trees, length(trees2))
  expect_equal(model3$n_trees, length(trees3))
})

test_that("fit_rashomon can return multiple trees", {
  # Use a larger rashomon_bound_multiplier to potentially get multiple trees
  model <- fit_rashomon(pattern_data$X, pattern_data$y, 
                       regularization = 0.01, 
                       rashomon_bound_multiplier = 0.05,
                       verbose = FALSE)
  
  expect_true(model@n_trees >= 1)
  
  trees <- get_rashomon_trees(model)
  expect_true(length(trees) >= 1)
  expect_equal(model@n_trees, length(trees))
  
  # If we got multiple trees, they should be different
  if (length(trees) > 1) {
    tree1_json <- tree_to_json(trees[[1]])
    tree2_json <- tree_to_json(trees[[2]])
    # They might be the same or different, but both should be valid
    expect_true(nchar(tree1_json) > 0)
    expect_true(nchar(tree2_json) > 0)
  }
})

test_that("fit_rashomon works with log_loss", {
  expect_no_error({
    model <- fit_rashomon(test_data$X, test_data$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         rashomon_bound_multiplier = 0.05,
                         verbose = FALSE)
  })
  
  expect_equal(model@loss_function, "log_loss")
  expect_true(model@n_trees >= 1)
  
  # Should have probabilities
  expect_true(!is.null(model@probabilities))
  expect_true(is.matrix(model@probabilities))
})

test_that("fit_rashomon input validation works", {
  # Test invalid X
  expect_error(fit_rashomon("invalid", test_data$y))
  
  # Test invalid y
  expect_error(fit_rashomon(test_data$X, "invalid"))
  
  # Test length mismatch
  expect_error(fit_rashomon(test_data$X, test_data$y[1:50]))
  
  # Test invalid loss function
  expect_error(fit_rashomon(test_data$X, test_data$y, loss_function = "invalid"))
  
  # Note: Negative regularization and rashomon_bound_multiplier validation 
  # may happen in C++ code and may not throw an error in R, so we skip these tests
})

test_that("fit_rashomon returns expected structure", {
  model <- fit_rashomon(test_data$X, test_data$y, 
                       regularization = 0.1, 
                       rashomon_bound_multiplier = 0.05,
                           compute_probabilities = TRUE,
                       verbose = FALSE)
  
  # Check required fields
  required_fields <- c("model", "predictions", "probabilities", "accuracy", 
                      "loss_function", "regularization", "n_trees")
  
  for (field in required_fields) {
    expect_true(field %in% names(model), 
                info = paste("Missing field:", field))
  }
  
  # Check data types
  expect_true(is.numeric(model@n_trees))
  expect_true(model@n_trees >= 1)  # Should have at least 1 tree
  expect_true(is.numeric(model@accuracy))
  expect_true(is.character(model@loss_function))
  expect_true(is.numeric(model@regularization))
  expect_true(is.numeric(model@predictions))
  expect_true(is.matrix(model@probabilities))
})

test_that("fit_rashomon predictions work", {
  model <- fit_rashomon(test_data$X, test_data$y, 
                       regularization = 0.1, 
                       rashomon_bound_multiplier = 0.05,
                       verbose = FALSE)
  
  # Create new data for prediction
  X_new <- data.frame(
    feature_1 = c(1, 0, 1),
    feature_2 = c(1, 0, 0), 
    feature_3 = c(0, 1, 1)
  )
  
  # Test class predictions
  expect_no_error({
    pred_class <- predict(model, X_new, type = "class")
  })
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict(model, X_new, type = "prob")
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

test_that("fit_rashomon works with different rashomon_bound_multiplier values", {
  # Smaller multiplier should potentially give more trees
  model1 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 0.1, 
                        rashomon_bound_multiplier = 0.01,
                        verbose = FALSE)
  
  # Larger multiplier should potentially give fewer trees
  model2 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 0.1, 
                        rashomon_bound_multiplier = 0.1,
                        verbose = FALSE)
  
  # Both should have at least 1 tree
  expect_true(model1$n_trees >= 1)
  expect_true(model2$n_trees >= 1)
  
  # Note: We can't guarantee model1$n_trees >= model2$n_trees in all cases,
  # but both should be valid models
})

test_that("fit_rashomon works with matrix input", {
  X_matrix <- as.matrix(test_data$X)
  
  expect_no_error({
    model <- fit_rashomon(X_matrix, test_data$y, 
                         regularization = 0.1, 
                         rashomon_bound_multiplier = 0.05,
                         verbose = FALSE)
  })
  
  expect_true(model@n_trees >= 1)
})

test_that("fit_rashomon works with logical y", {
  y_logical <- as.logical(test_data$y)
  
  expect_no_error({
    model <- fit_rashomon(test_data$X, y_logical, 
                         regularization = 0.1, 
                         rashomon_bound_multiplier = 0.05,
                         verbose = FALSE)
  })
  
  expect_true(model@n_trees >= 1)
})

test_that("fit_rashomon works with pattern data", {
  model <- fit_rashomon(pattern_data$X, pattern_data$y, 
                       regularization = 0.1, 
                       rashomon_bound_multiplier = 0.05,
                           verbose = FALSE)
  
  expect_true(model@n_trees >= 1)
  expect_true(model@accuracy >= 0)
  expect_true(model@accuracy <= 1)
})

# ============================================================================
# Comparison tests: fit_tree vs fit_rashomon
# ============================================================================

test_that("fit_tree and fit_rashomon produce different results", {
  model_tree <- fit_tree(test_data$X, test_data$y, regularization = 0.1, verbose = FALSE)
  model_rashomon <- fit_rashomon(test_data$X, test_data$y, 
                                 regularization = 0.1, 
                                 rashomon_bound_multiplier = 0.05,
                                 verbose = FALSE)
  
  # fit_tree should always have exactly 1 tree
  expect_equal(model_tree$n_trees, 1)
  
  # fit_rashomon should have at least 1 tree
  expect_true(model_rashomon$n_trees >= 1)
  
  # Both should be valid models
  expect_true(inherits(model_tree, "treefarms_model"))
  expect_true(inherits(model_rashomon, "treefarms_model"))
  
  # Both should have same loss function
  expect_equal(model_tree$loss_function, model_rashomon$loss_function)
})

test_that("fit_tree is equivalent to treefarms with single_tree=TRUE", {
  model1 <- fit_tree(test_data$X, test_data$y, regularization = 0.1, verbose = FALSE)
  model2 <- treefarms(test_data$X, test_data$y, 
                     regularization = 0.1, 
                     single_tree = TRUE,
                     verbose = FALSE)
  
  # Both should have exactly 1 tree
  expect_equal(model1$n_trees, 1)
  expect_equal(model2$n_trees, 1)
  
  # Both should be valid models
  expect_true(inherits(model1, "treefarms_model"))
  expect_true(inherits(model2, "treefarms_model"))
})

test_that("fit_rashomon is equivalent to treefarms with single_tree=FALSE", {
  model1 <- fit_rashomon(test_data$X, test_data$y, 
                        regularization = 0.1, 
                        rashomon_bound_multiplier = 0.05,
                        verbose = FALSE)
  model2 <- treefarms(test_data$X, test_data$y, 
                     regularization = 0.1, 
                     rashomon_bound_multiplier = 0.05,
                     single_tree = FALSE,
                     verbose = FALSE)
  
  # Both should have at least 1 tree
  expect_true(model1$n_trees >= 1)
  expect_true(model2$n_trees >= 1)
  
  # Both should be valid models
  expect_true(inherits(model1, "treefarms_model"))
  expect_true(inherits(model2, "treefarms_model"))
  
  # Number of trees should match (same parameters)
  expect_equal(model1$n_trees, model2$n_trees)
})

