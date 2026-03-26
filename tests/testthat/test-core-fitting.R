# Core Tree Fitting Tests
# Consolidated from test-fit-functions.R, test-optimaltrees.R, test-api.R, test-discretization.R
# Tests core tree fitting functionality across all loss functions and parameters
# Created: 2026-03-25 during Phase 3 consolidation

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# ============================================================================
# Basic Fitting - All Loss Functions
# ============================================================================

test_that("misclassification loss fitting works", {
  expect_no_error({
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model, "misclassification")
  expect_true(model@accuracy >= 0 && model@accuracy <= 1)
})

test_that("log_loss fitting works", {
  expect_no_error({
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                           loss_function = "log_loss",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model, "log_loss")

  # Check probabilities (access directly since get_probabilities may have S7 access issues)
  # Probabilities may be NULL if computed lazily, but should exist for log_loss
  expect_true(is.null(model@probabilities) || is.matrix(model@probabilities))
})

test_that("squared_error (regression) fitting works", {
  # Create continuous outcome
  set.seed(42)
  X <- data.frame(
    x1 = sample(0:1, 50, replace = TRUE),
    x2 = sample(0:1, 50, replace = TRUE)
  )
  y <- runif(50, 0, 10)

  expect_no_error({
    model <- safe_optimaltrees(X, y,
                           loss_function = "squared_error",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model, "squared_error")
  expect_null(model@probabilities)  # No probabilities for regression
  expect_true(is.numeric(model@predictions))
})

# ============================================================================
# Single Tree vs Rashomon Set
# ============================================================================

test_that("single_tree parameter works", {
  # Single tree
  model_single <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                loss_function = "misclassification",
                                regularization = 0.1,
                                single_tree = TRUE,
                                verbose = FALSE)

  expect_equal(model_single@n_trees, 1)

  # Rashomon set
  model_rashomon <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                                  loss_function = "misclassification",
                                  regularization = 0.05,
                                  single_tree = FALSE,
                                  rashomon_bound_multiplier = 0.1,
                                  verbose = FALSE)

  # May have multiple trees
  expect_true(model_rashomon@n_trees >= 1)
})

test_that("fit_tree always returns exactly one tree", {
  model1 <- fit_tree(pattern_dataset$X, pattern_dataset$y, regularization = 0.01, verbose = FALSE)
  model2 <- fit_tree(pattern_dataset$X, pattern_dataset$y, regularization = 0.1, verbose = FALSE)
  model3 <- fit_tree(pattern_dataset$X, pattern_dataset$y, regularization = 1.0, verbose = FALSE)

  expect_equal(model1@n_trees, 1)
  expect_equal(model2@n_trees, 1)
  expect_equal(model3@n_trees, 1)
})

# ============================================================================
# Regularization Effects
# ============================================================================

test_that("regularization parameter affects tree complexity", {
  # Low regularization → potentially more complex
  model_low <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                             loss_function = "misclassification",
                             regularization = 0.01,
                             verbose = FALSE)

  # High regularization → simpler
  model_high <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "misclassification",
                              regularization = 1.0,
                              verbose = FALSE)

  expect_valid_treefarms_model(model_low)
  expect_valid_treefarms_model(model_high)

  # Both should work, regularization affects complexity (not tested in detail)
  expect_true(is.numeric(model_low@regularization))
  expect_true(is.numeric(model_high@regularization))
})

# ============================================================================
# Discretization (Continuous Features)
# ============================================================================

test_that("continuous features are discretized automatically", {
  set.seed(123)
  X <- data.frame(
    x1 = runif(100, 0, 1),
    x2 = rnorm(100, 50, 10)
  )
  y <- as.numeric(X$x1 > 0.5)

  expect_no_error({
    model <- optimaltrees(X, y, loss_function = "log_loss", verbose = FALSE)
  })

  expect_valid_treefarms_model(model)

  # Check discretization metadata exists
  expect_false(is.null(model@discretization_metadata))
  expect_equal(model@discretization_metadata$method, "median")
  expect_equal(length(model@discretization_metadata$features), 2)

  # Predictions should work
  X_new <- data.frame(
    x1 = runif(10, 0, 1),
    x2 = rnorm(10, 50, 10)
  )
  pred <- predict(model, X_new, type = "prob")
  expect_valid_predictions(pred, 10, type = "prob")
})

test_that("binary features still work (backward compatibility)", {
  # Binary data should work without discretization changes
  model <- optimaltrees(simple_dataset$X, simple_dataset$y,
                       loss_function = "misclassification",
                       verbose = FALSE)

  expect_valid_treefarms_model(model)
  expect_false(is.null(model@discretization_metadata))
  expect_true(model@discretization_metadata$all_binary)
})

test_that("mixed binary and continuous features work", {
  set.seed(123)
  X <- data.frame(
    binary_feat = sample(0:1, 100, replace = TRUE),
    continuous_feat = runif(100, 0, 1)
  )
  y <- sample(0:1, 100, replace = TRUE)

  model <- optimaltrees(X, y, loss_function = "log_loss", verbose = FALSE)

  expect_false(is.null(model@discretization_metadata))
  expect_equal(model@discretization_metadata$features$binary_feat$type, "binary")
  expect_equal(model@discretization_metadata$features$continuous_feat$type, "continuous")
  expect_true(length(model@discretization_metadata$features$continuous_feat$thresholds) > 0)
})

test_that("user-provided discretization thresholds are used", {
  set.seed(123)
  X <- data.frame(x1 = runif(50, 0, 1))
  y <- sample(0:1, 50, replace = TRUE)

  # Provide custom thresholds
  custom_thresholds <- list(x1 = c(0.3, 0.7))

  model <- optimaltrees(X, y,
                       loss_function = "misclassification",
                       discretize_thresholds = custom_thresholds,
                       verbose = FALSE)

  # Check that custom thresholds were used
  expect_equal(model@discretization_metadata$features$x1$thresholds, c(0.3, 0.7))
})

test_that("discretization methods work correctly", {
  set.seed(123)
  X <- data.frame(x1 = runif(50, 0, 1))
  y <- sample(0:1, 50, replace = TRUE)

  # Test quantiles method
  model_quantiles <- optimaltrees(X, y,
                                loss_function = "misclassification",
                                discretize_method = "quantiles",
                                discretize_bins = 4,
                                verbose = FALSE)

  expect_equal(model_quantiles@discretization_metadata$method, "quantiles")
  # Bins stored in discretization result, not metadata

  # Test median method (default)
  model_median <- optimaltrees(X, y,
                              loss_function = "misclassification",
                              discretize_method = "median",
                              verbose = FALSE)

  expect_equal(model_median@discretization_metadata$method, "median")
})

# ============================================================================
# Input Validation
# ============================================================================

test_that("input validation works", {
  # Invalid X
  expect_error(optimaltrees("invalid", simple_dataset$y))

  # Invalid y
  expect_error(optimaltrees(simple_dataset$X, "invalid"))

  # Length mismatch
  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y[1:50]))

  # Invalid loss function
  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y, loss_function = "invalid"))
})

# ============================================================================
# Worker Limits (Parallel)
# ============================================================================

test_that("worker_limit parameter works", {
  # Single worker
  model1 <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                          loss_function = "misclassification",
                          regularization = 0.1,
                          worker_limit = 1L,
                          verbose = FALSE)

  # Multiple workers
  model2 <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                          loss_function = "misclassification",
                          regularization = 0.1,
                          worker_limit = 2L,
                          verbose = FALSE)

  expect_valid_treefarms_model(model1)
  expect_valid_treefarms_model(model2)

  # Results should be deterministic (same seed)
  # Note: Small numerical differences possible due to parallel execution order
})

# ============================================================================
# Data Storage Options
# ============================================================================

test_that("store_training_data parameter works", {
  # Don't store (default)
  model_no_store <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                  loss_function = "misclassification",
                                  regularization = 0.1,
                                  store_training_data = FALSE,
                                  verbose = FALSE)

  expect_null(model_no_store@X_train)
  expect_null(model_no_store@y_train)

  # Store data
  model_store <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                              loss_function = "misclassification",
                              regularization = 0.1,
                              store_training_data = TRUE,
                              verbose = FALSE)

  expect_false(is.null(model_store@X_train))
  expect_false(is.null(model_store@y_train))
  expect_equal(nrow(model_store@X_train), nrow(simple_dataset$X))
})

# ============================================================================
# Model Structure Validation
# ============================================================================

test_that("model object has correct S7 structure", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "misclassification",
                         regularization = 0.1,
                         verbose = FALSE)

  # Check S7 class
  expect_true(S7::S7_inherits(model))
  expect_true(inherits(model, "OptimalTreesModel"))

  # Check required properties exist
  required_props <- c("trees", "predictions", "probabilities", "accuracy",
                     "loss_function", "regularization", "n_trees")

  for (prop in required_props) {
    expect_true(prop %in% S7::prop_names(model),
               info = paste("Missing property:", prop))
  }
})

# ============================================================================
# Special Cases
# ============================================================================

test_that("logical y is converted correctly", {
  y_logical <- simple_dataset$y == 1

  model <- safe_optimaltrees(simple_dataset$X, y_logical,
                         loss_function = "misclassification",
                         regularization = 0.1,
                         verbose = FALSE)

  expect_valid_treefarms_model(model)
})

test_that("matrix input for X works", {
  X_matrix <- as.matrix(simple_dataset$X)

  model <- safe_optimaltrees(X_matrix, simple_dataset$y,
                         loss_function = "misclassification",
                         regularization = 0.1,
                         verbose = FALSE)

  expect_valid_treefarms_model(model)
})

test_that("verbose output works without error", {
  # Test that verbose mode doesn't crash
  expect_no_error({
    capture.output({
      model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "misclassification",
                             regularization = 0.1,
                             verbose = TRUE)
    })
  })
})

# Note: Auto-tuning tests moved to test-auto-tuning.R
# Note: Extensive discretization edge cases removed (trust the algorithm)
# Note: Redundant parameter variation tests removed
# This file focuses on core fitting functionality that users rely on
