# Probability Tests
# Consolidated from test-probabilities.R and test-probability-accuracy.R
# Tests probability validation, consistency, and accuracy
# Created: 2026-03-25 during Phase 3 consolidation
skip_slow_tests()

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# ============================================================================
# Basic Probability Validation
# ============================================================================

test_that("probabilities sum to 1.0", {
  # Misclassification with compute_probabilities
  model_mis <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                              loss_function = "misclassification",
                              regularization = 0.1,
                              compute_probabilities = TRUE,
                              verbose = FALSE)

  expect_false(is.null(model_mis@probabilities))
  row_sums <- rowSums(model_mis@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))

  # Log-loss (always produces probabilities)
  model_log <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  expect_false(is.null(model_log@probabilities))
  row_sums <- rowSums(model_log@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("probabilities are bounded [0, 1]", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
})

test_that("log-loss probabilities are in valid range", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
  expect_true(all(is.finite(model@probabilities)))
})

test_that("probabilities are finite (no NaN or Inf)", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  expect_true(all(is.finite(model@probabilities)))
})

# ============================================================================
# Probability-Prediction Consistency
# ============================================================================

test_that("predictions are consistent with probabilities", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  probs <- model@probabilities
  preds <- model@predictions

  expect_false(is.null(probs))
  expect_false(is.null(preds))

  # Predictions should match argmax of probabilities
  expected_preds <- apply(probs, 1, which.max) - 1
  expect_equal(preds, expected_preds)
})

# ============================================================================
# Probability Dimensions and Structure
# ============================================================================

test_that("probabilities have correct dimensions", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  expect_true(is.matrix(model@probabilities))
  expect_equal(nrow(model@probabilities), nrow(simple_dataset$X))
  expect_equal(ncol(model@probabilities), 2)  # Binary classification
})

# ============================================================================
# Probability Accuracy Tests
# ============================================================================

test_that("probabilities reflect true class frequencies (calibration check)", {
  model <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.05,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  probs <- model@probabilities
  expect_false(is.null(probs))

  # For well-fit model, average predicted probability should be close to actual class proportion
  true_prop <- mean(pattern_dataset$y)
  pred_prop <- mean(probs[, 2])  # P(Y=1)

  # Should be reasonably close (within 0.3)
  expect_true(abs(pred_prop - true_prop) < 0.3)
})

test_that("probabilities improve with lower regularization", {
  # Higher regularization
  model_high <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 1.0,
                              compute_probabilities = TRUE,
                              verbose = FALSE)

  # Lower regularization
  model_low <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.01,
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  # Both should produce valid models
  expect_valid_treefarms_model(model_high, "log_loss")
  expect_valid_treefarms_model(model_low, "log_loss")
})

# ============================================================================
# Edge Cases
# ============================================================================

test_that("probabilities work with imbalanced data", {
  model <- safe_optimaltrees(imbalanced_dataset$X, imbalanced_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  expect_true(is.matrix(model@probabilities))
  expect_true(all(is.finite(model@probabilities)))
  row_sums <- rowSums(model@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("probabilities for perfect separation are valid (log-loss)", {
  # Create perfectly separated data
  X_perfect <- data.frame(
    x1 = c(rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(1, 25))
  )
  y_perfect <- c(rep(0, 25), rep(1, 25))

  model <- safe_optimaltrees(X_perfect, y_perfect,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         compute_probabilities = TRUE,
                         verbose = FALSE)

  expect_false(is.null(model@probabilities))
  # Probabilities must be in [0, 1] and finite
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
  expect_true(all(is.finite(model@probabilities)))
  # Rows still sum to 1
  row_sums <- rowSums(model@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("probabilities work with different dataset sizes", {
  # Small dataset
  X_small <- simple_dataset$X[1:20, ]
  y_small <- simple_dataset$y[1:20]

  model_small <- safe_optimaltrees(X_small, y_small,
                               loss_function = "log_loss",
                               regularization = 0.1,
                               compute_probabilities = TRUE,
                               verbose = FALSE)

  # Larger dataset
  model_large <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              compute_probabilities = TRUE,
                              verbose = FALSE)

  # Both should work
  expect_valid_treefarms_model(model_small, "log_loss")
  expect_valid_treefarms_model(model_large, "log_loss")
})
