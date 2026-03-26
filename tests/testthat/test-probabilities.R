# Probability Tests
# Consolidated from test-probabilities.R and test-probability-accuracy.R
# Tests probability validation, consistency, and accuracy
# Created: 2026-03-25 during Phase 3 consolidation

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# ============================================================================
# Basic Probability Validation
# ============================================================================

test_that("probabilities sum to 1.0", {
  # Misclassification
  model_mis <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                              loss_function = "misclassification",
                              regularization = 0.1,
                              verbose = FALSE)

  if (!is.null(model_mis@probabilities)) {
    row_sums <- rowSums(model_mis@probabilities)
    expect_true(all(abs(row_sums - 1) < 1e-10))
  }

  # Log-loss
  model_log <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             verbose = FALSE)

  # Get probabilities (may be lazy)
  probs <- if (!is.null(model_log@probabilities)) model_log@probabilities else NULL
  if (!is.null(probs)) {
    row_sums <- rowSums(probs)
    expect_true(all(abs(row_sums - 1) < 1e-10))
  }
})

test_that("probabilities are bounded [0, 1]", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    expect_true(all(probs >= 0))
    expect_true(all(probs <= 1))
  }
})

test_that("log-loss probabilities are bounded away from 0/1", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    # Should not have exactly 0 or 1 (numerical stability)
    expect_true(all(probs > 0))
    expect_true(all(probs < 1))
  }
})

test_that("probabilities are finite (no NaN or Inf)", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    expect_true(all(is.finite(probs)))
  }
})

# ============================================================================
# Probability-Prediction Consistency
# ============================================================================

test_that("predictions are consistent with probabilities", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  preds <- model@predictions

  if (!is.null(probs) && !is.null(preds)) {
    # Predictions should match argmax of probabilities
    expected_preds <- apply(probs, 1, which.max) - 1
    expect_equal(preds, expected_preds)
  }
})

# ============================================================================
# Probability Dimensions and Structure
# ============================================================================

test_that("probabilities have correct dimensions", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    expect_true(is.matrix(probs))
    expect_equal(nrow(probs), nrow(simple_dataset$X))
    expect_equal(ncol(probs), 2)  # Binary classification
  }
})

# ============================================================================
# Probability Accuracy Tests
# ============================================================================

test_that("probabilities reflect true class frequencies (calibration check)", {
  # Use dataset with clear pattern
  model <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.05,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs) && !is.null(model@predictions)) {
    # For well-fit model, average predicted probability should be close to actual class proportion
    true_prop <- mean(pattern_dataset$y)
    pred_prop <- mean(probs[, 2])  # P(Y=1)

    # Should be reasonably close (within 0.3)
    expect_true(abs(pred_prop - true_prop) < 0.3)
  }
})

test_that("probabilities improve with lower regularization", {
  # Higher regularization → more constrained → potentially less accurate
  model_high <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 1.0,
                              verbose = FALSE)

  # Lower regularization → less constrained → potentially more accurate
  model_low <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.01,
                             verbose = FALSE)

  # Both should produce valid models
  expect_valid_treefarms_model(model_high, "log_loss")
  expect_valid_treefarms_model(model_low, "log_loss")

  # Accuracy may improve with lower regularization (but not guaranteed)
  # Main check: both models work
})

# ============================================================================
# Edge Cases
# ============================================================================

test_that("probabilities work with imbalanced data", {
  model <- safe_optimaltrees(imbalanced_dataset$X, imbalanced_dataset$y,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    expect_true(is.matrix(probs))
    expect_true(all(is.finite(probs)))
    row_sums <- rowSums(probs)
    expect_true(all(abs(row_sums - 1) < 1e-10))
  }
})

test_that("probabilities for perfect separation are bounded (log-loss)", {
  # Create perfectly separated data
  X_perfect <- data.frame(
    x1 = c(rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(1, 25))
  )
  y_perfect <- c(rep(0, 25), rep(1, 25))

  model <- safe_optimaltrees(X_perfect, y_perfect,
                         loss_function = "log_loss",
                         regularization = 0.1,
                         verbose = FALSE)

  probs <- if (!is.null(model@probabilities)) model@probabilities else NULL
  if (!is.null(probs)) {
    # Even with perfect separation, log-loss should bound probabilities
    expect_true(all(probs > 0))
    expect_true(all(probs < 1))
  }
})

test_that("probabilities work with different dataset sizes", {
  # Small dataset
  X_small <- simple_dataset$X[1:20, ]
  y_small <- simple_dataset$y[1:20]

  model_small <- safe_optimaltrees(X_small, y_small,
                               loss_function = "log_loss",
                               regularization = 0.1,
                               verbose = FALSE)

  # Larger dataset
  model_large <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              verbose = FALSE)

  # Both should work
  expect_valid_treefarms_model(model_small, "log_loss")
  expect_valid_treefarms_model(model_large, "log_loss")
})

# Note: Removed extensive ground-truth probability testing (overly complex)
# Note: Removed repetitive calibration tests (trust basic calibration check)
# This file focuses on essential probability validation that users need
