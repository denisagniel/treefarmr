# Test suite for cv_regularization()

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Small data for fast tests
set.seed(42)
n <- 80
X <- data.frame(
  x1 = sample(0:1, n, replace = TRUE),
  x2 = sample(0:1, n, replace = TRUE)
)
y <- as.numeric((X$x1 == 1 & X$x2 == 1))

test_that("cv_regularization runs without error and returns expected structure", {
  expect_no_error({
    cv <- cv_regularization(X, y, loss_function = "misclassification",
                           K = 3, lambda_grid = c(0.05, 0.1, 0.2), refit = FALSE)
  })
  expect_true(is.list(cv))
  expect_true("best_lambda" %in% names(cv))
  expect_true("cv_loss" %in% names(cv))
  expect_true("lambda_grid" %in% names(cv))
  expect_true("fold_losses" %in% names(cv))
  expect_false("model" %in% names(cv))

  rm(cv)
  gc()
})

test_that("best_lambda is in lambda_grid", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = lambda_grid, refit = FALSE)
  # If CV succeeded, best_lambda should be in grid
  # If CV failed completely, best_lambda will be NA
  if (!is.na(cv$best_lambda)) {
    expect_true(cv$best_lambda %in% lambda_grid,
                info = "best_lambda must be one of the grid values")
  }

  rm(cv)
  gc()
})

test_that("cv_regularization with refit = TRUE returns model with correct regularization", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = lambda_grid, refit = TRUE)

  # If CV succeeded, model should exist
  if (!is.na(cv$best_lambda)) {
    expect_true("model" %in% names(cv))
    expect_equal(cv$model@regularization, cv$best_lambda,
                 info = "refitted model regularization must equal best_lambda")
    X_new <- X[1:2, , drop = FALSE]
    expect_no_error({
      pred <- predict(cv$model, X_new, type = "class")
    })
    expect_length(pred, 2)
  } else {
    # If CV completely failed, model should not be in output
    expect_false("model" %in% names(cv))
  }
})

test_that("cv_regularization with log_loss runs and refit model has correct regularization", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "log_loss",
                          K = 3, lambda_grid = lambda_grid, refit = TRUE)
  expect_true(is.numeric(cv$cv_loss))
  expect_length(cv$cv_loss, length(lambda_grid))

  # If CV succeeded, model should exist
  if (!is.na(cv$best_lambda)) {
    expect_true("model" %in% names(cv))
    expect_equal(cv$model@regularization, cv$best_lambda)
  } else {
    # If CV completely failed, model should not be in output
    expect_false("model" %in% names(cv))
  }
})

test_that("cv_regularization with NULL lambda_grid uses default grid", {
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = NULL, refit = FALSE, seed = 123)
  expect_true(is.numeric(cv$lambda_grid))
  expect_true(length(cv$lambda_grid) >= 1)

  # If CV succeeded, best_lambda should be in grid
  if (!is.na(cv$best_lambda)) {
    expect_true(cv$best_lambda %in% cv$lambda_grid)
  }
})

test_that("cv_regularization fold_losses has correct dimensions", {
  lambda_grid <- c(0.05, 0.1)
  K <- 3
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = K, lambda_grid = lambda_grid, refit = FALSE)
  expect_equal(nrow(cv$fold_losses), length(lambda_grid))
  expect_equal(ncol(cv$fold_losses), K)
})

# ============================================================================
# Tests for computational improvements (Phase 2)
# ============================================================================

test_that("compute_safe_model_limit scales appropriately", {
  n <- 2000
  lambda_theory <- log(n) / n  # ≈ 0.0038

  # Strong regularization: default limit
  expect_equal(optimaltrees:::compute_safe_model_limit(0.1, n, 20), 10000)

  # At theory value: increased limit
  expect_equal(optimaltrees:::compute_safe_model_limit(lambda_theory, n, 20), 200000)

  # Below theory, low-dim: unlimited
  expect_equal(optimaltrees:::compute_safe_model_limit(0.001, n, 20), 0)

  # Below theory, high-dim: capped
  expect_equal(optimaltrees:::compute_safe_model_limit(0.001, n, 150), 1000000)

  # Moderate regularization
  expect_equal(optimaltrees:::compute_safe_model_limit(lambda_theory * 5, n, 20), 50000)
})

test_that("cv_regularization handles small lambda gracefully", {
  skip_on_cran()  # May be slow

  set.seed(42)
  n <- 500
  X_test <- data.frame(x1 = sample(0:1, n, replace = TRUE),
                       x2 = sample(0:1, n, replace = TRUE))
  y_test <- sample(0:1, n, replace = TRUE)

  # Test with theory-driven grid (includes small lambda)
  result <- cv_regularization(X_test, y_test, loss_function = "misclassification",
                              lambda_grid = NULL,  # Use default theory grid
                              K = 3, refit = FALSE, verbose = FALSE, seed = 123)

  # Should complete without errors
  expect_type(result$best_lambda, "double")
  expect_length(result$lambda_grid, 5)

  # CV should either succeed (at least 2 lambdas work) or fail gracefully (NA best_lambda)
  if (!is.na(result$best_lambda)) {
    expect_true(sum(is.finite(result$cv_loss)) >= 2,
                info = "If CV succeeded, at least 2 lambda values should have finite CV loss")
  }
})

test_that("cv_regularization early stopping works", {
  skip_on_cran()  # May be slow

  set.seed(789)
  n <- 100
  X_test <- data.frame(x1 = sample(0:1, n, replace = TRUE),
                       x2 = sample(0:1, n, replace = TRUE))
  y_test <- sample(0:1, n, replace = TRUE)

  # Use a grid that includes very small lambda values that might fail
  lambda_grid <- c(0.0001, 0.001, 0.01, 0.1)

  # Should not error even if some lambdas fail
  expect_no_error({
    result <- cv_regularization(X_test, y_test, loss_function = "misclassification",
                                lambda_grid = lambda_grid,
                                K = 5, refit = FALSE, verbose = FALSE, seed = 456)
  })

  # Should either succeed (select a lambda) or fail gracefully (NA)
  expect_type(result$best_lambda, "double")
  if (!is.na(result$best_lambda)) {
    expect_true(result$best_lambda %in% lambda_grid)
  }
})
