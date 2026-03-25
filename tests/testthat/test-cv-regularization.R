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
})

test_that("best_lambda is in lambda_grid", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = lambda_grid, refit = FALSE)
  expect_true(cv$best_lambda %in% lambda_grid,
              info = "best_lambda must be one of the grid values")
})

test_that("cv_regularization with refit = TRUE returns model with correct regularization", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = lambda_grid, refit = TRUE)
  expect_true("model" %in% names(cv))
  expect_equal(cv$model@regularization, cv$best_lambda,
               info = "refitted model regularization must equal best_lambda")
  X_new <- X[1:2, , drop = FALSE]
  expect_no_error({
    pred <- predict(cv$model, X_new, type = "class")
  })
  expect_length(pred, 2)
})

test_that("cv_regularization with log_loss runs and refit model has correct regularization", {
  lambda_grid <- c(0.05, 0.1, 0.2)
  cv <- cv_regularization(X, y, loss_function = "log_loss",
                          K = 3, lambda_grid = lambda_grid, refit = TRUE)
  expect_true(is.numeric(cv$cv_loss))
  expect_length(cv$cv_loss, length(lambda_grid))
  expect_true("model" %in% names(cv))
  expect_equal(cv$model@regularization, cv$best_lambda)
})

test_that("cv_regularization with NULL lambda_grid uses default grid", {
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = 3, lambda_grid = NULL, refit = FALSE, seed = 123)
  expect_true(is.numeric(cv$lambda_grid))
  expect_true(length(cv$lambda_grid) >= 1)
  expect_true(cv$best_lambda %in% cv$lambda_grid)
})

test_that("cv_regularization fold_losses has correct dimensions", {
  lambda_grid <- c(0.05, 0.1)
  K <- 3
  cv <- cv_regularization(X, y, loss_function = "misclassification",
                          K = K, lambda_grid = lambda_grid, refit = FALSE)
  expect_equal(nrow(cv$fold_losses), length(lambda_grid))
  expect_equal(ncol(cv$fold_losses), K)
})
