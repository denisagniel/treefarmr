# Test state isolation between multiple calls
# This verifies that the refactoring to instance-based state works correctly

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

test_that("State isolation: multiple calls with different configurations", {
  # Create test data
  X <- data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  )
  y <- c(rep(0, 50), rep(1, 50))

  # First call with low regularization
  result1 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.1, verbose = FALSE)
  expect_true(inherits(result1, "OptimalTreesModel"))
  expect_true(!is.null(result1@trees))
  expect_true(length(result1@trees) >= 1)

  # Second call with different regularization
  result2 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.5, verbose = FALSE)
  expect_true(inherits(result2, "OptimalTreesModel"))
  expect_true(!is.null(result2@trees))
  expect_true(length(result2@trees) >= 1)

  # Results should be different (different regularization may lead to different outcomes)
  expect_true(result1@regularization != result2@regularization)

  # Both should still be valid
  expect_true(!is.null(result1@trees))
  expect_true(!is.null(result2@trees))
})

test_that("State isolation: different loss functions", {
  # Create test data
  set.seed(42)
  X <- data.frame(
    x1 = sample(0:1, 50, replace = TRUE),
    x2 = sample(0:1, 50, replace = TRUE)
  )
  y <- sample(0:1, 50, replace = TRUE)

  # Call with misclassification loss
  result1 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.1, verbose = FALSE)
  expect_true(inherits(result1, "OptimalTreesModel"))

  # Call with log-loss
  result2 <- optimaltrees(X, y, loss_function = "log_loss", regularization = 0.1, verbose = FALSE)
  expect_true(inherits(result2, "OptimalTreesModel"))

  # Both should be valid
  expect_true(!is.null(result1@trees))
  expect_true(!is.null(result2@trees))

  # Loss functions should be different
  expect_equal(result1@loss_function, "misclassification")
  expect_equal(result2@loss_function, "log_loss")
})

test_that("State isolation: same data, multiple calls produce consistent results", {
  # Create test data
  set.seed(42)
  X <- data.frame(
    x1 = sample(0:1, 30, replace = TRUE),
    x2 = sample(0:1, 30, replace = TRUE)
  )
  y <- sample(0:1, 30, replace = TRUE)

  # Multiple calls with same configuration
  result1 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.1, verbose = FALSE)
  result2 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.1, verbose = FALSE)
  result3 <- optimaltrees(X, y, loss_function = "misclassification", regularization = 0.1, verbose = FALSE)

  # All should produce the same result (deterministic algorithm)
  expect_equal(result1@accuracy, result2@accuracy)
  expect_equal(result2@accuracy, result3@accuracy)
  expect_equal(result1@n_trees, result2@n_trees)

  # All should be valid
  expect_true(!is.null(result1@trees))
  expect_true(!is.null(result2@trees))
  expect_true(!is.null(result3@trees))
})



