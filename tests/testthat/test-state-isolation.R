# Test state isolation between multiple calls
# This verifies that the refactoring to instance-based state works correctly

library(testthat)
library(treefarmr)

test_that("State isolation: multiple calls with different configurations", {
  # Create test data
  X <- data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  )
  y <- c(rep(0, 50), rep(1, 50))
  
  # First call with low regularization
  result1 <- treefarms(X, y, loss = "misclassification", regularization = 0.1)
  expect_s3_class(result1, "treefarms")
  expect_true(!is.null(result1$tree))
  
  # Second call with different regularization
  result2 <- treefarms(X, y, loss = "misclassification", regularization = 0.5)
  expect_s3_class(result2, "treefarms")
  expect_true(!is.null(result2$tree))
  
  # Results should be different (different regularization leads to different trees)
  expect_true(result1$model_objective != result2$model_objective)
  
  # Both should still be valid
  expect_true(!is.null(result1$tree))
  expect_true(!is.null(result2$tree))
})

test_that("State isolation: different loss functions", {
  # Create test data
  X <- data.frame(
    x1 = sample(0:1, 50, replace = TRUE),
    x2 = sample(0:1, 50, replace = TRUE)
  )
  y <- sample(0:1, 50, replace = TRUE)
  
  # Call with misclassification loss
  result1 <- treefarms(X, y, loss = "misclassification", regularization = 0.1)
  expect_s3_class(result1, "treefarms")
  
  # Call with log-loss
  result2 <- treefarms(X, y, loss = "log_loss", regularization = 0.1)
  expect_s3_class(result2, "treefarms")
  
  # Both should be valid
  expect_true(!is.null(result1$tree))
  expect_true(!is.null(result2$tree))
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
  result1 <- treefarms(X, y, loss = "misclassification", regularization = 0.1)
  result2 <- treefarms(X, y, loss = "misclassification", regularization = 0.1)
  result3 <- treefarms(X, y, loss = "misclassification", regularization = 0.1)
  
  # All should produce the same result (deterministic algorithm)
  expect_equal(result1$model_objective, result2$model_objective)
  expect_equal(result2$model_objective, result3$model_objective)
  
  # All should be valid
  expect_true(!is.null(result1$tree))
  expect_true(!is.null(result2$tree))
  expect_true(!is.null(result3$tree))
})



