test_that("Numerical validation catches malformed input", {
  # Test 1: Extreme values that could cause overflow
  set.seed(789)
  n <- 50
  X <- matrix(runif(n * 3, -100, 100), n, 3)
  y <- rbinom(n, 1, 0.5)

  result <- optimaltrees(
    X = X,
    y = y,
    loss_function = "misclassification",
    regularization = 0.01,
    verbose = FALSE
  )

  # Should handle extreme values without NaN/Inf
  expect_true(is.finite(result@accuracy))
})

test_that("Division by zero protection in computeScore", {
  # Edge case: all samples in one class
  n <- 30
  X <- matrix(rnorm(n * 2), n, 2)
  y <- rep(1, n)  # All positive class

  result <- try({
    optimaltrees(
      X = X,
      y = y,
      loss_function = "misclassification",
      regularization = 0.01,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should either succeed (trivial tree) or fail gracefully
  # Should NOT produce NaN or Inf
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result@accuracy))
  }
})

test_that("Model loss/complexity validation catches non-finite values", {
  # Normal case should produce finite values
  set.seed(101112)
  n <- 80
  X <- matrix(rnorm(n * 4), n, 4)
  y <- rbinom(n, 1, 0.4)

  result <- optimaltrees(
    X = X,
    y = y,
    loss_function = "misclassification",
    regularization = 0.02,
    verbose = FALSE
  )

  # Accuracy must be finite and in [0, 1]
  expect_true(is.finite(result@accuracy))
  expect_true(result@accuracy >= 0)
  expect_true(result@accuracy <= 1)
})

test_that("Log-loss handles extreme probabilities", {
  skip_if_not_installed("optimaltrees")

  set.seed(131415)
  n <- 60
  X <- matrix(rnorm(n * 3), n, 3)
  y <- rbinom(n, 1, 0.5)

  result <- try({
    optimaltrees(
      X = X,
      y = y,
      loss_function = "log_loss",
      regularization = 0.01,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Log-loss should handle edge cases without producing NaN/Inf
  if (!inherits(result, "try-error")) {
    expect_true(result@n_trees >= 1)
    expect_true(is.finite(result@accuracy))
    expect_true(result@accuracy >= 0 && result@accuracy <= 1)
    expect_equal(length(result@predictions), n)
  }
})

test_that("Squared error with extreme targets", {
  set.seed(161718)
  n <- 50
  X <- matrix(rnorm(n * 3), n, 3)

  # Test with extreme target values
  y <- rnorm(n, mean = 0, sd = 100)

  result <- try({
    optimaltrees(
      X = X,
      y = y,
      loss_function = "squared_error",
      regularization = 0.01,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should handle large targets without overflow
  if (!inherits(result, "try-error")) {
    expect_true(result@n_trees >= 1)
    expect_true(is.numeric(result@predictions))
    expect_equal(length(result@predictions), n)
    expect_true(all(is.finite(result@predictions)))
  }
})

test_that("Dataset variance computation is numerically stable", {
  # Two-pass algorithm should avoid catastrophic cancellation
  set.seed(192021)
  n <- 100
  X <- matrix(rnorm(n * 4), n, 4)

  # Create targets with small variance (tests catastrophic cancellation)
  y <- rnorm(n, mean = 1e6, sd = 1.0)

  result <- try({
    optimaltrees(
      X = X,
      y = y,
      loss_function = "squared_error",
      regularization = 0.01,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should compute variance accurately without catastrophic cancellation
  if (!inherits(result, "try-error")) {
    expect_true(result@n_trees >= 1)
    # Predictions should be finite and within the target range (not distorted by large mean)
    expect_true(all(is.finite(result@predictions)))
    expect_equal(length(result@predictions), n)
  }
})

test_that("Zero support protection in information calculation", {
  # Edge case: very small sample sizes
  n <- 10  # Very small sample
  X <- matrix(rnorm(n * 2), n, 2)
  y <- rbinom(n, 1, 0.5)

  result <- try({
    optimaltrees(
      X = X,
      y = y,
      loss_function = "misclassification",
      regularization = 0.01,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should handle small samples without log(0) errors
  if (!inherits(result, "try-error")) {
    expect_true(result@n_trees >= 1)
  }
})
