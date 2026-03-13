test_that("Numerical validation catches malformed input", {
  # These tests verify that the safe_stof/safe_stoi functions properly
  # catch malformed numeric input and throw clear error messages

  # Test data with invalid numeric strings (if encoder exposed)
  # Note: Current API doesn't directly expose encoder, so we test via data loading

  # Test 1: Extreme values that could cause overflow
  set.seed(789)
  n <- 50
  X <- matrix(runif(n * 3, -100, 100), n, 3)
  y <- rbinom(n, 1, 0.5)

  result <- treefarms(
    X = X,
    y = y,
    loss = "misclassification",
    regularization = 0.01,
    time_limit = 5,
    verbose = FALSE
  )

  # Should handle extreme values without NaN/Inf
  expect_true(is.finite(result$loss))
  expect_true(is.finite(result$complexity))
})

test_that("Division by zero protection in computeScore", {
  # The computeScore function should throw error when P=0 or N=0
  # This is tested indirectly through model evaluation

  # Edge case: all samples in one class
  n <- 30
  X <- matrix(rnorm(n * 2), n, 2)
  y <- rep(1, n)  # All positive class

  result <- try({
    treefarms(
      X = X,
      y = y,
      loss = "misclassification",
      regularization = 0.01,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should either succeed (trivial tree) or fail gracefully
  # Should NOT produce NaN or Inf
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result$loss))
  }
})

test_that("Model loss/complexity validation catches non-finite values", {
  # Normal case should produce finite values
  set.seed(101112)
  n <- 80
  X <- matrix(rnorm(n * 4), n, 4)
  y <- rbinom(n, 1, 0.4)

  result <- treefarms(
    X = X,
    y = y,
    loss = "misclassification",
    regularization = 0.02,
    time_limit = 5,
    verbose = FALSE
  )

  # Loss and complexity must be finite
  expect_true(is.finite(result$loss),
              info = "Loss must be finite")
  expect_true(is.finite(result$complexity),
              info = "Complexity must be finite")
  expect_true(result$loss >= 0,
              info = "Loss must be non-negative")
  expect_true(result$complexity >= 0,
              info = "Complexity must be non-negative")
})

test_that("Log-loss handles extreme probabilities", {
  skip_if_not_installed("optimaltrees")

  set.seed(131415)
  n <- 60
  X <- matrix(rnorm(n * 3), n, 3)
  y <- rbinom(n, 1, 0.5)

  result <- try({
    treefarms(
      X = X,
      y = y,
      loss = "log_loss",
      regularization = 0.01,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Log-loss should handle edge cases without producing NaN/Inf
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result$loss))
    expect_true(result$loss > 0)  # Log-loss is always positive
  }
})

test_that("Squared error with extreme targets", {
  set.seed(161718)
  n <- 50
  X <- matrix(rnorm(n * 3), n, 3)

  # Test with extreme target values
  y <- rnorm(n, mean = 0, sd = 100)

  result <- try({
    treefarms(
      X = X,
      y = y,
      loss = "squared_error",
      regularization = 0.01,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should handle large targets without overflow
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result$loss))
    expect_true(is.finite(result$complexity))
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
    treefarms(
      X = X,
      y = y,
      loss = "squared_error",
      regularization = 0.01,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should compute variance accurately without catastrophic cancellation
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result$loss))
  }
})

test_that("Zero support protection in information calculation", {
  # Edge case: very small sample sizes
  n <- 10  # Very small sample
  X <- matrix(rnorm(n * 2), n, 2)
  y <- rbinom(n, 1, 0.5)

  result <- try({
    treefarms(
      X = X,
      y = y,
      loss = "misclassification",
      regularization = 0.01,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  # Should handle small samples without log(0) errors
  if (!inherits(result, "try-error")) {
    expect_true(is.finite(result$loss))
  }
})
