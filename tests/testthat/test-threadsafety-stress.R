test_that("Thread-safety with concurrent graph access", {
  skip_on_cran()

  # Test with deeper tree to stress graph operations
  set.seed(456)
  n <- 200
  p <- 10
  X <- matrix(rnorm(n * p), n, p)
  y <- rbinom(n, 1, 0.3)

  result <- optimaltrees(
    X = X,
    y = y,
    loss = "misclassification",
    regularization = 0.001,
    depth_budget = 3,  # Allow deeper trees
    worker_limit = 4,
    time_limit = 10,
    verbose = FALSE
  )

  # Should complete without crashes or data races
  expect_true(!is.null(result@trees))
  expect_true(length(result@trees) >= 1)
})
