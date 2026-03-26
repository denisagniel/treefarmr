test_that("Thread-safety stress test with worker_limit=4", {
  skip_on_cran()
  skip_if_not(Sys.getenv("RUN_STRESS_TESTS") == "true", "Stress tests disabled")

  # Simple test data
  set.seed(123)
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  y <- rbinom(n, 1, 0.5)

  # Run 100 iterations with worker_limit=4 (reduced from 1000 for CI)
  # In practice, run with RUN_STRESS_TESTS=true for full 1000 iterations
  n_iterations <- if (Sys.getenv("RUN_FULL_STRESS") == "true") 1000 else 100

  results <- list()
  for (i in 1:n_iterations) {
    result <- try({
      optimaltrees(
        X = X,
        y = y,
        loss = "misclassification",
        regularization = 0.01,
        worker_limit = 4,
        time_limit = 5,
        verbose = FALSE
      )
    }, silent = TRUE)

    # Check that no error occurred
    expect_false(inherits(result, "try-error"),
                 info = paste("Iteration", i, "failed"))

    if (!inherits(result, "try-error")) {
      # Extract key metrics
      results[[i]] <- list(
        loss = result$loss,
        complexity = result$complexity,
        n_leaves = result$n_leaves
      )
    }
  }

  # All iterations should have consistent results (same optimal tree)
  if (length(results) > 1) {
    # Check that loss values are stable
    losses <- sapply(results, function(x) x$loss)
    expect_true(sd(losses) < 1e-6,
                info = paste("Loss values should be stable, got sd =", sd(losses)))
  }

  # Success message
  message(sprintf("✓ Completed %d iterations with worker_limit=4", n_iterations))
})

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
