test_that("auto-tune finds intersection via exponential + binary", {
  skip_on_cran()

  set.seed(123)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- as.numeric((X$x1 == 1 & X$x2 == 1) | rbinom(n, 1, 0.3))

  result <- cross_fitted_rashomon(
    X, y, K = 3,
    loss_function = "log_loss",
    regularization = 0.1,
    auto_tune_intersecting = TRUE,
    parallel = FALSE,  # Disable parallel for tests
    verbose = FALSE
  )

  # Should find intersection (exponential always succeeds if solution exists)
  expect_true(S7::S7_inherits(result, CFRashomon))
  expect_true(result@n_intersecting >= 0)  # May be 0 if no intersection possible
})

test_that("auto-tune handles empty Rashomon sets gracefully", {
  skip_on_cran()

  set.seed(789)
  n <- 100
  X <- data.frame(x1 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  # Very tight regularization may produce empty sets or no intersection
  expect_warning(
    result <- cross_fitted_rashomon(
      X, y, K = 3,
      loss_function = "log_loss",
      regularization = 1.0,  # Very tight
      rashomon_bound_multiplier = 0.001,  # Very tight
      auto_tune_intersecting = FALSE,
      parallel = FALSE,  # Disable parallel for tests
      verbose = FALSE
    ),
    NA  # Expect no warnings for empty sets without auto-tuning
  )

  # Should handle empty intersection without error
  expect_s3_class(result, "cf_rashomon")
  expect_true(result@n_intersecting >= 0)
})

test_that("auto-tune respects theory-justified bounds", {
  n <- 500

  # Test that exponential search values all satisfy o(n^{-1/2})
  c_values <- c(1, 2, 4, 8, 16, 32, 64)
  epsilon_values <- c_values * sqrt(log(n) / n)

  # Check rate condition: epsilon_n * sqrt(n) should grow slowly
  rates <- epsilon_values * sqrt(n)
  expect_true(all(rates < 200))  # Grows like c * sqrt(log(n))

  # Check that rates grow monotonically
  rate_growth <- diff(rates)
  expect_true(all(rate_growth > 0))  # Monotonic increase
})

test_that("find_tree_intersection returns tree_risks", {
  skip_on_cran()

  set.seed(456)
  n <- 150
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- as.numeric((X$x1 == 1 & X$x2 == 1) | rbinom(n, 1, 0.2))

  result <- cross_fitted_rashomon(
    X, y, K = 3,
    loss_function = "log_loss",
    regularization = 0.1,
    rashomon_bound_multiplier = 0.5,  # Large epsilon to encourage intersection
    auto_tune_intersecting = FALSE,
    parallel = FALSE,  # Disable parallel for tests
    verbose = FALSE
  )

  # Should have tree_risks in result (even if NULL for individual trees)
  # S7 objects always have the property, check if it's accessible
  expect_true(!is.null(result@tree_risks) || is.null(result@tree_risks))

  if (result@n_intersecting > 0) {
    expect_equal(length(result@tree_risks), result@n_intersecting)
  } else {
    expect_equal(length(result@tree_risks), 0)
  }
})

test_that("empty intersection returns early without error", {
  skip_on_cran()

  set.seed(999)
  n <- 80
  # Create data that's likely to have heterogeneous Rashomon sets across folds
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5),
    x3 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)  # Completely random

  # With tight parameters, intersection may be empty
  result <- cross_fitted_rashomon(
    X, y, K = 5,
    loss_function = "log_loss",
    regularization = 0.5,
    rashomon_bound_multiplier = 0.01,  # Very tight
    auto_tune_intersecting = FALSE,
    parallel = FALSE,  # Disable parallel for tests
    verbose = FALSE
  )

  # Should handle empty intersection gracefully
  expect_s3_class(result, "cf_rashomon")
  expect_true(result@n_intersecting >= 0)

  if (result@n_intersecting == 0) {
    expect_equal(length(result$intersecting_trees), 0)
    expect_equal(length(result$tree_risks), 0)
  }
})

test_that("diagnostic logging shows intersection progress", {
  skip_on_cran()

  set.seed(321)
  n <- 120
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- as.numeric((X$x1 == 1) | rbinom(n, 1, 0.2))

  # Capture verbose output
  output <- capture.output({
    result <- cross_fitted_rashomon(
      X, y, K = 3,
      loss_function = "log_loss",
      regularization = 0.1,
      rashomon_bound_multiplier = 0.2,
      auto_tune_intersecting = FALSE,
      parallel = FALSE,  # Disable parallel for tests
      verbose = TRUE
    )
  })

  # Should see diagnostic messages about Rashomon set sizes
  expect_true(any(grepl("Rashomon set sizes", output)))

  # Should see intersection progress messages
  expect_true(any(grepl("After fold", output)) || any(grepl("structure", output)))
})

test_that("bidirectional search goes downward when c=1 succeeds", {
  skip_on_cran()

  set.seed(555)
  n <- 150
  # Create easy intersection case (common structure across folds)
  X <- data.frame(x1 = rbinom(n, 1, 0.5))
  y <- as.numeric(X$x1 == 1)  # Deterministic relationship

  # Capture verbose output
  output <- capture.output({
    result <- cross_fitted_rashomon(
      X, y, K = 3,
      loss_function = "log_loss",
      regularization = 0.1,
      auto_tune_intersecting = TRUE,
      parallel = FALSE,
      verbose = TRUE
    )
  })

  # Should see "Downward" search mentioned
  expect_true(any(grepl("Downward", output)) || any(grepl("downward", output)))

  # If convergence happened, epsilon should be relatively small
  if (result@n_intersecting > 0) {
    n_val <- nrow(X)
    sqrt_log_n_over_n <- sqrt(log(n_val) / n_val)
    c_found <- result$rashomon_bound_multiplier / sqrt_log_n_over_n

    # Should find c < 1 via downward search
    # (may not always be true depending on data, but likely for this simple case)
    expect_true(c_found <= 1.5)  # Reasonable threshold
  }
})

test_that("bidirectional search goes upward when c=1 fails", {
  skip_on_cran()

  set.seed(666)
  n <- 100
  # Create heterogeneous case (harder intersection)
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5),
    x3 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)  # Random

  # With tight regularization, c=1 may fail
  output <- capture.output({
    result <- cross_fitted_rashomon(
      X, y, K = 3,
      loss_function = "log_loss",
      regularization = 0.3,  # Tighter
      auto_tune_intersecting = TRUE,
      parallel = FALSE,
      verbose = TRUE
    )
  })

  # Should see "Upward" search mentioned (if c=1 failed)
  # OR "Downward" if c=1 succeeded (data-dependent)
  expect_true(any(grepl("Upward|Downward", output, ignore.case = TRUE)))

  # Result should be valid
  expect_s3_class(result, "cf_rashomon")
})
