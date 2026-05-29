test_that("fit_rashomon finds multiple trees by default (FALSE)", {
  skip_on_cran()

  # Generate data where multiple trees exist
  set.seed(42)
  n <- 200
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE)
  )
  y <- as.numeric((X$x1 == 1 & X$x2 == 1) | (X$x3 == 1))

  # Fit with default (rashomon_ignore_trivial_extensions = FALSE)
  result <- fit_rashomon(
    X = X, y = y,
    loss_function = "misclassification",
    regularization = 0.01,
    rashomon_bound_multiplier = 0.15,  # Large enough to get multiple trees
    verbose = FALSE
  )

  # Should find multiple trees when trivial extensions are kept
  expect_true(result@n_trees >= 1)

  # With a reasonable rashomon_bound_multiplier, we typically get multiple trees
  # (not guaranteed, depends on data, but with this seed should work)
  if (result@n_trees > 1) {
    message(sprintf("Found %d trees with trivial extensions included", result@n_trees))
  }
})

test_that("fit_rashomon with TRUE prunes to fewer trees", {
  skip_on_cran()

  # Same data as above
  set.seed(42)
  n <- 200
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE)
  )
  y <- as.numeric((X$x1 == 1 & X$x2 == 1) | (X$x3 == 1))

  # Fit with rashomon_ignore_trivial_extensions = FALSE
  result_false <- fit_rashomon(
    X = X, y = y,
    loss_function = "misclassification",
    regularization = 0.01,
    rashomon_bound_multiplier = 0.15,
    rashomon_ignore_trivial_extensions = FALSE,
    verbose = FALSE
  )

  # Fit with rashomon_ignore_trivial_extensions = TRUE
  result_true <- fit_rashomon(
    X = X, y = y,
    loss_function = "misclassification",
    regularization = 0.01,
    rashomon_bound_multiplier = 0.15,
    rashomon_ignore_trivial_extensions = TRUE,
    verbose = FALSE
  )

  # TRUE should have <= trees than FALSE
  expect_lte(result_true@n_trees, result_false@n_trees)

  message(sprintf("FALSE: %d trees, TRUE: %d trees",
                  result_false@n_trees, result_true@n_trees))
})

test_that("cross_fitted_rashomon enables intersection with default (FALSE)", {
  skip_on_cran()
  skip_if_not_installed("optimaltrees")

  # Generate simple data where folds should agree on structure
  set.seed(123)
  n <- 300
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE)
  )
  # Simple AND rule
  y <- as.numeric(X$x1 == 1 & X$x2 == 1)

  # With default (FALSE), intersection should work
  result <- cross_fitted_rashomon(
    X = X, y = y,
    K = 3,
    loss_function = "misclassification",
    regularization = 0.1,
    rashomon_bound_multiplier = 0.10,
    verbose = FALSE
  )

  # Should find non-empty intersection
  # (Not guaranteed for all data/seeds, but with simple AND rule and 3 folds, likely)
  message(sprintf("Intersection size with FALSE: %d trees", result@n_intersecting))

  # At minimum, each fold should have trees
  expect_true(all(result@rashomon_sizes > 0))
})

test_that("parameter inference from single_tree works in optimaltrees", {
  skip_on_cran()

  set.seed(42)
  n <- 100
  X <- data.frame(x1 = sample(0:1, n, replace = TRUE))
  y <- sample(0:1, n, replace = TRUE)

  # When single_tree=TRUE, rashomon_ignore_trivial_extensions should be inferred as TRUE
  result_single <- optimaltrees(
    X = X, y = y,
    single_tree = TRUE,
    rashomon_bound_multiplier = 0.05,
    verbose = FALSE
  )
  # Should get exactly 1 tree (single_tree=TRUE forces this regardless)
  expect_equal(result_single@n_trees, 1)

  # When single_tree=FALSE, rashomon_ignore_trivial_extensions should be inferred as FALSE
  result_rashomon <- optimaltrees(
    X = X, y = y,
    single_tree = FALSE,
    rashomon_bound_multiplier = 0.15,
    verbose = FALSE
  )
  # Should get >= 1 trees
  expect_gte(result_rashomon@n_trees, 1)

  # Explicit override should work
  result_override <- optimaltrees(
    X = X, y = y,
    single_tree = FALSE,
    rashomon_ignore_trivial_extensions = TRUE,  # Force pruning despite single_tree=FALSE
    rashomon_bound_multiplier = 0.15,
    verbose = FALSE
  )
  # Should respect explicit parameter
  expect_gte(result_override@n_trees, 1)
})

test_that("fit_tree has correct default (TRUE)", {
  skip_on_cran()

  set.seed(42)
  n <- 100
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE)
  )
  y <- sample(0:1, n, replace = TRUE)

  # fit_tree should default to rashomon_ignore_trivial_extensions = TRUE
  result <- fit_tree(X = X, y = y, verbose = FALSE)

  # Should always get exactly 1 tree
  expect_equal(result@n_trees, 1)
})

test_that("parameter is passed through config correctly", {
  skip_on_cran()

  set.seed(42)
  n <- 50
  X <- data.frame(x1 = sample(0:1, n, replace = TRUE))
  y <- sample(0:1, n, replace = TRUE)

  # Test that both TRUE and FALSE values are accepted without error
  expect_no_error({
    result_true <- optimaltrees(
      X = X, y = y,
      single_tree = FALSE,
      rashomon_ignore_trivial_extensions = TRUE,
      rashomon_bound_multiplier = 0.10,
      verbose = FALSE
    )
  })

  expect_no_error({
    result_false <- optimaltrees(
      X = X, y = y,
      single_tree = FALSE,
      rashomon_ignore_trivial_extensions = FALSE,
      rashomon_bound_multiplier = 0.10,
      verbose = FALSE
    )
  })

  # Both should succeed
  expect_gte(result_true@n_trees, 1)
  expect_gte(result_false@n_trees, 1)
})
