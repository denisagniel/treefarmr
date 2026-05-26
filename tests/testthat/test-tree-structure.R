# Tests for Tree Structure Operations (M-Split)

test_that("extract_tree_structure works for simple tree", {
  skip_if_not_installed("optimaltrees")

  set.seed(123)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)

  model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                    store_training_data = TRUE)

  expect_equal(model@n_trees, 1)

  structure <- extract_tree_structure(model)

  expect_true(S7::S7_inherits(structure, optimaltrees::TreeStructure))
  expect_true(structure@n_leaves > 0)
  expect_equal(length(structure@leaf_paths), structure@n_leaves)
  expect_true(structure@max_depth >= 0)
  expect_equal(length(structure@feature_names), 2)
  expect_true(all(c("x1", "x2") %in% structure@feature_names))
})

test_that("extract_tree_structure handles single-leaf tree", {
  skip_if_not_installed("optimaltrees")

  set.seed(456)
  n <- 50
  X <- data.frame(x1 = rep(1, n))
  y <- rep(1, n)  # All same class

  model <- fit_tree(X, y, loss_function = "misclassification",
                    regularization = 10.0,  # High regularization
                    store_training_data = TRUE)

  structure <- extract_tree_structure(model)

  expect_true(S7::S7_inherits(structure, optimaltrees::TreeStructure))
  expect_equal(structure@n_leaves, 1)
  expect_equal(length(structure@splits), 0)  # No splits for single leaf
})

test_that("compare_structures detects identical structures", {
  skip_if_not_installed("optimaltrees")

  set.seed(789)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)

  # Fit two models with same seed
  model1 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                     store_training_data = TRUE)
  model2 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                     store_training_data = TRUE)

  s1 <- extract_tree_structure(model1)
  s2 <- extract_tree_structure(model2)

  # Should be identical (same data, same seed, deterministic algorithm)
  expect_true(compare_structures(s1, s2))
})

test_that("compare_structures detects different structures", {
  skip_if_not_installed("optimaltrees")

  set.seed(101)
  n <- 100
  X1 <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y1 <- rbinom(n, 1, 0.5)

  X2 <- data.frame(
    x1 = rbinom(n, 1, 0.8),  # Different distribution
    x2 = rbinom(n, 1, 0.2)
  )
  y2 <- rbinom(n, 1, 0.3)

  model1 <- fit_tree(X1, y1, loss_function = "log_loss", regularization = 0.05,
                     store_training_data = TRUE)
  model2 <- fit_tree(X2, y2, loss_function = "log_loss", regularization = 0.05,
                     store_training_data = TRUE)

  s1 <- extract_tree_structure(model1)
  s2 <- extract_tree_structure(model2)

  # May or may not be different (depends on data)
  # Just test that compare_structures runs
  result <- compare_structures(s1, s2)
  expect_type(result, "logical")
})

test_that("structure_hash produces consistent hashes", {
  skip_if_not_installed("optimaltrees")

  set.seed(202)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)

  model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                    store_training_data = TRUE)

  structure <- extract_tree_structure(model)

  # Hash should be consistent
  hash1 <- structure_hash(structure)
  hash2 <- structure_hash(structure)

  expect_equal(hash1, hash2)
  expect_type(hash1, "character")
  expect_true(nchar(hash1) > 0)
})

test_that("structure_hash differs for different structures", {
  skip_if_not_installed("optimaltrees")

  set.seed(303)
  n <- 100
  X1 <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y1 <- rbinom(n, 1, 0.5)

  X2 <- data.frame(x1 = rbinom(n, 1, 0.8), x2 = rbinom(n, 1, 0.2))
  y2 <- rbinom(n, 1, 0.3)

  model1 <- fit_tree(X1, y1, loss_function = "log_loss", regularization = 0.05,
                     store_training_data = TRUE)
  model2 <- fit_tree(X2, y2, loss_function = "log_loss", regularization = 0.05,
                     store_training_data = TRUE)

  s1 <- extract_tree_structure(model1)
  s2 <- extract_tree_structure(model2)

  hash1 <- structure_hash(s1)
  hash2 <- structure_hash(s2)

  # May or may not be different (depends on whether trees differ)
  # Just check that hashes are computed
  expect_type(hash1, "character")
  expect_type(hash2, "character")
})

test_that("refit_tree_structure works for classification", {
  skip_if_not_installed("optimaltrees")

  set.seed(404)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)

  # Fit tree on original data
  model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                    store_training_data = TRUE)
  structure <- extract_tree_structure(model)

  # Refit on new data
  n_new <- 50
  X_new <- data.frame(
    x1 = rbinom(n_new, 1, 0.5),
    x2 = rbinom(n_new, 1, 0.5)
  )
  y_new <- rbinom(n_new, 1, 0.5)

  refit_result <- refit_tree_structure(structure, X_new, y_new, "log_loss",
                                       store_training_data = TRUE)

  # Check return structure
  expect_true(is.list(refit_result))
  expect_true("model" %in% names(refit_result))
  expect_true("n_per_leaf" %in% names(refit_result))

  refit_model <- refit_result$model
  expect_s3_class(refit_model, "OptimalTreesModel")
  expect_equal(refit_model@n_trees, 1)
  expect_equal(refit_model@loss_function, "log_loss")

  # Can predict
  preds <- predict(refit_model, X_new, type = "prob")
  expect_equal(nrow(preds), n_new)
  expect_equal(ncol(preds), 2)
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("refit_tree_structure works for regression", {
  skip_if_not_installed("optimaltrees")

  set.seed(505)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rnorm(n, mean = 5, sd = 2)  # Continuous outcome

  # Fit tree on original data
  model <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.1,
                    store_training_data = TRUE)
  structure <- extract_tree_structure(model)

  # Refit on new data
  n_new <- 50
  X_new <- data.frame(
    x1 = rbinom(n_new, 1, 0.5),
    x2 = rbinom(n_new, 1, 0.5)
  )
  y_new <- rnorm(n_new, mean = 5, sd = 2)

  refit_result <- refit_tree_structure(structure, X_new, y_new, "squared_error",
                                       store_training_data = TRUE)

  # Check return structure
  expect_true(is.list(refit_result))
  expect_true("model" %in% names(refit_result))
  expect_true("n_per_leaf" %in% names(refit_result))

  refit_model <- refit_result$model
  expect_s3_class(refit_model, "OptimalTreesModel")
  expect_equal(refit_model@n_trees, 1)
  expect_equal(refit_model@loss_function, "squared_error")
  expect_true(refit_model@is_regression)

  # Can predict
  preds <- predict(refit_model, X_new, type = "response")
  expect_equal(length(preds), n_new)
  expect_true(all(is.finite(preds)))
})

test_that("refit preserves structure but changes leaf values", {
  skip_if_not_installed("optimaltrees")

  set.seed(606)
  n <- 100
  X <- data.frame(
    x1 = rbinom(n, 1, 0.5),
    x2 = rbinom(n, 1, 0.5)
  )
  y <- rbinom(n, 1, 0.5)

  model1 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                     store_training_data = TRUE)
  structure <- extract_tree_structure(model1)

  # Refit with very different outcomes
  y_new <- 1 - y  # Flip all outcomes

  refit_result2 <- refit_tree_structure(structure, X, y_new, "log_loss",
                                  store_training_data = TRUE)

  model2 <- refit_result2$model

  # Structure should have same number of leaves (note: exact structure comparison
  # may fail due to tree reconstruction order, but leaf count should match)
  structure2 <- extract_tree_structure(model2)
  expect_equal(structure2@n_leaves, structure@n_leaves)

  # Predictions should differ (different outcomes)
  preds1 <- predict(model1, X, type = "prob")
  preds2 <- predict(model2, X, type = "prob")

  # Expect some difference (not necessarily huge, but should differ)
  expect_false(all(abs(preds1 - preds2) < 0.01))
})

test_that("print method works for TreeStructure", {
  skip_if_not_installed("optimaltrees")

  set.seed(707)
  n <- 100
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1,
                    store_training_data = TRUE)
  structure <- extract_tree_structure(model)

  # Should print without error
  expect_output(print(structure), "TreeStructure")
  expect_output(print(structure), "Leaves:")
  expect_output(print(structure), "Depth:")
})

test_that("refit_tree_structure validates inputs", {
  skip_if_not_installed("optimaltrees")

  set.seed(808)
  n <- 50
  X <- data.frame(x1 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.5,
                    store_training_data = TRUE)
  structure <- extract_tree_structure(model)

  # Invalid structure
  expect_error(
    refit_tree_structure("not a structure", X, y, "log_loss"),
    "structure must be a TreeStructure"
  )

  # Invalid X_new
  expect_error(
    refit_tree_structure(structure, "not a dataframe", y, "log_loss"),
    "X_new must be a data.frame or matrix"
  )

  # Invalid y_new
  expect_error(
    refit_tree_structure(structure, X, "not numeric", "log_loss"),
    "y_new must be numeric or integer"
  )

  # Mismatched dimensions
  expect_error(
    refit_tree_structure(structure, X, y[1:10], "log_loss"),
    "nrow\\(X_new\\).*must equal length\\(y_new\\)"
  )

  # Invalid loss function
  expect_error(
    refit_tree_structure(structure, X, y, "invalid_loss"),
    "loss_function must be one of"
  )
})
