# Parallel Execution Tests
# Tests worker_limit parameter and parallel execution consistency
# Slimmed: 2026-03-25 during Phase 3 consolidation

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# ============================================================================
# Worker Limit Validation
# ============================================================================

test_that("worker_limit parameter validation works", {
  # Valid worker_limit
  expect_no_error({
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           worker_limit = 1L,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model)

  # Invalid worker_limit values
  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y, worker_limit = 0),
               "must be a positive integer")

  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y, worker_limit = -1),
               "must be a positive integer")

  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y, worker_limit = "invalid"),
               "must be a positive integer")

  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y, worker_limit = c(1, 2)),
               "must be a positive integer")
})

# ============================================================================
# Consistency Across Worker Limits
# ============================================================================

test_that("results are consistent across different worker_limit values", {
  # Test with fixed seed for reproducibility
  set.seed(42)
  model_1 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                            loss_function = "misclassification",
                            regularization = 0.1,
                            worker_limit = 1L,
                            verbose = FALSE)

  set.seed(42)
  model_2 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                            loss_function = "misclassification",
                            regularization = 0.1,
                            worker_limit = 2L,
                            verbose = FALSE)

  set.seed(42)
  model_4 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                            loss_function = "misclassification",
                            regularization = 0.1,
                            worker_limit = 4L,
                            verbose = FALSE)

  expect_valid_treefarms_model(model_1)
  expect_valid_treefarms_model(model_2)
  expect_valid_treefarms_model(model_4)

  # Results should be numerically identical (within floating-point tolerance)
  expect_models_consistent(model_1, model_2, tolerance = 1e-10)
  expect_models_consistent(model_1, model_4, tolerance = 1e-10)
  expect_models_consistent(model_2, model_4, tolerance = 1e-10)
})

# ============================================================================
# Different Loss Functions
# ============================================================================

test_that("parallel execution works with log-loss", {
  set.seed(123)
  model_1_log <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                                loss_function = "log_loss",
                                regularization = 0.1,
                                worker_limit = 1L,
                                verbose = FALSE)

  set.seed(123)
  model_2_log <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                                loss_function = "log_loss",
                                regularization = 0.1,
                                worker_limit = 2L,
                                verbose = FALSE)

  expect_valid_treefarms_model(model_1_log, "log_loss")
  expect_valid_treefarms_model(model_2_log, "log_loss")

  # Results should be consistent
  expect_models_consistent(model_1_log, model_2_log, tolerance = 1e-10)
})

# ============================================================================
# Different Data Sizes
# ============================================================================

test_that("parallel execution with different data sizes", {
  # Small dataset
  expect_no_error({
    model_small <- safe_optimaltrees(minimal_dataset$X, minimal_dataset$y,
                                 loss_function = "misclassification",
                                 regularization = 0.1,
                                 worker_limit = 2L,
                                 verbose = FALSE)
  })

  expect_valid_treefarms_model(model_small)

  # Larger dataset
  expect_no_error({
    model_large <- safe_optimaltrees(many_features_dataset$X, many_features_dataset$y,
                                 loss_function = "misclassification",
                                 regularization = 0.1,
                                 worker_limit = 2L,
                                 verbose = FALSE)
  })

  expect_valid_treefarms_model(model_large)
})

# ============================================================================
# Thread Safety
# ============================================================================

test_that("parallel execution thread safety", {
  # Run multiple models in sequence with different worker limits
  # to test for race conditions

  models <- list()
  for (i in 1:3) {
    expect_no_error({
      models[[i]] <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                   loss_function = "misclassification",
                                   regularization = 0.1,
                                   worker_limit = (i %% 2) + 1,  # Alternate between 1 and 2
                                   verbose = FALSE)
    })

    expect_valid_treefarms_model(models[[i]])
  }

  # All models should be valid
  for (i in 1:3) {
    expect_true(is.finite(models[[i]]@accuracy))
    expect_true(models[[i]]@n_trees >= 0)
  }
})

# Note: Removed redundant parallel execution tests (baseline, RcppParallel, performance)
# Note: Removed over-defensive cleanup and verbose tests
# This file focuses on core parallel correctness and consistency
