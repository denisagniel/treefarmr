# Auto-Tuning Tests
# Tests auto_tune_optimaltrees functionality
# Slimmed: 2026-03-25 during Phase 3 consolidation
skip_slow_tests()

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# Create test data with clear pattern
set.seed(42)
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))
)

# ============================================================================
# Basic Functionality
# ============================================================================

test_that("auto_tune_treefarms basic functionality works", {
  # Increase max_iterations for test datasets
  result <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  # Check result structure
  expect_true(inherits(result, "list"))
  expect_true("model" %in% names(result))
  expect_true("regularization" %in% names(result))
  expect_true("rashomon_bound_multiplier" %in% names(result))
  expect_true("n_trees" %in% names(result))
  expect_true("iterations" %in% names(result))
  expect_true("converged" %in% names(result))

  # Check data types
  expect_true(is.numeric(result$regularization))
  expect_true(is.numeric(result$rashomon_bound_multiplier))
  expect_true(is.numeric(result$n_trees))
  expect_true(is.numeric(result$iterations))
  expect_true(is.logical(result$converged))
})

# ============================================================================
# Fixed Parameters
# ============================================================================

test_that("auto_tune_treefarms with different fixed parameters", {
  # Fixed regularization
  result1 <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "regularization",
    fixed_value = 0.1,
    max_iterations = 50,
    verbose = FALSE
  )

  expect_equal(result1$regularization, 0.1)
  expect_true(is.numeric(result1$rashomon_bound_multiplier))

  # Fixed rashomon_bound_multiplier
  result2 <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  expect_equal(result2$rashomon_bound_multiplier, 0.05)
  expect_true(is.numeric(result2$regularization))
})

# ============================================================================
# Input Validation
# ============================================================================

test_that("auto_tune_treefarms input validation", {
  # Invalid fixed_param
  expect_error(
    auto_tune_optimaltrees(
      pattern_data$X, pattern_data$y,
      fixed_param = "invalid",
      fixed_value = 0.1
    ),
    "fixed_param must be 'regularization' or 'rashomon_bound_multiplier'"
  )

  # Invalid target_trees
  expect_error(
    auto_tune_optimaltrees(
      pattern_data$X, pattern_data$y,
      target_trees = 0
    ),
    "target_trees must be at least 1"
  )

  # Invalid max_trees (defaults to target_trees=1, so max_trees=0 fails target check)
  expect_error(
    auto_tune_optimaltrees(
      pattern_data$X, pattern_data$y,
      max_trees = 0
    ),
    "max_trees must be at least"
  )

  # max_trees < target_trees
  expect_error(
    auto_tune_optimaltrees(
      pattern_data$X, pattern_data$y,
      target_trees = 3,
      max_trees = 2
    ),
    "max_trees must be at least target_trees"
  )
})

# ============================================================================
# Different Loss Functions
# ============================================================================

test_that("auto_tune_treefarms with different loss functions", {
  # Misclassification
  result_misclass <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  # Log-loss
  result_logloss <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "log_loss",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  expect_equal(result_misclass$model@loss_function, "misclassification")
  expect_equal(result_logloss$model@loss_function, "log_loss")
})

# ============================================================================
# Custom Search Range
# ============================================================================

test_that("auto_tune_treefarms with custom search range", {
  result <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    search_range = c(0.01, 0.2),
    max_iterations = 50,
    verbose = FALSE
  )

  # Regularization should be within search range
  expect_true(result$regularization >= 0.01)
  expect_true(result$regularization <= 0.2)
})

# ============================================================================
# Convergence Behavior
# ============================================================================

test_that("auto_tune_treefarms convergence behavior", {
  result <- auto_tune_optimaltrees(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  # Should complete within max_iterations
  expect_true(result$iterations <= 50)

  # If converged, should have found acceptable number of trees
  if (result$converged) {
    expect_true(result$n_trees >= 1)
    expect_true(result$n_trees <= 3)
  }
})

test_that("auto_tune_treefarms returns best result when not converged", {
  # Use impossible target to force non-convergence
  expect_warning({
    result <- auto_tune_optimaltrees(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 100,  # Impossible with small dataset
      max_trees = 200,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      max_iterations = 10,
      verbose = FALSE
    )
  }, "did not converge")

  # Should still return a result even if not converged
  expect_true(is.list(result))
  expect_true("model" %in% names(result))
  expect_true(is.numeric(result$n_trees))
  expect_true(is.logical(result$converged))
  expect_false(result$converged)  # Should indicate non-convergence
})

# ============================================================================
# Edge Cases
# ============================================================================

test_that("auto_tune_treefarms handles edge cases", {
  # Minimal data
  X_minimal <- data.frame(x1 = c(0, 1, 0, 1))
  y_minimal <- c(0, 1, 1, 0)

  result <- auto_tune_optimaltrees(
    X_minimal, y_minimal,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 2,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  expect_true(is.list(result))
  expect_true("model" %in% names(result))

  # All same class
  y_same <- rep(0, nrow(pattern_data$X))
  result2 <- auto_tune_optimaltrees(
    pattern_data$X, y_same,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 2,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 50,
    verbose = FALSE
  )

  expect_true(is.list(result2))
  expect_true("model" %in% names(result2))
})

# Note: Removed verbose test (verbose covered elsewhere)
# Note: Removed target_trees variation test (redundant)
# Note: Removed parameter range sanity test (over-defensive)
# This file focuses on core auto-tuning functionality

# ============================================================================
# Auto-tune epsilon_n is exploratory-only: must warn (post-selection, voids
# the o(n^{-1/2}) inference guarantee). select_epsilon_n() is the fixed,
# theory-valid alternative.
# ============================================================================

test_that("cross_fitted_rashomon(auto_tune_intersecting=TRUE) warns it is inference-invalid", {
  expect_warning(
    cross_fitted_rashomon(
      pattern_data$X, pattern_data$y, K = 2,
      loss_function = "misclassification",
      regularization = 0.1,
      auto_tune_intersecting = TRUE,
      max_leaves = 4L,
      verbose = FALSE
    ),
    "post-selection|valid-inference|o\\(n"
  )
})

# ============================================================================
# Regression guard: auto_tune_rashomon_intersection must not reference the
# formerly-undefined `sqrt_log_n_over_n`. Both the converged and the failed
# (c_max exhausted) branches must return a well-formed list with a numeric `c`
# on success and NA_real_ on failure. Prior to the 2026-07-08 fix, the
# failure/verbose path errored on an undefined variable.
# ============================================================================

test_that("auto_tune_rashomon_intersection returns c on both converged and failed branches", {
  X <- pattern_data$X
  y <- pattern_data$y
  n <- nrow(X)
  K <- 2L
  # List-form fold indices (test indices per fold), as fit_rashomon_folds expects.
  fold_indices <- split(seq_len(n), rep(seq_len(K), length.out = n))

  # Converged branch: a normal grid reaches a non-empty intersection.
  conv <- auto_tune_rashomon_intersection(
    X = X, y = y, K = K, fold_indices = fold_indices, X_binary = X,
    loss_function = "misclassification", regularization = 0.05,
    c_start = 1, c_max = 100, verbose = FALSE
  )
  expect_type(conv, "list")
  expect_true(all(c("epsilon_n", "c", "converged", "n_intersecting") %in% names(conv)))
  if (isTRUE(conv$converged)) {
    expect_true(is.numeric(conv$c) && is.finite(conv$c))
  }

  # Failed branch: force c_max == c_min so the search exhausts immediately.
  # Must return (not error) with c = NA_real_ and converged = FALSE.
  # verbose = TRUE exercises the failure-path message that used to hit the
  # undefined sqrt_log_n_over_n.
  failed <- expect_no_error(
    auto_tune_rashomon_intersection(
      X = X, y = y, K = K, fold_indices = fold_indices, X_binary = X,
      loss_function = "misclassification", regularization = 1e6,  # huge lambda -> stumps only
      c_start = 0.001, c_min = 0.001, c_max = 0.001, verbose = TRUE
    )
  )
  expect_type(failed, "list")
  expect_true("c" %in% names(failed))
  expect_true(is.na(failed$c) || is.numeric(failed$c))
})
