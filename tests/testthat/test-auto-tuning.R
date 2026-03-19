# Test suite for auto-tuning functionality

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Create test data with clear pattern for auto-tuning
set.seed(42)
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

test_that("auto_tune_treefarms basic functionality works", {
  expect_no_error({
    result <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
  
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

test_that("auto_tune_treefarms with different fixed parameters", {
  # Test with fixed regularization
  expect_no_error({
    result1 <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "regularization",
      fixed_value = 0.1,
      verbose = FALSE
    )
  })
  
  expect_equal(result1$regularization, 0.1)
  expect_true(is.numeric(result1$rashomon_bound_multiplier))
  
  # Test with fixed rashomon_bound_multiplier
  expect_no_error({
    result2 <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
  
  expect_equal(result2$rashomon_bound_multiplier, 0.05)
  expect_true(is.numeric(result2$regularization))
})

test_that("auto_tune_treefarms input validation", {
  # Test invalid fixed_param
  expect_error(
    auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      fixed_param = "invalid",
      fixed_value = 0.1
    ),
    "fixed_param must be 'regularization' or 'rashomon_bound_multiplier'"
  )
  
  # Test invalid target_trees
  expect_error(
    auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      target_trees = 0
    ),
    "target_trees must be at least 1"
  )
  
  # Test invalid max_trees
  expect_error(
    auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      max_trees = 0
    ),
    "max_trees must be at least 1"
  )
  
  # Test max_trees < target_trees
  expect_error(
    auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      target_trees = 3,
      max_trees = 2
    ),
    "max_trees must be at least target_trees"
  )
})

test_that("auto_tune_treefarms with different loss functions", {
  # Test with misclassification
  expect_no_error({
    result_misclass <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
  
  # Test with log_loss
  expect_no_error({
    result_logloss <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "log_loss",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
  
  expect_equal(result_misclass$model$loss_function, "misclassification")
  expect_equal(result_logloss$model$loss_function, "log_loss")
})

test_that("auto_tune_treefarms with custom search range", {
  expect_no_error({
    result <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      search_range = c(0.01, 0.2),
      verbose = FALSE
    )
  })
  
  # Regularization should be within search range
  expect_true(result$regularization >= 0.01)
  expect_true(result$regularization <= 0.2)
})

test_that("auto_tune_treefarms convergence behavior", {
  # Test with reasonable parameters that should converge
  result <- auto_tune_treefarms(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 10,
    verbose = FALSE
  )
  
  # Should complete within max_iterations
  expect_true(result$iterations <= 10)
  
  # If converged, should have found acceptable number of trees
  if (result$converged) {
    expect_true(result$n_trees >= 1)
    expect_true(result$n_trees <= 3)
  }
})

test_that("auto_tune_treefarms verbose output", {
  # Test that verbose doesn't cause errors
  expect_no_error({
    result <- auto_tune_treefarms(
      pattern_data$X, pattern_data$y,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 3,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = TRUE
    )
  })
})

test_that("auto_tune_treefarms returns best result when not converged", {
  # Use parameters that might not converge to exact target
  result <- auto_tune_treefarms(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 5,  # High target
    max_trees = 10,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    max_iterations = 5,  # Low iterations
    verbose = FALSE
  )
  
  # Should still return a result even if not converged
  expect_true(is.list(result))
  expect_true("model" %in% names(result))
  expect_true(is.numeric(result$n_trees))
})

test_that("auto_tune_treefarms with different target_trees values", {
  # Test with different target values
  for (target in c(1, 2, 3)) {
    expect_no_error({
      result <- auto_tune_treefarms(
        pattern_data$X, pattern_data$y,
        loss_function = "misclassification",
        target_trees = target,
        max_trees = target + 2,
        fixed_param = "rashomon_bound_multiplier",
        fixed_value = 0.05,
        verbose = FALSE
      )
    })
    
    expect_true(is.numeric(result$n_trees))
    expect_true(result$n_trees >= 0)
  }
})

test_that("auto_tune_treefarms handles edge cases", {
  # Test with minimal data
  X_minimal <- data.frame(x1 = c(0, 1, 0, 1))
  y_minimal <- c(0, 1, 1, 0)
  
  expect_no_error({
    result <- auto_tune_treefarms(
      X_minimal, y_minimal,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 2,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
  
  # Test with all same class
  y_same <- rep(0, nrow(pattern_data$X))
  expect_no_error({
    result <- auto_tune_treefarms(
      pattern_data$X, y_same,
      loss_function = "misclassification",
      target_trees = 1,
      max_trees = 2,
      fixed_param = "rashomon_bound_multiplier",
      fixed_value = 0.05,
      verbose = FALSE
    )
  })
})

test_that("auto_tune_treefarms parameter ranges are reasonable", {
  result <- auto_tune_treefarms(
    pattern_data$X, pattern_data$y,
    loss_function = "misclassification",
    target_trees = 1,
    max_trees = 3,
    fixed_param = "rashomon_bound_multiplier",
    fixed_value = 0.05,
    verbose = FALSE
  )
  
  # Regularization should be positive
  expect_true(result$regularization > 0)
  
  # Rashomon bound multiplier should be positive
  expect_true(result$rashomon_bound_multiplier > 0)
  
  # Number of trees should be non-negative
  expect_true(result$n_trees >= 0)
  
  # Iterations should be positive
  expect_true(result$iterations > 0)
})


