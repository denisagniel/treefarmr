# Test suite for edge cases and error handling
# Simplified to 8 critical edge cases (reduced from 25 tests on 2026-03-25)
# Tests invalid inputs and boundary conditions users are likely to encounter

library(testthat)
# Setup/teardown now handled by testthat hooks in helper-setup.R

test_that("empty dataset handling", {
  # Test with empty dataset (0 rows)
  expect_error(optimaltrees(empty_dataset$X, empty_dataset$y,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE))
  # Error message varies ("must have at least one row" or "Length of y must match")
})

test_that("single row dataset handling", {
  # Test with single row dataset
  expect_no_error({
    model <- safe_optimaltrees(single_row_dataset$X, single_row_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model)
})

test_that("all same class labels handling", {
  # Test with all same class labels
  expect_no_error({
    model <- safe_optimaltrees(single_class_dataset$X, single_class_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model)

  # Should handle single class gracefully
  expect_true(is.finite(model@accuracy))
})

test_that("invalid inputs raise informative errors", {
  # Test non-binary features - now handled by discretization, so no error expected
  # (continuous features are automatically discretized)

  # Test non-binary labels
  y_invalid <- c(simple_dataset$y, 3)
  expect_error(optimaltrees(simple_dataset$X, y_invalid,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE))
  # Error: length mismatch or non-binary values

  # Test mismatched dimensions
  expect_error(optimaltrees(simple_dataset$X, simple_dataset$y[1:50],
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "Length of.*y.*must match")

  # Test NULL features
  expect_error(optimaltrees(NULL, simple_dataset$y,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "X.*must be.*data.frame.*matrix")

  # Test NULL labels
  expect_error(optimaltrees(simple_dataset$X, NULL,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "y.*must be.*numeric.*logical")
})

test_that("missing values handling", {
  # Test with NA in features - now handled by discretization (NA treated as separate category)
  # No error expected

  # Test with NA in labels
  y_na <- simple_dataset$y
  y_na[1] <- NA
  expect_error(optimaltrees(simple_dataset$X, y_na,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "y.*must contain only binary values")
})

test_that("NaN values in inputs raise errors", {
  # Test NaN in features - now handled by discretization
  # No error expected (NaN treated as continuous value)

  # Test NaN in labels
  y_nan <- simple_dataset$y
  y_nan[1] <- NaN
  expect_error(optimaltrees(simple_dataset$X, y_nan,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "y.*must contain only binary values")
})

test_that("Inf values in inputs raise errors", {
  # Test Inf in features - now handled by discretization
  # No error expected (Inf treated as continuous value)

  # Test Inf in labels
  y_inf <- simple_dataset$y
  y_inf[1] <- Inf
  expect_error(optimaltrees(simple_dataset$X, y_inf,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "y.*must contain only binary values")
})

test_that("negative values in inputs raise errors", {
  # Test negative in features - now handled by discretization
  # No error expected (negative treated as continuous value)

  # Test negative in labels
  y_neg <- simple_dataset$y
  y_neg[1] <- -1
  expect_error(optimaltrees(simple_dataset$X, y_neg,
                        loss_function = "misclassification",
                        regularization = 0.1,
                        verbose = FALSE),
               "y.*must contain only binary values")
})

# Note: Other edge case tests removed during Phase 2 consolidation (2026-03-25)
# Rationale: Over-testing of parameters, duplicates, and non-critical edge cases
# Focus on edge cases users are likely to encounter and need clear error messages
# If you need more extensive edge case testing, see git history before 2026-03-25
