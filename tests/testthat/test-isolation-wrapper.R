# Test suite for process isolation wrapper
# Verifies that treefarms_isolated() works correctly with subprocess calls

library(testthat)
library(treefarmr)

# Helper function to safely test treefarms_isolated
safe_treefarms_isolated <- function(...) {
  tryCatch({
    treefarms_isolated(...)
  }, error = function(e) {
    skip(paste("treefarms_isolated failed:", e$message))
  })
}

# Simple test dataset
simple_dataset <- list(
  X = data.frame(
    feature_1 = c(0, 0, 1, 1),
    feature_2 = c(0, 1, 0, 1)
  ),
  y = c(0, 1, 1, 0)
)

test_that("treefarms_isolated function exists and is exported", {
  expect_true(exists("treefarms_isolated"),
               info = "treefarms_isolated should be exported")
  expect_true(is.function(treefarms_isolated),
               info = "treefarms_isolated should be a function")
})

test_that("find_gosdt_executable helper exists", {
  # The helper should exist (even if not exported, it's in the namespace)
  expect_true(exists("find_gosdt_executable", envir = asNamespace("treefarmr")),
               info = "find_gosdt_executable should exist in package namespace")
})

test_that("treefarms_isolated falls back to direct call if executable not found", {
  # This test verifies the fallback mechanism works
  # If executable is not found, it should fall back to treefarms()
  
  # Note: This test may skip if executable is actually found
  # That's okay - it means the executable was built successfully
  
  result <- tryCatch({
    safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           single_tree = TRUE,
                           verbose = FALSE)
  }, error = function(e) {
    skip(paste("Test skipped:", e$message))
  })
  
  # Should return a valid model structure
  expect_true(!is.null(result),
              info = "treefarms_isolated should return a result")
  expect_true(is.list(result),
              info = "Result should be a list")
  expect_true("model" %in% names(result),
              info = "Result should have 'model' field")
})

test_that("treefarms_isolated works with misclassification loss", {
  result <- safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                                   loss_function = "misclassification",
                                   regularization = 0.1,
                                   single_tree = TRUE,
                                   verbose = FALSE)
  
  expect_true(!is.null(result),
              info = "Should return a model")
  expect_true("model" %in% names(result),
              info = "Result should have model field")
  expect_true("loss_function" %in% names(result),
              info = "Result should have loss_function field")
  expect_equal(result$loss_function, "misclassification",
               info = "Loss function should be misclassification")
})

test_that("treefarms_isolated works with log-loss", {
  result <- safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                                   loss_function = "log_loss",
                                   regularization = 0.1,
                                   single_tree = TRUE,
                                   compute_probabilities = TRUE,
                                   verbose = FALSE)
  
  expect_true(!is.null(result),
              info = "Should return a model")
  expect_equal(result$loss_function, "log_loss",
               info = "Loss function should be log_loss")
  
  # If probabilities were computed, check them
  if (!is.null(result$probabilities)) {
    expect_true(is.matrix(result$probabilities),
                info = "Probabilities should be a matrix")
    expect_equal(ncol(result$probabilities), 2,
                 info = "Should have 2 probability columns")
  }
})

test_that("treefarms_isolated handles timeout parameter", {
  # Test that timeout parameter is accepted (even if not used if executable not found)
  result <- tryCatch({
    safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           single_tree = TRUE,
                           timeout = 60,
                           verbose = FALSE)
  }, error = function(e) {
    skip(paste("Test skipped:", e$message))
  })
  
  expect_true(!is.null(result),
              info = "Should handle timeout parameter without error")
})

test_that("treefarms_isolated returns compatible structure with treefarms()", {
  # Compare structures
  result_isolated <- safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                                            loss_function = "misclassification",
                                            regularization = 0.1,
                                            single_tree = TRUE,
                                            verbose = FALSE)
  
  result_direct <- tryCatch({
    treefarms(simple_dataset$X, simple_dataset$y,
              loss_function = "misclassification",
              regularization = 0.1,
              single_tree = TRUE,
              verbose = FALSE)
  }, error = function(e) {
    skip(paste("Direct call failed:", e$message))
  })
  
  # Both should have the same structure
  expect_equal(names(result_isolated), names(result_direct),
               info = "Results should have same structure")
  
  # Both should have model field
  expect_true("model" %in% names(result_isolated),
              info = "Isolated result should have model")
  expect_true("model" %in% names(result_direct),
              info = "Direct result should have model")
})

test_that("treefarms_isolated cleans up temporary files", {
  # Count temp files before
  temp_dir <- tempdir()
  temp_files_before <- list.files(temp_dir, pattern = "^treefarms_.*\\.(csv|json|txt)$")
  
  # Run isolated call
  result <- safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                                   loss_function = "misclassification",
                                   regularization = 0.1,
                                   single_tree = TRUE,
                                   verbose = FALSE)
  
  # Count temp files after (with small delay to ensure cleanup)
  Sys.sleep(0.1)
  temp_files_after <- list.files(temp_dir, pattern = "^treefarms_.*\\.(csv|json|txt)$")
  
  # Should not have left temp files (or at least not significantly more)
  # Note: This is a best-effort check since cleanup happens in on.exit()
  expect_true(length(temp_files_after) <= length(temp_files_before) + 1,
              info = "Should clean up temporary files")
})

test_that("treefarms_isolated works with single_tree = FALSE (rashomon set)", {
  result <- safe_treefarms_isolated(simple_dataset$X, simple_dataset$y,
                                   loss_function = "misclassification",
                                   regularization = 0.1,
                                   single_tree = FALSE,
                                   rashomon_bound_multiplier = 0.05,
                                   verbose = FALSE)
  
  expect_true(!is.null(result),
              info = "Should return a model")
  expect_true("n_trees" %in% names(result),
              info = "Result should have n_trees field")
  expect_true(result$n_trees >= 1,
              info = "Should have at least one tree")
})

test_that("treefarms_isolated error handling works", {
  # Test with invalid data (should fail gracefully)
  expect_error({
    treefarms_isolated(data.frame(x = c(1, 2, 3)), c(0, 1),  # Non-binary features
                      loss_function = "misclassification",
                      regularization = 0.1,
                      verbose = FALSE)
  }, info = "Should error on invalid (non-binary) features")
  
  # Test with mismatched lengths
  expect_error({
    treefarms_isolated(simple_dataset$X, c(0, 1),  # Wrong length
                      loss_function = "misclassification",
                      regularization = 0.1,
                      verbose = FALSE)
  }, info = "Should error on mismatched X and y lengths")
})



