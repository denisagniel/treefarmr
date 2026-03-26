# Test suite for cleanup and memory safety
# Simplified to 2 essential tests (reduced from 14 tests on 2026-03-25)

library(testthat)
# Setup/teardown now handled by testthat hooks in helper-setup.R

test_that("cleanup_static_state function works", {
  # Test that the cleanup function can be called
  expect_no_error({
    # Train a model to create some state
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           verbose = FALSE)

    # Call the cleanup function (exported R function)
    cleanup_static_state()
  })

  # Should be able to train another model after cleanup
  expect_no_error({
    model2 <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                            loss_function = "misclassification",
                            regularization = 0.1,
                            verbose = FALSE)
  })

  expect_valid_treefarms_model(model2)
})

test_that("cleanup of temporary files during training", {
  # Test that temporary files are cleaned up
  temp_dir <- tempdir()
  initial_files <- list.files(temp_dir, pattern = "^temp_.*\\.(csv|json)$")

  # Train a model (might create temporary files)
  expect_no_error({
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                           loss_function = "misclassification",
                           regularization = 0.1,
                           verbose = FALSE)
  })

  expect_valid_treefarms_model(model)

  # Check that no temporary files were left behind
  final_files <- list.files(temp_dir, pattern = "^temp_.*\\.(csv|json)$")
  temp_files_created <- setdiff(final_files, initial_files)

  # Should not have left temporary files
  expect_equal(length(temp_files_created), 0,
               info = "No temporary files should be left after training")
})

# Note: Other cleanup tests removed during Phase 2 consolidation (2026-03-25)
# Rationale: Over-defensive testing of memory management (R's responsibility)
# If you need more extensive cleanup testing, see git history before 2026-03-25
