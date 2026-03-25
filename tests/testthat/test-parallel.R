# Test suite for parallel/multithreading functionality
# Tests worker_limit parameter and parallel execution consistency

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Setup test environment
setup_test_environment()

test_that("worker_limit parameter validation works", {
  # Test valid worker_limit values
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           worker_limit = 1L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test invalid worker_limit values
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, worker_limit = 0), 
               "worker_limit must be a positive integer")
  
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, worker_limit = -1), 
               "worker_limit must be a positive integer")
  
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, worker_limit = "invalid"), 
               "worker_limit must be a positive integer")
  
  expect_error(treefarms(simple_dataset$X, simple_dataset$y, worker_limit = c(1, 2)), 
               "worker_limit must be a positive integer")
})

test_that("single-threaded execution works as baseline", {
  # Test worker_limit = 1 (baseline)
  expect_no_error({
    model_single <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                  loss_function = "misclassification", 
                                  regularization = 0.1,
                                  worker_limit = 1L,
                                  verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_single)
  
  # Should complete successfully
  expect_true(is.finite(model_single$accuracy))
  expect_true(model_single$n_trees >= 0)
})

test_that("multithreaded execution works", {
  # Test worker_limit = 2
  expect_no_error({
    model_dual <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.1,
                                worker_limit = 2L,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_dual)
  
  # Test worker_limit = 4 (if system supports it)
  expect_no_error({
    model_quad <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                loss_function = "misclassification", 
                                regularization = 0.1,
                                worker_limit = 4L,
                                verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_quad)
})

test_that("results are consistent across different worker_limit values", {
  # Test with fixed seed for reproducibility
  set.seed(42)
  model_1 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 1L,
                            verbose = FALSE)
  
  set.seed(42)
  model_2 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 2L,
                            verbose = FALSE)
  
  set.seed(42)
  model_4 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
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

test_that("parallel execution works with log-loss", {
  # Test multithreading with log-loss
  set.seed(123)
  model_1_log <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                loss_function = "log_loss", 
                                regularization = 0.1,
                                worker_limit = 1L,
                                verbose = FALSE)
  
  set.seed(123)
  model_2_log <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                                loss_function = "log_loss", 
                                regularization = 0.1,
                                worker_limit = 2L,
                                verbose = FALSE)
  
  expect_valid_treefarms_model(model_1_log, "log_loss")
  expect_valid_treefarms_model(model_2_log, "log_loss")
  
  # Results should be consistent
  expect_models_consistent(model_1_log, model_2_log, tolerance = 1e-10)
})

test_that("RcppParallel thread options don't interfere", {
  # Test that RcppParallel::setThreadOptions() doesn't break functionality
  if (requireNamespace("RcppParallel", quietly = TRUE)) {
    # Set different thread options
    RcppParallel::setThreadOptions(numThreads = 2)
    
    expect_no_error({
      model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                             loss_function = "misclassification", 
                             regularization = 0.1,
                             worker_limit = 1L,
                             verbose = FALSE)
    })
    
    expect_valid_treefarms_model(model)
    
    # Reset to default
    RcppParallel::setThreadOptions(numThreads = "auto")
  }
})

test_that("parallel execution with different data sizes", {
  # Test with small dataset
  expect_no_error({
    model_small <- safe_treefarms(minimal_dataset$X, minimal_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 worker_limit = 2L,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_small)
  
  # Test with larger dataset
  expect_no_error({
    model_large <- safe_treefarms(many_features_dataset$X, many_features_dataset$y, 
                                 loss_function = "misclassification", 
                                 regularization = 0.1,
                                 worker_limit = 2L,
                                 verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_large)
})

test_that("parallel execution with imbalanced data", {
  # Test multithreading with imbalanced data
  expect_no_error({
    model <- safe_treefarms(imbalanced_dataset$X, imbalanced_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           worker_limit = 2L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Should handle imbalanced data without issues
  expect_true(is.finite(model@accuracy))
})

test_that("parallel execution with auto-tuning", {
  # Test multithreading with auto-tuning
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = NULL,
                           worker_limit = 2L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Auto-tuned parameters should be reasonable
  expect_true(model@regularization > 0)
})

test_that("performance sanity check", {
  # Test that higher worker_limit doesn't make things significantly slower
  # (This is a basic sanity check, not a rigorous performance test)
  
  # Use a dataset that's large enough to potentially benefit from parallelization
  large_data <- list(
    X = data.frame(
      f1 = sample(0:1, 500, replace = TRUE),
      f2 = sample(0:1, 500, replace = TRUE),
      f3 = sample(0:1, 500, replace = TRUE),
      f4 = sample(0:1, 500, replace = TRUE),
      f5 = sample(0:1, 500, replace = TRUE)
    ),
    y = sample(0:1, 500, replace = TRUE)
  )
  
  # Time single-threaded execution
  start_time_1 <- Sys.time()
  model_1 <- safe_treefarms(large_data$X, large_data$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 1L,
                            verbose = FALSE)
  time_1 <- as.numeric(Sys.time() - start_time_1, units = "secs")
  
  # Time multi-threaded execution
  start_time_2 <- Sys.time()
  model_2 <- safe_treefarms(large_data$X, large_data$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 2L,
                            verbose = FALSE)
  time_2 <- as.numeric(Sys.time() - start_time_2, units = "secs")
  
  expect_valid_treefarms_model(model_1)
  expect_valid_treefarms_model(model_2)
  
  # Results should be consistent
  expect_models_consistent(model_1, model_2, tolerance = 1e-10)
  
  # Multi-threaded shouldn't be dramatically slower (allowing for overhead)
  # This is a loose check - the main goal is that it doesn't crash
  expect_true(time_2 < time_1 * 2, 
              info = "Multi-threaded execution shouldn't be dramatically slower")
})

test_that("parallel execution with extreme parameters", {
  # Test with very low regularization (might generate many trees)
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.001,
                           worker_limit = 2L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
  
  # Test with very high regularization
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 10.0,
                           worker_limit = 2L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("parallel execution thread safety", {
  # Test that parallel execution doesn't cause race conditions
  # Run multiple models in sequence with different worker limits
  
  models <- list()
  for (i in 1:3) {
    expect_no_error({
      models[[i]] <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                   loss_function = "misclassification", 
                                   regularization = 0.1,
                                   worker_limit = (i %% 2) + 1,  # Alternate between 1 and 2
                                   verbose = FALSE)
    })
    
    expect_valid_treefarms_model(models[[i]])
  }
  
  # All models should be valid
  for (i in 1:3) {
    expect_true(is.finite(models[[i]]$accuracy))
    expect_true(models[[i]]$n_trees >= 0)
  }
})

test_that("parallel execution with verbose output", {
  # Test that verbose output works with parallel execution
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           worker_limit = 2L,
                           verbose = TRUE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("parallel execution cleanup", {
  # Test that parallel execution cleans up properly
  # This is important for preventing memory leaks
  
  # Run multiple models with different worker limits
  for (worker_limit in c(1L, 2L, 4L)) {
    expect_no_error({
      model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                             loss_function = "misclassification", 
                             regularization = 0.1,
                             worker_limit = worker_limit,
                             verbose = FALSE)
    })
    
    expect_valid_treefarms_model(model)
  }
  
  # Force garbage collection
  gc()
  
  # Should be able to run another model without issues
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           worker_limit = 1L,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

# Cleanup
teardown_test_environment()
