# Test suite for cleanup and memory safety
# Tests .onUnload hook, cleanup functions, and memory management

library(testthat)
library(treefarmr)

# Setup test environment
setup_test_environment()

test_that(".onUnload hook executes without error", {
  # Test that the package can be unloaded without crashing
  expect_no_error({
    # Load the package
    library(treefarms)
    
    # Train a model to create some state
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
    
    # Unload the package (this should trigger .onUnload)
    unloadNamespace("treefarms")
  })
  
  # Reload the package for subsequent tests
  library(treefarms)
})

test_that("cleanup_static_state C++ function works", {
  # Test that the C++ cleanup function can be called
  expect_no_error({
    # Train a model to create some state
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
    
    # Call the cleanup function directly
    .Call("_treefarms_cleanup_static_state")
  })
  
  # Should be able to train another model after cleanup
  expect_no_error({
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model2)
})

test_that("repeated load/unload cycles work", {
  # Test multiple load/unload cycles
  for (i in 1:3) {
    expect_no_error({
      # Unload if already loaded
      if ("treefarms" %in% loadedNamespaces()) {
        unloadNamespace("treefarms")
      }
      
      # Load the package
      library(treefarms)
      
      # Train a model
      model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                             loss_function = "misclassification", 
                             regularization = 0.1,
                             verbose = FALSE)
      
      expect_valid_treefarms_model(model)
    })
  }
  
  # Final state should be clean
  expect_true("treefarms" %in% loadedNamespaces())
})

test_that("no segfaults after cleanup", {
  # Test that cleanup prevents segfaults
  expect_no_error({
    # Train multiple models
    models <- list()
    for (i in 1:5) {
      models[[i]] <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                   loss_function = "misclassification", 
                                   regularization = 0.1,
                                   verbose = FALSE)
    }
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train more models without issues
    model_new <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                               loss_function = "misclassification", 
                               regularization = 0.1,
                               verbose = FALSE)
    
    expect_valid_treefarms_model(model_new)
  })
})

test_that("cleanup of temporary files during training", {
  # Test that temporary files are cleaned up
  temp_dir <- tempdir()
  initial_files <- list.files(temp_dir, pattern = "^temp_.*\\.(csv|json)$")
  
  # Train a model (might create temporary files)
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
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

test_that("cleanup during error conditions", {
  # Test that cleanup works even when errors occur
  expect_error({
    # This should cause an error
    treefarms("invalid", simple_dataset$y, 
              loss_function = "misclassification", 
              regularization = 0.1,
              verbose = FALSE)
  })
  
  # Should still be able to train a model after the error
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("cleanup with parallel execution", {
  # Test cleanup after parallel execution
  expect_no_error({
    # Train models with different worker limits
    model1 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 1L,
                            verbose = FALSE)
    
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 2L,
                            verbose = FALSE)
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train another model
    model3 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            worker_limit = 1L,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model1)
    expect_valid_treefarms_model(model2)
    expect_valid_treefarms_model(model3)
  })
})

test_that("cleanup with auto-tuning", {
  # Test cleanup after auto-tuning
  expect_no_error({
    # Train model with auto-tuning
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = NULL,
                           verbose = FALSE)
    
    expect_valid_treefarms_model(model)
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train another model
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model2)
  })
})

test_that("cleanup with log-loss", {
  # Test cleanup after log-loss training
  expect_no_error({
    # Train model with log-loss
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
    
    expect_valid_treefarms_model(model, "log_loss")
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train another model
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model2)
  })
})

test_that("memory usage doesn't grow excessively", {
  # Test that memory usage doesn't grow excessively with repeated training
  initial_memory <- gc()
  
  # Train many models
  models <- list()
  for (i in 1:10) {
    expect_no_error({
      models[[i]] <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                   loss_function = "misclassification", 
                                   regularization = 0.1,
                                   verbose = FALSE)
    })
    
    expect_valid_treefarms_model(models[[i]])
  }
  
  # Force garbage collection
  gc()
  
  final_memory <- gc()
  
  # Memory usage shouldn't have grown excessively
  # This is a loose check - the main goal is that it doesn't crash
  memory_growth <- sum(final_memory[, "used"]) - sum(initial_memory[, "used"])
  expect_true(memory_growth < 100,  # Allow up to 100MB growth
              info = "Memory usage shouldn't grow excessively")
})

test_that("cleanup with different data sizes", {
  # Test cleanup with different data sizes
  datasets <- list(
    minimal_dataset,
    simple_dataset,
    many_features_dataset
  )
  
  for (dataset in datasets) {
    expect_no_error({
      # Train model
      model <- safe_treefarms(dataset$X, dataset$y, 
                             loss_function = "misclassification", 
                             regularization = 0.1,
                             verbose = FALSE)
      
      expect_valid_treefarms_model(model)
      
      # Force cleanup
      .Call("_treefarms_cleanup_static_state")
    })
  }
  
  # Should be able to train another model after all cleanups
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

test_that("cleanup with extreme parameters", {
  # Test cleanup with extreme parameters
  expect_no_error({
    # Train with very low regularization
    model1 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.001,
                            verbose = FALSE)
    
    # Train with very high regularization
    model2 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 10.0,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model1)
    expect_valid_treefarms_model(model2)
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train another model
    model3 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model3)
  })
})

test_that("cleanup with verbose output", {
  # Test cleanup after verbose training
  expect_no_error({
    # Train with verbose output
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = TRUE)
    
    expect_valid_treefarms_model(model)
    
    # Force cleanup
    .Call("_treefarms_cleanup_static_state")
    
    # Should be able to train another model
    model2 <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                            loss_function = "misclassification", 
                            regularization = 0.1,
                            verbose = FALSE)
    
    expect_valid_treefarms_model(model2)
  })
})

test_that("cleanup prevents memory leaks", {
  # Test that cleanup prevents memory leaks
  # This is a basic test - more sophisticated memory leak detection
  # would require specialized tools
  
  # Train many models without explicit cleanup
  models <- list()
  for (i in 1:20) {
    expect_no_error({
      models[[i]] <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                   loss_function = "misclassification", 
                                   regularization = 0.1,
                                   verbose = FALSE)
    })
    
    expect_valid_treefarms_model(models[[i]])
  }
  
  # Clear the models list
  models <- NULL
  
  # Force garbage collection
  gc()
  
  # Should still be able to train new models
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "misclassification", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model)
})

# Cleanup
teardown_test_environment()
