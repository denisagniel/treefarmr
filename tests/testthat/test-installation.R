# Test suite for package installation and load
# Tests .onLoad() completion, dynamic library loading, session termination, cleanup

library(testthat)

# Note: These tests check package loading/unloading behavior
# They should be run in a fresh R session when possible

test_that(".onLoad() completes without hanging", {
  # Test that package loads without hanging
  # This is a basic test - if .onLoad() hangs, the test will timeout
  
  # Load package (should already be loaded, but test loading)
  expect_no_error({
    # library(treefarmr) # REMOVED: legacy package name
  })
  
  # Check that options are set
  expect_true(!is.null(getOption("treefarms.verbose")),
              info = "treefarms.verbose option should be set")
  expect_true(!is.null(getOption("treefarms.default_regularization")),
              info = "treefarms.default_regularization option should be set")
  expect_true(!is.null(getOption("treefarms.default_rashomon_bound_multiplier")),
              info = "treefarms.default_rashomon_bound_multiplier option should be set")
})

test_that("dynamic library loads correctly", {
  # Test that C++ symbols are resolved
  expect_no_error({
    # Try to call a C++ function
    result <- treefarms_configure_cpp('{"loss_function": "misclassification", "regularization": 0.1}')
  })
  
  # Should complete without error
  expect_true(is.null(result) || length(result) == 0,
              info = "treefarms_configure_cpp should complete")
})

test_that("package functions are available after load", {
  # Test that exported functions are available
  expect_true(exists("treefarms"),
              info = "treefarms function should be available")
  expect_true(exists("fit_tree"),
              info = "fit_tree function should be available")
  expect_true(exists("fit_rashomon"),
              info = "fit_rashomon function should be available")
  expect_true(exists("get_probabilities"),
              info = "get_probabilities function should be available")
  expect_true(exists("get_predictions"),
              info = "get_predictions function should be available")
  expect_true(exists("predict_treefarms"),
              info = "predict_treefarms function should be available")
})

test_that("C++ functions are callable", {
  # Test that C++ functions can be called
  expect_no_error({
    # Configure
    treefarms_configure_cpp('{"loss_function": "misclassification", "regularization": 0.1}')
    
    # Get statistics functions
    time <- treefarms_time_cpp()
    iterations <- treefarms_iterations_cpp()
    size <- treefarms_size_cpp()
    status <- treefarms_status_cpp()
  })
  
  # Check return types
  expect_true(is.numeric(time) || is.null(time),
              info = "treefarms_time_cpp should return numeric or NULL")
  expect_true(is.numeric(iterations) || is.null(iterations),
              info = "treefarms_iterations_cpp should return numeric or NULL")
  expect_true(is.numeric(size) || is.null(size),
              info = "treefarms_size_cpp should return numeric or NULL")
  expect_true(is.numeric(status) || is.null(status),
              info = "treefarms_status_cpp should return numeric or NULL")
})

test_that("package can be used immediately after load", {
  # Test that package can be used immediately after loading
  # library(treefarmr) # REMOVED: legacy package name
  
  # Create simple test data
  set.seed(42)
  X <- data.frame(
    feature_1 = sample(0:1, 10, replace = TRUE),
    feature_2 = sample(0:1, 10, replace = TRUE)
  )
  y <- sample(0:1, 10, replace = TRUE)
  
  # Should be able to fit model immediately
  expect_no_error({
    model <- treefarms(X, y, 
                     loss_function = "log_loss", 
                     regularization = 0.1,
                     verbose = FALSE)
  })
  
  expect_true(inherits(model, "treefarms_model"),
              info = "Model should be created successfully")
})

test_that("cleanup functions work", {
  # Test cleanup function (currently no-op, but should not error)
  expect_no_error({
    cleanup_static_state()
  })
})

test_that("package options are set correctly", {
  # Check that package options are set
  verbose_opt <- getOption("treefarms.verbose")
  reg_opt <- getOption("treefarms.default_regularization")
  rash_opt <- getOption("treefarms.default_rashomon_bound_multiplier")
  
  expect_true(!is.null(verbose_opt),
              info = "treefarms.verbose option should be set")
  expect_true(!is.null(reg_opt),
              info = "treefarms.default_regularization option should be set")
  expect_true(!is.null(rash_opt),
              info = "treefarms.default_rashomon_bound_multiplier option should be set")
  
  # Check default values
  expect_true(is.logical(verbose_opt),
              info = "treefarms.verbose should be logical")
  expect_true(is.numeric(reg_opt),
              info = "treefarms.default_regularization should be numeric")
  expect_true(is.numeric(rash_opt),
              info = "treefarms.default_rashomon_bound_multiplier should be numeric")
})

test_that("package version is accessible", {
  # Test that package version can be accessed
  expect_no_error({
    version <- packageVersion("treefarmr")
  })
  
  expect_true(!is.null(version),
              info = "Package version should be accessible")
})

test_that("package help is available", {
  # Test that help is available for main functions
  expect_no_error({
    help_treefarms <- tryCatch(help("treefarms"), error = function(e) NULL)
    help_fit_tree <- tryCatch(help("fit_tree"), error = function(e) NULL)
    help_get_probabilities <- tryCatch(help("get_probabilities"), error = function(e) NULL)
  })
  
  # Help may or may not be available depending on installation
  # Just check that accessing help doesn't crash
})

test_that("package can handle rapid function calls", {
  # Test that package can handle rapid function calls after load
  # library(treefarmr) # REMOVED: legacy package name
  
  # Create test data
  set.seed(42)
  X <- data.frame(
    feature_1 = sample(0:1, 10, replace = TRUE),
    feature_2 = sample(0:1, 10, replace = TRUE)
  )
  y <- sample(0:1, 10, replace = TRUE)
  
  # Make rapid function calls
  for (i in 1:5) {
    expect_no_error({
      treefarms_configure_cpp('{"loss_function": "misclassification", "regularization": 0.1}')
    })
  }
  
  # Should still be able to fit model
  expect_no_error({
    model <- treefarms(X, y, 
                     loss_function = "log_loss", 
                     regularization = 0.1,
                     verbose = FALSE)
  })
  
  expect_true(inherits(model, "treefarms_model"),
              info = "Model should be created after rapid function calls")
})

# Note: Testing actual package unload is difficult in R
# The .onUnload() function will be called when package is unloaded,
# but we can't easily test this in a testthat context



