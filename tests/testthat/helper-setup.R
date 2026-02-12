# Helper functions and setup utilities for TreeFARMR testing

# Helper function to control RcppParallel threading for tests
set_test_threads <- function(n) {
  if (requireNamespace("RcppParallel", quietly = TRUE)) {
    RcppParallel::setThreadOptions(numThreads = n)
  }
}

# Helper function to reset threading to default
reset_test_threads <- function() {
  if (requireNamespace("RcppParallel", quietly = TRUE)) {
    RcppParallel::setThreadOptions(numThreads = "auto")
  }
}

# Helper function to validate treefarmr model structure
expect_valid_treefarms_model <- function(model, expected_loss_function = NULL) {
  # Normalize expected loss for comparison
  if (!is.null(expected_loss_function) && expected_loss_function == "regression") {
    expected_loss_function <- "squared_error"
  }

  # Check class
  expect_true(inherits(model, "treefarms_model"),
              info = "Model should inherit from treefarms_model")

  # Check required fields (key must exist; probabilities may be NULL for regression)
  required_fields <- c("model", "predictions", "probabilities", "accuracy",
                       "loss_function", "regularization", "n_trees",
                       "X_train", "training_time", "training_iterations")

  for (field in required_fields) {
    expect_true(field %in% names(model),
                info = paste("Missing field:", field))
  }

  # Determine if regression from model or expected argument
  is_regression <- identical(model$loss_function, "squared_error")

  # Check data types
  expect_true(is.numeric(model$n_trees), info = "n_trees should be numeric")
  expect_true(is.character(model$loss_function), info = "loss_function should be character")
  expect_true(is.numeric(model$regularization), info = "regularization should be numeric")
  expect_true(is.data.frame(model$X_train), info = "X_train should be data.frame")
  expect_true(is.numeric(model$y_train) || is.null(model[["y_train"]]),
              info = "y_train should be numeric or absent/NULL when training data not stored")
  expect_true(is.numeric(model$training_time), info = "training_time should be numeric")
  expect_true(is.numeric(model$training_iterations), info = "training_iterations should be numeric")

  # Accuracy: numeric, NA, or NULL (when lazy and training data not stored)
  acc <- model$accuracy
  expect_true(is.numeric(acc) || (length(acc) == 1L && is.na(acc)) || is.null(acc),
              info = "accuracy should be numeric, NA, or NULL")
  if (!is.null(acc) && !is.na(acc)) {
    if (!is_regression) {
      expect_true(acc >= 0 && acc <= 1, info = "accuracy should be between 0 and 1")
    } else {
      expect_true(acc >= 0, info = "accuracy (MSE) should be non-negative")
    }
  }

  expect_true(model$n_trees >= 0, info = "n_trees should be non-negative")
  expect_true(model$regularization >= 0, info = "regularization should be non-negative")
  expect_true(model$training_time >= 0, info = "training_time should be non-negative")
  expect_true(model$training_iterations >= 0, info = "training_iterations should be non-negative")

  # Check loss function if specified
  if (!is.null(expected_loss_function)) {
    expect_equal(model$loss_function, expected_loss_function,
                 info = paste("Expected loss function:", expected_loss_function))
  }

  # Predictions: numeric and length match when available (use n_train for sample count)
  n_train <- if (!is.null(model$n_train)) model$n_train else nrow(model$X_train)
  preds <- model$predictions
  if (!is.null(preds)) {
    expect_true(is.numeric(preds), info = "predictions should be numeric")
    expect_equal(length(preds), n_train,
                 info = "predictions length should match training data rows")
  }

  if (is_regression) {
    # Regression: no probability checks; predictions are fitted values (numeric vector)
  } else {
    # Classification: probabilities optional when NULL (lazy, no stored data)
    probs <- model$probabilities
    if (is.null(probs)) {
      # Skip probability and binary-prediction checks when probabilities not available
      if (!is.null(model$predictions) && length(model$predictions) == n_train) {
        expect_true(is.numeric(model$predictions), info = "predictions should be numeric")
      }
    } else {
      expect_true(is.matrix(probs), info = "probabilities should be matrix")
      expect_equal(nrow(probs), n_train,
                   info = "probabilities rows should match training data rows")
      expect_equal(ncol(probs), 2, info = "probabilities should have 2 columns")
      expect_true(all(probs >= 0), info = "probabilities should be >= 0")
      expect_true(all(probs <= 1), info = "probabilities should be <= 1")
      row_sums <- rowSums(probs)
      expect_true(all(abs(row_sums - 1) < 1e-10),
                  info = "probability rows should sum to 1")
      expect_true(all(model$predictions %in% c(0, 1)),
                  info = "predictions should be binary (0 or 1)")
    }
  }
}

# Helper function to validate predictions
expect_valid_predictions <- function(pred, n_samples, type = "class") {
  if (type == "class") {
    expect_true(is.numeric(pred), info = "Class predictions should be numeric")
    expect_equal(length(pred), n_samples, info = "Prediction length should match samples")
    expect_true(all(pred %in% c(0, 1)), info = "Class predictions should be binary")
  } else if (type == "prob") {
    expect_true(is.matrix(pred), info = "Probability predictions should be matrix")
    expect_equal(nrow(pred), n_samples, info = "Probability rows should match samples")
    expect_equal(ncol(pred), 2, info = "Probabilities should have 2 columns")
    expect_true(all(pred >= 0), info = "Probabilities should be >= 0")
    expect_true(all(pred <= 1), info = "Probabilities should be <= 1")
    row_sums <- rowSums(pred)
    expect_true(all(abs(row_sums - 1) < 1e-10),
                info = "Probability rows should sum to 1")
  } else if (type == "response") {
    expect_true(is.numeric(pred), info = "Response (fitted) predictions should be numeric")
    expect_equal(length(pred), n_samples, info = "Prediction length should match samples")
    expect_true(all(is.finite(pred)), info = "Response predictions should be finite")
  }
}

# Helper function to create a treefarms model with error handling
safe_treefarms <- function(X, y, ..., verbose = FALSE) {
  tryCatch({
    treefarms(X, y, ..., verbose = verbose)
  }, error = function(e) {
    stop("treefarms failed: ", e$message)
  })
}

# Helper function to compare two models for consistency
expect_models_consistent <- function(model1, model2, tolerance = 1e-10) {
  # Same loss function
  expect_equal(model1$loss_function, model2$loss_function,
               info = "Models should have same loss function")
  
  # Same number of trees (within tolerance for floating point)
  expect_equal(model1$n_trees, model2$n_trees,
               info = "Models should have same number of trees")
  
  # Same accuracy (within tolerance)
  expect_equal(model1$accuracy, model2$accuracy, tolerance = tolerance,
               info = "Models should have same accuracy")
  
  # Same regularization
  expect_equal(model1$regularization, model2$regularization, tolerance = tolerance,
               info = "Models should have same regularization")
  
  # Same predictions (within tolerance)
  expect_equal(model1$predictions, model2$predictions, tolerance = tolerance,
               info = "Models should have same predictions")
  
  # Same probabilities (within tolerance)
  expect_equal(model1$probabilities, model2$probabilities, tolerance = tolerance,
               info = "Models should have same probabilities")
}

# Setup and teardown for tests
setup_test_environment <- function() {
  # Set reproducible seed
  set.seed(42)
  
  # Reset threading to default
  reset_test_threads()
  
  # Clear any existing options
  options(treefarms.verbose = FALSE)
}

teardown_test_environment <- function() {
  # Reset threading
  reset_test_threads()
  
  # Clean up any temporary files
  temp_files <- list.files(tempdir(), pattern = "^temp_.*\\.(csv|json)$", full.names = TRUE)
  if (length(temp_files) > 0) {
    tryCatch({
      unlink(temp_files)
    }, error = function(e) {
      # Ignore cleanup errors
    })
  }
}
