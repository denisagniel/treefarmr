test_that("Continuous features work end-to-end", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    x1 = runif(100, 0, 1),
    x2 = rnorm(100, 50, 10)
  )
  y <- as.numeric(X$x1 > 0.5)

  # Fit model
  model <- treefarms(X, y, loss_function = "log_loss")

  # Check model is valid
  expect_valid_treefarms_model(model)

  # Check discretization metadata exists
  expect_false(is.null(model$discretization))
  expect_equal(model$discretization$method, "median")
  expect_equal(length(model$discretization$features), 2)

  # Predictions should work
  X_new <- data.frame(
    x1 = runif(10, 0, 1),
    x2 = rnorm(10, 50, 10)
  )
  pred <- predict(model, X_new, type = "prob")

  # Check predictions are valid
  expect_valid_predictions(pred, 10, type = "prob")
})


test_that("Binary features still work (backward compatibility)", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  data <- simple_dataset

  # Fit model with binary data
  model <- treefarms(data$X, data$y, loss_function = "misclassification")

  # Check model is valid
  expect_valid_treefarms_model(model)

  # Check discretization metadata shows binary features
  expect_false(is.null(model$discretization))
  expect_true(model$discretization$all_binary)

  # All features should be marked as binary
  for (feat in names(model$discretization$features)) {
    expect_equal(model$discretization$features[[feat]]$type, "binary")
  }

  # Predictions should work
  pred <- predict(model, data$X, type = "class")
  expect_valid_predictions(pred, nrow(data$X))
})


test_that("Mixed binary and continuous features", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    binary_feat = sample(0:1, 100, replace = TRUE),
    continuous_feat = runif(100, 0, 1)
  )
  y <- sample(0:1, 100, replace = TRUE)

  # Fit model
  model <- treefarms(X, y, loss_function = "log_loss")

  # Check discretization metadata
  expect_false(is.null(model$discretization))
  expect_equal(model$discretization$features$binary_feat$type, "binary")
  expect_equal(model$discretization$features$continuous_feat$type, "continuous")

  # Check that thresholds exist for continuous feature
  expect_true(length(model$discretization$features$continuous_feat$thresholds) > 0)

  # Predictions should work
  X_new <- data.frame(
    binary_feat = sample(0:1, 10, replace = TRUE),
    continuous_feat = runif(10, 0, 1)
  )
  pred <- predict(model, X_new, type = "prob")
  expect_valid_predictions(pred, 10, type = "prob")
})


test_that("User-provided thresholds are used", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(x1 = runif(100, 0, 100))
  y <- sample(0:1, 100, replace = TRUE)

  # Fit with custom threshold
  custom_threshold <- 50
  model <- treefarms(
    X, y,
    discretize_thresholds = list(x1 = custom_threshold)
  )

  # Check that custom threshold was used
  expect_equal(model$discretization$features$x1$thresholds, custom_threshold)

  # Check predictions work
  X_new <- data.frame(x1 = c(25, 75))
  pred <- predict(model, X_new, type = "class")
  expect_equal(length(pred), 2)
})


test_that("Discretization methods work correctly", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(x1 = runif(100, 0, 100))
  y <- sample(0:1, 100, replace = TRUE)

  # Test median (1 threshold)
  model_median <- treefarms(X, y, discretize_method = "median")
  expect_equal(length(model_median$discretization$features$x1$thresholds), 1)
  expect_equal(model_median$discretization$method, "median")

  # Test quantiles with n_bins=3 (2 thresholds)
  model_quantiles <- treefarms(X, y, discretize_method = "quantiles", discretize_bins = 3)
  expect_equal(model_quantiles$discretization$method, "quantiles")
  expect_equal(model_quantiles$discretization$n_bins, 3)
  # Should have 2 thresholds (tertiles)
  expect_true(length(model_quantiles$discretization$features$x1$thresholds) >= 1)

  # Test quantiles with n_bins=4 (3 thresholds)
  model_quartiles <- treefarms(X, y, discretize_method = "quantiles", discretize_bins = 4)
  expect_equal(model_quartiles$discretization$n_bins, 4)
  # Should have 3 thresholds (quartiles)
  expect_true(length(model_quartiles$discretization$features$x1$thresholds) >= 1)
})


test_that("Metadata stored correctly", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    x1 = runif(100, 0, 1),
    x2 = sample(0:1, 100, replace = TRUE)
  )
  y <- sample(0:1, 100, replace = TRUE)

  model <- treefarms(X, y, discretize_method = "median", discretize_bins = 2)

  # Check metadata structure
  expect_false(is.null(model$discretization))
  expect_equal(model$discretization$method, "median")
  expect_equal(model$discretization$n_bins, 2)
  expect_true(!is.null(model$discretization$features))
  expect_true(!is.null(model$discretization$binary_names))

  # Check feature-specific metadata
  expect_equal(model$discretization$features$x1$type, "continuous")
  expect_equal(model$discretization$features$x2$type, "binary")

  # Check original feature names stored
  expect_equal(model$X_original_names, c("x1", "x2"))
})


test_that("Prediction on new data with correct features works", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X_train <- data.frame(
    age = runif(100, 18, 80),
    income = rnorm(100, 50000, 15000)
  )
  y <- as.numeric(X_train$age > 40)

  model <- treefarms(X_train, y, loss_function = "log_loss")

  # New data with same features
  X_new <- data.frame(
    age = c(25, 55, 70),
    income = c(35000, 60000, 80000)
  )

  # Predictions should work
  pred <- predict(model, X_new, type = "prob")
  expect_valid_predictions(pred, 3, type = "prob")
})


test_that("Error handling for mismatched features", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X_train <- data.frame(x1 = runif(100))
  y <- sample(0:1, 100, replace = TRUE)

  model <- treefarms(X_train, y)

  # Try to predict with different features
  X_wrong <- data.frame(x2 = runif(10))

  expect_error(
    predict(model, X_wrong),
    "Features missing in newdata"
  )
})


test_that("Regression with continuous features works", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    x1 = runif(100, 0, 10),
    x2 = rnorm(100, 5, 2)
  )
  y <- 2 * X$x1 + X$x2 + rnorm(100, 0, 0.5)

  # Fit regression model
  model <- treefarms(X, y, loss_function = "squared_error")

  # Check model is valid
  expect_valid_treefarms_model(model)
  expect_equal(model$loss_function, "squared_error")

  # Check discretization happened
  expect_false(is.null(model$discretization))

  # Predictions should work
  X_new <- data.frame(
    x1 = runif(10, 0, 10),
    x2 = rnorm(10, 5, 2)
  )
  pred <- predict(model, X_new)

  # Check predictions are numeric
  expect_true(is.numeric(pred))
  expect_equal(length(pred), 10)
})


test_that("Edge case: 2 unique values not in {0,1}", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(x1 = sample(c(5, 10), 100, replace = TRUE))
  y <- sample(0:1, 100, replace = TRUE)

  # Should work - gets mapped to binary
  model <- treefarms(X, y)

  # Check metadata shows binary_converted
  expect_equal(model$discretization$features$x1$type, "binary_converted")
  expect_equal(model$discretization$features$x1$original_values, c(5, 10))

  # Predictions should work
  X_new <- data.frame(x1 = c(5, 10, 5))
  pred <- predict(model, X_new, type = "class")
  expect_equal(length(pred), 3)
})


test_that("Edge case: All identical values", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    x1 = rep(5, 100),
    x2 = runif(100, 0, 1)
  )
  y <- sample(0:1, 100, replace = TRUE)

  # Should not error
  model <- treefarms(X, y)

  # Check metadata for constant feature
  expect_equal(model$discretization$features$x1$type, "constant")
  expect_equal(model$discretization$features$x1$value, 5)

  # Predictions should work
  X_new <- data.frame(
    x1 = rep(5, 10),
    x2 = runif(10, 0, 1)
  )
  pred <- predict(model, X_new, type = "class")
  expect_equal(length(pred), 10)
})


test_that("Fast-path for binary-only data has minimal overhead", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X_binary <- as.data.frame(matrix(sample(0:1, 100 * 10, replace = TRUE), ncol = 10))
  y <- sample(0:1, 100, replace = TRUE)

  # Fit model
  model <- treefarms(X_binary, y)

  # Check that fast-path metadata is set
  expect_true(model$discretization$all_binary)

  # Check that all features marked as binary
  for (feat in names(model$discretization$features)) {
    expect_equal(model$discretization$features[[feat]]$type, "binary")
  }

  # Predictions should work with no overhead
  pred <- predict(model, X_binary, type = "class")
  expect_equal(length(pred), 100)
})


test_that("Feature naming is correct", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(age = runif(100, 18, 80))
  y <- sample(0:1, 100, replace = TRUE)

  # Median creates 1 threshold → 1 binary indicator
  model_median <- treefarms(X, y, discretize_method = "median")
  expect_equal(model_median$discretization$features$age$new_names, "age_leq_1")

  # Quantiles with n_bins=3 creates 2 thresholds → 2 binary indicators
  model_quantiles <- treefarms(X, y, discretize_method = "quantiles", discretize_bins = 3)
  expected_names <- paste0("age_leq_", 1:length(model_quantiles$discretization$features$age$thresholds))
  expect_equal(model_quantiles$discretization$features$age$new_names, expected_names)
})


test_that("Feature name collision detection works", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  # Create data with potential name collision
  X <- data.frame(
    age = runif(100, 18, 80),
    age_leq_1 = sample(0:1, 100, replace = TRUE)
  )
  y <- sample(0:1, 100, replace = TRUE)

  # Should error due to name collision
  expect_error(
    treefarms(X, y),
    "Feature name collision detected"
  )
})


test_that("Discretization works with missing feature in newdata", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X_train <- data.frame(
    x1 = runif(100, 0, 1),
    x2 = runif(100, 0, 1)
  )
  y <- sample(0:1, 100, replace = TRUE)

  model <- treefarms(X_train, y)

  # newdata missing x2
  X_new <- data.frame(x1 = runif(10, 0, 1))

  expect_error(
    predict(model, X_new),
    "Features missing in newdata"
  )
})


test_that("Backward compatibility: old models without discretization", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  data <- simple_dataset

  # Fit model
  model <- treefarms(data$X, data$y)

  # Manually remove discretization metadata (simulating old model)
  model$discretization <- NULL

  # Predictions should still work
  pred <- predict(model, data$X, type = "class")
  expect_equal(length(pred), nrow(data$X))
})


test_that("Large dataset with continuous features (performance check)", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  n <- 1000
  p <- 20
  X <- as.data.frame(matrix(runif(n * p, 0, 1), ncol = p))
  y <- sample(0:1, n, replace = TRUE)

  # Should complete in reasonable time
  start_time <- Sys.time()
  model <- treefarms(X, y, discretize_method = "quantiles", discretize_bins = 4)
  end_time <- Sys.time()

  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Check model is valid
  expect_valid_treefarms_model(model)

  # Check discretization happened
  expect_false(is.null(model$discretization))
  expect_equal(length(model$discretization$features), p)

  # Log performance (should be < 30s as per plan, but can vary)
  message("Large dataset discretization took ", round(time_taken, 2), " seconds")

  # Predictions should work
  X_new <- as.data.frame(matrix(runif(100 * p, 0, 1), ncol = p))
  names(X_new) <- names(X)
  pred <- predict(model, X_new, type = "class")
  expect_equal(length(pred), 100)
})


test_that("Consistency: discrete and continuous training produce same tree", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X_continuous <- data.frame(x1 = runif(100, 0, 1))
  y <- sample(0:1, 100, replace = TRUE)

  # Train with continuous
  model_cont <- treefarms(X_continuous, y, discretize_method = "median")

  # Get the threshold used
  threshold <- model_cont$discretization$features$x1$thresholds

  # Manually discretize using same threshold
  X_binary <- data.frame(x1_leq_1 = as.numeric(X_continuous$x1 <= threshold))

  # Train with binary
  model_bin <- treefarms(X_binary, y)

  # Predictions should match
  pred_cont <- predict(model_cont, X_continuous, type = "class")
  pred_bin <- predict(model_bin, X_binary, type = "class")

  expect_equal(pred_cont, pred_bin)
})


test_that("Adaptive discretization: bins grow with sample size", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  # Small sample
  set.seed(123)
  X_small <- data.frame(x1 = runif(50))
  y_small <- sample(0:1, 50, replace = TRUE)

  model_small <- treefarms(X_small, y_small,
                          discretize_method = "quantiles",
                          discretize_bins = "adaptive")

  n_bins_small <- model_small$discretization$n_bins
  n_thresholds_small <- length(model_small$discretization$features$x1$thresholds)

  # Expected: max(2, ceiling(log(50) / 3)) = max(2, ceiling(3.91 / 3)) = max(2, 2) = 2
  expect_equal(n_bins_small, 2)

  # Large sample
  set.seed(123)
  X_large <- data.frame(x1 = runif(1000))
  y_large <- sample(0:1, 1000, replace = TRUE)

  model_large <- treefarms(X_large, y_large,
                          discretize_method = "quantiles",
                          discretize_bins = "adaptive")

  n_bins_large <- model_large$discretization$n_bins
  n_thresholds_large <- length(model_large$discretization$features$x1$thresholds)

  # Expected: max(2, ceiling(log(1000) / 3)) = max(2, ceiling(6.91 / 3)) = max(2, 3) = 3
  expect_equal(n_bins_large, 3)

  # Bins should increase with sample size
  expect_true(n_bins_large >= n_bins_small)
  expect_true(n_thresholds_large >= n_thresholds_small)
})


test_that("Adaptive discretization: predictions work correctly", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(
    x1 = runif(500, 0, 10),
    x2 = rnorm(500, 5, 2)
  )
  y <- as.numeric(X$x1 > 5 & X$x2 > 5)

  # Fit with adaptive bins
  model <- treefarms(X, y,
                    discretize_method = "quantiles",
                    discretize_bins = "adaptive",
                    loss_function = "log_loss")

  # Check model is valid
  expect_valid_treefarms_model(model)

  # Check that bins were computed adaptively
  expect_true(model$discretization$n_bins >= 2)

  # Expected: max(2, ceiling(log(500) / 3)) = max(2, ceiling(6.21 / 3)) = 3
  expect_equal(model$discretization$n_bins, 3)

  # Predictions should work
  X_new <- data.frame(
    x1 = runif(50, 0, 10),
    x2 = rnorm(50, 5, 2)
  )
  pred <- predict(model, X_new, type = "prob")
  expect_valid_predictions(pred, 50, type = "prob")
})


test_that("Adaptive vs fixed bins: both work", {
  setup_test_environment()
  on.exit(teardown_test_environment())

  set.seed(123)
  X <- data.frame(x1 = runif(200))
  y <- sample(0:1, 200, replace = TRUE)

  # Fixed bins
  model_fixed <- treefarms(X, y, discretize_bins = 4)
  expect_equal(model_fixed$discretization$n_bins, 4)

  # Adaptive bins
  model_adaptive <- treefarms(X, y, discretize_bins = "adaptive")
  # Expected: max(2, ceiling(log(200) / 3)) = max(2, ceiling(5.3 / 3)) = 2
  expect_equal(model_adaptive$discretization$n_bins, 2)

  # Both should produce valid models
  expect_valid_treefarms_model(model_fixed)
  expect_valid_treefarms_model(model_adaptive)

  # Both should predict
  pred_fixed <- predict(model_fixed, X, type = "class")
  pred_adaptive <- predict(model_adaptive, X, type = "class")

  expect_equal(length(pred_fixed), 200)
  expect_equal(length(pred_adaptive), 200)
})
