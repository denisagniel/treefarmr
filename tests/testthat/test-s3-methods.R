# Test suite for S3 methods (print, summary, predict)

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Create test data
set.seed(42)
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

# Create test models
test_model <- treefarms(pattern_data$X, pattern_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.01,
                       verbose = FALSE)

test_model_logloss <- treefarms(pattern_data$X, pattern_data$y, 
                               loss_function = "log_loss", 
                               regularization = 0.01,
                               verbose = FALSE)

test_that("print.treefarms_model works", {
  # Test that print doesn't cause errors
  expect_no_error({
    print(test_model)
  })
  
  # Test with log_loss model
  expect_no_error({
    print(test_model_logloss)
  })
  
  # Test with auto-tuned model
  auto_model <- treefarms(pattern_data$X, pattern_data$y, 
                         loss_function = "misclassification", 
                         regularization = NULL,
                         verbose = FALSE)
  
  expect_no_error({
    print(auto_model)
  })
})

test_that("summary.treefarms_model works", {
  # Test that summary doesn't cause errors
  expect_no_error({
    summary_result <- summary(test_model)
  })
  
  # Check summary structure
  expect_true(is.list(summary_result))
  expect_true("model_type" %in% names(summary_result))
  expect_true("n_trees" %in% names(summary_result))
  expect_true("accuracy" %in% names(summary_result))
  expect_true("loss_function" %in% names(summary_result))
  expect_true("regularization" %in% names(summary_result))
  expect_true("training_time" %in% names(summary_result))
  expect_true("training_iterations" %in% names(summary_result))
  
  # Check data types
  expect_true(is.character(summary_result$model_type))
  expect_true(is.numeric(summary_result$n_trees))
  expect_true(is.numeric(summary_result$accuracy))
  expect_true(is.character(summary_result$loss_function))
  expect_true(is.numeric(summary_result@regularization))
  expect_true(is.numeric(summary_result$training_time))
  expect_true(is.numeric(summary_result$training_iterations))
  
  # Test with log_loss model
  expect_no_error({
    summary_result_logloss <- summary(test_model_logloss)
  })
  
  expect_equal(summary_result_logloss$loss_function, "log_loss")
})

test_that("predict.treefarms_model works", {
  # Create new data for prediction
  X_new <- data.frame(
    x1 = c(0, 1, 0, 1),
    x2 = c(0, 0, 1, 1)
  )
  
  # Test class predictions
  expect_no_error({
    pred_class <- predict(test_model, X_new, type = "class")
  })
  
  expect_true(is.numeric(pred_class))
  expect_equal(length(pred_class), nrow(X_new))
  expect_true(all(pred_class %in% c(0, 1)))
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict(test_model, X_new, type = "prob")
  })
  
  expect_true(is.matrix(pred_prob))
  expect_equal(nrow(pred_prob), nrow(X_new))
  expect_equal(ncol(pred_prob), 2)
  expect_true(all(pred_prob >= 0))
  expect_true(all(pred_prob <= 1))
  
  # Test default type (should be class)
  expect_no_error({
    pred_default <- predict(test_model, X_new)
  })
  
  expect_equal(pred_default, pred_class)
})

test_that("predict.treefarms_model input validation", {
  # Test invalid type
  expect_error(predict(test_model, pattern_data$X, type = "invalid"), 
               "type must be 'class' or 'prob'")
  
  # Test invalid newdata
  expect_error(predict(test_model, "invalid"), 
               "newdata must be a data.frame or matrix")
  
  # Test missing features
  X_missing <- pattern_data$X[, 1, drop = FALSE]  # Missing x2
  expect_error(predict(test_model, X_missing), 
               "newdata must have the same features as training data")
})

test_that("print.cf_rashomon works", {
  # Create cross-fitted Rashomon model
  cf_model <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  
  # Test that print doesn't cause errors
  expect_no_error({
    print(cf_model)
  })
})

test_that("summary.cf_rashomon works", {
  # Create cross-fitted Rashomon model
  cf_model <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  
  # Test that summary doesn't cause errors
  expect_no_error({
    summary_result <- summary(cf_model)
  })
  
  # Check summary structure
  expect_true(is.list(summary_result))
  expect_true("model_type" %in% names(summary_result))
  expect_true("K" %in% names(summary_result))
  expect_true("n_intersecting" %in% names(summary_result))
  expect_true("loss_function" %in% names(summary_result))
  expect_true("regularization" %in% names(summary_result))
  expect_true("rashomon_sizes" %in% names(summary_result))
  
  # Check data types
  expect_true(is.character(summary_result$model_type))
  expect_true(is.numeric(summary_result@K))
  expect_true(is.numeric(summary_result$n_intersecting))
  expect_true(is.character(summary_result$loss_function))
  expect_true(is.numeric(summary_result@regularization))
  expect_true(is.numeric(summary_result$rashomon_sizes))
})

test_that("predict.cf_rashomon works", {
  # Create cross-fitted Rashomon model
  cf_model <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  
  # Create new data for prediction
  X_new <- data.frame(
    x1 = c(0, 1, 0, 1),
    x2 = c(0, 0, 1, 1)
  )
  
  # Test class predictions
  expect_no_error({
    pred_class <- predict(cf_model, X_new, type = "class")
  })
  
  expect_true(is.numeric(pred_class))
  expect_equal(length(pred_class), nrow(X_new))
  expect_true(all(pred_class %in% c(0, 1)))
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict(cf_model, X_new, type = "prob")
  })
  
  expect_true(is.matrix(pred_prob))
  expect_equal(nrow(pred_prob), nrow(X_new))
  expect_equal(ncol(pred_prob), 2)
  expect_true(all(pred_prob >= 0))
  expect_true(all(pred_prob <= 1))
  
  # Test default type (should be class)
  expect_no_error({
    pred_default <- predict(cf_model, X_new)
  })
  
  expect_equal(pred_default, pred_class)
})

test_that("S3 methods handle edge cases", {
  # Test with model that has no trees
  no_tree_model <- treefarms(pattern_data$X, pattern_data$y, 
                            loss_function = "misclassification", 
                            regularization = 10,  # High regularization
                            verbose = FALSE)
  
  expect_no_error({
    print(no_tree_model)
    summary_result <- summary(no_tree_model)
  })
  
  expect_equal(summary_result$n_trees, 0)
  
  # Test prediction with no trees
  X_new <- data.frame(x1 = c(0, 1), x2 = c(0, 1))
  expect_no_error({
    pred_class <- predict(no_tree_model, X_new, type = "class")
    pred_prob <- predict(no_tree_model, X_new, type = "prob")
  })
  
  expect_equal(length(pred_class), 2)
  expect_equal(nrow(pred_prob), 2)
})

test_that("S3 methods with different data types", {
  # Test with matrix input
  X_matrix <- as.matrix(pattern_data$X[1:5, ])
  
  expect_no_error({
    pred_class <- predict(test_model, X_matrix, type = "class")
    pred_prob <- predict(test_model, X_matrix, type = "prob")
  })
  
  expect_equal(length(pred_class), nrow(X_matrix))
  expect_equal(nrow(pred_prob), nrow(X_matrix))
})

test_that("S3 methods with single row prediction", {
  # Test with single row
  X_single <- pattern_data$X[1, , drop = FALSE]
  
  expect_no_error({
    pred_class <- predict(test_model, X_single, type = "class")
    pred_prob <- predict(test_model, X_single, type = "prob")
  })
  
  expect_equal(length(pred_class), 1)
  expect_equal(nrow(pred_prob), 1)
  expect_equal(ncol(pred_prob), 2)
})

test_that("S3 methods with empty prediction data", {
  # Test with empty data frame
  X_empty <- pattern_data$X[integer(0), ]
  
  expect_no_error({
    pred_class <- predict(test_model, X_empty, type = "class")
    pred_prob <- predict(test_model, X_empty, type = "prob")
  })
  
  expect_equal(length(pred_class), 0)
  expect_equal(nrow(pred_prob), 0)
  expect_equal(ncol(pred_prob), 2)
})

test_that("S3 methods preserve class attributes", {
  # Test that methods preserve class attributes
  expect_true(inherits(test_model, "treefarms_model"))
  expect_true(inherits(test_model_logloss, "treefarms_model"))
  
  # Test cross-fitted Rashomon class
  cf_model <- cross_fitted_rashomon(pattern_data$X, pattern_data$y, 
                                   K = 3,
                                   loss_function = "misclassification", 
                                   regularization = 0.01,
                                   verbose = FALSE)
  
  expect_true(inherits(cf_model, "cf_rashomon"))
})

test_that("S3 methods handle verbose output", {
  # Test that methods work with verbose models
  verbose_model <- treefarms(pattern_data$X, pattern_data$y, 
                            loss_function = "misclassification", 
                            regularization = 0.01,
                            verbose = TRUE)
  
  expect_no_error({
    print(verbose_model)
    summary_result <- summary(verbose_model)
    pred_class <- predict(verbose_model, pattern_data$X[1:5, ], type = "class")
  })
})

test_that("S3 methods with auto-tuned models", {
  # Test with auto-tuned model
  auto_model <- treefarms(pattern_data$X, pattern_data$y, 
                         loss_function = "misclassification", 
                         regularization = NULL,
                         verbose = FALSE)
  
  expect_no_error({
    print(auto_model)
    summary_result <- summary(auto_model)
    pred_class <- predict(auto_model, pattern_data$X[1:5, ], type = "class")
  })
  
  # Summary should show auto-tuned regularization
  expect_true(is.numeric(summary_result@regularization))
  expect_true(summary_result@regularization > 0)
})


