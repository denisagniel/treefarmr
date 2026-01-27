# Test suite for prediction functionality

library(testthat)
library(treefarmr)

# Create test data
set.seed(42)
train_data <- list(
  X = data.frame(
    feature_1 = sample(0:1, 100, replace = TRUE),
    feature_2 = sample(0:1, 100, replace = TRUE),
    feature_3 = sample(0:1, 100, replace = TRUE)
  ),
  y = sample(0:1, 100, replace = TRUE)
)

# Create test data with clear pattern
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

test_that("predict_treefarms basic functionality works", {
  # Train a model
  model <- treefarms(pattern_data$X, pattern_data$y, 
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
    pred_class <- predict_treefarms(model, X_new, type = "class")
  })
  
  expect_true(is.numeric(pred_class))
  expect_equal(length(pred_class), nrow(X_new))
  expect_true(all(pred_class %in% c(0, 1)))
  
  # Test probability predictions
  expect_no_error({
    pred_prob <- predict_treefarms(model, X_new, type = "prob")
  })
  
  expect_true(is.matrix(pred_prob))
  expect_equal(nrow(pred_prob), nrow(X_new))
  expect_equal(ncol(pred_prob), 2)
  expect_true(all(pred_prob >= 0))
  expect_true(all(pred_prob <= 1))
  
  # Probabilities should sum to 1 for each row
  row_sums <- rowSums(pred_prob)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("predict_treefarms input validation", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test invalid object
  expect_error(predict_treefarms("invalid", train_data$X), 
               "object must be a treefarms_model")
  
  # Test invalid newdata
  expect_error(predict_treefarms(model, "invalid"), 
               "newdata must be a data.frame or matrix")
  
  # Test invalid type
  expect_error(predict_treefarms(model, train_data$X, type = "invalid"), 
               "type must be 'class' or 'prob'")
  
  # Test missing features
  X_missing <- train_data$X[, 1:2]  # Missing feature_3
  expect_error(predict_treefarms(model, X_missing), 
               "newdata must have the same features as training data")
  
  # Test extra features
  X_extra <- cbind(train_data$X, extra_feature = sample(0:1, nrow(train_data$X), replace = TRUE))
  expect_error(predict_treefarms(model, X_extra), 
               "newdata must have the same features as training data")
})

test_that("predict_treefarms with different data types", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with matrix input
  X_matrix <- as.matrix(train_data$X[1:5, ])
  expect_no_error({
    pred_class <- predict_treefarms(model, X_matrix, type = "class")
    pred_prob <- predict_treefarms(model, X_matrix, type = "prob")
  })
  
  expect_equal(length(pred_class), nrow(X_matrix))
  expect_equal(nrow(pred_prob), nrow(X_matrix))
})

test_that("predict_treefarms with different loss functions", {
  # Test with misclassification loss
  model_misclass <- treefarms(pattern_data$X, pattern_data$y, 
                             loss_function = "misclassification", 
                             regularization = 0.01,
                             verbose = FALSE)
  
  # Test with log_loss
  model_logloss <- treefarms(pattern_data$X, pattern_data$y, 
                            loss_function = "log_loss", 
                            regularization = 0.01,
                            verbose = FALSE)
  
  X_new <- data.frame(x1 = c(0, 1), x2 = c(0, 1))
  
  # Both should work
  expect_no_error({
    pred1 <- predict_treefarms(model_misclass, X_new, type = "class")
    pred2 <- predict_treefarms(model_logloss, X_new, type = "class")
  })
  
  expect_equal(length(pred1), 2)
  expect_equal(length(pred2), 2)
})

test_that("predict_treefarms consistency", {
  model <- treefarms(pattern_data$X, pattern_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.01,
                    verbose = FALSE)
  
  X_new <- data.frame(x1 = c(0, 1), x2 = c(0, 1))
  
  # Multiple predictions should be consistent
  pred1 <- predict_treefarms(model, X_new, type = "class")
  pred2 <- predict_treefarms(model, X_new, type = "class")
  
  expect_equal(pred1, pred2)
  
  # Probability predictions should also be consistent
  prob1 <- predict_treefarms(model, X_new, type = "prob")
  prob2 <- predict_treefarms(model, X_new, type = "prob")
  
  expect_equal(prob1, prob2)
})

test_that("predict_treefarms with single row", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with single row
  X_single <- train_data$X[1, , drop = FALSE]
  
  expect_no_error({
    pred_class <- predict_treefarms(model, X_single, type = "class")
    pred_prob <- predict_treefarms(model, X_single, type = "prob")
  })
  
  expect_equal(length(pred_class), 1)
  expect_equal(nrow(pred_prob), 1)
  expect_equal(ncol(pred_prob), 2)
})

test_that("predict_treefarms with empty data", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with empty data frame
  X_empty <- train_data$X[integer(0), ]
  
  expect_no_error({
    pred_class <- predict_treefarms(model, X_empty, type = "class")
    pred_prob <- predict_treefarms(model, X_empty, type = "prob")
  })
  
  expect_equal(length(pred_class), 0)
  expect_equal(nrow(pred_prob), 0)
  expect_equal(ncol(pred_prob), 2)
})

test_that("predict_treefarms probability characteristics", {
  model <- treefarms(pattern_data$X, pattern_data$y, 
                    loss_function = "log_loss", 
                    regularization = 0.01,
                    verbose = FALSE)
  
  X_new <- data.frame(x1 = c(0, 1, 0, 1), x2 = c(0, 0, 1, 1))
  
  pred_prob <- predict_treefarms(model, X_new, type = "prob")
  
  # Probabilities should be bounded away from 0 and 1
  expect_true(all(pred_prob > 0))
  expect_true(all(pred_prob < 1))
  
  # Probabilities should sum to 1
  row_sums <- rowSums(pred_prob)
  expect_true(all(abs(row_sums - 1) < 1e-10))
  
  # Probabilities should be reasonable (not too extreme)
  expect_true(all(pred_prob > 0.1))
  expect_true(all(pred_prob < 0.9))
})

test_that("predict_treefarms with auto-tuned model", {
  # Train model with auto-tuning
  model <- treefarms(pattern_data$X, pattern_data$y, 
                    loss_function = "misclassification", 
                    regularization = NULL,
                    verbose = FALSE)
  
  X_new <- data.frame(x1 = c(0, 1), x2 = c(0, 1))
  
  expect_no_error({
    pred_class <- predict_treefarms(model, X_new, type = "class")
    pred_prob <- predict_treefarms(model, X_new, type = "prob")
  })
  
  expect_equal(length(pred_class), 2)
  expect_equal(nrow(pred_prob), 2)
})

test_that("predict_treefarms feature name matching", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with reordered columns
  X_reordered <- train_data$X[, c("feature_3", "feature_1", "feature_2")]
  
  expect_no_error({
    pred_class <- predict_treefarms(model, X_reordered, type = "class")
  })
  
  expect_equal(length(pred_class), nrow(X_reordered))
  
  # Test with different column names (should fail)
  X_wrong_names <- train_data$X
  names(X_wrong_names) <- c("wrong1", "wrong2", "wrong3")
  
  expect_error(predict_treefarms(model, X_wrong_names), 
               "newdata must have the same features as training data")
})

test_that("predict_treefarms handles missing values gracefully", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with NA values
  X_na <- train_data$X[1:5, ]
  X_na[1, 1] <- NA
  
  expect_error(predict_treefarms(model, X_na), 
               "newdata contains missing values")
})

test_that("predict_treefarms with non-binary features", {
  model <- treefarms(train_data$X, train_data$y, 
                    loss_function = "misclassification", 
                    regularization = 0.1,
                    verbose = FALSE)
  
  # Test with non-binary values
  X_nonbinary <- train_data$X[1:5, ]
  X_nonbinary[1, 1] <- 2
  
  expect_error(predict_treefarms(model, X_nonbinary), 
               "newdata must contain only binary values")
})


