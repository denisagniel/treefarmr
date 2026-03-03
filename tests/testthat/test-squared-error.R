# Test suite for squared-error (regression) loss
# Requirement 5: continuous outcome, fitted values, predict returns vector

library(testthat)
library(treefarmr)

setup_test_environment()

# Small regression dataset: binary X, continuous y
regression_dataset <- function(n = 40, p = 3, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  y <- runif(n, 0, 10)
  list(X = X, y = y)
}

test_that("squared_error fit and model structure", {
  dat <- regression_dataset(n = 40, p = 3)
  expect_no_error({
    model <- safe_treefarms(dat$X, dat$y,
                           loss_function = "squared_error",
                           regularization = 0.1,
                           verbose = FALSE)
  })
  expect_valid_treefarms_model(model, "squared_error")
  expect_null(model$probabilities)
  expect_true(is.numeric(model$predictions))
  expect_equal(length(model$predictions), nrow(dat$X))
})

test_that("squared_error predictions and predict() match", {
  dat <- regression_dataset(n = 30, p = 2)
  model <- safe_treefarms(dat$X, dat$y,
                         loss_function = "squared_error",
                         regularization = 0.15,
                         store_training_data = TRUE,
                         verbose = FALSE)
  pred_train <- predict(model, dat$X)
  expect_true(is.numeric(pred_train), info = "predict() should return numeric vector")
  expect_equal(length(pred_train), nrow(dat$X),
               info = "predict length should match training rows")
  expect_equal(unname(pred_train), unname(model$predictions),
               info = "predict(X_train) should match model$predictions")
  expect_valid_predictions(pred_train, nrow(dat$X), type = "response")
})

test_that("squared_error predict on newdata", {
  dat <- regression_dataset(n = 25, p = 3)
  model <- safe_treefarms(dat$X, dat$y,
                         loss_function = "squared_error",
                         regularization = 0.1,
                         verbose = FALSE)
  newdata <- dat$X[1:5, , drop = FALSE]
  pred_new <- predict(model, newdata)
  expect_true(is.numeric(pred_new))
  expect_equal(length(pred_new), nrow(newdata))
  expect_valid_predictions(pred_new, nrow(newdata), type = "response")
})

test_that("squared_error accuracy is MSE", {
  dat <- regression_dataset(n = 20, p = 2)
  model <- safe_treefarms(dat$X, dat$y,
                         loss_function = "squared_error",
                         regularization = 0.2,
                         store_training_data = TRUE,
                         verbose = FALSE)
  expect_true(is.numeric(model$accuracy))
  expect_true(model$accuracy >= 0)
  mse_manual <- mean((dat$y - model$predictions)^2)
  expect_equal(model$accuracy, mse_manual, tolerance = 1e-10)
})

test_that("refit_structure_on_data works for regression", {
  dat <- regression_dataset(n = 20, p = 2)
  model <- safe_treefarms(dat$X, dat$y,
                         loss_function = "squared_error",
                         regularization = 0.1,
                         single_tree = TRUE,
                         verbose = FALSE)
  trees <- get_rashomon_trees(model)
  skip_if(is.null(trees) || length(trees) < 1L, "No Rashomon trees to refit")
  t1 <- trees[[1]]
  expect_no_error({
    refit <- refit_structure_on_data(t1, dat$X, dat$y)
  })
  expect_true(is.list(refit))
})

teardown_test_environment()
